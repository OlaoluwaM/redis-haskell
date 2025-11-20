module Redis.Commands.SaveSpec where

import Path
import Redis.Commands.BGSave
import Redis.RDB.Config
import Redis.Server.Settings
import Redis.ServerState
import Redis.Store.Data
import Test.Hspec

import Data.ByteString qualified as BSC
import Data.HashMap.Strict qualified as HashMap
import Hedgehog qualified as H
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Redis.RDB.Binary qualified as Binary

import Control.Concurrent.STM (atomically, newTMVar, newTVar, readTVarIO)
import Data.Attoparsec.ByteString (parseOnly)
import Data.Foldable (for_)
import Data.Maybe (fromJust, fromMaybe)
import Data.String.Interpolate (i)
import Data.Text (Text)
import Data.Time (addUTCTime, getCurrentTime)
import Effectful qualified as Eff
import Effectful.FileSystem qualified as Eff
import GHC.Base (undefined)
import Redis.Commands.Parser (Command (..), commandParser)
import Redis.Handler (handleCommandReq)
import Redis.Helper (mkBulkString, mkCmdReqStr)
import Redis.RDB.Config (mkRDBConfig)
import Redis.RDB.Format (RDBFile (RDBFile))
import Redis.RDB.Load (loadRDBFile)
import Redis.RESP (RESPDataType (..), serializeRESPDataType)
import Redis.Server.Context (ServerContext)
import Redis.Test (PassableTestContext (..), runTestServer)
import Redis.Utils (myTracePrettyM)
import System.Directory (doesFileExist)

-- Helper function to check if a parsed command is an Echo command
isSaveCommand :: Command -> Bool
isSaveCommand Save = True
isSaveCommand _ = False

-- Helper function to check if a parsed command is invalid
isInvalidCommand :: Command -> Bool
isInvalidCommand (InvalidCommand _) = True
isInvalidCommand _ = False

saveCmd :: RESPDataType
saveCmd = mkBulkString "SAVE"

testRdbOutputDir :: Path Rel Dir
testRdbOutputDir = [reldir|test/Redis/Commands/Save/output|]

genTestSettingArgs :: (H.MonadGen m) => m MkTestSettingsArg
genTestSettingArgs = do
    useCompression <- Gen.bool
    generateChecksum <- Gen.bool
    -- TODO: Can we put a seed or unique identifier on this?
    let rdbFilename = [relfile|save_command_test_dump.rdb|]
    pure $
        MkTestSettingsArg
            { useCompression
            , generateChecksum
            , rdbFilename
            }

saveCmdPropTest :: H.Property
saveCmdPropTest = H.property $ do
    let expectedServerResponse = serializeRESPDataType (SimpleString "OK")
    mkTestSettingArgs@MkTestSettingsArg{rdbFilename} <- H.forAll genTestSettingArgs

    let initialStore =
            HashMap.fromList
                [
                    ( StoreKey "key1"
                    , mkStoreValue
                        (MkRedisStr . RedisStr $ "value1")
                        (read "2024-01-01 00:00:00 UTC")
                        Nothing
                    )
                ]

    initialServerState <- H.evalIO $ initializeServerState initialStore

    let setCmdReq = mkCmdReqStr [mkBulkString "SET", mkBulkString "key2", mkBulkString "value2"]

    H.evalIO $
        runTestServer
            (handleCommandReq @ServerContext setCmdReq)
            ( PassableTestContext
                { settings = Nothing
                , serverState = Just initialServerState
                }
            )

    let saveCmdReq = mkCmdReqStr [saveCmd]
    let testSettingsForSnapshot = mkTestSettings mkTestSettingArgs
    let testRdbConfigForSnapshot = mkRDBConfigFromTestSettingsArgs mkTestSettingArgs

    result <-
        H.evalIO $
            runTestServer
                (handleCommandReq @ServerContext saveCmdReq)
                ( PassableTestContext
                    { settings = Just testSettingsForSnapshot
                    , serverState = Just initialServerState
                    }
                )

    snapshotExists <- H.evalIO $ doesFileExist . toFilePath $ (testRdbOutputDir </> rdbFilename)
    snapshot <- H.evalIO $ Eff.runEff . Eff.runFileSystem $ Binary.decodeFile @_ @RDBFile testRdbConfigForSnapshot (toFilePath $ testRdbOutputDir </> rdbFilename)

    storeFromSnapshot <- H.evalIO $ do
        now <- getCurrentTime
        pure $ fst $ loadRDBFile now snapshot

    storeFromServerState <- H.evalIO $ getStoreFromServerState initialServerState

    snapshotExists H.=== True
    result H.=== expectedServerResponse
    storeFromSnapshot H.=== storeFromServerState

{-
    What do we want to do
    - Make a prop test for the SAVE command that checks:
        - That the RDB file is created
        - That the RDB file contains the expected data
        - That the server responds with the expected RESP response

    To check whether the snapshot is correct we'll want to load it back into a value of type `Store` and perform assertions and/or comparisons on it with the store used to create the snapshot

    Perhaps rather than accepting a list of [(StoreKey, StoreValue)], we should instead take in an initial value of type `Store` directly. This would make it easier to do comparisons and perform assertions

    I wonder how we can append a seed value to the prop test snapshot file names so that we can identify which snapshot corresponds to which test case. Or maybe we don't even need to keep the files around after the test run is complete? It would be nice though if the snapshot files were only kept if the test failed
-}

spec_save_cmd_tests :: Spec
spec_save_cmd_tests = do
    describe "Save Command Parser Tests" $ do
        it "should parse Save command" $ do
            let cmdReq = mkCmdReqStr [saveCmd]
            let result = parseOnly commandParser cmdReq
            result `shouldBe` Right Save

        it "should fail when unexpected argument is provided" $ do
            let cmdReq = mkCmdReqStr [saveCmd, mkBulkString "Hello"]
            let result = parseOnly commandParser cmdReq
            either (const True) isInvalidCommand result `shouldBe` True

        it "should consider too many arguments as invalid" $ do
            let cmdReq = mkCmdReqStr [saveCmd, mkBulkString "arg1", mkBulkString "arg2"]
            let result = parseOnly commandParser cmdReq
            either (const True) isInvalidCommand result `shouldBe` True

        -- TODO: We can probably make this into a property test that generates various casing combinations
        context "recognizes various SAVE command formats" $ do
            for_
                [ ("despite casing" :: Text, mkCmdReqStr [mkBulkString "SAVE"])
                , ("despite casing (2)", mkCmdReqStr [mkBulkString "SaVe"])
                , ("despite casing (3)", mkCmdReqStr [mkBulkString "save"])
                ]
                $ \(testDesc, input) ->
                    it [i|Can parse a SAVE command string #{testDesc}|] $ do
                        let result = parseOnly commandParser input
                        either (const False) isSaveCommand result `shouldBe` True

    fdescribe "SAVE Command Handler Tests" $ do
        it "can create a snapshot of the key value store" $ do
            let expected = serializeRESPDataType (SimpleString "OK")
            let rdbFilename = [relfile|save_command_test_dump.rdb|]

            let initialStore =
                    HashMap.fromList
                        [
                            ( StoreKey "key1"
                            , mkStoreValue
                                (MkRedisStr . RedisStr $ "value1")
                                (read "2024-01-01 00:00:00 UTC")
                                Nothing
                            )
                        ,
                            ( StoreKey "key3"
                            , mkStoreValue
                                (MkRedisStr . RedisStr $ "value1")
                                (read "2024-01-01 00:00:00 UTC")
                                (Just $ TTLTimestamp (read "2024-01-02 00:00:00 UTC") Milliseconds)
                            )
                        ]

            initialServerState <- initializeServerState initialStore

            let setCmdReq1 = mkCmdReqStr [mkBulkString "SET", mkBulkString "key2", mkBulkString "value2"]
            let setCmdReq2 = mkCmdReqStr [mkBulkString "SET", mkBulkString "key8", mkBulkString "value8", mkBulkString "PX", mkBulkString "100"]

            runTestServer
                (handleCommandReq @ServerContext setCmdReq1)
                ( PassableTestContext
                    { settings = Nothing
                    , serverState = Just initialServerState
                    }
                )

            runTestServer
                (handleCommandReq @ServerContext setCmdReq2)
                ( PassableTestContext
                    { settings = Nothing
                    , serverState = Just initialServerState
                    }
                )

            let saveCmdReq = mkCmdReqStr [saveCmd]
            let testSettingsArgs =
                    MkTestSettingsArg
                        { useCompression = False
                        , generateChecksum = True
                        , rdbFilename
                        }
            let testSettingsForSnapshot = mkTestSettings testSettingsArgs
            let testRdbConfigForSnapshot = mkRDBConfigFromTestSettingsArgs testSettingsArgs

            result <-
                runTestServer
                    (handleCommandReq @ServerContext saveCmdReq)
                    ( PassableTestContext
                        { settings = Just testSettingsForSnapshot
                        , serverState = Just initialServerState
                        }
                    )

            snapshotExists <- doesFileExist . toFilePath $ (testRdbOutputDir </> rdbFilename)
            snapshot <- Eff.runEff . Eff.runFileSystem $ Binary.decodeFile @_ @RDBFile testRdbConfigForSnapshot (toFilePath $ testRdbOutputDir </> rdbFilename)
            myTracePrettyM "Snapshot loaded from file:" snapshot

            storeFromSnapshot <- do
                now <- getCurrentTime
                pure $ fst $ loadRDBFile now snapshot

            myTracePrettyM "Store from snapshot loaded from file:" storeFromSnapshot

            storeFromServerState <- getStoreFromServerState initialServerState

            myTracePrettyM "Store from server state loaded from memory:" storeFromServerState

            storeFromSnapshot `shouldBe` storeFromServerState
            snapshotExists `shouldBe` True
            result `shouldBe` expected

data MkTestSettingsArg = MkTestSettingsArg
    { useCompression :: Bool
    , generateChecksum :: Bool
    , rdbFilename :: Path Rel File
    }
    deriving stock (Eq, Show)

mkTestSettings :: MkTestSettingsArg -> ServerSettings
mkTestSettings MkTestSettingsArg{..} =
    ServerSettings $
        HashMap.fromList
            [ (Setting "dir", DirPathVal . Rel $ testRdbOutputDir)
            , (Setting "dbfilename", FilePathVal . Rel $ rdbFilename)
            , (Setting "rdbcompression", BoolVal useCompression)
            , (Setting "rdbchecksum", BoolVal generateChecksum)
            ]

mkRDBConfigFromTestSettingsArgs :: MkTestSettingsArg -> RDBConfig
mkRDBConfigFromTestSettingsArgs MkTestSettingsArg{..} =
    mkRDBConfig
        MkRDBConfigArg
            { useCompression = useCompression
            , generateChecksum = generateChecksum
            }

initializeServerState :: Store -> IO ServerState
initializeServerState store = do
    atomically $ do
        lastRDBSaveCurrent <- newTMVar Nothing
        kvStore <- newTVar store
        lastRDBSave <- newTVar $ LastRDBSave lastRDBSaveCurrent Nothing
        pure $ ServerState kvStore lastRDBSave

getStoreFromServerState :: ServerState -> IO Store
getStoreFromServerState serverState = readTVarIO serverState.keyValueStoreRef
