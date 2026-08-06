module Redis.Commands.LastSaveSpec where

import Path
import Redis.RDB.Config
import Redis.Server.Config (RedisConfig, RedisConfigF (..))
import Redis.Server.Config.Defaults (DefaultRedisConfig (..), defaultRedisConfig)
import Redis.ServerState
import Redis.Store.Data
import Redis.Store.Timestamp
import Test.Hspec

import Data.HashMap.Strict qualified as HashMap

import Control.Concurrent.STM (atomically, check, modifyTVar, newTMVar, newTVar, putTMVar, readTVar, readTVarIO, takeTMVar)
import Control.Exception (finally)
import Control.Monad (when)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Attoparsec.ByteString (parseOnly)
import Data.Foldable (for_)
import Data.String.Interpolate (i)
import Data.Text (Text)
import Data.Time (UTCTime, getCurrentTime)
import Optics (set)
import Redis.Commands.Parser (Command (..), commandParser)
import Redis.Handler (handleCommandReq)
import Redis.Helper (bgSaveCmd, isInvalidCommand, mkBulkString, mkCmdReqStr, saveCmd)
import Redis.RESP (RESPDataType (..), RESPInt (..), respIntegerParser)
import Redis.Server.Context (ServerContext)
import Redis.Test (PassableTestContext (..), runTestServer)
import System.Directory (doesFileExist, removeFile)

isLastSaveCommand :: Command -> Bool
isLastSaveCommand LastSave = True
isLastSaveCommand _ = False

lastSaveCmd :: RESPDataType
lastSaveCmd = mkBulkString "LASTSAVE"

testRdbOutputDir :: Path Rel Dir
testRdbOutputDir = [reldir|test/Redis/Commands/LastSave/output|]

spec_last_save_cmd_tests :: Spec
spec_last_save_cmd_tests = do
    describe "LASTSAVE Command Parser Tests" $ do
        it "should parse Save command" $ do
            let cmdReq = mkCmdReqStr [lastSaveCmd]
            let result = parseOnly commandParser cmdReq
            result `shouldBe` Right LastSave

        it "should fail when unexpected argument is provided" $ do
            let cmdReq = mkCmdReqStr [lastSaveCmd, mkBulkString "Hello"]
            let result = parseOnly commandParser cmdReq
            either (const True) isInvalidCommand result `shouldBe` True

        it "should consider too many arguments as invalid" $ do
            let cmdReq = mkCmdReqStr [lastSaveCmd, mkBulkString "arg1", mkBulkString "arg2"]
            let result = parseOnly commandParser cmdReq
            either (const True) isInvalidCommand result `shouldBe` True

        context "recognizes various SAVE command formats" $ do
            for_
                [ ("despite casing" :: Text, mkCmdReqStr [mkBulkString "LASTSAVE"])
                , ("despite casing (2)", mkCmdReqStr [mkBulkString "LAsTSaVe"])
                , ("despite casing (3)", mkCmdReqStr [mkBulkString "lastsave"])
                , ("despite casing (4)", mkCmdReqStr [mkBulkString "lastsaVE"])
                ]
                $ \(testDesc, input) ->
                    it [i|Can parse a SAVE command string #{testDesc}|] $ do
                        let result = parseOnly commandParser input
                        either (const False) isLastSaveCommand result `shouldBe` True

    describe "LASTSAVE Command Handler Tests" $ do
        it "can retrieve the last time a snapshot was created (synchronously or asynchronously)" $ do
            let rdbFilename = [relfile|save_command_test_dump.rdb|]

            let initialStore =
                    HashMap.fromList
                        [
                            ( StoreKey "key1"
                            , mkStoreValue
                                (MkRedisStr . RedisStr $ "value1")
                                Nothing
                            )
                        ,
                            ( StoreKey "key3"
                            , mkStoreValue
                                (MkRedisStr . RedisStr $ "value1")
                                (Just $ mkUnixTimestampMSFromUTCTime (read "2024-01-02 00:00:00 UTC"))
                            )
                        ]

            initialServerState <- initializeServerState initialStore

            let setCmdReq1 = mkCmdReqStr [mkBulkString "SET", mkBulkString "key2", mkBulkString "value2"]
            let setCmdReq2 = mkCmdReqStr [mkBulkString "SET", mkBulkString "key8", mkBulkString "value8", mkBulkString "PX", mkBulkString "100"]

            runTestServer
                (handleCommandReq @ServerContext setCmdReq1)
                ( PassableTestContext
                    { config = Nothing
                    , serverState = Just initialServerState
                    , metadata = Nothing
                    }
                )

            runTestServer
                (handleCommandReq @ServerContext setCmdReq2)
                ( PassableTestContext
                    { config = Nothing
                    , serverState = Just initialServerState
                    , metadata = Nothing
                    }
                )

            let saveCmdReq = mkCmdReqStr [saveCmd]
            let lastSaveCmdReq = mkCmdReqStr [lastSaveCmd]
            let testSettingsArgs =
                    MkTestSettingsArg
                        { useCompression = False
                        , generateChecksum = True
                        , rdbFilename
                        }
            let testSettingsForSnapshot = mkTestSettings testSettingsArgs
            let rdbOutputPath = toFilePath (testRdbOutputDir </> rdbFilename)
            let testContext = PassableTestContext{config = Just testSettingsForSnapshot, serverState = Just initialServerState, metadata = Nothing}

            (`finally` removeFileIfExists rdbOutputPath) $ do
                runTestServer (handleCommandReq @ServerContext saveCmdReq) testContext

                resultAfterSyncSaveOp <- runTestServer (handleCommandReq @ServerContext lastSaveCmdReq) testContext

                unixTimestampMSNow <- fromIntegral . (.timestamp) . mkUnixTimestampMSFromUTCTime <$> liftIO getCurrentTime

                (RESPInt lastSaveTimestampAfterSyncSaveOp) <- either (fail . ("Server did not return with expected response of a timestamp: " <>)) pure $ parseOnly respIntegerParser resultAfterSyncSaveOp

                let timeDiff = unixTimestampMSNow - lastSaveTimestampAfterSyncSaveOp
                let timeDiffTolerance = 100 -- in milliseconds
                timeDiff `shouldSatisfy` (<= timeDiffTolerance)

                LastRDBSave{lastCompleted = lastCompletedBeforeBgSave} <- readTVarIO initialServerState.lastRDBSaveRef

                let bgSaveCmdReq = mkCmdReqStr [bgSaveCmd]
                runTestServer (handleCommandReq @ServerContext bgSaveCmdReq) testContext

                -- BGSAVE forks the actual save onto a background thread and returns immediately, so
                -- rather than racing it with the next LASTSAVE, wait for it to genuinely finish.
                -- `lastCompleted` only changes once, on the save's success path, so watching it for a
                -- transition away from its pre-BGSAVE value is a deterministic "wait for the save to
                -- complete" rather than hoping the background thread hasn't been scheduled yet. (Peeking
                -- `saveLock` instead doesn't work: it's already full from the earlier synchronous SAVE,
                -- so reading it can race ahead of the new BGSAVE's own take/put and return stale data.)
                atomically $ do
                    currentLastRDBSave <- readTVar initialServerState.lastRDBSaveRef
                    check (currentLastRDBSave.lastCompleted /= lastCompletedBeforeBgSave)

                resultAfterAsyncSaveOp <- runTestServer (handleCommandReq @ServerContext lastSaveCmdReq) testContext

                (RESPInt lastSaveTimestampAfterAsyncSaveOp) <-
                    either (fail . ("Server did not return with expected response of a timestamp: " <>)) pure $ parseOnly respIntegerParser resultAfterAsyncSaveOp

                -- The background save has now genuinely completed, so LASTSAVE should reflect it
                -- rather than the earlier synchronous save's timestamp.
                lastSaveTimestampAfterAsyncSaveOp `shouldSatisfy` (>= lastSaveTimestampAfterSyncSaveOp)

        it "returns the last completed save time without blocking while a save is in progress" $ do
            initialServerState <- initializeServerState HashMap.empty

            let baselineSaveTime = read "2024-01-02 00:00:00 UTC" :: UTCTime
            atomically $ modifyTVar initialServerState.lastRDBSaveRef (set #lastCompleted (Just baselineSaveTime))

            LastRDBSave{saveLock} <- readTVarIO initialServerState.lastRDBSaveRef
            atomically $ takeTMVar saveLock -- Simulate a save currently in progress, without actually running one

            result <-
                runTestServer
                    (handleCommandReq @ServerContext (mkCmdReqStr [lastSaveCmd]))
                    (PassableTestContext{config = Nothing, serverState = Just initialServerState, metadata = Nothing})
                    `finally` atomically (putTMVar saveLock ())

            (RESPInt lastSaveTimestampWhileInProgress) <-
                either (fail . ("Server did not return with expected response of a timestamp: " <>)) pure $ parseOnly respIntegerParser result

            let expectedTimestamp = fromIntegral . (.timestamp) . mkUnixTimestampMSFromUTCTime $ baselineSaveTime

            -- LASTSAVE only ever reads `lastCompleted`, which a save-in-progress never touches until it
            -- finishes, so this should return the baseline unchanged rather than block on the held lock.
            lastSaveTimestampWhileInProgress `shouldBe` expectedTimestamp

data MkTestSettingsArg = MkTestSettingsArg
    { useCompression :: Bool
    , generateChecksum :: Bool
    , rdbFilename :: Path Rel File
    }
    deriving stock (Eq, Show)

mkTestSettings :: MkTestSettingsArg -> RedisConfig
mkTestSettings MkTestSettingsArg{..} =
    defaults
        { rdbFileDirPath = Rel testRdbOutputDir
        , rdbFilenamePath = rdbFilename
        , useRDBCompression = useCompression
        , genRdbChecksum = generateChecksum
        }
  where
    defaults = defaultRedisConfig.redisConf

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
        saveLock <- newTMVar ()
        kvStore <- newTVar store
        lastRDBSave <- newTVar $ LastRDBSave saveLock Nothing
        pure $ ServerState kvStore lastRDBSave

getStoreFromServerState :: ServerState -> IO Store
getStoreFromServerState serverState = readTVarIO serverState.keyValueStoreRef

removeFileIfExists :: FilePath -> IO ()
removeFileIfExists path = do
    exists <- doesFileExist path
    when exists (removeFile path)
