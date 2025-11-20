module Redis.Commands.BGSaveSpec where

import Test.Hspec

import Data.Attoparsec.ByteString (parseOnly)
import Data.Foldable (for_)
import Data.String.Interpolate (i)
import Data.Text (Text)
import Redis.Commands.BGSave
import Redis.Commands.Parser (Command (..), commandParser)
import Redis.Handler (handleCommandReq)
import Redis.Helper (echoCmd, mkBulkString, mkCmdReqStr)
import Redis.RESP (RESPDataType (..), serializeRESPDataType)
import Redis.Server.Context (ServerContext)
import Redis.Test (PassableTestContext (..), runTestServer)

-- Helper function to check if a parsed command is an Echo command
isBgSaveCommand :: Command -> Bool
isBgSaveCommand (BGSave _) = True
isBgSaveCommand _ = False

-- Helper function to check if a parsed command is invalid
isInvalidCommand :: Command -> Bool
isInvalidCommand (InvalidCommand _) = True
isInvalidCommand _ = False

bgSaveCmd :: RESPDataType
bgSaveCmd = mkBulkString "BGSAVE"

spec_bg_save_cmd_tests :: Spec
spec_bg_save_cmd_tests = do
    describe "BGSave Command Parser Tests" $ do
        it "should parse BGSave command with required argument" $ do
            let cmdReq = mkCmdReqStr [bgSaveCmd]
            let result = parseOnly commandParser cmdReq
            result `shouldBe` Right (BGSave (BGSaveCmdArg Nothing))

        it "should fail when unexpected argument is provided" $ do
            let cmdReq = mkCmdReqStr [bgSaveCmd, mkBulkString "Hello"]
            let result = parseOnly commandParser cmdReq
            either (const True) isInvalidCommand result `shouldBe` True

        it "should consider too many arguments as invalid" $ do
            let cmdReq = mkCmdReqStr [bgSaveCmd, mkBulkString "arg1", mkBulkString "arg2"]
            let result = parseOnly commandParser cmdReq
            either (const True) isInvalidCommand result `shouldBe` True

        context "recognizes various BGSAVE command formats" $ do
            for_
                [ ("despite casing" :: Text, mkCmdReqStr [mkBulkString "BGSave"])
                , ("despite casing (2)", mkCmdReqStr [mkBulkString "BgSAvE"])
                , ("despite casing (3)", mkCmdReqStr [mkBulkString "bgsave"])
                ]
                $ \(testDesc, input) ->
                    it [i|Can parse a BGSAVE command string #{testDesc}|] $ do
                        let result = parseOnly commandParser input
                        either (const False) isBgSaveCommand result `shouldBe` True

    -- describe "BGSAVE Command Handler Tests" $ do
    --     it "should echo back the message" $ do
    --         let message = "Hello, Redis!"
    --         let cmdReq = mkCmdReqStr [bgSaveCmd]
    --         let expected = serializeRESPDataType (mkBulkString message)
    --         result <-
    --             runTestServer
    --                 (handleCommandReq @ServerContext cmdReq)
    --                 ( PassableTestContext
    --                     { settings = Nothing
    --                     , serverState = Nothing
    --                     }
    --                 )

    --         result `shouldBe` expected
