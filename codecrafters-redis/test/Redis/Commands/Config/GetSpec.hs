module Redis.Commands.Config.GetSpec where

import Path
import Redis.RESP
import Test.Hspec

import Data.Attoparsec.ByteString (parseOnly)
import Data.ByteString (ByteString)
import Data.Either (isLeft)
import Data.Foldable (for_)
import Data.Text.Encoding (encodeUtf8)
import Redis.Commands.Config.Get (ConfigGetCmdArg (..))
import Redis.Commands.Parser (Command (..), ConfigSubCommand (..), commandParser)
import Redis.Handler (handleCommandReq)
import Redis.Helper (mkBulkString, mkCmdReqStr)
import Redis.Server.Config (RedisConfig, RedisConfigF (..))
import Redis.Server.Config.Defaults (DefaultRedisConfig (..), defaultRedisConfig)
import Redis.Server.Context (ServerContext)
import Redis.Test (PassableTestContext (..), runTestServer)
import Redis.Utils (ShowBS (showBs))

-- Helper function to check if a parsed command is a ConfigGet command
isConfigGetCommand :: Command -> Bool
isConfigGetCommand (Config (ConfigGet _)) = True
isConfigGetCommand _ = False

-- Helper function to create CONFIG command
configCmd :: RESPDataType
configCmd = mkBulkString "CONFIG"

-- Helper function to create GET subcommand
getSubCmd :: RESPDataType
getSubCmd = mkBulkString "GET"

{- | CONFIG GET now surfaces every field of 'RedisConfig' generically (see 'GNamedFields' in "Redis.Server.Config.Types"), keyed by the field's
actual Haskell record field name (e.g. "rdbFileDirPath", "port") rather than the shorter redis.conf-style names ("dir", "port") used elsewhere
for CLI/config-file parsing. That's a known, accepted divergence from real Redis's CONFIG GET output for now -- these tests assert on the names
the implementation actually produces, not on redis.conf naming.
-}
defaultTestConfig :: RedisConfig
defaultTestConfig = defaults
  where
    DefaultRedisConfig defaults = defaultRedisConfig

-- Runs CONFIG GET with the given patterns against a server seeded with the given config, returning the raw serialized RESP response
runConfigGet :: [RESPDataType] -> RedisConfig -> IO ByteString
runConfigGet patterns testConfig =
    runTestServer
        (handleCommandReq @ServerContext (mkCmdReqStr ([configCmd, getSubCmd] ++ patterns)))
        PassableTestContext{serverState = Nothing, metadata = Nothing, config = Just testConfig}

-- Parses a CONFIG GET response into its flattened list of name/value bulk strings, for order-independent comparison
parseConfigGetResult :: ByteString -> Either String [RESPDataType]
parseConfigGetResult = fmap respArrayToList . parseOnly arrayParser

spec_config_get_cmd_tests :: Spec
spec_config_get_cmd_tests = do
    describe "CONFIG GET Command Parser Tests" $ do
        it "should parse CONFIG GET command with single parameter" $ do
            let cmdReq = mkCmdReqStr [configCmd, getSubCmd, mkBulkString "timeout"]
            let result = parseOnly commandParser cmdReq
            result `shouldBe` Right (Config (ConfigGet (ConfigGetCmdArg ["timeout"])))

        for_ [("", ["timeout", "database"]), ("with wildcards", ["timeout", "maxmemory", "save*"])] $ \(desc, options) ->
            it ("should parse CONFIG GET command with multiple parameters " <> desc) $ do
                let cmdReq = mkCmdReqStr ([configCmd, getSubCmd] ++ map (mkBulkString . encodeUtf8) options)
                let result = parseOnly commandParser cmdReq
                result `shouldBe` Right (Config (ConfigGet (ConfigGetCmdArg options)))

        it "should be case-insensitive for CONFIG command name" $ do
            let cmdReq = mkCmdReqStr [mkBulkString "config", getSubCmd, mkBulkString "timeout"]
            let result = parseOnly commandParser cmdReq
            result `shouldSatisfy` either (const False) isConfigGetCommand

        it "should be case-insensitive for GET subcommand name" $ do
            let cmdReq = mkCmdReqStr [configCmd, mkBulkString "get", mkBulkString "timeout"]
            let result = parseOnly commandParser cmdReq
            result `shouldSatisfy` either (const False) isConfigGetCommand

        it "should be case-insensitive for both CONFIG and GET" $ do
            let cmdReq = mkCmdReqStr [mkBulkString "CoNfIg", mkBulkString "GeT", mkBulkString "timeout"]
            let result = parseOnly commandParser cmdReq
            result `shouldSatisfy` either (const False) isConfigGetCommand

        it "should fail when no subcommand provided" $ do
            let cmdReq = mkCmdReqStr [configCmd]
            let result = parseOnly commandParser cmdReq
            result `shouldSatisfy` isLeft

        it "should fail when no parameters provided to GET subcommand" $ do
            let cmdReq = mkCmdReqStr [configCmd, getSubCmd]
            let result = parseOnly commandParser cmdReq
            result `shouldSatisfy` isLeft

        it "should reject non-text parameters" $ do
            let cmdReq = mkCmdReqStr [configCmd, getSubCmd, RESPInteger (RESPInt 123)]
            let result = parseOnly commandParser cmdReq
            result `shouldSatisfy` isLeft

        it "should reject null bulk string as parameter" $ do
            let cmdReq = mkCmdReqStr [configCmd, getSubCmd, MkBulkStringResponse NullBulkString]
            let result = parseOnly commandParser cmdReq
            result `shouldSatisfy` isLeft

        context "recognizes various CONFIG GET command formats" $ do
            for_
                [ ("with single parameter", mkCmdReqStr [configCmd, getSubCmd, mkBulkString "timeout"])
                , ("with multiple parameters", mkCmdReqStr [configCmd, getSubCmd, mkBulkString "timeout", mkBulkString "databases", mkBulkString "maxmemory"])
                , ("with wildcard", mkCmdReqStr [configCmd, getSubCmd, mkBulkString "*"])
                , ("with pattern", mkCmdReqStr [configCmd, getSubCmd, mkBulkString "save*"])
                , ("case insensitive CONFIG", mkCmdReqStr [mkBulkString "config", getSubCmd, mkBulkString "timeout"])
                , ("case insensitive GET", mkCmdReqStr [configCmd, mkBulkString "get", mkBulkString "timeout"])
                , ("case insensitive both", mkCmdReqStr [mkBulkString "CONFIG", mkBulkString "GET", mkBulkString "timeout"])
                , ("mixed case", mkCmdReqStr [mkBulkString "CoNfIg", mkBulkString "GeT", mkBulkString "timeout"])
                ]
                $ \(testDesc, input) ->
                    it ("Can parse a CONFIG GET command string " <> testDesc) $ do
                        let result = parseOnly commandParser input
                        result `shouldSatisfy` either (const False) isConfigGetCommand

        context "rejects invalid CONFIG GET command formats" $ do
            for_
                [ ("invalid subcommand", mkCmdReqStr [configCmd, mkBulkString "SET", mkBulkString "timeout"])
                , ("mixed valid/invalid parameters", mkCmdReqStr [configCmd, getSubCmd, mkBulkString "timeout", RESPInteger (RESPInt 456)])
                , ("wrong command", mkCmdReqStr [mkBulkString "CONFI", getSubCmd, mkBulkString "timeout"])
                , ("wrong subcommand", mkCmdReqStr [configCmd, mkBulkString "GE", mkBulkString "timeout"])
                ]
                $ \(testDesc, input) ->
                    it ("Fails to parse a CONFIG GET command string " <> testDesc) $ do
                        let result = parseOnly commandParser input
                        result `shouldSatisfy` isLeft

    describe "Integration Tests - CONFIG GET Handler" $ do
        context "Happy Path Scenarios" $ do
            it "should retrieve a specific config field by exact name" $ do
                result <- runConfigGet [mkBulkString "port"] defaultTestConfig{port = 7000}
                result `shouldBe` serializeRESPDataType (mkNonNullRESPArray [mkNonNullBulkString "port", mkNonNullBulkString "7000"])

            it "should retrieve multiple config fields by exact name" $ do
                result <- runConfigGet [mkBulkString "port", mkBulkString "useRDBCompression"] defaultTestConfig
                let expected =
                        [ mkNonNullBulkString "port"
                        , mkNonNullBulkString "6379"
                        , mkNonNullBulkString "useRDBCompression"
                        , mkNonNullBulkString "False"
                        ]
                either expectationFailure (`shouldMatchList` expected) (parseConfigGetResult result)

            it "should match multiple fields sharing a glob prefix" $ do
                result <- runConfigGet [mkBulkString "rdb*"] defaultTestConfig
                let expected =
                        [ mkNonNullBulkString "rdbFileDirPath"
                        , mkNonNullBulkString "."
                        , mkNonNullBulkString "rdbFilenamePath"
                        , mkNonNullBulkString "dump.rdb"
                        ]
                either expectationFailure (`shouldMatchList` expected) (parseConfigGetResult result)

            it "should return every field for the wildcard pattern '*'" $ do
                result <- runConfigGet [mkBulkString "*"] defaultTestConfig
                let expected =
                        [ mkNonNullBulkString "rdbFileDirPath"
                        , mkNonNullBulkString "."
                        , mkNonNullBulkString "rdbFilenamePath"
                        , mkNonNullBulkString "dump.rdb"
                        , mkNonNullBulkString "useRDBCompression"
                        , mkNonNullBulkString "False"
                        , mkNonNullBulkString "genRdbChecksum"
                        , mkNonNullBulkString "True"
                        , mkNonNullBulkString "port"
                        , mkNonNullBulkString "6379"
                        ]
                either expectationFailure (`shouldMatchList` expected) (parseConfigGetResult result)

            it "should combine an exact name with a glob pattern in one request" $ do
                result <- runConfigGet [mkBulkString "port", mkBulkString "rdb*"] defaultTestConfig
                let expected =
                        [ mkNonNullBulkString "port"
                        , mkNonNullBulkString "6379"
                        , mkNonNullBulkString "rdbFileDirPath"
                        , mkNonNullBulkString "."
                        , mkNonNullBulkString "rdbFilenamePath"
                        , mkNonNullBulkString "dump.rdb"
                        ]
                either expectationFailure (`shouldMatchList` expected) (parseConfigGetResult result)

            it "should match a single-character wildcard with '?'" $ do
                result <- runConfigGet [mkBulkString "por?"] defaultTestConfig
                result `shouldBe` serializeRESPDataType (mkNonNullRESPArray [mkNonNullBulkString "port", mkNonNullBulkString "6379"])

    describe "Failure Modes and Error Conditions" $ do
        it "should return an empty array for a name that matches no field" $ do
            result <- runConfigGet [mkBulkString "nonexistent-parameter"] defaultTestConfig
            result `shouldBe` serializeRESPDataType (mkNonNullRESPArray [])

        it "should return an empty array for a pattern that matches no field" $ do
            result <- runConfigGet [mkBulkString "xyz*"] defaultTestConfig
            result `shouldBe` serializeRESPDataType (mkNonNullRESPArray [])

        it "should not match when case differs (Redis semantics)" $ do
            -- Pattern matching is case-sensitive; "PORT" doesn't match the real field "port"
            result <- runConfigGet [mkBulkString "PORT"] defaultTestConfig
            result `shouldBe` serializeRESPDataType (mkNonNullRESPArray [])

    describe "Edge Cases and Corner Cases" $ do
        it "should treat a backslash as a literal character rather than an escape" $ do
            -- This Glob variant doesn't implement backslash-escaping: "po\\*t" is literal 'p','o','\\' followed by a still-live
            -- wildcard '*' and literal 't' -- not an escaped literal '*'. Since "port" contains no backslash, it doesn't match.
            result <- runConfigGet [mkBulkString "po\\*t"] defaultTestConfig
            result `shouldBe` serializeRESPDataType (mkNonNullRESPArray [])

        it "should handle a very long RDB filename value without truncation" $ do
            let longFilenameStr = "very-long-rdb-filename-" <> replicate 200 'x' <> ".rdb"
            longFilename <- maybe (fail "Failed to parse long RDB filename") pure $ parseRelFile @Maybe longFilenameStr

            result <- runConfigGet [mkBulkString "rdbFilenamePath"] defaultTestConfig{rdbFilenamePath = longFilename}

            let expected = serializeRESPDataType $ mkNonNullRESPArray [mkNonNullBulkString "rdbFilenamePath", mkNonNullBulkString (showBs longFilename)]
            result `shouldBe` expected
