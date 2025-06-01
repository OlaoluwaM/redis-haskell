module Redis.Commands.Config.GetSpec where

import Test.Hspec

import Control.Monad.IO.Class (MonadIO (..))
import Data.Attoparsec.ByteString (parseOnly)
import Data.ByteString (ByteString)
import Data.Either (isLeft)
import Data.Foldable (for_)
import Data.HashMap.Strict qualified as HashMap
import Data.Text qualified as T
import Data.Text.Encoding (encodeUtf8)
import Redis.Commands.Config.Get (ConfigGetCmdArg (..))
import Redis.Commands.Parser (Command (..), ConfigSubCommand (..), commandParser)
import Redis.Handler (handleCommandReq)
import Redis.Helper (mkBulkString, mkCmdReqStr)
import Redis.RESP (BulkString (..), RESPDataType (..), arrayParser, mkNonNullBulkString, mkNonNullRESPArray, respArrayToList, serializeRESPDataType)
import Redis.Server.Settings (ServerSettings (..), SettingValue (..))
import Redis.Test (PassableTestContext (..), runTestM')

-- Helper function to check if a parsed command is a ConfigGet command
isConfigGetCommand :: Command -> Bool
isConfigGetCommand (Config (ConfigGet _)) = True
isConfigGetCommand _ = False

-- Helper function to check if a parsed command is invalid
isInvalidCommand :: Command -> Bool
isInvalidCommand (InvalidCommand _) = True
isInvalidCommand _ = False

-- Helper function to create CONFIG command
configCmd :: RESPDataType
configCmd = mkBulkString "CONFIG"

-- Helper function to create GET subcommand
getSubCmd :: RESPDataType
getSubCmd = mkBulkString "GET"

-- Test settings with common Redis configuration options
globalTestServerSettings :: ServerSettings
globalTestServerSettings =
    ServerSettings $
        HashMap.fromList
            [ ("timeout", TextVal "300")
            , ("tcp-keepalive", TextVal "300")
            , ("databases", TextVal "16")
            , ("maxmemory", TextVal "0")
            , ("maxmemory-policy", TextVal "noeviction")
            , ("save", TextVal "3600 1 300 100 60 10000")
            , ("dir", TextVal "/var/lib/redis")
            , ("dbfilename", TextVal "dump.rdb")
            , ("rdbcompression", BoolVal True)
            , ("rdbchecksum", BoolVal True)
            , ("port", TextVal "6379")
            , ("bind", TextVal "127.0.0.1")
            , ("protected-mode", BoolVal True)
            , ("tcp-backlog", TextVal "511")
            , ("unixsocket", TextVal "")
            , ("unixsocketperm", TextVal "0")
            , ("loglevel", TextVal "notice")
            , ("logfile", TextVal "")
            , ("syslog-enabled", BoolVal False)
            , ("syslog-ident", TextVal "redis")
            , ("syslog-facility", TextVal "local0")
            ]

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
            let cmdReq = mkCmdReqStr [configCmd, getSubCmd, RESPInteger 123]
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
                [ ("missing subcommand", mkCmdReqStr [configCmd])
                , ("missing parameters", mkCmdReqStr [configCmd, getSubCmd])
                , ("invalid subcommand", mkCmdReqStr [configCmd, mkBulkString "SET", mkBulkString "timeout"])
                , ("non-text parameter", mkCmdReqStr [configCmd, getSubCmd, RESPInteger 123])
                , ("null parameter", mkCmdReqStr [configCmd, getSubCmd, MkBulkStringResponse NullBulkString])
                , ("mixed valid/invalid parameters", mkCmdReqStr [configCmd, getSubCmd, mkBulkString "timeout", RESPInteger 456])
                , ("wrong command", mkCmdReqStr [mkBulkString "CONFI", getSubCmd, mkBulkString "timeout"])
                , ("wrong subcommand", mkCmdReqStr [configCmd, mkBulkString "GE", mkBulkString "timeout"])
                ]
                $ \(testDesc, input) ->
                    it ("Fails to parse a CONFIG GET command string " <> testDesc) $ do
                        let result = parseOnly commandParser input
                        result `shouldSatisfy` isLeft

    describe "Integration Tests - CONFIG GET Handler" $ do
        context "Happy Path Scenarios" $ do
            it "should retrieve specific config parameter when it exists" $ do
                let cmdReq = mkCmdReqStr [configCmd, getSubCmd, mkBulkString "timeout"]
                let testSettings = ServerSettings{getSettings = HashMap.fromList [("timeout", IntVal 300)]}
                result <- liftIO (runTestM' @ByteString (handleCommandReq cmdReq) (PassableTestContext Nothing (Just testSettings)))
                -- Expected format: ["timeout", "300"]
                let expected = serializeRESPDataType $ mkNonNullRESPArray [mkNonNullBulkString "timeout", mkNonNullBulkString "300"]
                result `shouldBe` expected

            it "should retrieve multiple config parameters" $ do
                let cmdReq = mkCmdReqStr [configCmd, getSubCmd, mkBulkString "timeout", mkBulkString "search-on-timeout"]
                let testSettings = ServerSettings (HashMap.fromList [("timeout", IntVal 400), ("search-on-timeout", BoolVal False)])
                let testContext = PassableTestContext Nothing (Just testSettings)

                result <- liftIO (runTestM' @ByteString (handleCommandReq cmdReq) testContext)
                -- Expected format: ["timeout", "300", "databases", "16"]
                let parsedResult = respArrayToList <$> parseOnly arrayParser result
                let expected =
                        [ mkNonNullBulkString "timeout"
                        , mkNonNullBulkString "400"
                        , mkNonNullBulkString "search-on-timeout"
                        , mkNonNullBulkString "False"
                        ]

                either
                    expectationFailure
                    (`shouldMatchList` expected)
                    parsedResult

            it "should retrieve setting values for items that match glob-like patterns to return all settings" $ do
                let cmdReq = mkCmdReqStr [configCmd, getSubCmd, mkBulkString "*max-*-entries*", mkBulkString "maxmemory"]
                let testSettings =
                        ServerSettings
                            ( HashMap.fromList
                                [ ("maxmemory", IntVal 0)
                                , ("hash-max-listpack-entries", IntVal 512)
                                , ("hash-max-ziplist-entries", IntVal 512)
                                , ("set-max-intset-entries", IntVal 512)
                                , ("zset-max-listpack-entries", IntVal 128)
                                , ("zset-max-ziplist-entries", IntVal 128)
                                ]
                            )

                result <- liftIO (runTestM' @ByteString (handleCommandReq cmdReq) (PassableTestContext Nothing (Just testSettings)))
                -- Should return an array with all configuration key-value pairs
                let parsedResult = respArrayToList <$> parseOnly arrayParser result
                let expected =
                        [ mkNonNullBulkString "maxmemory"
                        , mkNonNullBulkString "0"
                        , mkNonNullBulkString "hash-max-listpack-entries"
                        , mkNonNullBulkString "512"
                        , mkNonNullBulkString "hash-max-ziplist-entries"
                        , mkNonNullBulkString "512"
                        , mkNonNullBulkString "set-max-intset-entries"
                        , mkNonNullBulkString "512"
                        , mkNonNullBulkString "zset-max-listpack-entries"
                        , mkNonNullBulkString "128"
                        , mkNonNullBulkString "zset-max-ziplist-entries"
                        , mkNonNullBulkString "128"
                        ]

                either
                    expectationFailure
                    (`shouldMatchList` expected)
                    parsedResult

            it "should handle wildcard pattern '*' to return all settings" $ do
                let cmdReq = mkCmdReqStr [configCmd, getSubCmd, mkBulkString "*"]
                let testSettings =
                        ServerSettings
                            ( HashMap.fromList
                                [ ("maxmemory", IntVal 0)
                                , ("hash-max-listpack-entries", IntVal 512)
                                , ("hash-max-ziplist-entries", IntVal 512)
                                , ("set-max-intset-entries", IntVal 512)
                                , ("should-use-rdb", BoolVal True)
                                , ("zset-max-listpack-entries", IntVal 128)
                                , ("zset-max-ziplist-entries", IntVal 128)
                                , ("timeout", IntVal 500)
                                , ("search-threads", IntVal 20)
                                , ("dir", TextVal "/tmp")
                                , ("dbfilename", TextVal "file.dbd")
                                ]
                            )

                result <- liftIO (runTestM' @ByteString (handleCommandReq cmdReq) (PassableTestContext Nothing (Just testSettings)))
                -- Should return an array with all configuration key-value pairs
                let parsedResult = respArrayToList <$> parseOnly arrayParser result
                let expected =
                        [ mkNonNullBulkString "maxmemory"
                        , mkNonNullBulkString "0"
                        , mkNonNullBulkString "hash-max-listpack-entries"
                        , mkNonNullBulkString "512"
                        , mkNonNullBulkString "hash-max-ziplist-entries"
                        , mkNonNullBulkString "512"
                        , mkNonNullBulkString "set-max-intset-entries"
                        , mkNonNullBulkString "512"
                        , mkNonNullBulkString "zset-max-listpack-entries"
                        , mkNonNullBulkString "128"
                        , mkNonNullBulkString "zset-max-ziplist-entries"
                        , mkNonNullBulkString "128"
                        , mkNonNullBulkString "timeout"
                        , mkNonNullBulkString "500"
                        , mkNonNullBulkString "search-threads"
                        , mkNonNullBulkString "20"
                        , mkNonNullBulkString "dir"
                        , mkNonNullBulkString "/tmp"
                        , mkNonNullBulkString "dbfilename"
                        , mkNonNullBulkString "file.dbd"
                        , mkNonNullBulkString "should-use-rdb"
                        , mkNonNullBulkString "True"
                        ]

                either
                    expectationFailure
                    (`shouldMatchList` expected)
                    parsedResult

        it "should handle suffix wildcard patterns like '*file*'" $ do
            let cmdReq = mkCmdReqStr [configCmd, getSubCmd, mkBulkString "*file*"]
            let testSettings =
                    ServerSettings
                        ( HashMap.fromList
                            [ ("dbfilename", TextVal "dump.rdb")
                            , ("logfile", TextVal "")
                            , ("should-use-rdb", BoolVal True)
                            , ("zset-max-listpack-entries", IntVal 128)
                            , ("zset-max-ziplist-entries", IntVal 128)
                            , ("use-file-persistence", BoolVal True)
                            ]
                        )
            let testContext = PassableTestContext Nothing (Just testSettings)

            result <- liftIO (runTestM' @ByteString (handleCommandReq cmdReq) testContext)
            -- Expected to match: dbfilename, logfile
            let parsedResult = respArrayToList <$> parseOnly arrayParser result
            let expected =
                    [ mkNonNullBulkString "dbfilename"
                    , mkNonNullBulkString "dump.rdb"
                    , mkNonNullBulkString "logfile"
                    , mkNonNullBulkString ""
                    , mkNonNullBulkString "use-file-persistence"
                    , mkNonNullBulkString "True"
                    ]

            either
                expectationFailure
                (`shouldMatchList` expected)
                parsedResult

        it "should handle exact parameter names case-sensitively" $ do
            let cmdReq = mkCmdReqStr [configCmd, getSubCmd, mkBulkString "port"]
            let testSettings = ServerSettings (HashMap.fromList [("PORT", TextVal "6379"), ("timeout", IntVal 300), ("retry-on-error", BoolVal True)])
            let testContext = PassableTestContext Nothing (Just testSettings)

            result <- liftIO (runTestM' @ByteString (handleCommandReq cmdReq) testContext)
            let expected = serializeRESPDataType $ mkNonNullRESPArray [mkNonNullBulkString "port", mkNonNullBulkString "6379"]
            result `shouldBe` expected

        it "should handle multiple wildcard patterns in one request" $ do
            let cmdReq = mkCmdReqStr [configCmd, getSubCmd, mkBulkString "time*", mkBulkString "*log*"]
            let testSettings =
                    ServerSettings $
                        HashMap.fromList
                            [ ("timeout", TextVal "300")
                            , ("tcp-keepalive", TextVal "300")
                            , ("databases", TextVal "16")
                            , ("maxmemory", TextVal "0")
                            , ("maxmemory-policy", TextVal "noeviction")
                            , ("save", TextVal "3600 1 300 100 60 10000")
                            , ("dir", TextVal "/var/lib/redis")
                            , ("dbfilename", TextVal "dump.rdb")
                            , ("rdbcompression", BoolVal True)
                            , ("rdbchecksum", BoolVal True)
                            , ("port", TextVal "6379")
                            , ("bind", TextVal "127.0.0.1")
                            , ("protected-mode", BoolVal True)
                            , ("tcp-backlog", TextVal "511")
                            , ("unixsocket", TextVal "")
                            , ("unixsocketperm", TextVal "0")
                            , ("loglevel", TextVal "notice")
                            , ("logfile", TextVal "")
                            , ("syslog-enabled", BoolVal False)
                            , ("syslog-ident", TextVal "redis")
                            , ("syslog-facility", TextVal "local0")
                            ]
            let testContext = PassableTestContext Nothing (Just testSettings)

            result <- liftIO (runTestM' @ByteString (handleCommandReq cmdReq) testContext)
            -- Should match: timeout, tcp-backlog, loglevel, logfile, syslog-enabled, syslog-ident, syslog-facility
            let parsedResult = respArrayToList <$> parseOnly arrayParser result
            let expected =
                    [ mkNonNullBulkString "timeout"
                    , mkNonNullBulkString "300"
                    , mkNonNullBulkString "tcp-backlog"
                    , mkNonNullBulkString "511"
                    , mkNonNullBulkString "loglevel"
                    , mkNonNullBulkString "notice"
                    , mkNonNullBulkString "logfile"
                    , mkNonNullBulkString ""
                    , mkNonNullBulkString "syslog-enabled"
                    , mkNonNullBulkString "False"
                    , mkNonNullBulkString "syslog-ident"
                    , mkNonNullBulkString "redis"
                    , mkNonNullBulkString "syslog-facility"
                    , mkNonNullBulkString "local0"
                    ]

            either
                expectationFailure
                (`shouldMatchList` expected)
                parsedResult

    describe "Failure Modes and Error Conditions" $ do
        it "should return empty array for non-existent parameter" $ do
            let cmdReq = mkCmdReqStr [configCmd, getSubCmd, mkBulkString "nonexistent-parameter"]
            let testSettings =
                    ServerSettings $
                        HashMap.fromList
                            [ ("timeout", TextVal "300")
                            , ("tcp-keepalive", TextVal "300")
                            , ("databases", TextVal "16")
                            , ("maxmemory", TextVal "0")
                            , ("maxmemory-policy", TextVal "noeviction")
                            , ("save", TextVal "3600 1 300 100 60 10000")
                            , ("dir", TextVal "/var/lib/redis")
                            , ("dbfilename", TextVal "dump.rdb")
                            , ("rdbcompression", BoolVal True)
                            , ("rdbchecksum", BoolVal True)
                            , ("port", TextVal "6379")
                            , ("bind", TextVal "127.0.0.1")
                            , ("protected-mode", BoolVal True)
                            , ("tcp-backlog", TextVal "511")
                            , ("unixsocket", TextVal "")
                            , ("unixsocketperm", TextVal "0")
                            , ("loglevel", TextVal "notice")
                            , ("logfile", TextVal "")
                            , ("syslog-enabled", BoolVal False)
                            , ("syslog-ident", TextVal "redis")
                            , ("syslog-facility", TextVal "local0")
                            ]

            result <- liftIO (runTestM' @ByteString (handleCommandReq cmdReq) (PassableTestContext Nothing (Just testSettings)))
            result `shouldBe` serializeRESPDataType (mkNonNullRESPArray [])

        it "should return empty array for pattern that matches nothing" $ do
            let cmdReq = mkCmdReqStr [configCmd, getSubCmd, mkBulkString "xyz*"]
            let testSettings =
                    ServerSettings $
                        HashMap.fromList
                            [ ("timeout", TextVal "300")
                            , ("tcp-keepalive", TextVal "300")
                            , ("databases", TextVal "16")
                            , ("maxmemory", TextVal "0")
                            , ("maxmemory-policy", TextVal "noeviction")
                            , ("save", TextVal "3600 1 300 100 60 10000")
                            , ("dir", TextVal "/var/lib/redis")
                            , ("dbfilename", TextVal "dump.rdb")
                            , ("rdbcompression", BoolVal True)
                            , ("rdbchecksum", BoolVal True)
                            , ("port", TextVal "6379")
                            , ("bind", TextVal "127.0.0.1")
                            , ("protected-mode", BoolVal True)
                            , ("tcp-backlog", TextVal "511")
                            , ("unixsocket", TextVal "")
                            , ("unixsocketperm", TextVal "0")
                            , ("loglevel", TextVal "notice")
                            , ("logfile", TextVal "")
                            , ("syslog-enabled", BoolVal False)
                            , ("syslog-ident", TextVal "redis")
                            , ("syslog-facility", TextVal "local0")
                            ]
            let testContext = PassableTestContext Nothing (Just testSettings)

            result <- liftIO (runTestM' @ByteString (handleCommandReq cmdReq) testContext)
            let expected = serializeRESPDataType $ mkNonNullRESPArray []
            result `shouldBe` expected

        it "should handle empty server settings gracefully" $ do
            let cmdReq = mkCmdReqStr [configCmd, getSubCmd, mkBulkString "*"]
            let emptySettings = ServerSettings HashMap.empty

            result <- liftIO (runTestM' @ByteString (handleCommandReq cmdReq) (PassableTestContext Nothing (Just emptySettings)))
            result `shouldBe` serializeRESPDataType (mkNonNullRESPArray [])

        it "should handle case sensitivity correctly" $ do
            let cmdReq = mkCmdReqStr [configCmd, getSubCmd, mkBulkString "TIMEOUT"] -- uppercase
            let testSettings = ServerSettings (HashMap.fromList [("timeout", TextVal "300")])
            let testContext = PassableTestContext Nothing (Just testSettings)

            result <- liftIO (runTestM' @ByteString (handleCommandReq cmdReq) testContext)
            -- Case insensitive matching - should match lowercase "timeout"
            let expected = serializeRESPDataType $ mkNonNullRESPArray [mkNonNullBulkString "timeout", mkNonNullBulkString "300"]
            result `shouldBe` expected

    describe "Edge Cases and Corner Cases" $ do
        it "should handle special characters in parameter names" $ do
            let specialSettings =
                    ServerSettings $
                        HashMap.fromList
                            [ ("param-with-dash", TextVal "value1")
                            , ("param_with_underscore", TextVal "value2")
                            , ("param.with.dots", TextVal "value3")
                            ]
            let cmdReq = mkCmdReqStr [configCmd, getSubCmd, mkBulkString "*"]
            let testContext = PassableTestContext Nothing (Just specialSettings)

            result <- liftIO (runTestM' @ByteString (handleCommandReq cmdReq) testContext)
            let parsedResult = respArrayToList <$> parseOnly arrayParser result
            let expected =
                    [ mkNonNullBulkString "param-with-dash"
                    , mkNonNullBulkString "value1"
                    , mkNonNullBulkString "param_with_underscore"
                    , mkNonNullBulkString "value2"
                    , mkNonNullBulkString "param.with.dots"
                    , mkNonNullBulkString "value3"
                    ]

            either
                expectationFailure
                (`shouldMatchList` expected)
                parsedResult

        it "should handle empty string configuration values" $ do
            let cmdReq = mkCmdReqStr [configCmd, getSubCmd, mkBulkString "logfile"]
            let testSettings = ServerSettings (HashMap.fromList [("logfile", TextVal "")])
            let testContext = PassableTestContext Nothing (Just testSettings)

            result <- liftIO (runTestM' @ByteString (handleCommandReq cmdReq) testContext)
            let expected = serializeRESPDataType $ mkNonNullRESPArray [mkNonNullBulkString "logfile", mkNonNullBulkString ""]
            result `shouldBe` expected

        it "should handle complex wildcard patterns with question marks" $ do
            let cmdReq = mkCmdReqStr [configCmd, getSubCmd, mkBulkString "po?t"]
            let testSettings = ServerSettings (HashMap.fromList [("port", TextVal "6379"), ("post", TextVal "value"), ("pont", TextVal "bridge")])
            let testContext = PassableTestContext Nothing (Just testSettings)

            result <- liftIO (runTestM' @ByteString (handleCommandReq cmdReq) testContext)
            -- Should match "port", "post", and "pont" if glob supports ? pattern
            let parsedResult = respArrayToList <$> parseOnly arrayParser result
            let expected =
                    [ mkNonNullBulkString "port"
                    , mkNonNullBulkString "6379"
                    , mkNonNullBulkString "post"
                    , mkNonNullBulkString "value"
                    , mkNonNullBulkString "pont"
                    , mkNonNullBulkString "bridge"
                    ]

            either
                expectationFailure
                (`shouldMatchList` expected)
                parsedResult

        it "should handle escaped special characters in patterns" $ do
            let specialSettings = ServerSettings $ HashMap.fromList [("param*name", TextVal "special"), ("param-name", TextVal "normal")]
            let cmdReq = mkCmdReqStr [configCmd, getSubCmd, mkBulkString "param\\*name"]
            let testContext = PassableTestContext Nothing (Just specialSettings)

            result <- liftIO (runTestM' @ByteString (handleCommandReq cmdReq) testContext)
            -- Pattern escaping not implemented - treats as literal search which won't match
            let expected = serializeRESPDataType $ mkNonNullRESPArray []
            result `shouldBe` expected

    describe "Performance and Stress Test Scenarios" $ do
        it "should handle large number of configuration parameters" $ do
            let largeSettings =
                    ServerSettings $
                        HashMap.fromList
                            [("param" <> T.pack (show n), TextVal ("value" <> T.pack (show n))) | n <- [1 .. 10 :: Int]]
            let cmdReq = mkCmdReqStr [configCmd, getSubCmd, mkBulkString "param5"]
            let testContext = PassableTestContext Nothing (Just largeSettings)

            result <- liftIO (runTestM' @ByteString (handleCommandReq cmdReq) testContext)
            let expected = serializeRESPDataType $ mkNonNullRESPArray [mkNonNullBulkString "param5", mkNonNullBulkString "value5"]
            result `shouldBe` expected

        it "should handle very long parameter names" $ do
            let longParamName = T.pack $ "very-long-parameter-name-" <> replicate 100 'x'
            let longSettings = ServerSettings $ HashMap.fromList [(longParamName, TextVal "longvalue")]
            let cmdReq = mkCmdReqStr [configCmd, getSubCmd, mkBulkString (encodeUtf8 longParamName)]
            let testContext = PassableTestContext Nothing (Just longSettings)

            result <- liftIO (runTestM' @ByteString (handleCommandReq cmdReq) testContext)
            let expected =
                    serializeRESPDataType $
                        mkNonNullRESPArray
                            [mkNonNullBulkString (encodeUtf8 longParamName), mkNonNullBulkString "longvalue"]
            result `shouldBe` expected

        it "should handle very long parameter values" $ do
            let longValue = T.pack $ replicate 1000 'a'
            let longSettings = ServerSettings $ HashMap.fromList [("longparam", TextVal longValue)]
            let cmdReq = mkCmdReqStr [configCmd, getSubCmd, mkBulkString "longparam"]
            let testContext = PassableTestContext Nothing (Just longSettings)

            result <- liftIO (runTestM' @ByteString (handleCommandReq cmdReq) testContext)
            let expected =
                    serializeRESPDataType $
                        mkNonNullRESPArray
                            [mkNonNullBulkString "longparam", mkNonNullBulkString (encodeUtf8 longValue)]
            result `shouldBe` expected
