module Redis.Commands.ParserSpec where

import Redis.Commands.Parser

import Data.Attoparsec.ByteString (parseOnly)
import Data.Foldable (for_)
import Data.String.Interpolate (i)
import Data.Text (Text)
import Redis.Helper (echoCmd, mkBulkString, mkCmdReqStr, pingCmd)
import Redis.RESP (RESPDataType (..))
import Test.Hspec (Spec, describe, it)

isPingCommand :: Command -> Bool
isPingCommand (Ping _) = True
isPingCommand _ = False

isEchoCommand :: Command -> Bool
isEchoCommand (Echo _) = True
isEchoCommand _ = False

-- isGetCommand :: Command -> Bool
-- isGetCommand (Get _) = True
-- isGetCommand _ = False

-- isSetCommand :: Command -> Bool
-- isSetCommand (Set _) = True
-- isSetCommand _ = False

isInvalidCommand :: Command -> Bool
isInvalidCommand (InvalidCommand _) = True
isInvalidCommand _ = False

spec_command_parser_unit_tests :: Spec
spec_command_parser_unit_tests = do
    describe "Command Parser" do
        -- https://redis.io/docs/latest/commands/ping/
        -- The PING command requires no arguments, but can optionally take one argument
        for_
            [ ("without an argument" :: Text, mkCmdReqStr [pingCmd])
            , ("with an argument", mkCmdReqStr [mkBulkString "ping", mkBulkString "arg"])
            , ("despite casing", mkCmdReqStr [mkBulkString "PINg", mkBulkString "arg"])
            , ("despite casing (3)", mkCmdReqStr [mkBulkString "PiNg", mkBulkString "arg"])
            , ("despite casing (4)", mkCmdReqStr [mkBulkString "Ping", mkBulkString "arg"])
            ]
            $ \(testDesc, input) ->
                it [i|Can parse a PING command string #{testDesc}|] do
                    let result = parseOnly commandParser input
                    either (const False) isPingCommand result

        for_
            [ ("when argument is invalid" :: Text, mkCmdReqStr [pingCmd, RESPInteger 10])
            , ("when command is malformed", mkCmdReqStr [mkBulkString "PIN", mkBulkString "arg"])
            ,
                ( "when command str is passed with too many arguments"
                , mkCmdReqStr [mkBulkString "Ping", mkBulkString "arg", mkBulkString "hello", mkBulkString "world"]
                )
            ]
            $ \(tesDesc, input) ->
                it [i|Fails to parse a PING command string #{tesDesc}|] do
                    let result = parseOnly commandParser input
                    either (const True) isInvalidCommand result

        -- https://redis.io/docs/latest/commands/echo/
        -- The ECHO command requires, at most, 1 argument
        for_
            [ ("if it is valid" :: Text, mkCmdReqStr [echoCmd, mkBulkString "Hello, Redis!"])
            , ("if it is valid (2)", mkCmdReqStr [mkBulkString "ecHO", mkBulkString "Hello"])
            , ("despite casing", mkCmdReqStr [mkBulkString "Echo", mkBulkString "Hello"])
            , ("despite casing (2)", mkCmdReqStr [mkBulkString "EcHO", mkBulkString "Hello"])
            , ("despite casing (3)", mkCmdReqStr [mkBulkString "echo", mkBulkString "Hello"])
            ]
            $ \(testDesc, input) ->
                it [i|Can parse an ECHO command string #{testDesc}|] do
                    let result = parseOnly commandParser input
                    either (const False) isEchoCommand result

        for_
            [
                ( "if command is supplied too many arguments" :: Text
                , mkCmdReqStr [echoCmd, mkBulkString "Hello, Redis!", mkBulkString "Hello", mkBulkString "Hello, World!"]
                )
            , ("when argument is invalid" :: Text, mkCmdReqStr [echoCmd, RESPInteger 10])
            , ("when command is malformed", mkCmdReqStr [mkBulkString "ech", mkBulkString "arg"])
            , ("when command is malformed (2)", mkCmdReqStr [mkBulkString "eco", mkBulkString "arg"])
            , ("when command is not provided any arguments", mkCmdReqStr [echoCmd])
            ]
            $ \(testDesc, input) ->
                it [i|Fails to parse an ECHO command string #{testDesc}|] do
                    let result = parseOnly commandParser input
                    either (const True) isInvalidCommand result

-- https://redis.io/docs/latest/commands/set/
-- The SET command has 2 required arguments and 3 optional ones
-- for_
--     [ ("if it is valid" :: Text, mkCmdReqStr [setCmd, mkBulkString "state", mkBulkString "ohio"])
--     , ("if it is valid (2)", mkCmdReqStr [mkBulkString "Set", mkBulkString "state", mkBulkString "Georgia"])
--     , ("despite casing", mkCmdReqStr [mkBulkString "sEt", mkBulkString "state", mkBulkString "Georgia"])
--     , ("despite casing (2)", mkCmdReqStr [mkBulkString "sET", mkBulkString "state", mkBulkString "Georgia"])
--     ]
--     $ \(testDesc, input) ->
--         it [i|Can parse a SET command string #{testDesc}|] do
--             let result = parseOnly commandParser input
--             either (const False) isSetCommand result

-- for_
--     [
--         ( "if command is supplied too few arguments" :: Text
--         , mkCmdReqStr [setCmd, mkBulkString "key"]
--         )
--     , ("when argument is invalid" :: Text, mkCmdReqStr [setCmd, RESPInteger 10])
--     , ("when command is malformed", mkCmdReqStr [mkBulkString "se", mkBulkString "arg"])
--     , ("when command is not provided any arguments", mkCmdReqStr [setCmd])
--     ]
--     $ \(testDesc, input) ->
--         it [i|Fails to parse a SET command string #{testDesc}|] do
--             let result = parseOnly commandParser input
--             either (const True) isInvalidCommand result

-- https://redis.io/docs/latest/commands/get/
-- GET command has only 1 required argument
-- for_
--     [ ("if it is valid" :: Text, mkCmdReqStr [getCmd, mkBulkString "state"])
--     , ("if it is valid (2)", mkCmdReqStr [mkBulkString "Get", mkBulkString "state"])
--     , ("despite casing", mkCmdReqStr [mkBulkString "gEt", mkBulkString "state"])
--     , ("despite casing (2)", mkCmdReqStr [mkBulkString "gET", mkBulkString "state"])
--     ]
--     $ \(testDesc, input) ->
--         it [i|Can parse a GET command string #{testDesc}|] do
--             let result = parseOnly commandParser input
--             either (const False) isGetCommand result

-- for_
--     [
--         ( "if command is supplied too many arguments" :: Text
--         , mkCmdReqStr [getCmd, mkBulkString "key", mkBulkString "val", mkBulkString "NX", mkBulkString "GET", mkBulkString "KEEPTTL", mkBulkString "9394"]
--         )
--     , ("when argument is invalid" :: Text, mkCmdReqStr [getCmd, RESPInteger 10])
--     , ("when command is malformed", mkCmdReqStr [mkBulkString "ge", mkBulkString "arg"])
--     , ("when command is not provided any arguments", mkCmdReqStr [getCmd])
--     ]
--     $ \(testDesc, input) ->
--         it [i|Fails to parse a GET command string #{testDesc}|] do
--             let result = parseOnly commandParser input
--             either (const True) isInvalidCommand result
