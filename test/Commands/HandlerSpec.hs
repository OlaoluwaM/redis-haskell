module Commands.HandlerSpec where

import Test.Hspec

import Commands.Handler

import Data.Attoparsec.ByteString (parseOnly)
import Data.Foldable (for_)
import Data.String.Interpolate (i)
import Data.Text (Text)
import Helpers (mkBulkString, mkCmdRESPRepr, mkCmdReqStr)
import RESP.Types (RESPDataType (RESPInteger))

isPingCommand :: Command -> Bool
isPingCommand (Ping _) = True
isPingCommand _ = False

isEchoCommand :: Command -> Bool
isEchoCommand (Echo _) = True
isEchoCommand _ = False

isGetCommand :: Command -> Bool
isGetCommand (Get _) = True
isGetCommand _ = False

isSetCommand :: Command -> Bool
isSetCommand (Set _) = True
isSetCommand _ = False

isInvalidCommand :: Command -> Bool
isInvalidCommand (InvalidCommand _) = True
isInvalidCommand _ = False

pingCmd :: RESPDataType
pingCmd = mkCmdRESPRepr "PING"

echoCmd :: RESPDataType
echoCmd = mkCmdRESPRepr "ECHO"

setCmd :: RESPDataType
setCmd = mkCmdRESPRepr "SET"

getCmd :: RESPDataType
getCmd = mkCmdRESPRepr "GET"

spec_tests :: Spec
spec_tests = do
    describe "Commands Parser" do
        -- https://redis.io/docs/latest/commands/ping/
        -- The PING command requires no arguments, but can optionally take one argument
        for_
            [ ("without an argument" :: Text, mkCmdReqStr [pingCmd])
            , ("with an argument", mkCmdReqStr [mkCmdRESPRepr "ping", mkBulkString "arg"])
            , ("despite casing", mkCmdReqStr [mkCmdRESPRepr "PINg", mkBulkString "arg"])
            , ("despite casing (3)", mkCmdReqStr [mkCmdRESPRepr "PiNg", mkBulkString "arg"])
            , ("despite casing (4)", mkCmdReqStr [mkCmdRESPRepr "Ping", mkBulkString "arg"])
            ]
            $ \(testDesc, input) ->
                it [i|Can parse a PING command string #{testDesc}|] do
                    let result = parseOnly commandParser input
                    either (const False) isPingCommand result

        for_
            [ ("when argument is invalid" :: Text, mkCmdReqStr [pingCmd, RESPInteger 10])
            , ("when command is malformed", mkCmdReqStr [mkCmdRESPRepr "PIN", mkBulkString "arg"])
            ,
                ( "when command str is passed with too many arguments"
                , mkCmdReqStr [mkCmdRESPRepr "Ping", mkBulkString "arg", mkBulkString "hello", mkBulkString "world"]
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
            , ("if it is valid (2)", mkCmdReqStr [mkCmdRESPRepr "ecHO", mkBulkString "Hello"])
            , ("despite casing", mkCmdReqStr [mkCmdRESPRepr "Echo", mkBulkString "Hello"])
            , ("despite casing (2)", mkCmdReqStr [mkCmdRESPRepr "EcHO", mkBulkString "Hello"])
            , ("despite casing (3)", mkCmdReqStr [mkCmdRESPRepr "echo", mkBulkString "Hello"])
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
            , ("when command is malformed", mkCmdReqStr [mkCmdRESPRepr "ech", mkBulkString "arg"])
            , ("when command is malformed (2)", mkCmdReqStr [mkCmdRESPRepr "eco", mkBulkString "arg"])
            , ("when command is not provided any arguments", mkCmdReqStr [echoCmd])
            ]
            $ \(testDesc, input) ->
                it [i|Fails to parse an ECHO command string #{testDesc}|] do
                    let result = parseOnly commandParser input
                    either (const True) isInvalidCommand result

        -- https://redis.io/docs/latest/commands/set/
        -- The SET command has 2 required arguments and 3 optional ones
        for_
            [ ("if it is valid" :: Text, mkCmdReqStr [setCmd, mkBulkString "state", mkBulkString "ohio"])
            , ("if it is valid (2)", mkCmdReqStr [mkCmdRESPRepr "Set", mkBulkString "state", mkBulkString "Georgia"])
            , ("despite casing", mkCmdReqStr [mkCmdRESPRepr "sEt", mkBulkString "state", mkBulkString "Georgia"])
            , ("despite casing (2)", mkCmdReqStr [mkCmdRESPRepr "sET", mkBulkString "state", mkBulkString "Georgia"])
            ]
            $ \(testDesc, input) ->
                it [i|Can parse a SET command string #{testDesc}|] do
                    let result = parseOnly commandParser input
                    either (const False) isSetCommand result

        for_
            [
                ( "if command is supplied too few arguments" :: Text
                , mkCmdReqStr [setCmd, mkBulkString "key"]
                )
            , ("when argument is invalid" :: Text, mkCmdReqStr [setCmd, RESPInteger 10])
            , ("when command is malformed", mkCmdReqStr [mkCmdRESPRepr "se", mkBulkString "arg"])
            , ("when command is not provided any arguments", mkCmdReqStr [setCmd])
            ]
            $ \(testDesc, input) ->
                it [i|Fails to parse a SET command string #{testDesc}|] do
                    let result = parseOnly commandParser input
                    either (const True) isInvalidCommand result

        -- https://redis.io/docs/latest/commands/get/
        -- GET command has only 1 required argument
        for_
            [ ("if it is valid" :: Text, mkCmdReqStr [getCmd, mkBulkString "state"])
            , ("if it is valid (2)", mkCmdReqStr [mkCmdRESPRepr "Get", mkBulkString "state"])
            , ("despite casing", mkCmdReqStr [mkCmdRESPRepr "gEt", mkBulkString "state"])
            , ("despite casing (2)", mkCmdReqStr [mkCmdRESPRepr "gET", mkBulkString "state"])
            ]
            $ \(testDesc, input) ->
                it [i|Can parse a GET command string #{testDesc}|] do
                    let result = parseOnly commandParser input
                    either (const False) isGetCommand result

        for_
            [
                ( "if command is supplied too many arguments" :: Text
                , mkCmdReqStr [getCmd, mkBulkString "key", mkBulkString "val", mkBulkString "NX", mkBulkString "GET", mkBulkString "KEEPTTL", mkBulkString "9394"]
                )
            , ("when argument is invalid" :: Text, mkCmdReqStr [getCmd, RESPInteger 10])
            , ("when command is malformed", mkCmdReqStr [mkCmdRESPRepr "ge", mkBulkString "arg"])
            , ("when command is not provided any arguments", mkCmdReqStr [getCmd])
            ]
            $ \(testDesc, input) ->
                it [i|Fails to parse a GET command string #{testDesc}|] do
                    let result = parseOnly commandParser input
                    either (const True) isInvalidCommand result
