module Commands.Handler (handleCommand) where

import Commands.Types
import RESP.Parser
import RESP.Types

import Data.Text qualified as T

import Commands.Parser (commandParser)
import Data.Attoparsec.ByteString (parseOnly)
import PyF (fmt)
import Utils (fromEither, mapLeft)

-- I can return two things here, a response to send to the client and a set of STM actions
handleCommand :: RawCommandRequest -> RawCommandResponse
handleCommand = serializeRESPDataType . execCommand . fromEither . mapLeft (InvalidCommand . T.pack) . parseOnly commandParser

execCommand :: Command -> RESPDataType
execCommand (Ping (Just txt)) = MkBulkStringResponse $ BulkString txt
execCommand (Ping Nothing) = SimpleString "PONG"
execCommand (Echo txt) = MkBulkStringResponse $ BulkString txt
execCommand (InvalidCommand msg) = MkBulkStringResponse $ BulkString [fmt|Invalid Command: {msg}|]
