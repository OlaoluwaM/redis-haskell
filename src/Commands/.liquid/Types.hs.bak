module Commands.Types where

import Data.ByteString (ByteString)
import Data.Text (Text)
import RESP.Types (BulkString)

type RawCommandRequest = ByteString
type RawCommandResponse = ByteString

data ParsedCommandRequest = ParsedCommandRequest {command :: BulkString, args :: [BulkString]} deriving (Eq, Show)

-- All the commands
-- https://redis.io/docs/latest/commands/
data Command = Ping (Maybe Text) | Echo Text | Set SetCmd | Get GetCmd | InvalidCommand Text deriving (Eq, Show)

data SetCmd = SetCmd {key :: Text, val :: Text} deriving (Eq, Show)
newtype GetCmd = GetCmd {key :: Text} deriving (Eq, Show)
