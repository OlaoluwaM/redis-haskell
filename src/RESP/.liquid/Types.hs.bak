module RESP.Types where

import Data.Text (Text)
import Data.Vector (Vector)

-- Spec: https://redis.io/docs/latest/develop/reference/protocol-spec/

-- All supported RESP data types
data RESPDataType = SimpleString Text | MkBulkStringResponse BulkString | MkArrayResponse Array | RESPInteger Int deriving (Eq, Show)

data BulkString = BulkString Text | NullBulkString deriving (Eq, Show)

-- This type represents an RESP data type array as a response value, NOT as a carrier for a command/request.
data Array = Array (Vector RESPDataType) | NullArray deriving (Eq, Show)
