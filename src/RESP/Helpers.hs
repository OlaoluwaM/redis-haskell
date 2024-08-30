module RESP.Helpers where

import RESP.Types

import Data.ByteString (ByteString)
import Data.Vector (fromList)

mkNonNullBulkString :: ByteString -> RESPDataType
mkNonNullBulkString = MkBulkStringResponse . BulkString

mkNonNullRESPArray :: [RESPDataType] -> RESPDataType
mkNonNullRESPArray = MkArrayResponse . Array . fromList
