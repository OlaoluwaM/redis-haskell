module Helpers where

import Data.ByteString (ByteString)
import Data.Vector (fromList)
import RESP.Parser (serializeRESPDataType)
import RESP.Types
import "hs-redis-clone" Helpers (toOptionString)

mkCmdReqStr :: [RESPDataType] -> ByteString
mkCmdReqStr = serializeRESPDataType . MkArrayResponse . Array . fromList

mkCmdRESPRepr :: ByteString -> RESPDataType
mkCmdRESPRepr = MkBulkStringResponse . BulkString

mkBulkString :: ByteString -> RESPDataType
mkBulkString = MkBulkStringResponse . BulkString

bulkStrToOptionString :: [BulkString] -> ByteString
bulkStrToOptionString = toOptionString
