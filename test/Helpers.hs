module Helpers where

import Data.ByteString (ByteString)
import Data.Vector (fromList)
import RESP.Parser (serializeRESPDataType)
import RESP.Types

mkCmdReqStr :: [RESPDataType] -> ByteString
mkCmdReqStr = serializeRESPDataType . MkArrayResponse . Array . fromList

mkCmdRESPRepr :: ByteString -> RESPDataType
mkCmdRESPRepr = MkBulkStringResponse . BulkString

mkBulkString :: ByteString -> RESPDataType
mkBulkString = MkBulkStringResponse . BulkString
