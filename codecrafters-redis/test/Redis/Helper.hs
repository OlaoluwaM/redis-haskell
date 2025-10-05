module Redis.Helper (
    mkCmdReqStr,
    mkBulkString,
    bulkStrToOptionString,
    encodeThenDecodeUsingRDBBinary,
    encodeThenDecodeOrFailUsingRDBBinary,
    pingCmd,
    echoCmd,
    setCmd,
    getCmd,
) where

import Redis.RDB.Binary


import Data.ByteString (ByteString)
import Data.Vector (fromList)
import Redis.RDB.TestConfig (RDBConfig)
import Redis.RESP (Array (..), BulkString (..), RESPDataType (MkArrayResponse, MkBulkStringResponse), serializeRESPDataType, toOptionString)

mkCmdReqStr :: [RESPDataType] -> ByteString
mkCmdReqStr = serializeRESPDataType . MkArrayResponse . Array . fromList

mkBulkString :: ByteString -> RESPDataType
mkBulkString = MkBulkStringResponse . BulkString

bulkStrToOptionString :: [BulkString] -> ByteString
bulkStrToOptionString = toOptionString

pingCmd :: RESPDataType
pingCmd = mkBulkString "PING"

echoCmd :: RESPDataType
echoCmd = mkBulkString "ECHO"

setCmd :: RESPDataType
setCmd = mkBulkString "SET"

getCmd :: RESPDataType
getCmd = mkBulkString "GET"

encodeThenDecodeUsingRDBBinary :: (RDBBinary a) => RDBConfig -> a -> a
encodeThenDecodeUsingRDBBinary rdbConfig = decode rdbConfig . encode rdbConfig

encodeThenDecodeOrFailUsingRDBBinary :: (RDBBinary a) => RDBConfig -> a -> Either String a
encodeThenDecodeOrFailUsingRDBBinary rdbConfig = decodeOrFail rdbConfig . encode rdbConfig
