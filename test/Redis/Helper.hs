module Redis.Helper (
    mkCmdReqStr,
    mkBulkString,
    bulkStrToOptionString,
    encodeThenDecodeRDBBinary,
    pingCmd,
    echoCmd,
    setCmd,
    getCmd,
) where

import Redis.RDB.Binary

import Data.Binary (Binary (get, put))
import Data.Binary.Get (runGet)
import Data.Binary.Put (runPut, runPutM)
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

encodeThenDecodeRDBBinary :: (RDBBinary a) => RDBConfig -> a -> a
encodeThenDecodeRDBBinary rdbConfig = fst . runGet (execRDBGet rdbConfig rdbGet) . snd . runPutM . execRDBPut rdbConfig . rdbPut
