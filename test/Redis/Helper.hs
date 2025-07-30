module Redis.Helper (
    mkCmdReqStr,
    mkBulkString,
    bulkStrToOptionString,
    encodeThenDecode,
    pingCmd,
    echoCmd,
    setCmd,
    getCmd,
) where

import Data.Binary (Binary (get, put))
import Data.Binary.Get (runGet)
import Data.Binary.Put (runPut)
import Data.ByteString (ByteString)
import Data.Vector (fromList)
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

encodeThenDecode :: (Binary a) => a -> a
encodeThenDecode = runGet get . runPut . put
