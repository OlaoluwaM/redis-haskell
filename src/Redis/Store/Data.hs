module Redis.Store.Data (
    RedisDataType (..),
    RedisHashMap (..),
    RedisList (..),
    RedisStr (..),
    getStrTypeRepForRedisDataType,
) where

import Data.ByteString (ByteString)
import Data.Data (Data, Typeable)
import Data.HashMap.Strict (HashMap)
import Data.Sequence (Seq)
import Data.String (IsString)

-- https://redis.io/docs/latest/develop/data-types/
data RedisDataType = MkRedisStr RedisStr | MkRedisList RedisList | MkRedisHash (HashMap ByteString ByteString)
    deriving stock (Eq, Show, Typeable, Data)

newtype RedisStr = RedisStr ByteString
    deriving stock (Eq, Show, Data)

newtype RedisList = RedisList (Seq ByteString)
    deriving stock (Eq, Show, Data)

newtype RedisHashMap = RedisHashMap (HashMap ByteString ByteString)
    deriving stock (Eq, Show, Data)

getStrTypeRepForRedisDataType :: (IsString a) => RedisDataType -> a
getStrTypeRepForRedisDataType (MkRedisStr _) = "String"
getStrTypeRepForRedisDataType (MkRedisList _) = "List"
getStrTypeRepForRedisDataType (MkRedisHash _) = "Hash"
