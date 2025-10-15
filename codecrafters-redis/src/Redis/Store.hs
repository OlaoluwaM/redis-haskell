module Redis.Store (
    StoreState,
    StoreValue (..),
    StoreKey (..),
    TTLTimestamp (..),
    TTLPrecision (..),
    Store,
    genInitialStore,
    getItemFromStore,
    addItemToStore,
    mkStoreValue,
    getItemTTLValue,
) where

import Data.HashMap.Strict qualified as HashMap

import Control.Concurrent.STM (TVar)
import Data.ByteString (ByteString)
import Data.HashMap.Strict (HashMap)
import Data.Hashable (Hashable)
import Data.Time (UTCTime)
import Redis.Store.Data (RedisDataType)

data StoreValue = StoreValue {value :: RedisDataType, insertTime :: UTCTime, ttlTimestamp :: Maybe TTLTimestamp}
    deriving stock (Eq, Show)

data TTLTimestamp = TTLTimestamp
    { timestamp :: UTCTime
    , precision :: TTLPrecision
    }
    deriving stock (Eq, Show)

data TTLPrecision = Milliseconds | Seconds
    deriving stock (Eq, Show)

newtype StoreKey = StoreKey {key :: ByteString}
    deriving stock (Eq, Show)
    deriving newtype (Hashable)

type Store = HashMap StoreKey StoreValue
type StoreState = TVar Store

genInitialStore :: Store
genInitialStore = HashMap.empty

getItemFromStore :: StoreKey -> Store -> Maybe StoreValue
getItemFromStore = HashMap.lookup

addItemToStore :: StoreKey -> StoreValue -> Store -> Store
addItemToStore = HashMap.insert

mkStoreValue :: RedisDataType -> UTCTime -> Maybe TTLTimestamp -> StoreValue
mkStoreValue value insertTime ttl = StoreValue{value, insertTime, ttlTimestamp = ttl}

getItemTTLValue :: StoreValue -> Maybe UTCTime
getItemTTLValue = fmap (.timestamp) . (.ttlTimestamp)
