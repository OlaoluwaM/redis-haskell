module Redis.Store (
    StoreState,
    StoreValue (..),
    StoreKey (..),
    Store,
    genInitialStore,
    getItemFromStore,
    addItemToStore,
    mkStoreValue,
) where

import Data.HashMap.Strict qualified as HashMap

import Control.Concurrent.STM (TVar)
import Data.ByteString (ByteString)
import Data.HashMap.Strict (HashMap)
import Data.Hashable (Hashable)
import Data.Time (UTCTime)
import Redis.Store.Data (RedisDataType)

data StoreValue = StoreValue {value :: RedisDataType, insertTime :: UTCTime, ttlTimestamp :: Maybe UTCTime}
    deriving stock (Eq, Show)

newtype StoreKey = StoreKey {key :: ByteString}
    deriving stock (Eq, Show)
    deriving newtype (Hashable)

type Store = HashMap StoreKey (TVar StoreValue)
type StoreState = TVar (HashMap StoreKey (TVar StoreValue))

genInitialStore :: Store
genInitialStore = HashMap.empty

getItemFromStore :: StoreKey -> Store -> Maybe (TVar StoreValue)
getItemFromStore = HashMap.lookup

addItemToStore :: StoreKey -> TVar StoreValue -> Store -> Store
addItemToStore = HashMap.insert

mkStoreValue :: RedisDataType -> UTCTime -> Maybe UTCTime -> StoreValue
mkStoreValue value insertTime ttl = StoreValue{value, insertTime, ttlTimestamp = ttl}
