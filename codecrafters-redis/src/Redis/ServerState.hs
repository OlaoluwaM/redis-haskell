module Redis.ServerState (
    StoreValue (..),
    StoreKey (..),
    TTLTimestamp (..),
    TTLPrecision (..),
    Store,
    ServerState (..),
    LastRDBSave (..),
    genInitialStore,
    genInitialServerStateEff,
    getItemFromStore,
    addItemToStore,
    mkStoreValue,
    getItemTTLValue,
) where

import Control.Concurrent.STM qualified as STM
import Data.HashMap.Strict qualified as HashMap

import Data.ByteString (ByteString)
import Data.HashMap.Strict (HashMap)
import Data.Hashable (Hashable)
import Data.Time (UTCTime)
import GHC.Generics (Generic)
import Redis.Store.Data (RedisDataType)

data ServerState = ServerState
    { keyValueStoreRef :: STM.TVar Store
    , lastRDBSaveRef :: STM.TVar LastRDBSave
    }

type Store = HashMap StoreKey StoreValue

newtype StoreKey = StoreKey {key :: ByteString}
    deriving stock (Eq, Show, Generic)
    deriving newtype (Hashable)

data StoreValue = StoreValue {value :: RedisDataType, insertTime :: UTCTime, ttlTimestamp :: Maybe TTLTimestamp}
    deriving stock (Eq, Show, Generic)

data TTLTimestamp = TTLTimestamp
    { timestamp :: UTCTime
    , precision :: TTLPrecision
    }
    deriving stock (Eq, Show, Generic)

data TTLPrecision = Milliseconds | Seconds
    deriving stock (Eq, Show, Generic)

data LastRDBSave = LastRDBSave
    { current :: STM.TMVar (Maybe UTCTime)
    {- ^ Timestamp of last RDB save operation
        We use a TMVar, 1 for compatibility with STM, and 2 to allow only one thread to perform the save operation at a time per https://redis.io/docs/latest/commands/bgsave/#:~:text=An%20error%20is%20returned%20if%20there%20is%20already%20a%20background%20save%20running%20or%20if%20there%20is%20another%20non%2Dbackground%2Dsave%20process%20running%2C%20specifically%20an%20in%2Dprogress%20AOF%20rewrite

        Also, we want this to be a `Maybe` to distinguish between when an RDB save has been performed before and when it hasn't been performed yet. If we ever end up with a `Maybe (Maybe UTCTime)` in this context, we do not want to `join`
    -}
    , previous :: Maybe UTCTime
    -- ^ Previous timestamp of last RDB save operation
    }
    deriving stock (Generic)

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

genInitialServerStateEff :: STM.STM ServerState
genInitialServerStateEff = do
    let initialLastRDBSave = Nothing
    kvStore <- STM.newTVar genInitialStore
    current <- STM.newTMVar initialLastRDBSave
    lastRDBSave <- STM.newTVar $ LastRDBSave current initialLastRDBSave
    pure $ ServerState kvStore lastRDBSave
