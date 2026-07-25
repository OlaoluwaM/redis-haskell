module Redis.ServerState (
    StoreValue (..),
    StoreKey (..),
    Store,
    ServerState (..),
    LastRDBSave (..),
    genInitialServerStateEff,
    getItemFromStore,
    addItemToStore,
    mkStoreValue,
    getItemTTLValue,
) where

import Redis.Store.Timestamp

import Control.Concurrent.STM qualified as STM
import Data.HashMap.Strict qualified as HashMap

import Data.ByteString (ByteString)
import Data.HashMap.Strict (HashMap)
import Data.Hashable (Hashable)
import Data.Maybe (fromMaybe)
import Data.Time (UTCTime)
import GHC.Generics (Generic)
import Redis.Store.Data (RedisDataType)

data ServerState = ServerState
    { keyValueStoreRef :: STM.TVar Store
    , lastRDBSaveRef :: STM.TVar LastRDBSave
    }

type Store = HashMap StoreKey StoreValue

-- Redis keys are binary safe strings https://redis.io/docs/latest/develop/using-commands/keyspace/#:~:text=Although,Redis,-%2E
newtype StoreKey = StoreKey {key :: ByteString}
    deriving stock (Eq, Show, Generic)
    deriving newtype (Hashable)

-- Redis does not store insert time/time of creation
data StoreValue = StoreValue {value :: RedisDataType, ttlTimestamp :: Maybe UnixTimestampMS}
    deriving stock (Eq, Show, Generic)

data LastRDBSave = LastRDBSave
    { saveLock :: STM.TMVar ()
    {- ^ Mutex guarding RDB saves: there can be only one active RDB save at a time, and this is held
        (emptied) for the duration of one per https://redis.io/docs/latest/commands/bgsave/#:~:text=An%20error%20is%20returned%20if%20there%20is%20already%20a%20background%20save%20running%20or%20if%20there%20is%20another%20non%2Dbackground%2Dsave%20process%20running%2C%20specifically%20an%20in%2Dprogress%20AOF%20rewrite
        We use a TMVar, 1 for compatibility with STM, and 2 to allow only one thread to perform the save operation at a time.

        This carries no payload: `lastCompleted` below is the sole source of truth for the last
        completed save time, readable at any time (including while a save is in progress and this
        lock is held/empty) so commands like LASTSAVE can always get a non-blocking answer.
    -}
    , lastCompleted :: Maybe UTCTime
    -- ^ Timestamp of last RDB save operation that completed successfully, if any
    }
    deriving stock (Generic)

getItemFromStore :: StoreKey -> Store -> Maybe StoreValue
getItemFromStore = HashMap.lookup

addItemToStore :: StoreKey -> StoreValue -> Store -> Store
addItemToStore = HashMap.insert

mkStoreValue :: RedisDataType -> Maybe UnixTimestampMS -> StoreValue
mkStoreValue value ttl = StoreValue{value, ttlTimestamp = ttl}

getItemTTLValue :: StoreValue -> Maybe UnixTimestampMS
getItemTTLValue = (.ttlTimestamp)

genInitialServerStateEff :: Maybe Store -> STM.STM ServerState
genInitialServerStateEff mStore = do
    kvStore <- STM.newTVar . fromMaybe HashMap.empty $ mStore
    saveLock <- STM.newTMVar ()
    lastRDBSave <- STM.newTVar $ LastRDBSave saveLock Nothing
    pure $ ServerState kvStore lastRDBSave
