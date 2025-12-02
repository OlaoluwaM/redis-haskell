module Redis.RDB.Load (
    RDBStateInfo (..),
    loadRDBFile,
) where

import Redis.RDB.Data
import Redis.RDB.Format
import Redis.Store.Data

import Data.HashMap.Strict qualified as HashMap

import Data.Foldable (foldl')
import Data.List.NonEmpty qualified as NE
import Redis.ServerState (Store, StoreKey (..), StoreValue (..))

data RDBStateInfo = RDBStateInfo
    { redisVer :: Maybe RedisVersion
    , redisBits :: Maybe RedisBits
    , creationTime :: Maybe CTime
    , usedMem :: Maybe UsedMem
    }
    deriving stock (Eq, Show)

loadRDBFile :: RDBFile -> (Store, RDBStateInfo)
loadRDBFile rdbFile = (store, stateInfo)
  where
    -- We only support loading the first RDB entry from the RDB file since our server doesn't support multiple databases
    store = maybe HashMap.empty (HashMap.fromList . loadKeyValueEntries . NE.head) (NE.nonEmpty rdbFile.dbEntries)
    stateInfo = foldl' loadRDBStateInfoFromAuxFields (RDBStateInfo{redisVer = Nothing, redisBits = Nothing, creationTime = Nothing, usedMem = Nothing}) rdbFile.auxFieldEntries

loadKeyValueEntries :: RDbEntry -> [(StoreKey, StoreValue)]
loadKeyValueEntries RDbEntry{keyValEntries} = map loadKeyValueEntry keyValEntries

loadKeyValueEntry :: KeyValueOpCode -> (StoreKey, StoreValue)
loadKeyValueEntry (FCOpCode KeyValWithExpiryInMS{..}) =
    let key = StoreKey $ fromRDBStringOrIntVal encodedKey
        val = fromRDBStringOrIntVal encodedValue
        ttl = toUnixTimestampMSFromRDBUnixTimestampMS expiryTimeMs
     in ( key
        , StoreValue
            { value = MkRedisStr (RedisStr val)
            , ttlTimestamp = Just ttl
            }
        )
loadKeyValueEntry (FDOpcode KeyValWithExpiryInS{..}) =
    let key = StoreKey $ fromRDBStringOrIntVal encodedKey
        val = fromRDBStringOrIntVal encodedValue
        ttl = toUnixTimestampMSFromRDBUnixTimestampS expiryTimeS
     in ( key
        , StoreValue
            { value = MkRedisStr (RedisStr val)
            , ttlTimestamp = Just ttl
            }
        )
loadKeyValueEntry (KeyValOpCode KeyValWithNoExpiryInfo{..}) =
    let key = StoreKey $ fromRDBStringOrIntVal encodedKey
        val = fromRDBStringOrIntVal encodedValue
     in (key, StoreValue{value = MkRedisStr (RedisStr val), ttlTimestamp = Nothing})

loadRDBStateInfoFromAuxFields :: RDBStateInfo -> AuxField -> RDBStateInfo
loadRDBStateInfoFromAuxFields currentStateInfo (AuxFieldRedisVer redisVer) =
    currentStateInfo{redisVer = Just redisVer}
loadRDBStateInfoFromAuxFields currentStateInfo (AuxFieldRedisBits redisBits) =
    currentStateInfo{redisBits = Just redisBits}
loadRDBStateInfoFromAuxFields currentStateInfo (AuxFieldCTime cTime) =
    currentStateInfo{creationTime = Just cTime}
loadRDBStateInfoFromAuxFields currentStateInfo (AuxFieldUsedMem usedMem) =
    currentStateInfo{usedMem = Just usedMem}
loadRDBStateInfoFromAuxFields stateInfo _ = stateInfo
