module Redis.RDB.Load where

import Redis.RDB.Data
import Redis.RDB.Format
import Redis.Store.Data

import Data.HashMap.Strict qualified as HashMap

import Data.Foldable (foldl')
import Data.List.NonEmpty qualified as NE
import Data.Time (UTCTime)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Redis.Store (Store, StoreKey (..), StoreValue (..), TTLPrecision (..), TTLTimestamp (..))

data RDBStateInfo = RDBStateInfo
    { redisVer :: Maybe RedisVersion
    , redisBits :: Maybe RedisBits
    , creationTime :: Maybe CTime
    , usedMem :: Maybe UsedMem
    }
    deriving stock (Eq, Show)

loadRDBFile :: UTCTime -> RDBFile -> (Store, RDBStateInfo)
loadRDBFile currentTime rdbFile = (store, stateInfo)
  where
    -- We only support loading the first RDB entry from the RDB file since our server doesn't support multiple databases
    store = maybe HashMap.empty (HashMap.fromList . loadKeyValueEntries currentTime . NE.head) (NE.nonEmpty rdbFile.dbEntries)
    stateInfo = foldl' loadRDBStateInfoFromAuxFields (RDBStateInfo{redisVer = Nothing, redisBits = Nothing, creationTime = Nothing, usedMem = Nothing}) rdbFile.auxFieldEntries

loadKeyValueEntries :: UTCTime -> RDbEntry -> [(StoreKey, StoreValue)]
loadKeyValueEntries insertTime RDbEntry{keyValEntries} = map (loadKeyValueEntry insertTime) keyValEntries

loadKeyValueEntry :: UTCTime -> KeyValueOpCode -> (StoreKey, StoreValue)
loadKeyValueEntry insertTime (FCOpCode KeyValWithExpiryInMS{..}) =
    let key = StoreKey $ fromRDBStringOrIntVal encodedKey
        val = fromRDBStringOrIntVal encodedValue
        ttl = toPosixTimeFromRDBUnixTimestampMS expiryTimeMs
     in ( key
        , StoreValue
            { value = MkRedisStr (RedisStr val)
            , insertTime
            , ttlTimestamp = Just TTLTimestamp{timestamp = posixSecondsToUTCTime ttl, precision = Milliseconds}
            }
        )
loadKeyValueEntry insertTime (FDOpcode KeyValWithExpiryInS{..}) =
    let key = StoreKey $ fromRDBStringOrIntVal encodedKey
        val = fromRDBStringOrIntVal encodedValue
        ttl = toPosixTimeFromRDBUnixTimestampS expiryTimeS
     in ( key
        , StoreValue
            { value = MkRedisStr (RedisStr val)
            , insertTime
            , ttlTimestamp = Just TTLTimestamp{timestamp = posixSecondsToUTCTime ttl, precision = Seconds}
            }
        )
loadKeyValueEntry insertTime (KeyValOpCode KeyValWithNoExpiryInfo{..}) =
    let key = StoreKey $ fromRDBStringOrIntVal encodedKey
        val = fromRDBStringOrIntVal encodedValue
     in (key, StoreValue{value = MkRedisStr (RedisStr val), insertTime, ttlTimestamp = Nothing})

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
