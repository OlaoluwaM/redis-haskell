module Redis.RDB.Serialize where

import Redis.RDB.Format
import Redis.Store.Data

import Data.HashMap.Strict qualified as HashMap
import Data.Text.Encoding qualified as T

import Data.Default (Default (..))
import Data.Maybe (isJust)
import Data.Time.Clock.POSIX (POSIXTime, utcTimeToPOSIXSeconds)
import Redis.RDB.Data (fromPosixTimeToRDBUnixTimestampMS, fromPosixTimeToRDBUnixTimestampS, toRDBString, toRDBStringOrIntVal)
import Redis.Server.Settings (
    ServerSettings (ServerSettings),
    Setting (setting),
    SettingValue,
    serializeSettingsValue,
 )
import Redis.Server.Version (redisVersion)
import Redis.Store (Store, StoreKey (..), StoreValue (..), TTLTimestamp (..))
import Rerefined (unsafeRefine)

serializeRedisStore :: POSIXTime -> ServerSettings -> Store -> RDBFile
serializeRedisStore creationTime settings store =
    let totalNumOfKeys = calcNumOfKeysInStore store
        numOfKeysWithTTL = calculateNumOfKeysWithTTLInStore store
        keyValEntries = mkRDBKeyValEntries store
     in RDBFile
            { magicString = Redis
            , version = RDBv7
            , auxFieldEntries = mkAuxFields settings creationTime
            , dbEntries = [RDbEntry{resizeDBEntry = ResizeDB totalNumOfKeys numOfKeysWithTTL, keyValEntries, entryId = def}]
            }

mkAuxFields :: ServerSettings -> POSIXTime -> [AuxField]
mkAuxFields (ServerSettings settings) creationTime =
    [ AuxFieldRedisVer (RedisVersion $ unsafeRefine redisVersion)
    , AuxFieldRedisBits RedisBits64 -- TODO: Is there any way to determine whether a program is being run on a 32 bit system vs on a 64 bit system?
    , AuxFieldCTime . CTime . fromPosixTimeToRDBUnixTimestampS $ creationTime
    ]
        <> fmap (uncurry serializeSetting) (HashMap.toList settings)
  where
    serializeSetting :: Setting -> SettingValue -> AuxField
    serializeSetting setting val = AuxFieldCustom (toRDBString $ T.encodeUtf8 setting.setting) (toRDBStringOrIntVal . serializeSettingsValue $ val)

calcNumOfKeysInStore :: Store -> Word
calcNumOfKeysInStore store = fromIntegral $ length $ HashMap.keys store

calculateNumOfKeysWithTTLInStore :: Store -> Word
calculateNumOfKeysWithTTLInStore = calcNumOfKeysInStore . HashMap.filter hasTTL
  where
    hasTTL :: StoreValue -> Bool
    hasTTL StoreValue{ttlTimestamp} = isJust ttlTimestamp

mkRDBKeyValEntries :: Store -> [KeyValueOpCode]
mkRDBKeyValEntries store = map (uncurry mkRDBKeyValEntry) (HashMap.toList store)
  where
    mkRDBKeyValEntry :: StoreKey -> StoreValue -> KeyValueOpCode
    mkRDBKeyValEntry key storeVal = do
        case (storeVal.value, storeVal.ttlTimestamp) of
            (MkRedisStr strVal, Just ttl) -> mkKeyValStrEntryForValWithExpiry key strVal ttl
            (MkRedisStr strVal, Nothing) -> mkKeyValStrEntryForValWithoutExpiry key strVal
            x -> error $ "Only string values are supported right now in the Redis store. So this error really shouldn't have happened " <> show x

    mkKeyValStrEntryForValWithExpiry :: StoreKey -> RedisStr -> TTLTimestamp -> KeyValueOpCode
    mkKeyValStrEntryForValWithExpiry (StoreKey key) (RedisStr val) (TTLTimestamp ttl _) =
        let rdbStringKey = toRDBStringOrIntVal key
            rdbStringVal = toRDBStringOrIntVal val
         in FCOpCode $
                KeyValWithExpiryInMS
                    { valueType = Str
                    , encodedKey = rdbStringKey
                    , encodedValue = rdbStringVal
                    , expiryTimeMs = fromPosixTimeToRDBUnixTimestampMS . utcTimeToPOSIXSeconds $ ttl
                    }

    mkKeyValStrEntryForValWithoutExpiry :: StoreKey -> RedisStr -> KeyValueOpCode
    mkKeyValStrEntryForValWithoutExpiry (StoreKey key) (RedisStr val) =
        let rdbStringKey = toRDBStringOrIntVal key
            rdbStringVal = toRDBStringOrIntVal val
         in KeyValOpCode $
                KeyValWithNoExpiryInfo
                    { valueType = Str
                    , encodedKey = rdbStringKey
                    , encodedValue = rdbStringVal
                    }

-- deserializeToRedisStore :: RDBFile -> (Store, ServerSettings)
-- deserializeToRedisStore rdbFile = (store, settings)
--   where
--     store = HashMap.fromList $ concatMap deserializeDBEntry rdbFile.dbEntries
--     settings = ServerSettings $ HashMap.fromList $ map deserializeAuxField rdbFile.auxFieldEntries

--     deserializeDBEntry :: RDbEntry -> [(StoreKey, TVar StoreValue)]
--     deserializeDBEntry (RDbEntry{keyValEntries}) = map deserializeKeyValEntry keyValEntries

--     deserializeKeyValEntry :: KeyValueOpCode -> (StoreKey, TVar StoreValue)
--     deserializeKeyValEntry (FCOpCode KeyValWithExpiryInMS{..}) =
--         let key = fromRDBStringOrIntVal encodedKey
--             val = fromRDBStringOrIntVal encodedValue
--             ttl = fromRDBUnixTimestampMSToPOSIXTime expiryTimeMs
--          in (key, newTVarIO $ StoreValue{value = MkRedisStr val, ttlTimestamp = Just ttl})
--     deserializeKeyValEntry (KeyValOpCode KeyValWithNoExpiryInfo{..}) =
--         let key = fromRDBStringOrIntVal encodedKey
--             val = fromRDBStringOrIntVal encodedValue
--          in (key, newTVarIO $ StoreValue{value = MkRedisStr val, ttlTimestamp = Nothing})

--     deserializeAuxField :: AuxField -> (Setting, SettingValue)
--     deserializeAuxField (AuxFieldCustom key val) = (Setting key, SettingValue val)
--     deserializeAuxField _ = error "Unsupported aux field type"
