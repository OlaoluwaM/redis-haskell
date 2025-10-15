module Redis.RDB.Serialize where

import Redis.RDB.Format
import Redis.Store.Data

import Data.HashMap.Strict qualified as HashMap
import Data.Text.Encoding qualified as T

import Control.Concurrent.STM (TVar, atomically, readTVar, readTVarIO)
import Control.Monad (filterM)
import Control.Monad.IO.Class (MonadIO (..))
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

serializeRedisStore :: (MonadIO m) => POSIXTime -> ServerSettings -> Store -> m RDBFile
serializeRedisStore creationTime settings store = do
    let totalNumOfKeys = calcNumOfKeysInStore store
    numOfKeysWithTTL <- calculateNumOfKeysWithTTLInStore store
    keyValEntries <- mkRDBKeyValEntries store
    pure $
        RDBFile
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

calculateNumOfKeysWithTTLInStore :: (MonadIO m) => Store -> m Word
calculateNumOfKeysWithTTLInStore store = do
    let foo = HashMap.toList store
    g <-
        filterM
            ( \(_, storeValTvar) -> liftIO $ atomically $ do
                StoreValue{ttlTimestamp} <- readTVar storeValTvar
                pure $ isJust ttlTimestamp
            )
            foo
    pure $ fromIntegral $ length g

mkRDBKeyValEntries :: forall m. (MonadIO m) => Store -> m [KeyValueOpCode]
mkRDBKeyValEntries store = mapM (uncurry mkRDBKeyValEntry) (HashMap.toList store)
  where
    mkRDBKeyValEntry :: StoreKey -> TVar StoreValue -> m KeyValueOpCode
    mkRDBKeyValEntry key storeValTvar = do
        storeVal <- liftIO $ readTVarIO storeValTvar
        case (storeVal.value, storeVal.ttlTimestamp) of
            (MkRedisStr strVal, Just ttl) -> pure $ mkKeyValStrEntryForValWithExpiry key strVal ttl
            (MkRedisStr strVal, Nothing) -> pure $ mkKeyValStrEntryForValWithoutExpiry key strVal
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
