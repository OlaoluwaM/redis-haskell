module Redis.RDB.Serialize where

-- import Data.Time.Clock.POSIX
-- import Redis.RDB.Data (fromPosixTimeToRDBUnixTimestampS)
-- import Redis.RDB.Format
-- import Redis.Server.Settings
-- import Redis.Server.Version (redisVersion)
-- import Redis.Store (Store)

-- serializeRedisStore :: POSIXTime -> ServerSettings -> Store -> RDBFile
-- serializeRedisStore creationTime settings store =
--     RDBFile
--         { magicString = Redis
--         , version = RDBv7
--         , auxFieldEntries = mkAuxFields settings creationTime
--         , dbEntries = []
--         }

-- mkAuxFields :: ServerSettings -> POSIXTime -> [AuxField]
-- mkAuxFields (ServerSettings settings) creationTime =
--     [ AuxFieldRedisVer (RedisVersion redisVersion)
--     , AuxFieldRedisBits RedisBits64 -- TODO: Is there any way to determine whether a program is being run on a 32 bit system vs on a 64 bit system?
--     , AuxFieldCTime . CTime . fromPosixTimeToRDBUnixTimestampS $ creationTime
--     ]
--   where
--     serializeSetting :: Setting -> SettingValue -> AuxField
--     serializeSetting setting val = undefined
