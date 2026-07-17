module Redis.Server.Config.Defaults (defaultRedisConfig) where

import Path

import Redis.Server.Config.Types (RedisConfig, RedisConfigF (..))

newtype DefaultRedisConfig = DefaultRedisConfig {redisConf :: RedisConfig}
    deriving newtype (Show)

defaultRedisConfig :: DefaultRedisConfig
defaultRedisConfig =
    DefaultRedisConfig $
        RedisConfigF
            { rdbFilenamePath = [relfile|dump.rdb|]
            , useRDBCompression = False
            , genRdbChecksum = True
            , port = 6379
            , rdbFileDirPath = Rel [reldir|./|]
            }
