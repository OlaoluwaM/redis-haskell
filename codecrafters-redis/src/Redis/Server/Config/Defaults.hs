module Redis.Server.Config.Defaults (
    defaultRedisConfig,
    emptyPartialRedisConfig,
    DefaultRedisConfig (..),
) where

import Path

import Data.Monoid (Last (..))
import GHC.Generics (Generic)
import Redis.Server.Config.Types (PartialRedisConfig, RedisConfig, RedisConfigF (..))

newtype DefaultRedisConfig = DefaultRedisConfig {redisConf :: RedisConfig}
    deriving stock (Show, Generic)

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

emptyPartialRedisConfig :: PartialRedisConfig
emptyPartialRedisConfig =
    RedisConfigF
        { rdbFilenamePath = Last Nothing
        , useRDBCompression = Last Nothing
        , genRdbChecksum = Last Nothing
        , port = Last Nothing
        , rdbFileDirPath = Last Nothing
        }
