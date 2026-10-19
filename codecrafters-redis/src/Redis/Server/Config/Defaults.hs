module Redis.Server.Config.Defaults (
    defaultRedisConfig,
    emptyPartialRedisConfig,
    DefaultRedisConfig (..),
) where

import Path

import Data.Monoid (Last (..))
import GHC.Generics (Generic)
import Redis.Server.Config.Types (PartialRedisConfig, Port, RedisConfig, RedisConfigF (..), ReplicaOfType (NO_ONE))
import Rerefined (refineTH)

newtype DefaultRedisConfig = DefaultRedisConfig {redisConf :: RedisConfig}
    deriving stock (Show, Generic)

defaultRedisConfig :: DefaultRedisConfig
defaultRedisConfig =
    DefaultRedisConfig $
        RedisConfigF
            { rdbFilenamePath = [relfile|dump.rdb|]
            , useRDBCompression = False
            , genRdbChecksum = True
            , port = $$(refineTH 6379) :: Port
            , rdbFileDirPath = Rel [reldir|./|]
            , replicaOf = NO_ONE
            }

emptyPartialRedisConfig :: PartialRedisConfig
emptyPartialRedisConfig =
    RedisConfigF
        { rdbFilenamePath = Last Nothing
        , useRDBCompression = Last Nothing
        , genRdbChecksum = Last Nothing
        , port = Last Nothing
        , rdbFileDirPath = Last Nothing
        , replicaOf = Last Nothing
        }
