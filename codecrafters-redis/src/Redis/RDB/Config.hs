{-# LANGUAGE UndecidableInstances #-}

module Redis.RDB.Config (
    RDBConfig (useLzfCompression, generateChecksum, skipChecksumValidation),
    defaultRDBConfig,
    MkRDBConfigArg (..),
    mkRDBConfig,
) where

import GHC.Generics (Generic)
import Redis.Server.Config.Defaults (DefaultRedisConfig (..), defaultRedisConfig)
import Redis.Server.Config.Types (RedisConfigF (..))

data RDBConfig = RDBConfig
    { useLzfCompression :: Bool
    , generateChecksum :: Bool
    , skipChecksumValidation :: Bool
    }
    deriving stock (Eq, Show, Generic)

defaultRDBConfig :: RDBConfig
defaultRDBConfig =
    mkRDBConfig
        MkRDBConfigArg
            { useCompression = redisConf.useRDBCompression
            , generateChecksum = redisConf.genRdbChecksum
            }
  where
    redisConf = defaultRedisConfig.redisConf

data MkRDBConfigArg = MkRDBConfigArg
    { useCompression :: Bool
    , generateChecksum :: Bool
    }
    deriving stock (Eq, Show, Generic)

mkRDBConfig :: MkRDBConfigArg -> RDBConfig
mkRDBConfig MkRDBConfigArg{..} =
    RDBConfig
        { useLzfCompression = useCompression
        , generateChecksum = generateChecksum
        , skipChecksumValidation = not generateChecksum
        }
