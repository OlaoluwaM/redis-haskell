module Redis.Server.Config.Get (
    getRDBDumpFilePathFromConfig,
    genRDBConfigFromConfig,
) where

import Path

import Redis.RDB.Config (MkRDBConfigArg (..), RDBConfig, mkRDBConfig)
import Redis.Server.Config.Types (RedisConfig, RedisConfigF (..))

getRDBDumpFilePathFromConfig :: RedisConfig -> SomeBase File
getRDBDumpFilePathFromConfig RedisConfigF{rdbFileDirPath, rdbFilenamePath} =
    case rdbFileDirPath of
        Abs dir -> Abs (dir </> rdbFilenamePath)
        Rel dir -> Rel (dir </> rdbFilenamePath)

genRDBConfigFromConfig :: RedisConfig -> RDBConfig
genRDBConfigFromConfig RedisConfigF{useRDBCompression, genRdbChecksum} =
    mkRDBConfig
        MkRDBConfigArg
            { useCompression = useRDBCompression
            , generateChecksum = genRdbChecksum
            }
