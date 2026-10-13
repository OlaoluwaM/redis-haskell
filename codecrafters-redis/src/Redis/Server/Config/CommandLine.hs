module Redis.Server.Config.CommandLine (
    parserForCommandLineConfig,
    RedisConfigFromCommandLine (..),
) where

import Redis.Server.Config.Readers qualified as Readers
import Redis.Server.Config.Types qualified as Config

import Data.Monoid (Last (..))
import Options.Applicative (
    Parser,
    help,
    long,
    metavar,
    option,
    optional,
    short,
 )
import Redis.Server.Config.Types (ConfigFieldType, RedisConfigF (..), getConfigFieldName)

newtype RedisConfigFromCommandLine = RedisConfigFromCommandLine Config.PartialRedisConfig

parserForCommandLineConfig :: Parser RedisConfigFromCommandLine
parserForCommandLineConfig =
    RedisConfigFromCommandLine
        <$> (RedisConfigF <$> rdbFileDirectoryParser <*> rdbFilenameParser <*> rdbCompressionParser <*> rdbChecksumParser <*> redisPortParser)

{- | Parser for RDB directory setting
This aligns with the default value for `dir` in the default redis.conf: https://github.com/redis/redis/blob/c66fbda23fa294a7710b89ad70c1aa168abcf84c/redis.conf#L516
-}
rdbFileDirectoryParser :: Parser (Last (ConfigFieldType Config.RDBFileDir))
rdbFileDirectoryParser =
    fmap Last $
        optional $
            option
                Readers.rdbFileDirReader
                (long (getConfigFieldName @Config.RDBFileDir) <> metavar "RDB_DIR_PATH" <> help "Directory containing RDB file")

-- {- | Parser for RDB filename setting
-- This aligns with the default value for `dbfilename` in the default redis.conf: https://github.com/redis/redis/blob/c66fbda23fa294a7710b89ad70c1aa168abcf84c/redis.conf#L493
-- -}
rdbFilenameParser :: Parser (Last (ConfigFieldType Config.RDBFilename))
rdbFilenameParser =
    fmap Last $
        optional $
            option
                Readers.rdbFilenameReader
                (long (getConfigFieldName @Config.RDBFilename) <> metavar "RDB_FILENAME" <> help "Directory containing RDB file with extension")

-- {- | Parser for RDB compression setting
-- These align with the default values for `rdbcompression` in the default redis.conf: https://github.com/redis/redis/blob/c66fbda23fa294a7710b89ad70c1aa168abcf84c/redis.conf#L466
-- -}
rdbCompressionParser :: Parser (Last (ConfigFieldType Config.UseRDBCompression))
rdbCompressionParser =
    fmap Last $
        optional $
            option
                Readers.rdbCompressionReader
                (long (getConfigFieldName @Config.UseRDBCompression) <> metavar "RDB_COMPRESSION" <> help "Enable or disable RDB compression (default: enabled)")

-- {- | Parser for RDB checksum setting
-- These align with the default values for `rdbchecksum` in the default redis.conf: https://github.com/redis/redis/blob/c66fbda23fa294a7710b89ad70c1aa168abcf84c/redis.conf#L475
-- -}
rdbChecksumParser :: Parser (Last (ConfigFieldType Config.GenRDBChecksum))
rdbChecksumParser =
    fmap Last $
        optional $
            option
                Readers.rdbChecksumReader
                (long (getConfigFieldName @Config.GenRDBChecksum) <> metavar "RDB_CHECKSUM" <> help "Enable or disable RDB checksum (default: enabled)")

-- {- | Parser for Redis port setting
-- This aligns with the default value for `port` in the default redis.conf: https://github.com/redis/redis/blob/c66fbda23fa294a7710b89ad70c1aa168abcf84c/redis.conf#L57
-- -}

redisPortParser :: Parser (Last (ConfigFieldType Config.RedisPort))
redisPortParser =
    fmap Last $
        optional $
            option
                Readers.portReader
                ( long (getConfigFieldName @Config.RedisPort)
                    <> short 'p'
                    <> metavar "PORT_NUMBER"
                    <> help "Port number for the Redis server to listen on"
                )
