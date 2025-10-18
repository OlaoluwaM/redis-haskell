module Redis.Server.Settings (
    ServerSettings (..),
    Setting (..),
    Settings (..),
    serializeSettingsValue,
    serverSettings,

    -- ** Testing
    SettingValue (..),
) where

import Control.Applicative (Alternative (..))
import Control.Arrow ((&&&))
import Data.ByteString (ByteString)
import Data.Default (Default (..))
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HashMap
import Data.Hashable (Hashable)
import Data.Maybe (catMaybes)
import Data.String (IsString (..))
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)
import GHC.Generics (Generic)
import Options.Applicative (
    Parser,
    argument,
    auto,
    help,
    long,
    maybeReader,
    metavar,
    option,
    optional,
    value,
 )
import Options.Applicative.Types (ReadM, readerAsk)
import Path (
    Abs,
    Dir,
    File,
    Path,
    SomeBase (..),
    filename,
    parseAbsDir,
    parseAbsFile,
    parseRelDir,
    parseRelFile,
    reldir,
    relfile,
 )

-- Redis configurations as defined https://redis.io/docs/latest/operate/oss_and_stack/management/config/
-- This module particularly implements the ability to set those configurations using command line options and arguments (https://redis.io/docs/latest/operate/oss_and_stack/management/config/#passing-arguments-using-the-command-line)
-- Reading from a configuration file, a `redis.conf`  or `redis-full.conf` file is not implemented yet though I reckon it would be a case of implementing a parser for the configuration file and generating a ServerSettings type from said parser to then merge with whatever settings we gleam from this command-line parser below with a preference for values from the command-line parser

-- It looks like codecrafters assumes redis v7.4, at least based on the kind of configurations they instruct us to implement which is present in the v7.4 config but not in the v8 config: https://raw.githubusercontent.com/redis/redis/7.4/redis.conf

-- We're making this a HashMap to avoid needing to access records dynamically given how un-idiomaitc that is

data Settings = Settings
    { settingsFromConfigFile :: Maybe RedisConfFile -- Path to a redis.conf file. Ideally we would parse this file and use it to set server settings in addition to what we get from the command line (with a preference of the latter), but for now we just accept it as an argument and do nothing with it
    , settingsFromCommandLine :: ServerSettings -- Settings provided via command line arguments
    }

newtype ServerSettings = ServerSettings {settings :: HashMap Setting SettingValue}
    deriving stock (Eq, Show, Generic)

newtype Setting = Setting {setting :: Text}
    deriving stock (Eq, Show, Generic)
    deriving newtype (Hashable)

data SettingValue = TextVal Text | IntVal Integer | BoolVal Bool | FloatVal Double | FilePathVal (SomeBase File) | DirPathVal (SomeBase Dir)
    deriving stock (Eq, Show, Generic)

instance Default ServerSettings where
    def = ServerSettings HashMap.empty

serverSettings :: Parser Settings
serverSettings =
    Settings
        <$> optional parserForRedisConfigArgument
        <*> parserForCommandLineServerSettings

parserForRedisConfigArgument :: Parser RedisConfFile
parserForRedisConfigArgument =
    argument
        parseRedisConfFile
        (metavar "REDIS_CONFIG_FILE" <> help "Path to redis config file")

newtype RedisConfFile = RedisConfFile {redisConfFile :: Path Abs File}
    deriving stock (Eq, Show)

parseRedisConfFile :: ReadM RedisConfFile
parseRedisConfFile = do
    rawPath <- readerAsk
    path <-
        maybe
            (fail "Path provided for redis config file is not an absolute file path")
            pure
            $ parseAbsFile @Maybe rawPath

    if filename path == [relfile|redis.conf|]
        then pure (RedisConfFile path)
        else fail "The file provided is not named redis.conf"

serializeSettingsValue :: SettingValue -> ByteString
serializeSettingsValue = \case
    TextVal txt -> encodeUtf8 txt
    IntVal num -> fromString . show $ num
    FloatVal float -> fromString . show $ float
    BoolVal boolVal -> fromString . show $ boolVal
    FilePathVal x -> fromString . show $ x
    DirPathVal x -> fromString . show $ x

parserForCommandLineServerSettings :: Parser ServerSettings
parserForCommandLineServerSettings =
    ServerSettings . HashMap.fromList . catMaybes
        <$> sequenceA
            [ optional rdbFileDirectoryParser
            , optional rdbFilenameParser
            , optional rdbCompressionParser
            , optional rdbChecksumParser
            ]

{- | Parser for RDB directory setting
This aligns with the default value for `dir` in the default redis.conf: https://github.com/redis/redis/blob/c66fbda23fa294a7710b89ad70c1aa168abcf84c/redis.conf#L516
-}
rdbFileDirectoryParser :: Parser (Setting, SettingValue)
rdbFileDirectoryParser =
    (const (Setting "dir") &&& DirPathVal)
        <$> option
            (maybeReader (fmap Abs . parseAbsDir) <|> maybeReader (fmap Rel . parseRelDir))
            (long "dir" <> metavar "RDB_DIR_PATH" <> help "Directory containing RDB file" <> value (Rel [reldir|./|]))

{- | Parser for RDB filename setting
This aligns with the default value for `dbfilename` in the default redis.conf: https://github.com/redis/redis/blob/c66fbda23fa294a7710b89ad70c1aa168abcf84c/redis.conf#L493
-}
rdbFilenameParser :: Parser (Setting, SettingValue)
rdbFilenameParser =
    (const (Setting "dbfilename") &&& FilePathVal . Rel)
        <$> option
            (maybeReader parseRelFile)
            (long "dbfilename" <> metavar "RDB_FILENAME" <> help "Directory containing RDB file with extension" <> value [relfile|dump.rdb|])

{- | Parser for RDB compression setting
These align with the default values for `rdbcompression` in the default redis.conf: https://github.com/redis/redis/blob/c66fbda23fa294a7710b89ad70c1aa168abcf84c/redis.conf#L466
-}
rdbCompressionParser :: Parser (Setting, SettingValue)
rdbCompressionParser =
    (const (Setting "rdbcompression") &&& BoolVal)
        <$> option auto (long "rdbcompression" <> metavar "RDB_COMPRESSION" <> help "Enable or disable RDB compression (default: enabled)" <> value True)

{- | Parser for RDB checksum setting
These align with the default values for `rdbchecksum` in the default redis.conf: https://github.com/redis/redis/blob/c66fbda23fa294a7710b89ad70c1aa168abcf84c/redis.conf#L475
-}
rdbChecksumParser :: Parser (Setting, SettingValue)
rdbChecksumParser =
    (const (Setting "rdbchecksum") &&& BoolVal)
        <$> option auto (long "rdbchecksum" <> metavar "RDB_CHECKSUM" <> help "Enable or disable RDB checksum (default: enabled)" <> value True)
