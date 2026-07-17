module Redis.Server.Settings (
    ServerSettings (..),
    Setting (..),
    Settings (..),
    ServerSettingsRef,
    serializeSettingsValue,
    serverSettings,
    defaultRDBFileDirectory,
    defaultRDBFilename,
    defaultGenerateChecksumWithRDB,
    defaultUseCompressionWithRDB,
    rdbCompressionSettingKey,
    rdbChecksumSettingKey,
    rdbFilenameSettingKey,
    rdbFileDirectorySettingKey,
    redisPortSettingKey,
    redisDefaultPort,
    defaultServerSettings,

    -- ** Testing
    SettingValue (..),
) where

import Path

import Control.Concurrent.STM qualified as STM
import Data.HashMap.Strict qualified as HashMap

import Control.Applicative (Alternative (..))
import Control.Arrow ((&&&))
import Data.ByteString (ByteString)
import Data.HashMap.Strict (HashMap)
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
    short,
    value,
 )
import Options.Applicative.Types (ReadM, readerAsk)
import Redis.Server.Metadata (RedisConfFilePath (..))
import Redis.Utils (genericShow, runReadM)
import System.FilePath (dropTrailingPathSeparator)

-- Redis configurations as defined https://redis.io/docs/latest/operate/oss_and_stack/management/config/

-- This module particularly implements the ability to set those configurations using command line options and arguments (https://redis.io/docs/latest/operate/oss_and_stack/management/config/#passing-arguments-using-the-command-line)

-- Reading from a configuration file, a `redis.conf`  or `redis-full.conf` file is not implemented yet though I reckon it would be a case of implementing a parser for the configuration file and generating a ServerSettings type from said parser to then overriding some of those settings with whatever settings we gleam from the command-line parser. So it would be config file settings + command-line settings with a preference for the latter

-- It looks like codecrafters assumes redis v7.4, at least based on the kind of configurations they instruct us to implement which is present in the v7.4 config but not in the v8 config: https://raw.githubusercontent.com/redis/redis/7.4/redis.conf

-- We're making this a HashMap to avoid needing to access records dynamically given how un-idiomaitc that is

data Settings = Settings
    { settingsConfigFilePath :: Maybe RedisConfFilePath -- Path to a redis.conf file. Ideally we would parse the file at this path and use its contents to initialize our server settings then apply overrides based on what we get from the command line, but for now we just accept it as an argument and do nothing with it
    , settingsFromConfigFile :: Maybe ServerSettings
    , settingsFromCommandLine :: ServerSettings -- Settings provided via command line arguments
    }

type ServerSettingsRef = STM.TVar ServerSettings

newtype ServerSettings = ServerSettings {settings :: HashMap Setting SettingValue}
    deriving stock (Eq, Show, Generic)

newtype Setting = Setting {setting :: Text}
    deriving stock (Eq, Show, Generic)
    deriving newtype (Hashable)

data SettingValue = TextVal Text | IntVal Integer | BoolVal Bool | FloatVal Double | FilePathVal (SomeBase File) | DirPathVal (SomeBase Dir)
    deriving stock (Eq, Show, Generic)

defaultServerSettings :: ServerSettings
defaultServerSettings = ServerSettings HashMap.empty

serverSettings :: Parser Settings
serverSettings =
    Settings
        <$> optional parserForRedisConfigFilePathArgument
        <*> pure Nothing
        <*> parserForCommandLineServerSettings

parserForRedisConfigFilePathArgument :: Parser RedisConfFilePath
parserForRedisConfigFilePathArgument =
    argument
        parseRedisConfFilePath
        (metavar "REDIS_CONFIG_FILE" <> help "Path to redis config file")

parseRedisConfFilePath :: ReadM RedisConfFilePath
parseRedisConfFilePath = do
    rawPath <- readerAsk
    path <-
        maybe
            (fail "Path provided for redis config file is not an absolute file path")
            pure
            $ parseAbsFile @Maybe rawPath

    if filename path == [relfile|redis.conf|]
        then pure (RedisConfFilePath path)
        else fail "The file provided is not named redis.conf"

serializeSettingsValue :: SettingValue -> ByteString
serializeSettingsValue = \case
    TextVal txt -> encodeUtf8 txt
    IntVal num -> genericShow num
    FloatVal float -> genericShow float
    BoolVal boolVal -> genericShow boolVal
    FilePathVal x -> fromString . fromSomeFile $ x
    DirPathVal x -> fromString . dropTrailingPathSeparator . fromSomeDir $ x

parserForCommandLineServerSettings :: Parser ServerSettings
parserForCommandLineServerSettings =
    ServerSettings . HashMap.fromList . catMaybes
        <$> sequenceA
            [ optional rdbFileDirectoryParser
            , optional rdbFilenameParser
            , optional rdbCompressionParser
            , optional rdbChecksumParser
            , optional redisPortParser
            ]

{- | Parser for RDB directory setting
This aligns with the default value for `dir` in the default redis.conf: https://github.com/redis/redis/blob/c66fbda23fa294a7710b89ad70c1aa168abcf84c/redis.conf#L516
-}
rdbFileDirectoryParser :: Parser (Setting, SettingValue)
rdbFileDirectoryParser =
    (const rdbFileDirectorySettingKey &&& DirPathVal)
        <$> option
            (maybeReader (fmap Abs . parseAbsDir) <|> maybeReader (fmap Rel . parseRelDir))
            (long rdbFileDirectorySettingKeyText <> metavar "RDB_DIR_PATH" <> help "Directory containing RDB file" <> value (Rel defaultRDBFileDirectory))

rdbFileDirectorySettingKey :: Setting
rdbFileDirectorySettingKey = Setting rdbFileDirectorySettingKeyText

rdbFileDirectorySettingKeyText :: (IsString a) => a
rdbFileDirectorySettingKeyText = "dir"

defaultRDBFileDirectory :: Path Rel Dir
defaultRDBFileDirectory = [reldir|./|]

{- | Parser for RDB filename setting
This aligns with the default value for `dbfilename` in the default redis.conf: https://github.com/redis/redis/blob/c66fbda23fa294a7710b89ad70c1aa168abcf84c/redis.conf#L493
-}
rdbFilenameParser :: Parser (Setting, SettingValue)
rdbFilenameParser =
    (const rdbFilenameSettingKey &&& FilePathVal . Rel)
        <$> option
            (maybeReader parseRelFile)
            (long rdbFilenameSettingKeyText <> metavar "RDB_FILENAME" <> help "Directory containing RDB file with extension" <> value defaultRDBFilename)

rdbFilenameSettingKey :: Setting
rdbFilenameSettingKey = Setting rdbFilenameSettingKeyText

rdbFilenameSettingKeyText :: (IsString a) => a
rdbFilenameSettingKeyText = "dbfilename"

defaultRDBFilename :: Path Rel File
defaultRDBFilename = [relfile|dump.rdb|]

{- | Parser for RDB compression setting
These align with the default values for `rdbcompression` in the default redis.conf: https://github.com/redis/redis/blob/c66fbda23fa294a7710b89ad70c1aa168abcf84c/redis.conf#L466
-}
rdbCompressionParser :: Parser (Setting, SettingValue)
rdbCompressionParser =
    (const rdbCompressionSettingKey &&& BoolVal)
        <$> option auto (long rdbCompressionSettingKeyText <> metavar "RDB_COMPRESSION" <> help "Enable or disable RDB compression (default: enabled)" <> value defaultUseCompressionWithRDB)

rdbCompressionSettingKey :: Setting
rdbCompressionSettingKey = Setting rdbCompressionSettingKeyText

rdbCompressionSettingKeyText :: (IsString a) => a
rdbCompressionSettingKeyText = "rdbcompression"

defaultUseCompressionWithRDB :: Bool
defaultUseCompressionWithRDB = False

{- | Parser for RDB checksum setting
These align with the default values for `rdbchecksum` in the default redis.conf: https://github.com/redis/redis/blob/c66fbda23fa294a7710b89ad70c1aa168abcf84c/redis.conf#L475
-}
rdbChecksumParser :: Parser (Setting, SettingValue)
rdbChecksumParser =
    (const rdbChecksumSettingKey &&& BoolVal)
        <$> option auto (long rdbChecksumSettingKeyText <> metavar "RDB_CHECKSUM" <> help "Enable or disable RDB checksum (default: enabled)" <> value defaultGenerateChecksumWithRDB)

rdbChecksumSettingKey :: Setting
rdbChecksumSettingKey = Setting rdbChecksumSettingKeyText

rdbChecksumSettingKeyText :: (IsString a) => a
rdbChecksumSettingKeyText = "rdbchecksum"

defaultGenerateChecksumWithRDB :: Bool
defaultGenerateChecksumWithRDB = True

redisPortParser :: Parser (Setting, SettingValue)
redisPortParser =
    (const (Setting "port") &&& TextVal . genericShow)
        <$> option
            auto
            ( long redisPortSettingKeyText
                <> short 'p'
                <> metavar "PORT_NUMBER"
                <> help ("Port number for the Redis server to listen on (default: " <> genericShow redisDefaultPort <> ")")
                <> value redisDefaultPort
            )

redisPortSettingKey :: Setting
redisPortSettingKey = Setting redisPortSettingKeyText

redisPortSettingKeyText :: (IsString a) => a
redisPortSettingKeyText = "port"

redisDefaultPort :: Int
redisDefaultPort = 6379
