module Redis.Server.Settings (
    ServerSettings (..),
    Setting (..),
    serverSettingsOptParser,
    serializeSettingsValue,

    -- ** Testing
    SettingValue (..),
) where

import Control.Applicative (Alternative (..))
import Control.Arrow ((&&&))
import Data.Bool (bool)
import Data.ByteString (ByteString)
import Data.Default (Default (..))
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HashMap
import Data.Hashable (Hashable)
import Data.Maybe (catMaybes)
import Data.String (IsString (..))
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding (encodeUtf8)
import GHC.Generics (Generic)
import Options.Applicative (
    Parser,
    help,
    long,
    maybeReader,
    metavar,
    option,
    optional,
 )
import System.FilePath (isValid)

-- Redis configurations as defined https://redis.io/docs/latest/operate/oss_and_stack/management/config/
-- This module particularly implements the ability to set those configurations using command line options and arguments (https://redis.io/docs/latest/operate/oss_and_stack/management/config/#passing-arguments-using-the-command-line)
-- Reading from a configuration file, a `redis.conf`  or `redis-full.conf` file is not implemented yet though I reckon it would be a case of implementing a parser for the configuration file and generating a ServerSettings type from said parser to then merge with whatever settings we gleam from this command-line parser below with a preference for values from the command-line parser

-- It looks like codecrafters assumes redis v7.4, at least based on the kind of configurations they instruct us to implement which is present in the v7.4 config but not in the v8 config: https://raw.githubusercontent.com/redis/redis/7.4/redis.conf

newtype Setting = Setting {setting :: Text}
    deriving stock (Eq, Show, Generic)
    deriving newtype (Hashable)

-- We're making this a HashMap to avoid needing to access records dynamically given how un-idiomaitc that is
newtype ServerSettings = ServerSettings {settings :: HashMap Setting SettingValue}
    deriving stock (Eq, Show, Generic)

data SettingValue = TextVal Text | IntVal Integer | BoolVal Bool | FloatVal Double
    deriving stock (Eq, Show, Generic)

instance Default ServerSettings where
    def = ServerSettings HashMap.empty

serverSettingsOptParser :: Parser ServerSettings
serverSettingsOptParser =
    (\x y -> ServerSettings . HashMap.fromList . catMaybes $ [x, y])
        <$> optional
            ( (const (Setting "dir") &&& TextVal . T.strip)
                <$> option (maybeReader isValidPath <|> fail "Invalid path provided for rdb dir") (long "dir" <> metavar "RDB_DIR_PATH" <> help "Directory containing RDB file")
            )
        <*> optional
            ( (const (Setting "dbfilename") &&& TextVal . T.strip)
                <$> option (maybeReader isValidPath <|> fail "Invalid path provided for rdb file name") (long "dbfilename" <> metavar "RDB_FILENAME" <> help "Directory containing RDB file with extension")
            )

isValidPath :: String -> Maybe Text
isValidPath potentialPath = bool Nothing (Just . T.pack $ potentialPath) (isValid potentialPath)

serializeSettingsValue :: SettingValue -> ByteString
serializeSettingsValue = \case
    TextVal txt -> encodeUtf8 txt
    IntVal num -> fromString . show $ num
    FloatVal float -> fromString . show $ float
    BoolVal boolVal -> fromString . show $ boolVal
