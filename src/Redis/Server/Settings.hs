module Redis.Server.Settings (
    ServerSettings (..),
) where

import Control.Applicative (Alternative (..))
import Control.Arrow ((&&&))
import Data.Bool (bool)
import Data.Default (Default (..))
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HashMap
import Data.Maybe (catMaybes)
import Data.Text (Text)
import Data.Text qualified as T
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

-- We're making this a HashMap to avoid needing to access records dynamically given how un-idiomaitc that is
newtype ServerSettings = ServerSettings {getSettings :: HashMap Text SettingValue}
    deriving stock (Eq, Show, Generic)

data SettingValue = TextVal Text | StringVal String | BoolVal Bool
    deriving stock (Eq, Show, Generic)

instance Default ServerSettings where
    def = ServerSettings HashMap.empty

serverSettingsOptParser :: Parser ServerSettings
serverSettingsOptParser =
    (\x y -> ServerSettings . HashMap.fromList . catMaybes $ [x, y])
        <$> optional
            ( (const "dir" &&& TextVal)
                <$> option (maybeReader isValidPath <|> fail "Invalid path provided for rdb dir") (long "dir" <> metavar "RDB_DIR_PATH" <> help "Directory containing RDB file")
            )
        <*> optional
            ( (const "dbfilename" &&& TextVal)
                <$> option (maybeReader isValidPath <|> fail "Invalid path provided for rdb file name") (long "dbfilename" <> metavar "RDB_FILENAME" <> help "Directory containing RDB file with extension")
            )

isValidPath :: String -> Maybe Text
isValidPath potentialPath = bool Nothing (Just . T.pack $ potentialPath) (isValid potentialPath)
