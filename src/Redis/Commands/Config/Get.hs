module Redis.Commands.Config.Get (
    ConfigGetCmdArg (..),
    mkConfigGetCmdArg,
    handleConfigGet,
) where

import Data.HashMap.Strict qualified as HashMap

import Control.Exception (Exception (displayException))
import Control.Lens (view)
import Control.Monad.Reader (MonadReader (ask))
import Data.Aeson (ToJSON (..))
import Data.Bifunctor (bimap)
import Data.String (IsString (fromString))
import Data.Text (Text)
import Data.Text qualified as T
import GHC.Generics (Generic)
import Network.Socket (Socket)
import Redis.RESP (BulkString, decodeUtf8BulkString', mkNonNullBulkString, mkNonNullRESPArray, serializeRESPDataType)
import Redis.Server.Context (HasClientSocket (..), HasSettings (..))
import Redis.Server.ServerT (MonadSocket (..))
import Redis.Server.Settings (ServerSettings (ServerSettings), serializeSettingsValue)
import System.FilePath.Glob

-- https://redis.io/docs/latest/commands/config-get/

newtype ConfigGetCmdArg = ConfigGetCmdArg {configOptions :: [Text]}
    deriving stock (Eq, Show, Generic)
    deriving anyclass (ToJSON)

mkConfigGetCmdArg :: (MonadFail m) => [BulkString] -> m ConfigGetCmdArg
mkConfigGetCmdArg [] = fail "GET command requires an argument"
mkConfigGetCmdArg configsToGet = either (fail . displayException) (pure . ConfigGetCmdArg) $ traverse decodeUtf8BulkString' configsToGet

handleConfigGet :: (HasClientSocket r Socket, HasSettings r ServerSettings, MonadReader r m, MonadSocket m b) => ConfigGetCmdArg -> m b
handleConfigGet (ConfigGetCmdArg configOptionPatternsToGet) = do
    env <- ask

    let socket = view clientSocket env
    let (ServerSettings serverSettings) = view settings env
    let normalizedConfigGetOptionPatternsToGet = map (compile . T.unpack . T.toLower) configOptionPatternsToGet

    let serverSettingOptions = HashMap.mapKeys (T.unpack . T.toLower) serverSettings

    let filteredMap = HashMap.filterWithKey (\settingKey _ -> any (`match` settingKey) normalizedConfigGetOptionPatternsToGet) serverSettingOptions
    let result = concatMap (fromTuple . bimap fromString serializeSettingsValue) . HashMap.toList $ filteredMap

    sendThroughSocket socket . serializeRESPDataType . mkNonNullRESPArray . map mkNonNullBulkString $ result
  where
    fromTuple :: (a, a) -> [a]
    fromTuple (a, a') = [a, a']
