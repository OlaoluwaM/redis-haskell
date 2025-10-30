module Redis.Commands.Config.Get (
    ConfigGetCmdArg (..),
    mkConfigGetCmdArg,
    handleConfigGet,
) where

import Redis.RESP

import Data.HashMap.Strict qualified as HashMap
import Effectful.Concurrent.STM qualified as STMEff
import Effectful.Reader.Static qualified as ReaderEff

import Control.Exception (Exception (displayException))
import Data.Aeson (ToJSON (..))
import Data.Bifunctor (bimap)
import Data.String (IsString (fromString))
import Data.Text (Text)
import Data.Text qualified as T
import Effect.Communication (sendMessage)
import Effectful (Eff)
import GHC.Generics (Generic)
import Optics (view)
import Redis.Effects (RedisClientCommunication, RedisServerSettings)
import Redis.Server.Settings (ServerSettings (..), Setting (..), serializeSettingsValue)
import System.FilePath.Glob (compile, match)

-- https://redis.io/docs/latest/commands/config-get/

newtype ConfigGetCmdArg = ConfigGetCmdArg {configOptions :: [Text]}
    deriving stock (Eq, Show, Generic)
    deriving anyclass (ToJSON)

mkConfigGetCmdArg :: (MonadFail m) => [BulkString] -> m ConfigGetCmdArg
mkConfigGetCmdArg [] = fail "GET command requires an argument"
mkConfigGetCmdArg configsToGet = either (fail . displayException) (pure . ConfigGetCmdArg) $ traverse decodeUtf8BulkString' configsToGet

handleConfigGet ::
    forall r es.
    (RedisClientCommunication r es, RedisServerSettings r es) =>
    ConfigGetCmdArg -> Eff es ()
handleConfigGet (ConfigGetCmdArg configOptionPatternsToGet) = do
    env <- ReaderEff.ask @r

    let socket = view #clientSocket env
    let serverSettingsStateRef = view #serverSettingsRef env

    serverSettings <- (.settings) <$> STMEff.readTVarIO serverSettingsStateRef
    let normalizedConfigGetOptionPatternsToGet = map (compile . T.unpack . T.toLower) configOptionPatternsToGet

    let serverSettingOptions = HashMap.mapKeys (T.unpack . T.toLower . (.setting)) serverSettings

    let filteredMap = HashMap.filterWithKey (\settingKey _ -> any (`match` settingKey) normalizedConfigGetOptionPatternsToGet) serverSettingOptions
    let result = concatMap (fromTuple . bimap fromString serializeSettingsValue) . HashMap.toList $ filteredMap

    sendMessage socket . serializeRESPDataType . mkNonNullRESPArray . map mkNonNullBulkString $ result

fromTuple :: (a, a) -> [a]
fromTuple (a, a') = [a, a']
