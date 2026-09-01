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
import Data.Bifunctor (Bifunctor (first))
import Data.Text (Text)
import Data.Text qualified as T
import Effectful (Eff)
import GHC.Generics (Generic)
import Optics (view)
import Redis.Effect.Communication (sendMessage)
import Redis.Effects (RedisClientCommunication, RedisServerConfig)
import Redis.Server.Config.Types (NamedField (..), collectNamedFields)
import Redis.Utils (ShowBS (showBs))
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
    (RedisClientCommunication r es, RedisServerConfig r es) =>
    ConfigGetCmdArg -> Eff es ()
handleConfigGet (ConfigGetCmdArg configOptionPatternsToGet) = do
    env <- ReaderEff.ask @r

    let socket = view #clientSocket env
    let serverConfigRef = view #serverConfigRef env

    serverConfig <- STMEff.readTVarIO serverConfigRef

    let serverConfigMap = HashMap.fromList $ map (\(NamedField k v) -> (k, showBs v)) $ collectNamedFields serverConfig
    let normalizedConfigGetOptionPatternsToGet = map (compile . T.unpack) configOptionPatternsToGet

    let filteredMap = HashMap.filterWithKey (\settingKey _ -> let settingKeyStr = T.unpack settingKey in any (`match` settingKeyStr) normalizedConfigGetOptionPatternsToGet) serverConfigMap

    let result = concatMap (fromTuple . first showBs) . HashMap.toList $ filteredMap

    sendMessage socket . serializeRESPDataType . mkNonNullRESPArray . map mkNonNullBulkString $ result

fromTuple :: (a, a) -> [a]
fromTuple (a, a') = [a, a']
