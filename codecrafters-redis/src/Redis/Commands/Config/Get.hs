module Redis.Commands.Config.Get (
    ConfigGetCmdArg (..),
    mkConfigGetCmdArg,
    handleConfigGet,
) where

import Redis.RESP
import Redis.Server.Config

import Data.HashMap.Strict qualified as HashMap
import Effectful.Concurrent.STM qualified as STMEff
import Effectful.Reader.Static qualified as ReaderEff

import Control.Exception (Exception (displayException))
import Data.Aeson (ToJSON (..))
import Data.Bifunctor (Bifunctor (first))
import Data.ByteString (ByteString)
import Data.Functor.Const (Const (..))
import Data.Text (Text)
import Data.Text qualified as T
import Effectful (Eff)
import GHC.Generics (Generic)
import Optics (view)
import Redis.Effect.Communication (sendMessage)
import Redis.Effects (RedisClientCommunication, RedisServerConfig)
import Redis.Utils (ShowBS (..))
import System.FilePath.Glob (compile, match)

-- https://redis.io/docs/latest/commands/config-get/

newtype ConfigGetCmdArg = ConfigGetCmdArg {configOptions :: [Text]}
    deriving stock (Eq, Show, Generic)
    deriving anyclass (ToJSON)

mkConfigGetCmdArg :: (MonadFail m) => [BulkString] -> m ConfigGetCmdArg
mkConfigGetCmdArg [] = fail "GET command requires an argument"
mkConfigGetCmdArg configsToGet = either (fail . displayException) (pure . ConfigGetCmdArg) $ traverse decodeUtf8BulkString' configsToGet

mkRedisConfigWithUpstreamKey :: RedisConfig -> RedisConfigF (Const (Text, ByteString))
mkRedisConfigWithUpstreamKey config =
    RedisConfigF
        { useRDBCompression = Const (getConfigFieldName @UseRDBCompression, showBs config.useRDBCompression)
        , replicaOf = Const (getConfigFieldName @ReplicaOf, showBs config.replicaOf)
        , rdbFilenamePath =
            Const
                ( getConfigFieldName @RDBFilename
                , showBs config.rdbFilenamePath
                )
        , rdbFileDirPath = Const (getConfigFieldName @RDBFileDir, showBs config.rdbFileDirPath)
        , port = Const (getConfigFieldName @RedisPort, showBs config.port)
        , genRdbChecksum = Const (getConfigFieldName @GenRDBChecksum, showBs config.genRdbChecksum)
        }

handleConfigGet ::
    forall r es.
    (RedisClientCommunication r es, RedisServerConfig r es) =>
    ConfigGetCmdArg ->
    Eff es ()
handleConfigGet (ConfigGetCmdArg configOptionPatternsToGet) = do
    env <- ReaderEff.ask @r

    let socket = view #clientSocket env
    let serverConfigRef = view #serverConfigRef env

    serverConfig <- mkRedisConfigWithUpstreamKey <$> STMEff.readTVarIO serverConfigRef

    let serverConfigMap = HashMap.fromList $ collectFieldSpecs serverConfig
    let normalizedConfigGetOptionPatternsToGet = map (compile . T.unpack) configOptionPatternsToGet

    let filteredMap = HashMap.filterWithKey (\settingKey _ -> let settingKeyStr = T.unpack settingKey in any (`match` settingKeyStr) normalizedConfigGetOptionPatternsToGet) serverConfigMap

    let result = concatMap (fromTuple . first showBs) . HashMap.toList $ filteredMap

    sendMessage socket . serializeRESPDataType . mkNonNullRESPArray . map mkNonNullBulkString $ result

fromTuple :: (a, a) -> [a]
fromTuple (a, a') = [a, a']
