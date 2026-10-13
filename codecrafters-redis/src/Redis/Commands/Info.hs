module Redis.Commands.Info (
    InfoCmdArg (..),
    mkInfoCmdArg,
    handleInfo,
) where

import Redis.RESP
import Redis.Server.Metadata

import Data.List.NonEmpty qualified as NE
import Data.Text qualified as T
import Data.Text.Encoding qualified as T
import Effectful.Concurrent.STM qualified as STMEff
import Effectful.Reader.Static qualified as ReaderEff
import Path qualified

import Data.Aeson (ToJSON (..))
import Data.List.NonEmpty (NonEmpty)
import Data.Maybe (fromMaybe, mapMaybe)
import Data.String (fromString)
import Data.Text (Text)
import Effectful (Eff, (:>))
import GHC.Generics (Generic)
import Optics (view)
import Redis.Commands.Info.Replication (genReplicationInfoSectionStr)
import Redis.Commands.Info.Server (MkServerInfoArgs (..), genServerInfoSectionStr)
import Redis.Effect.Communication (sendMessage)
import Redis.Effect.Time (Time)
import Redis.Effects (RedisClientCommunication, RedisServerConfig, RedisServerMetadata)
import Redis.Server.Config (RedisConfigF (..))
import Redis.Utils (universe, genericShow)

-- https://redis.io/docs/latest/commands/info/

newtype InfoCmdArg = InfoCmdArg {infoSectionsToShow :: NonEmpty InfoCmdSection}
    deriving stock (Eq, Show, Generic)
    deriving anyclass (ToJSON)

data InfoCmdSection = Server | Clients | Memory | Persistence | Threads | Stats | Replication | CPU | CommandStats | LatencyStats | Sentinel | Cluster | Modules | Keyspace | Keysizes | ErrorStats | Hotkeys | All | Default | Everything
    deriving stock (Eq, Show, Generic, Bounded, Enum)
    deriving anyclass (ToJSON)

mkInfoCmdArg :: (MonadFail m) => [BulkString] -> m InfoCmdArg
mkInfoCmdArg [] = pure . InfoCmdArg . NE.singleton $ Default
mkInfoCmdArg rawInfoCmdSections =
    pure
        . InfoCmdArg
        . fromMaybe (NE.singleton Default)
        . NE.nonEmpty
        . mapMaybe
            ( \case
                NullBulkString -> Nothing
                BulkString s -> case T.decodeUtf8' s of
                    Left _ -> Nothing
                    Right sectionText -> parseInfoCmdSection . T.toLower $ sectionText
            )
        $ rawInfoCmdSections

parseInfoCmdSection :: Text -> Maybe InfoCmdSection
parseInfoCmdSection "server" = Just Server
parseInfoCmdSection "clients" = Just Clients
parseInfoCmdSection "memory" = Just Memory
parseInfoCmdSection "persistence" = Just Persistence
parseInfoCmdSection "threads" = Just Threads
parseInfoCmdSection "stats" = Just Stats
parseInfoCmdSection "replication" = Just Replication
parseInfoCmdSection "cpu" = Just CPU
parseInfoCmdSection "commandstats" = Just CommandStats
parseInfoCmdSection "latencystats" = Just LatencyStats
parseInfoCmdSection "sentinel" = Just Sentinel
parseInfoCmdSection "cluster" = Just Cluster
parseInfoCmdSection "modules" = Just Modules
parseInfoCmdSection "keyspace" = Just Keyspace
parseInfoCmdSection "keysizes" = Just Keysizes
parseInfoCmdSection "errorstats" = Just ErrorStats
parseInfoCmdSection "hotkeys" = Just Hotkeys
parseInfoCmdSection "all" = Just All
parseInfoCmdSection "default" = Just Default
parseInfoCmdSection "everything" = Just Everything
parseInfoCmdSection _ = Nothing

-- As witnessed when testing the INFO command on an official Redis server (docker image)
defaultSectionsToShow :: [InfoCmdSection]
defaultSectionsToShow = [Server, Clients, Memory, Persistence, Stats, Replication, CPU, Modules, ErrorStats, Cluster, Keyspace]

allSectionsThatCanBeShown :: [InfoCmdSection]
allSectionsThatCanBeShown = filter (\section -> not (section == All || section == Everything || section == Default)) $ universe @InfoCmdSection

genEmptyInfoSectionStr :: InfoCmdSection -> Text
genEmptyInfoSectionStr sectionTitle = "# " <> (fromString . show $ sectionTitle) <> "\r\n"

handleInfo :: forall r es. (Time :> es, RedisClientCommunication r es, RedisServerConfig r es, RedisServerMetadata r es) => InfoCmdArg -> Eff es ()
handleInfo infoCmdArg = do
    env <- ReaderEff.ask @r
    let socket = view #clientSocket env
    let serverConfigRef = view #serverConfigRef env
    let serverMetadata = view #serverMetadata env

    redisConfig <- STMEff.readTVarIO serverConfigRef

    let serverPort = genericShow redisConfig.port
    let configFilePath = maybe "" (Path.toFilePath . (.redisConfFilePath)) serverMetadata.configFilePath
    let startupTime = serverMetadata.startTime
    let serverEnv = serverMetadata.environment

    let infoSectionsToShow = processInfoSectionsToShow infoCmdArg

    -- NOTE: We are note extracting this out because it will have a lot of parameter envy. We'd need to pass the parameter of every sectionStr gen function to `genInfoSectionStr` if we made it a standalone function
    let genInfoSectionStr = \case
            Server -> genServerInfoSectionStr MkServerInfoArgs{startupTime, tcpPort = serverPort, configFilePath} serverEnv
            Replication -> pure genReplicationInfoSectionStr
            section -> pure . genEmptyInfoSectionStr $ section

    let addNewlineAfterEachSection = T.intercalate "\r\n"

    infoStr <- addNewlineAfterEachSection <$> traverse genInfoSectionStr infoSectionsToShow

    let output = serializeRESPDataType . mkNonNullBulkString . T.encodeUtf8 $ infoStr
    sendMessage socket output

processInfoSectionsToShow :: InfoCmdArg -> [InfoCmdSection]
processInfoSectionsToShow (InfoCmdArg requiredSections)
    | All `elem` requiredSections = allSectionsThatCanBeShown
    | Everything `elem` requiredSections = allSectionsThatCanBeShown
    | Default `elem` requiredSections = defaultSectionsToShow
    | otherwise = NE.toList requiredSections
