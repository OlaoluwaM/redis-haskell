module Redis.Commands.BGSave where

import Data.HashMap.Strict qualified as HashMap
import Redis.RDB.Binary qualified as RDB

import Control.Concurrent.STM (readTVarIO)
import Control.Exception (Exception (..))
import Control.Exception.Safe (StringException, throwString)
import Control.Monad.Catch (MonadCatch, MonadThrow (..), handle)
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Reader (MonadReader (ask))
import Data.Aeson (ToJSON)
import Data.String (fromString)
import Data.Time.Clock.POSIX (getPOSIXTime)
import GHC.Generics (Generic)
import Network.Socket (Socket)
import Optics (A_Lens, LabelOptic, view)
import Path
import Redis.RDB.Config (RDBConfig (RDBConfig, generateChecksum, skipChecksumValidation, useLzfCompression))
import Redis.RDB.Save (saveRedisStoreToRDB)
import Redis.RESP (RESPDataType (SimpleString), serializeRESPDataType, BulkString (..))
import Redis.Server.ServerT (MonadSocket (..))
import Redis.Server.Settings
import Redis.Store (StoreState)
import Control.Monad (void)

-- https://redis.io/docs/latest/commands/bgsave/

newtype BGSaveCmdArg = BGSaveCmdArg (Maybe ShouldSchedule)
    deriving stock (Eq, Show, Generic)
    deriving anyclass (ToJSON)

newtype ShouldSchedule = ShouldSchedule Bool
    deriving stock (Eq, Show)
    deriving newtype (ToJSON)

mkBGSaveCmdArg :: (MonadFail m) => [BulkString] -> m BGSaveCmdArg
mkBGSaveCmdArg [] = pure $ BGSaveCmdArg Nothing
mkBGSaveCmdArg [BulkString "SCHEDULE"] = pure $ BGSaveCmdArg (Just $ ShouldSchedule True)
mkBGSaveCmdArg [BulkString invalidArg] = fail $ "Invalid argument to BGSAVE command: " <> show invalidArg <> ". Only SCHEDULE is supported."
mkBGSaveCmdArg _ = fail "BGSAVE command accepts only 1 argument"

handleBGSave ::
    ( LabelOptic "clientSocket" A_Lens r r Socket Socket
    , LabelOptic "store" A_Lens r r StoreState StoreState
    , LabelOptic "settings" A_Lens r r ServerSettings ServerSettings
    , MonadIO m
    , MonadReader r m
    , MonadSocket m b
    , MonadCatch m
    ) =>
    m b
handleBGSave = do
    env <- ask

    let settings = view #settings env
    let socket = view #clientSocket env
    let kvStoreState = view #store env

    handle @_ @StringException
        (sendThroughSocket socket . fromString . displayException)
        (handleBGSave' settings socket kvStoreState)

handleBGSave' ::
    (MonadIO m, MonadSocket m b, MonadThrow m) =>
    ServerSettings -> Socket -> StoreState -> m b
handleBGSave' settings socket kvStoreState = do
    kvStore <- liftIO $ readTVarIO kvStoreState
    rdbDirPath <- getRDBFilePathFromSettings settings

    void $ sendThroughSocket socket . serializeRESPDataType $ SimpleString "Starting"

    currentTime <- liftIO getPOSIXTime
    rdbConfig <- mkRDBConfigFromSettings settings

    let rdbFile = saveRedisStoreToRDB currentTime kvStore
    liftIO $ RDB.encodeFile rdbConfig (fromSomeFile rdbDirPath) rdbFile


getRDBFilePathFromSettings :: (MonadThrow m) => ServerSettings -> m (SomeBase File)
getRDBFilePathFromSettings (ServerSettings settings) = do
    dirPath <- maybe (throwString "Missing dir setting") pure $ HashMap.lookup (Setting "dir") settings
    fileName <- maybe (throwString "Missing dbfilename setting") pure $ HashMap.lookup (Setting "dbfilename") settings

    case (dirPath, fileName) of
        (DirPathVal (Abs dir), FilePathVal (Rel file)) -> pure $ Abs $ dir </> file
        (DirPathVal (Rel dir), FilePathVal (Rel file)) -> pure $ Rel $ dir </> file
        _ -> throwString "Incompatible dir and file path types"

mkRDBConfigFromSettings :: (MonadThrow m) => ServerSettings -> m RDBConfig
mkRDBConfigFromSettings (ServerSettings settings) = do
    rawRDBCompressionSetting <- maybe (throwString "Missing rdbcompression setting") pure $ HashMap.lookup (Setting "rdbcompression") settings
    rawRDBChecksumSetting <- maybe (throwString "Missing rdbchecksum setting") pure $ HashMap.lookup (Setting "rdbchecksum") settings

    case (rawRDBCompressionSetting, rawRDBChecksumSetting) of
        (BoolVal rdbCompressionSettingVal, BoolVal rdbChecksumSettingVal) ->
            pure $
                RDBConfig
                    { useLzfCompression = rdbCompressionSettingVal
                    , generateChecksum = rdbChecksumSettingVal
                    , skipChecksumValidation = not rdbChecksumSettingVal
                    }
        _ -> throwString "Internal service error: Incompatible type for rdbcompression and/or rdbchecksum types"
