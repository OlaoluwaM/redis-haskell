module Redis.Commands.Save (
    mkSaveCmdArg,
    handleSave,
) where

import Data.HashMap.Strict qualified as HashMap
import Redis.RDB.Binary qualified as RDB

import Control.Concurrent.STM (readTVarIO)
import Control.Exception (Exception (..))
import Control.Exception.Safe (StringException, throwString)
import Control.Monad.Catch (MonadCatch, MonadThrow (..), handle)
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Reader (MonadReader (ask))
import Data.String (fromString)
import Data.Time.Clock.POSIX (getPOSIXTime)
import Network.Socket (Socket)
import Optics (A_Lens, LabelOptic, view)
import Path
import Redis.RDB.Config (RDBConfig (RDBConfig, generateChecksum, skipChecksumValidation, useLzfCompression))
import Redis.RDB.Save (saveRedisStoreToRDB)
import Redis.RESP (BulkString, RESPDataType (SimpleString), serializeRESPDataType)
import Redis.Server.ServerT (MonadSocket (..))
import Redis.Server.Settings
import Redis.Store (StoreState)

-- https://redis.io/docs/latest/commands/save/

mkSaveCmdArg :: (MonadFail m) => [BulkString] -> m ()
mkSaveCmdArg [] = pure ()
mkSaveCmdArg _ = fail "SAVE command does not accept any arguments"

handleSave ::
    ( LabelOptic "clientSocket" A_Lens r r Socket Socket
    , LabelOptic "store" A_Lens r r StoreState StoreState
    , LabelOptic "settings" A_Lens r r ServerSettings ServerSettings
    , MonadIO m
    , MonadReader r m
    , MonadSocket m b
    , MonadCatch m
    ) =>
    m b
handleSave = do
    env <- ask

    let settings = view #settings env
    let socket = view #clientSocket env
    let kvStoreState = view #store env

    handle @_ @StringException
        (sendThroughSocket socket . fromString . displayException)
        (handleSave' settings socket kvStoreState)

handleSave' ::
    (MonadIO m, MonadSocket m b, MonadThrow m) =>
    ServerSettings -> Socket -> StoreState -> m b
handleSave' settings socket kvStoreState = do
    kvStore <- liftIO $ readTVarIO kvStoreState
    rdbDirPath <- getRDBFilePathFromSettings settings

    currentTime <- liftIO getPOSIXTime
    rdbConfig <- mkRDBConfigFromSettings settings

    let rdbFile = saveRedisStoreToRDB currentTime kvStore
    liftIO $ RDB.encodeFile rdbConfig (fromSomeFile rdbDirPath) rdbFile

    sendThroughSocket socket . serializeRESPDataType $ SimpleString "OK"

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
