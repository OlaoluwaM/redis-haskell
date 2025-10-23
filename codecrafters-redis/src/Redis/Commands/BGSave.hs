module Redis.Commands.BGSave where

import Path
import Redis.Server.Settings
import Redis.Store

import Data.HashMap.Strict qualified as HashMap
import Redis.RDB.Binary qualified as RDB

import Blammo.Logging.Simple (MonadLogger, runSimpleLoggingT)
import Control.Concurrent (forkFinally, forkIOWithUnmask)
import Control.Concurrent.STM (atomically, modifyTVar, putTMVar, readTVar, readTVarIO, tryTakeTMVar)
import Control.Exception (Exception (..), SomeException (SomeException), try)
import Control.Exception.Safe (StringException, throwIO, throwString)
import Control.Monad (void)
import Control.Monad.Catch (MonadCatch (catch), MonadMask, MonadThrow (..), bracketOnError, finally, handle)
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Reader (MonadReader (ask))
import Data.Aeson (ToJSON)
import Data.Text (Text)
import Data.Time.Clock.POSIX (getCurrentTime, getPOSIXTime)
import GHC.Generics (Generic)
import GHC.Stack (HasCallStack)
import Network.Socket (Socket)
import Optics (A_Lens, LabelOptic, set, view)
import Redis.RDB.Config (RDBConfig (RDBConfig, generateChecksum, skipChecksumValidation, useLzfCompression))
import Redis.RDB.Save (saveRedisStoreToRDB)
import Redis.RESP (BulkString (..), RESPDataType (SimpleString), serializeRESPDataType)
import Redis.Server.ServerT (MonadSocket (..))
import Redis.Utils (logInternalServerError)
import Control.Concurrent.Classy (MonadConc (readTVarConc))

-- https://redis.io/docs/latest/commands/bgsave/

newtype BGSaveCmdArg = BGSaveCmdArg (Maybe ShouldSchedule)
    deriving stock (Eq, Show, Generic)
    deriving anyclass (ToJSON)

newtype ShouldSchedule = ShouldSchedule Bool
    deriving stock (Eq, Show)
    deriving newtype (ToJSON)

data SaveFailure
    = MissingRDBFilePathSetting
    | MissingRDBCompressionSetting
    | MissingRDBChecksumSetting
    | IncompatibleRDBCompressionOrChecksumSetting
    | MissingDirSetting
    | MissingDBFilenameSetting
    | IncompatibleDirAndFilePathTypes
    | BackgroundSaveInProgress
    | RDBSaveFailed String
    deriving stock (Eq, Show)

instance Exception SaveFailure

mkBGSaveCmdArg :: (MonadFail m) => [BulkString] -> m BGSaveCmdArg
mkBGSaveCmdArg [] = pure $ BGSaveCmdArg Nothing
mkBGSaveCmdArg [BulkString "SCHEDULE"] = pure $ BGSaveCmdArg (Just $ ShouldSchedule True)
mkBGSaveCmdArg [BulkString invalidArg] = fail $ "Invalid argument to BGSAVE command: " <> show invalidArg <> ". Only SCHEDULE is supported."
mkBGSaveCmdArg _ = fail "BGSAVE command accepts only 1 argument"

handleSave ::
    ( LabelOptic "clientSocket" A_Lens r r Socket Socket
    , LabelOptic "store" A_Lens r r ServerStateRef ServerStateRef
    , LabelOptic "settings" A_Lens r r ServerSettings ServerSettings
    , MonadIO m
    , MonadReader r m
    , MonadSocket m ()
    , MonadMask m -- Implies MonadCatch
    ) =>
    m ()
handleSave = do
    env <- ask

    let settings = view #settings env
    let socket = view #clientSocket env
    let serverStateRef = view #store env

    catch @_ @SaveFailure (performRDBSave serverStateRef settings socket) $ \e -> do
        let errMsg = displayException e
        handleBGSaveError socket e

handleBGSave ::
    ( LabelOptic "clientSocket" A_Lens r r Socket Socket
    , LabelOptic "store" A_Lens r r ServerStateRef ServerStateRef
    , LabelOptic "settings" A_Lens r r ServerSettings ServerSettings
    , MonadIO m
    , MonadReader r m
    , MonadSocket m ()
    ) =>
    m ()
handleBGSave = do
    env <- ask

    let settings = view #settings env
    let socket = view #clientSocket env
    let serverStateRef = view #store env

    sendThroughSocket @_ @() socket . serializeRESPDataType $ SimpleString "Starting"

    void $
        liftIO $
            forkFinally
                (performRDBSave serverStateRef settings socket)
                ( \case
                    Left err -> handleBGSaveError socket err
                    Right _ -> sendThroughSocket @_ @() socket . serializeRESPDataType $ SimpleString "Background saving finished"
                )

performRDBSave ::
    ( MonadIO m
    , MonadMask m
    , MonadSocket m ()
    , HasCallStack
    , MonadConc m
    ) =>
    ServerStateRef -> ServerSettings -> Socket -> m ()
performRDBSave serverStateRef settings socket = do
    serverState <- readTVarConc serverStateRef
    rdbDirPath <- getRDBFilePathFromSettings settings
    rdbConfig <- mkRDBConfigFromSettings settings

    kvStore <- liftIO $ readTVarIO serverState.keyValueStore
    lastRDBSave <- liftIO $ readTVarIO serverState.lastRDBSave

    let notifyUserOfExistingBackgroundSave = sendThroughSocket @_ @() socket . serializeRESPDataType $ SimpleString "A background save is already in progress"

    -- We need to do all this to protect against an asynchronous exception occurring after we've potentially taken a lock on our TMVar but before we've put it back as such an interruption could lead us to a deadlock
    bracketOnError
        (liftIO $ atomically $ tryTakeTMVar lastRDBSave.current)
        ( \case
            Nothing -> notifyUserOfExistingBackgroundSave
            Just _ -> do
                currentTime <- liftIO getPOSIXTime
                let rdbFile = saveRedisStoreToRDB currentTime kvStore

                liftIO $ RDB.encodeFile rdbConfig (fromSomeFile rdbDirPath) rdbFile

                saveTime <- liftIO getCurrentTime
                liftIO $ atomically $ do
                    putTMVar lastRDBSave.current (Just saveTime)
                    modifyTVar serverState.lastRDBSave (set #previous (Just saveTime))
                sendThroughSocket @_ @() socket . serializeRESPDataType $ SimpleString "Background saving completed"
        )
        ( \case
            Nothing -> do
                -- Rather than throwing a user exception here with error regarding how this state should be impossible, we log it out as an error and send a message to the user's socket. This way we can be made aware of a critical failure without crashing
                -- Specializing to Text because I think the ToJSON instance for Text is more performant than that of String, since Aeson's Value type uses Text for strings internally
                liftIO $ runSimpleLoggingT $ logInternalServerError @_ @Text "Impossible exception: bracketOnError should always succeed if tryTakeTMVar fails with a Nothing. If not then something has gone very or wrong, or maybe the act of sending data to the user's socket failed or was interrupted somehow?"
                notifyUserOfExistingBackgroundSave
            Just mLastRDBSaveTimeM -> liftIO $ atomically $ putTMVar lastRDBSave.current mLastRDBSaveTimeM
        )

handleBGSaveError :: (MonadIO m, MonadSocket m (), Exception e) => Socket -> e -> m ()
handleBGSaveError socket e = do
    liftIO $ runSimpleLoggingT $ logInternalServerError (displayException e)
    sendThroughSocket @_ @() socket . serializeRESPDataType $ SimpleString "Background saving failed"

getRDBFilePathFromSettings :: (MonadThrow m, HasCallStack, Exception SaveFailure) => ServerSettings -> m (SomeBase File)
getRDBFilePathFromSettings (ServerSettings settings) = do
    dirPath <- maybe (throwM MissingDirSetting) pure $ HashMap.lookup (Setting "dir") settings
    fileName <- maybe (throwM MissingDBFilenameSetting) pure $ HashMap.lookup (Setting "dbfilename") settings

    case (dirPath, fileName) of
        (DirPathVal (Abs dir), FilePathVal (Rel file)) -> pure $ Abs $ dir </> file
        (DirPathVal (Rel dir), FilePathVal (Rel file)) -> pure $ Rel $ dir </> file
        _ -> throwM IncompatibleDirAndFilePathTypes

mkRDBConfigFromSettings :: (MonadThrow m, HasCallStack, Exception SaveFailure) => ServerSettings -> m RDBConfig
mkRDBConfigFromSettings (ServerSettings settings) = do
    rawRDBCompressionSetting <- maybe (throwM MissingRDBCompressionSetting) pure $ HashMap.lookup (Setting "rdbcompression") settings
    rawRDBChecksumSetting <- maybe (throwM MissingRDBChecksumSetting) pure $ HashMap.lookup (Setting "rdbchecksum") settings

    case (rawRDBCompressionSetting, rawRDBChecksumSetting) of
        (BoolVal rdbCompressionSettingVal, BoolVal rdbChecksumSettingVal) ->
            pure $
                RDBConfig
                    { useLzfCompression = rdbCompressionSettingVal
                    , generateChecksum = rdbChecksumSettingVal
                    , skipChecksumValidation = not rdbChecksumSettingVal
                    }
        _ -> throwM IncompatibleRDBCompressionOrChecksumSetting
