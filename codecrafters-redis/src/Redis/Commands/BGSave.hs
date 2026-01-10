module Redis.Commands.BGSave (
    handleBGSave,
    handleSave,
    mkBGSaveCmdArg,
    mkSaveCmdArg,
    ShouldSchedule (..),
    BGSaveCmdArg (..),
) where

import Path
import Redis.Server.Settings
import Redis.ServerState

import Effectful.Concurrent qualified as Eff
import Effectful.Concurrent.STM qualified as STMEff
import Effectful.Exception qualified as Eff
import Effectful.FileSystem qualified as Eff
import Effectful.Reader.Static qualified as ReaderEff
import Redis.RDB.Binary qualified as RDB

import Control.Concurrent.STM (
    modifyTVar,
    putTMVar,
    tryTakeTMVar,
 )
import Control.Exception (Exception (..), SomeException)
import Control.Monad (void)
import Data.Aeson (ToJSON)
import Data.Text (Text)
import Effect.Communication (Communication, sendMessage)
import Effect.Time (Time, getCurrentTime, getPosixTime)
import Effectful (Eff, (:>))
import Effectful.Log (Log)
import GHC.Generics (Generic)
import GHC.Stack (HasCallStack)
import Network.Socket (Socket)
import Optics (set, view)
import Redis.Effects (RDBWrite)
import Redis.RDB.Save (saveRedisStoreToRDB)
import Redis.RESP (BulkString (..), RESPDataType (SimpleString), serializeRESPDataType)
import Redis.Server.Settings.Get (genRDBConfigFromSettings, getRDBDumpFilePathFromSettings)
import Redis.Utils (logInternalServerError)

-- https://redis.io/docs/latest/commands/save/
-- https://redis.io/docs/latest/commands/bgsave/

newtype BGSaveCmdArg = BGSaveCmdArg (Maybe ShouldSchedule)
    deriving stock (Eq, Show, Generic)
    deriving anyclass (ToJSON)

-- Modeled but not used
newtype ShouldSchedule = ShouldSchedule Bool
    deriving stock (Eq, Show)
    deriving newtype (ToJSON)

mkBGSaveCmdArg :: (MonadFail m) => [BulkString] -> m BGSaveCmdArg
mkBGSaveCmdArg [] = pure $ BGSaveCmdArg Nothing
mkBGSaveCmdArg [BulkString "SCHEDULE"] = pure $ BGSaveCmdArg (Just $ ShouldSchedule True)
mkBGSaveCmdArg [BulkString invalidArg] = fail $ "Invalid argument to BGSAVE command: " <> show invalidArg <> ". Only SCHEDULE is supported."
mkBGSaveCmdArg _ = fail "BGSAVE command accepts only 1 argument"

mkSaveCmdArg :: (MonadFail m) => [BulkString] -> m ()
mkSaveCmdArg [] = pure ()
mkSaveCmdArg _ = fail "SAVE command does not accept any arguments"

handleSave ::
    forall r es.
    (RDBWrite r es) =>
    Eff es ()
handleSave = do
    env <- ReaderEff.ask @r

    let socket = view #clientSocket env
    let serverState = view #serverState env
    let serverSettingsRef = view #serverSettingsRef env

    let onSuccess = sendMessage socket . serializeRESPDataType $ SimpleString "OK"

    -- We are using handleSync here because should an asynchronous exception occur during the save process, we want to ensure that such an exception is propagated to the parent thread from which the thread that will end up performing this `handleSave` operation will be spawned. However, though, it looks like the distinction between `handle` and `handleSync` doesn't truly matter in practice here because unless we throw an asynchronous exception to the thread executing the `handleSave` operation, no asynchronous exceptions *can* be received here. If we start up the redis server and kill it that only affects the main thread, as in, the asynchronous exception that is raised from killing the process is sent to the main thread, not to any of the child threads spawned from it tasked with handling client connections and requests. Using `handleSync` here would ensure that if we were ever to throw an asynchronous exception to the child thread executing the save process, the async exception is not swallowed silently but rather propagated appropriately to the parent thread to be handled, cleaning up the child thread appropriately (at least, that would be ideal). Another thing to note is that we can't even easily gain access to the threadId of the relevant child thread either for a `throwTo` async exception. In any case, I'd like for an asynchronous exception to interrupt the save process in a similar manner as a synchronous exception would, so perhaps `handle` would be more appropriate after all.
    Eff.handle @SomeException
        (\e -> notifyOfSaveError socket e "Saving failed")
        (performRDBSave serverState serverSettingsRef socket onSuccess)

handleBGSave ::
    forall r es.
    (RDBWrite r es) =>
    BGSaveCmdArg -> Eff es ()
handleBGSave _ = do
    env <- ReaderEff.ask @r

    let socket = view #clientSocket env
    let serverState = view #serverState env
    let serverSettingsRef = view #serverSettingsRef env

    sendMessage socket . serializeRESPDataType $ SimpleString "Starting"

    void $
        Eff.forkFinally
            (performRDBBackgroundSave socket serverState serverSettingsRef)
            ( \case
                Left err -> notifyOfSaveError socket err "Background saving failed"
                Right _ -> sendMessage socket . serializeRESPDataType $ SimpleString "Background saving finished"
            )
  where
    performRDBBackgroundSave socket serverState serverSettingsRef =
        Eff.handle @SomeException
            Eff.throwIO -- If an error is thrown by performRDBSave, we handle it by rethrowing so we can satisfy/pop off the Error effect constraint and have the outer forkFinally handle it correctly
            (performRDBSave serverState serverSettingsRef socket (onSuccess socket))

    onSuccess socket = sendMessage socket . serializeRESPDataType $ SimpleString "Background saving completed"

performRDBSave ::
    ( HasCallStack
    , Log :> es
    , Eff.FileSystem :> es
    , Eff.Concurrent :> es
    , Communication :> es
    , Time :> es
    ) =>
    ServerState -> ServerSettingsRef -> Socket -> Eff es () -> Eff es ()
performRDBSave serverState serverSettingsRef socket onSuccess = do
    settings <- STMEff.readTVarIO serverSettingsRef
    let rdbDirPath = getRDBDumpFilePathFromSettings settings
    let rdbConfig = genRDBConfigFromSettings settings

    kvStore <- STMEff.readTVarIO serverState.keyValueStoreRef
    lastRDBSave <- STMEff.readTVarIO serverState.lastRDBSaveRef

    let notifyUserOfExistingBackgroundSave = sendMessage socket . serializeRESPDataType $ SimpleString "A background save is already in progress"

    -- We need to do all this to protect against an asynchronous exception occurring after we've potentially taken a lock on our TMVar but before we've put it back as such an interruption could lead us to a deadlock
    Eff.bracketOnError
        (STMEff.atomically $ tryTakeTMVar lastRDBSave.inProgress)
        ( \case
            Nothing -> do
                -- Rather than throwing a user exception here with error regarding how this state should be impossible, we log it out as an error and send a message to the user's socket. This way we can be made aware of a critical failure without crashing
                -- Specializing to Text because I think the ToJSON instance for Text is more performant than that of String, since Aeson's Value type uses Text for strings internally
                logInternalServerError @_ @Text "Impossible exception: bracketOnError should always succeed if tryTakeTMVar fails with a Nothing. If not then something has gone very or wrong, or maybe the act of sending data to the user's socket failed or was interrupted somehow?"
                notifyUserOfExistingBackgroundSave
            Just mLastRDBSaveTimeM -> do
                logInternalServerError @_ @Text "RDB save failed due to an exception"
                STMEff.atomically $ putTMVar lastRDBSave.inProgress mLastRDBSaveTimeM
        )
        ( \case
            Nothing -> do
                notifyUserOfExistingBackgroundSave
            Just _ -> do
                currentTime <- getPosixTime
                let rdbFile = saveRedisStoreToRDB currentTime kvStore

                RDB.encodeFile rdbConfig (fromSomeFile rdbDirPath) rdbFile

                saveTime <- getCurrentTime
                STMEff.atomically $ do
                    putTMVar lastRDBSave.inProgress (Just saveTime)
                    modifyTVar serverState.lastRDBSaveRef (set #lastCompleted (Just saveTime))

                onSuccess
        )

notifyOfSaveError :: (Log :> es, Communication :> es, Exception e) => Socket -> e -> Text -> Eff es ()
notifyOfSaveError socket e returnMsg = do
    logInternalServerError $ "Error occured while attempting to save RDB file: " <> displayException e
    sendMessage socket . serializeRESPDataType $ SimpleString returnMsg
