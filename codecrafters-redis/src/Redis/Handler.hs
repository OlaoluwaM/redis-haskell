module Redis.Handler (
    handleCommandReq,
) where

import Data.Text qualified as T

import Blammo.Logging (MonadLogger)
import Blammo.Logging.Simple (Message ((:#)), logInfo, (.=))
import Control.Monad.Catch (MonadCatch)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Reader (MonadReader (ask))
import Data.Attoparsec.ByteString (parseOnly)
import Data.ByteString (ByteString)
import Data.String.Interpolate (i)
import Network.Socket (Socket)
import Optics (A_Lens, LabelOptic, view)
import Redis.Commands.Config.Get (handleConfigGet)
import Redis.Commands.Echo (handleEcho)
import Redis.Commands.Get (handleGet)
import Redis.Commands.Parser (Command (..), ConfigSubCommand (ConfigGet), commandParser, mkInvalidCommand)
import Redis.Commands.Ping (handlePing)
import Redis.Commands.Save (handleSave)
import Redis.Commands.Set (handleSet)
import Redis.Server.ServerT (MonadSocket (..))
import Redis.Server.Settings (ServerSettings)
import Redis.Store (StoreState)
import Redis.Utils (fromEither, mapLeft)

handleCommandReq ::
    ( MonadLogger m
    , LabelOptic "clientSocket" A_Lens r r Socket Socket
    , LabelOptic "store" A_Lens r r StoreState StoreState
    , LabelOptic "settings" A_Lens r r ServerSettings ServerSettings
    , MonadSocket m b
    , MonadReader r m
    , MonadIO m
    , MonadCatch m
    ) =>
    ByteString -> m b
handleCommandReq rawCmdReq = do
    let command = fromEither . mapLeft (mkInvalidCommand . T.pack) . parseOnly commandParser $ rawCmdReq
    logInfo $ "Handling req for command" :# ["command" .= command]
    dispatchCmd command

dispatchCmd ::
    ( LabelOptic "clientSocket" A_Lens r r Socket Socket
    , LabelOptic "settings" A_Lens r r ServerSettings ServerSettings
    , MonadReader r m
    , MonadSocket m b
    , LabelOptic "store" A_Lens r r StoreState StoreState
    , MonadIO m
    , MonadCatch m
    ) =>
    Command -> m b
dispatchCmd (Ping pingCmdArgs) = handlePing pingCmdArgs
dispatchCmd (Echo echoCmdArgs) = handleEcho echoCmdArgs
dispatchCmd (Set setCmdArgs) = handleSet setCmdArgs
dispatchCmd (Get getCmdArgs) = handleGet getCmdArgs
dispatchCmd (Config (ConfigGet configGetCmdArgs)) = handleConfigGet configGetCmdArgs
dispatchCmd Save = handleSave
dispatchCmd (InvalidCommand msg) = do
    env <- ask
    let socket = view #clientSocket env
    sendThroughSocket socket [i|(error). Invalid Command or command not yet implemented: #{msg}|]
