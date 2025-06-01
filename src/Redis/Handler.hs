module Redis.Handler (
    handleCommandReq,
) where

import Data.Text qualified as T

import Blammo.Logging (MonadLogger)
import Blammo.Logging.Simple (Message ((:#)), logInfo, (.=))
import Control.Lens (view)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Reader (MonadReader (ask))
import Data.Attoparsec.ByteString (parseOnly)
import Data.ByteString (ByteString)
import Data.String.Interpolate (i)
import Network.Socket (Socket)
import Redis.Commands.Config.Get (handleConfigGet)
import Redis.Commands.Echo (handleEcho)
import Redis.Commands.Get (handleGet)
import Redis.Commands.Parser (Command (..), ConfigSubCommand (ConfigGet), commandParser, mkInvalidCommand)
import Redis.Commands.Ping (handlePing)
import Redis.Commands.Set (handleSet)
import Redis.Server.Context (HasClientSocket (clientSocket), HasSettings, HasStore)
import Redis.Server.ServerT (MonadSocket (..))
import Redis.Server.Settings (ServerSettings)
import Redis.Store (StoreState)
import Redis.Utils (fromEither, mapLeft)

handleCommandReq :: (MonadLogger m, HasClientSocket r Socket, HasSettings r ServerSettings, MonadSocket m b, HasStore r StoreState, MonadReader r m, MonadIO m) => ByteString -> m b
handleCommandReq rawCmdReq = do
    let command = fromEither . mapLeft (mkInvalidCommand . T.pack) . parseOnly commandParser $ rawCmdReq
    logInfo $ "Handling req for command" :# ["command" .= command]
    dispatchCmd command

dispatchCmd :: (HasClientSocket r Socket, HasSettings r ServerSettings, MonadReader r m, MonadSocket m b, HasStore r StoreState, MonadIO m) => Command -> m b
dispatchCmd (Ping pingCmdArgs) = handlePing pingCmdArgs
dispatchCmd (Echo echoCmdArgs) = handleEcho echoCmdArgs
dispatchCmd (Set setCmdArgs) = handleSet setCmdArgs
dispatchCmd (Get getCmdArgs) = handleGet getCmdArgs
dispatchCmd (Config (ConfigGet configGetCmdArgs)) = handleConfigGet configGetCmdArgs
dispatchCmd (InvalidCommand msg) = do
    env <- ask
    let socket = view clientSocket env
    sendThroughSocket socket [i|(error). Invalid Command or command not yet implemented: #{msg}|]
