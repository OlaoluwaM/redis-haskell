module Redis.Handler (
    handleCommandReq,
) where

import Data.Text qualified as T

import Blammo.Logging (MonadLogger)
import Blammo.Logging.Simple (Message ((:#)), logInfo, (.=))
import Redis.Commands.Echo (handleEcho)
import Redis.Commands.Parser (Command (..), commandParser, mkInvalidCommand)
import Redis.Commands.Ping (handlePing)
import Control.Lens (view)
import Control.Monad.Reader (MonadReader (ask))
import Data.Attoparsec.ByteString (parseOnly)
import Data.ByteString (ByteString)
import Data.String.Interpolate (i)
import Network.Socket (Socket)
import Redis.Server.Env (HasClientSocket (clientSocket))
import Redis.Server.ServerT (MonadSocket (..))
import Redis.Utils (fromEither, mapLeft)

handleCommandReq :: (MonadLogger m, HasClientSocket r Socket, MonadSocket m b, MonadReader r m) => ByteString -> m b
handleCommandReq rawCmdReq = do
    let command = fromEither . mapLeft (mkInvalidCommand . T.pack) . parseOnly commandParser $ rawCmdReq
    logInfo $ "Handling req for command" :# ["command" .= command]
    dispatchCmd command

dispatchCmd :: (HasClientSocket r Socket, MonadReader r m, MonadSocket m b) => Command -> m b
dispatchCmd (Ping pingCmdArg) = handlePing pingCmdArg
dispatchCmd (Echo echoCmdArg) = handleEcho echoCmdArg
dispatchCmd (InvalidCommand msg) = do
    env <- ask
    let socket = view clientSocket env
    sendThroughSocket socket [i|Invalid Command: #{msg}|]
