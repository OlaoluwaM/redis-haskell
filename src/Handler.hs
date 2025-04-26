module Handler (
    handleCommandReq,
) where

import Data.Text qualified as T

import Blammo.Logging (MonadLogger)
import Blammo.Logging.Simple (Message ((:#)), logInfo, (.=))
import Commands.Parser (Command (..), commandParser, mkInvalidCommand)
import Commands.Ping (handlePingReq)
import Control.Lens (view)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Reader (MonadReader (ask))
import Data.Attoparsec.ByteString (parseOnly)
import Data.ByteString (ByteString)
import Data.String.Interpolate (i)
import Network.Socket (Socket)
import Network.Socket.ByteString (sendAll)
import Server.Env (HasClientSocket (clientSocket))
import Utils (fromEither, mapLeft)

handleCommandReq :: (MonadLogger m, HasClientSocket r Socket, MonadIO m, MonadReader r m) => ByteString -> m ()
handleCommandReq rawCmdReq = do
    let command = fromEither . mapLeft (mkInvalidCommand . T.pack) . parseOnly commandParser $ rawCmdReq
    logInfo $ "Handling req for command" :# ["command" .= command]
    dispatchCmd command

dispatchCmd :: (HasClientSocket r Socket, MonadReader r m, MonadIO m) => Command -> m ()
dispatchCmd (Ping pingCmdArgs) = handlePingReq pingCmdArgs
dispatchCmd (InvalidCommand msg) = do
    env <- ask
    let socket = view clientSocket env
    liftIO $ sendAll socket [i|Invalid Command: #{msg}|]
