module Commands.Echo (EchoCmdArg (..), handleEcho, mkEchoCmdArg) where

import Control.Lens
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Reader (MonadReader (ask))
import Data.Aeson (ToJSON)
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8', encodeUtf8)
import GHC.Generics (Generic)
import Network.Socket (Socket)
import Network.Socket.ByteString (sendAll)
import RESP (BulkString (BulkString), mkNonNullBulkString, serializeRESPDataType)
import Server.Env

-- https://redis.io/docs/latest/commands/echo/
-- We assume the option `message` argument of the echo command should only ever be considered as text

newtype EchoCmdArg = EchoCmdArg Text
    deriving stock (Eq, Show, Generic)
    deriving anyclass (ToJSON)

handleEcho :: (HasClientSocket r Socket, MonadReader r m, MonadIO m) => EchoCmdArg -> m ()
handleEcho (EchoCmdArg msg) = do
    env <- ask
    let socket = view clientSocket env
    liftIO . sendAll socket . serializeRESPDataType . mkNonNullBulkString . encodeUtf8 $ msg

mkEchoCmdArg :: (MonadFail m) => [BulkString] -> m EchoCmdArg
mkEchoCmdArg [] = fail "ECHO command requires at least 1 argument. None were provided"
mkEchoCmdArg [BulkString txt] = case decodeUtf8' txt of
    (Right msg) -> pure . EchoCmdArg $ msg
    Left _ -> fail "Message received as argument for ECHO command is not valid text"
mkEchoCmdArg _ = fail "ECHO command requires only 1 argument"
