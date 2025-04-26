module Commands.Ping (PingCmdArg (..), handlePingReq, mkPingCmdArgs) where

import Control.Lens
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Reader (MonadReader (ask))
import Data.Aeson (ToJSON)
import Data.Either.Extra (eitherToMaybe)
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8', encodeUtf8)
import GHC.Generics (Generic)
import Network.Socket (Socket)
import Network.Socket.ByteString (sendAll)
import RESP (BulkString (BulkString), RESPDataType (..), mkNonNullBulkString, serializeRESPDataType)
import Server.Env

-- https://redis.io/docs/latest/commands/ping/
-- We assume the option `message` argument of the ping command should only ever be considered as text

newtype PingCmdArg = PingCmdArg (Maybe Text)
    deriving stock (Eq, Show, Generic)
    deriving anyclass (ToJSON)

handlePingReq :: (HasClientSocket r Socket, MonadReader r m, MonadIO m) => PingCmdArg -> m ()
handlePingReq (PingCmdArg x) = do
    env <- ask
    let socket = view clientSocket env
    case x of
        (Just txt) -> liftIO . sendAll socket . serializeRESPDataType . mkNonNullBulkString . encodeUtf8 $ txt
        Nothing -> liftIO . sendAll socket . serializeRESPDataType . SimpleString $ "PONG"

mkPingCmdArgs :: (MonadFail m) => [BulkString] -> m PingCmdArg
mkPingCmdArgs [] = pure . PingCmdArg $ Nothing
mkPingCmdArgs [BulkString txt] = pure . PingCmdArg . eitherToMaybe $ decodeUtf8' txt
mkPingCmdArgs _ = fail "PING command does accept multiple arguments"
