module Redis.Commands.Ping (PingCmdArg (..), handlePing, mkPingCmdArg) where

import Control.Monad.Reader (MonadReader (ask))
import Data.Aeson (ToJSON)
import Data.Either.Extra (eitherToMaybe)
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8', encodeUtf8)
import GHC.Generics (Generic)
import Network.Socket (Socket)
import Optics (A_Lens, LabelOptic, view)
import Redis.RESP (BulkString (BulkString), RESPDataType (..), mkNonNullBulkString, serializeRESPDataType)
import Redis.Server.ServerT (MonadSocket (..))

-- https://redis.io/docs/latest/commands/ping/
-- We assume the option `message` argument of the ping command should only ever be considered as text

newtype PingCmdArg = PingCmdArg (Maybe Text)
    deriving stock (Eq, Show, Generic)
    deriving anyclass (ToJSON)

handlePing ::
    ( LabelOptic "clientSocket" A_Lens r r Socket Socket
    , MonadReader r m
    , MonadSocket m b
    ) =>
    PingCmdArg -> m b
handlePing (PingCmdArg x) = do
    env <- ask
    let socket = view #clientSocket env
    case x of
        (Just txt) -> sendThroughSocket socket . serializeRESPDataType . mkNonNullBulkString . encodeUtf8 $ txt
        Nothing -> sendThroughSocket socket . serializeRESPDataType . SimpleString $ "PONG"

mkPingCmdArg :: (MonadFail m) => [BulkString] -> m PingCmdArg
mkPingCmdArg [] = pure . PingCmdArg $ Nothing
mkPingCmdArg [BulkString txt] = pure . PingCmdArg . eitherToMaybe $ decodeUtf8' txt
mkPingCmdArg _ = fail "PING command does accept multiple arguments"
