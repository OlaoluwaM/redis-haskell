module Commands.Connection.Ping where

import Commands.Types (CmdRunner)
import Data.ByteString (ByteString)
import RESP.Helpers (mkNonNullBulkString)
import RESP.Types (RESPDataType (SimpleString))

mkPingCmdRunner :: Maybe ByteString -> CmdRunner
mkPingCmdRunner (Just txt) = pure . mkNonNullBulkString $ txt
mkPingCmdRunner Nothing = pure . SimpleString $ "PONG"
