module Commands.Connection.Echo where

import Commands.Types (CmdRunner)
import Data.ByteString (ByteString)
import RESP.Helpers (mkNonNullBulkString)

mkEchoCmdRunner :: ByteString -> CmdRunner
mkEchoCmdRunner = pure . mkNonNullBulkString
