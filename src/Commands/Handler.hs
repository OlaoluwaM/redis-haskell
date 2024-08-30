module Commands.Handler (handleCommandReq) where

import Commands.General.Types

import Data.Text qualified as T

import Commands.Connection.Echo (mkEchoCmdRunner)
import Commands.Connection.Ping (mkPingCmdRunner)
import Commands.General.Parser (commandParser)
import Commands.String.Set (mkSetCmdRunner)
import Data.Attoparsec.ByteString (parseOnly)
import Data.String.Interpolate (i)
import RESP.Helpers (mkNonNullBulkString)
import Utils (fromEither, mapLeft)

handleCommandReq :: RawCommandRequest -> CmdRunner
handleCommandReq cmdReq = do
    let command = fromEither . mapLeft (InvalidCommand . T.pack) . parseOnly commandParser $ cmdReq
    mkCmdRunner command

mkCmdRunner :: Command -> CmdRunner
mkCmdRunner (Ping arg) = mkPingCmdRunner arg
mkCmdRunner (Echo arg) = mkEchoCmdRunner arg
mkCmdRunner (Set (SetCmd key val cmdOpts)) = mkSetCmdRunner key val cmdOpts
mkCmdRunner (InvalidCommand msg) = pure . mkNonNullBulkString $ [i|Invalid Command: #{msg}|]
mkCmdRunner _ = undefined
