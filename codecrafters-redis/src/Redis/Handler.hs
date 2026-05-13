module Redis.Handler (
    handleCommandReq,
) where

import Redis.Commands.BGSave
import Redis.Commands.Config.Get
import Redis.Commands.Echo
import Redis.Commands.Get
import Redis.Commands.Info
import Redis.Commands.LastSave
import Redis.Commands.Set
import Redis.Effects

import Data.Text qualified as T
import Effectful.Reader.Static qualified as ReaderEff

import Blammo.Logging (Message (..), (.=))
import Control.Exception (Exception (displayException))
import Data.Attoparsec.ByteString (parseOnly)
import Data.ByteString (ByteString)
import Data.String.Interpolate (i)
import Data.Text.Encoding (decodeUtf8')
import Effectful (Eff, (:>))
import Effectful.FileSystem qualified as Eff
import Optics (view)
import Redis.Commands.Keys (handleKeys)
import Redis.Commands.Parser (
    Command (..),
    ConfigSubCommand (ConfigGet),
    commandParser,
    mkInvalidCommand,
 )
import Redis.Commands.Ping (handlePing)
import Redis.Effect.Communication (sendMessage)
import Redis.Effect.Logging (logDebug)
import Redis.Utils (fromEither, mapLeft)

handleCommandReq ::
    forall r es.
    ( RedisClientCommunication r es
    , RedisServerState r es
    , RedisServerSettings r es
    , RedisServerMetadata r es
    , Eff.FileSystem :> es
    , Logging :> es
    ) =>
    ByteString -> Eff es ()
handleCommandReq rawCmdReq = do
    let command = fromEither . mapLeft (mkInvalidCommand . T.pack) . parseOnly commandParser $ rawCmdReq
    logDebug $ "Handling req for command" :# ["Command" .= command, "Raw command" .= mapLeft displayException (decodeUtf8' rawCmdReq)]
    dispatchCmd @r command

dispatchCmd ::
    forall r es.
    ( RedisClientCommunication r es
    , RedisServerState r es
    , RedisServerSettings r es
    , RedisServerMetadata r es
    , Eff.FileSystem :> es
    , Logging :> es
    ) =>
    Command -> Eff es ()
dispatchCmd (Ping pingCmdArgs) = handlePing @r pingCmdArgs
dispatchCmd (Echo echoCmdArgs) = handleEcho @r echoCmdArgs
dispatchCmd (Set setCmdArgs) = handleSet @r setCmdArgs
dispatchCmd (Get getCmdArgs) = handleGet @r getCmdArgs
dispatchCmd (Config (ConfigGet configGetCmdArgs)) = handleConfigGet @r configGetCmdArgs
dispatchCmd Save = handleSave @r
dispatchCmd (BGSave bgSaveCmdArgs) = handleBGSave @r bgSaveCmdArgs
dispatchCmd LastSave = handleLastSave @r
dispatchCmd (Keys keyCmdArgs) = handleKeys @r keyCmdArgs
dispatchCmd (Info infoCmdArgs) = handleInfo @r infoCmdArgs
dispatchCmd (InvalidCommand msg) = do
    env <- ReaderEff.ask @r
    let socket = view #clientSocket env
    sendMessage socket [i|(error). Invalid Command or command not yet implemented: #{msg}|]
