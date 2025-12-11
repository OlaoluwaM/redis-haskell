module Redis.Commands.Keys (
    KeysCmdArg (..),
    mkKeysCmdArg,
    handleKeys,
) where

import Redis.RESP
import Redis.ServerState

import Data.HashMap.Strict qualified as HashMap
import Effectful.Reader.Static qualified as ReaderEff

import Data.Aeson (ToJSON (..))
import Data.String (IsString (fromString))
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding (decodeUtf8)
import Effect.Communication (sendMessage)
import Effectful (Eff)
import Effectful.Concurrent.STM qualified as STMEff
import GHC.Generics (Generic)
import Optics (view)
import Redis.Effects (RedisClientCommunication, RedisServerState)
import System.FilePath.Glob (compile, match)

-- https://redis.io/docs/latest/commands/keys/

newtype KeysCmdArg = KeysCmdArg {rawKeyGlobPattern :: Text}
    deriving stock (Eq, Show, Generic)
    deriving anyclass (ToJSON)

mkKeysCmdArg :: (MonadFail m) => [BulkString] -> m KeysCmdArg
mkKeysCmdArg [] = fail "KEYS command requires an argument"
mkKeysCmdArg [BulkString rawKeyGlobPattern] = pure . KeysCmdArg . decodeUtf8 $ rawKeyGlobPattern
mkKeysCmdArg _ = fail "KEYS command requires only 1 argument"

handleKeys ::
    forall r es.
    ( RedisClientCommunication r es
    , RedisServerState r es
    ) =>
    KeysCmdArg -> Eff es ()
handleKeys (KeysCmdArg rawKeyGlobPattern) = do
    env <- ReaderEff.ask @r

    let socket = view #clientSocket env
    let serverState = view #serverState env

    store <- STMEff.readTVarIO serverState.keyValueStoreRef

    let keyGlobPattern = compile . T.unpack $ rawKeyGlobPattern

    let storeWithKeysMatchingPattern = HashMap.filterWithKey (\(StoreKey key) _ -> let normalizedStoreKey = T.unpack . T.toLower . decodeUtf8 $ key in keyGlobPattern `match` normalizedStoreKey) store

    let result = HashMap.keys storeWithKeysMatchingPattern

    sendMessage socket . serializeRESPDataType . mkNonNullRESPArray . map (mkNonNullBulkString . (.key)) $ result
