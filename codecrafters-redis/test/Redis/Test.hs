{-# LANGUAGE FieldSelectors #-}
{-# LANGUAGE UndecidableInstances #-}

module Redis.Test (
    PassableTestContext (..),
    runTestServer,
) where

import Blammo.Logging.Setup qualified as Blammo
import Data.List.NonEmpty qualified as NE
import Effectful qualified as Eff
import Effectful.Concurrent.STM qualified as Eff
import Effectful.FileSystem qualified as Eff
import Effectful.Reader.Static qualified as ReaderEff
import Redis.Effect.Communication qualified as Eff
import Redis.Effect.Logging qualified as Eff
import Redis.Effect.Time qualified as Eff

import Blammo.Logging (Logger)
import Control.Concurrent.STM (atomically, newTVarIO)
import Data.ByteString (ByteString)
import Data.Maybe (fromMaybe)
import Data.Monoid (Last (..))
import Effectful (Eff)
import GHC.Generics (Generic)

import Network.Socket (
    AddrInfo (addrFamily, addrFlags, addrProtocol, addrSocketType),
    AddrInfoFlag (..),
    Socket,
    SocketType (..),
    defaultHints,
    getAddrInfo,
    socket,
 )
import Redis.Effects (ServerEffects)
import Redis.Server.Context (ServerContext (..))
import Redis.Server.Settings (ServerSettings, defaultServerSettings)
import Redis.ServerState (ServerState (..), genInitialServerStateEff)

data PassableTestContext = PassableTestContext
    { serverState :: Maybe ServerState
    , settings :: Maybe ServerSettings
    }
    deriving stock (Generic)

runTestServer :: Eff (ServerEffects ServerContext) a -> PassableTestContext -> IO ByteString
runTestServer action testContext =
    do
        loopbackSocket <- mkLoopbackSocket
        initialServerState <- atomically $ genInitialServerStateEff Nothing
        serverSettings <- newTVarIO $ fromMaybe defaultServerSettings testContext.settings

        let serverState = fromMaybe initialServerState testContext.serverState

        mRes <- Blammo.withLoggerEnv $ \logger -> do
            let env = ServerContext loopbackSocket serverState serverSettings
            runServer env logger action

        pure $ fromMaybe "We got nothing bro. This probably shouldn't have happened" $ getLast mRes
  where
    runServer :: ServerContext -> Logger -> Eff (ServerEffects ServerContext) a -> IO (Last ByteString)
    runServer env logger =
        Eff.runEff
            . Eff.runCommunicationPure
            . Eff.runLoggingWithLogger logger
            . Eff.runGetTimeIO
            . Eff.runConcurrent
            . Eff.runFileSystem
            . ReaderEff.runReader env

mkLoopbackSocket :: IO Socket
mkLoopbackSocket = do
    let hints = defaultHints{addrFlags = [AI_NUMERICHOST, AI_NUMERICSERV], addrSocketType = Stream}
    addr <- NE.head <$> getAddrInfo (Just hints) (Just "127.0.0.1") (Just "5000")
    socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)
