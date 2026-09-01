{-# LANGUAGE FieldSelectors #-}
{-# LANGUAGE UndecidableInstances #-}

module Redis.Test (
    PassableTestContext (..),
    runTestServer,
) where

import Blammo.Logging.Logger qualified as Blammo
import Data.List.NonEmpty qualified as NE
import Effectful qualified as Eff
import Effectful.Concurrent.STM qualified as Eff
import Effectful.FileSystem qualified as Eff
import Effectful.Reader.Static qualified as ReaderEff
import Redis.Effect.Communication qualified as Eff
import Redis.Effect.Logging qualified as Eff
import Redis.Effect.Time qualified as Eff

import Blammo.Logging (Logger)
import Redis.Server.Config (RedisConfig)
import Redis.Server.Config.Defaults (defaultRedisConfig, DefaultRedisConfig (..))
import Blammo.Logging.LogSettings (defaultLogSettings)
import Control.Concurrent.STM (atomically, newTVarIO)
import Data.ByteString (ByteString)
import Data.Maybe (fromMaybe)
import Data.Monoid (Last (..))
import Data.Time (UTCTime, getCurrentTime)
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
import Redis.Server.Metadata (Environment (..), RedisConfFilePath, ServerMetadata (..))
import Redis.ServerState (ServerState (..), genInitialServerStateEff)

data PassableTestContext = PassableTestContext
    { serverState :: Maybe ServerState
    , metadata :: Maybe TestServerMetadata
    , config :: Maybe RedisConfig
    }
    deriving stock (Generic)

data TestServerMetadata = TestServerMetadata
    { testStartTime :: UTCTime
    , testConfigFilePath :: Maybe RedisConfFilePath
    }

runTestServer :: Eff (ServerEffects ServerContext) a -> PassableTestContext -> IO ByteString
runTestServer action testContext =
    do
        loopbackSocket <- mkLoopbackSocket
        initialServerState <- atomically $ genInitialServerStateEff Nothing
        serverConfig <- newTVarIO . fromMaybe defaultRedisConfig.redisConf $ testContext.config

        now <- getCurrentTime
        let defaultServerMetadata = ServerMetadata{startTime = now, configFilePath = Nothing, environment = TEST}

        let serverState = fromMaybe initialServerState testContext.serverState
        let serverMetadata = maybe defaultServerMetadata fromTestServerMetadata testContext.metadata

        logger <- Blammo.newTestLogger defaultLogSettings
        let env = ServerContext loopbackSocket serverState serverConfig serverMetadata
        mRes <- runServer env logger action

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

    fromTestServerMetadata :: TestServerMetadata -> ServerMetadata
    fromTestServerMetadata TestServerMetadata{testStartTime, testConfigFilePath} =
        ServerMetadata
            { startTime = testStartTime
            , configFilePath = testConfigFilePath
            , environment = TEST
            }

mkLoopbackSocket :: IO Socket
mkLoopbackSocket = do
    let hints = defaultHints{addrFlags = [AI_NUMERICHOST, AI_NUMERICSERV], addrSocketType = Stream}
    addr <- NE.head <$> getAddrInfo (Just hints) (Just "127.0.0.1") (Just "5000")
    socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)
