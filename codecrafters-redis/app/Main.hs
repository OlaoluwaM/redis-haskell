{-# OPTIONS_GHC -Wno-warnings-deprecations #-}

module Main (main) where

import Redis.Server.Settings
import Redis.ServerState

import Blammo.Logging qualified as Blammo
import Blammo.Logging.Setup qualified as Blammo
import Data.ByteString qualified as BS
import Data.List.NonEmpty qualified as NE
import Effectful qualified as Eff
import Effectful.FileSystem qualified as Eff
import Redis.Effect.Logging qualified as Eff
import Redis.Server.Metadata qualified as Metadata

import Blammo.Logging (Logger, Message (..), (.=))
import Control.Concurrent (forkFinally, myThreadId)
import Control.Concurrent.STM (atomically, newTVarIO)
import Control.Exception (
    Exception (displayException),
    SomeException,
    bracket,
    bracketOnError,
    catch,
    throwIO,
 )
import Control.Monad (forever, unless, void)
import Data.String (IsString (fromString))
import Data.Time (getCurrentTime)
import GHC.Conc (labelThread)
import Network.Run.TCP (openTCPServerSocket, resolve)
import Network.Socket (AddrInfoFlag (..), HostName, ServiceName, Socket, SocketType (..), accept, close, gracefulClose)
import Network.Socket.ByteString (recv)
import Options.Applicative (
    execParser,
    fullDesc,
    header,
    helper,
    info,
    progDesc,
    simpleVersioner,
    (<**>),
 )
import Redis.Handler (handleCommandReq)
import Redis.RDB.Load (loadStoreFromRDBDump)
import Redis.Server (runServer)
import Redis.Server.Context (ServerContext (..))
import Redis.Server.Metadata (ServerMetadata (..))
import Redis.Server.Settings.Get (getRedisPortFromSettings)
import Redis.Server.Version (redisVersion)
import System.IO (BufferMode (NoBuffering), hSetBuffering, stderr, stdout)

main :: IO ()
main = do
    -- Disable output buffering
    hSetBuffering stdout NoBuffering
    hSetBuffering stderr NoBuffering

    settings <- execParser serverSettingsParser
    environment <- Metadata.loadEnvironment
    let port = getRedisPortFromSettings settings.settingsFromCommandLine
    let configFilePathM = settings.settingsConfigFilePath

    Blammo.withLoggerEnv $ \logger -> do
        Blammo.runLoggerLoggingT logger $ Blammo.logDebug "Loading initial store from RDB dump file..."
        mInitialStore <-
            (fmap . fmap) fst
                . Eff.runEff
                . Eff.runLoggingWithLogger logger
                . Eff.runFileSystem
                $ loadStoreFromRDBDump settings.settingsFromCommandLine

        serverSettingsRef <- newTVarIO settings.settingsFromCommandLine
        initialServerStateRef <- atomically $ genInitialServerStateEff mInitialStore

        maybe
            (Blammo.runLoggerLoggingT logger $ Blammo.logDebug "No initial store loaded from RDB dump file. Defaulting to empty store.")
            (const . Blammo.runLoggerLoggingT logger $ Blammo.logDebug "Initial store loaded from RDB dump file.")
            mInitialStore

        Blammo.runLoggerLoggingT logger $ Blammo.logInfo (fromString $ "Redis server listening on port " <> fromString port)

        startupTime <- getCurrentTime

        let serverMetadata =
                ServerMetadata
                    { startTime = startupTime
                    , configFilePath = configFilePathM
                    , environment = environment
                    }

        runTCPServer Nothing port logger (handleRedisClientConnection initialServerStateRef serverSettingsRef logger serverMetadata)
  where
    serverSettingsParser =
        info
            (serverSettings <**> helper <**> simpleVersioner redisVersion)
            (fullDesc <> progDesc "Redis server build in Haskell per CodeCrafters" <> header "A haskell redis server")

handleRedisClientConnection :: ServerState -> ServerSettingsRef -> Logger -> ServerMetadata -> Socket -> IO ()
handleRedisClientConnection serverState settingsRef logger metadata socket = do
    req <- recv socket 1024
    unless (BS.null req) $ do
        catch @SomeException
            ( runServer
                (ServerContext socket serverState settingsRef metadata)
                logger
                (handleCommandReq @ServerContext req)
            )
            ( \e -> do
                Blammo.runLoggerLoggingT logger $ Blammo.logError $ "An error occurred while handling client request" :# ["Error" .= displayException e]
                throwIO e -- Rethrow after logging so that the connection can be closed properly
            )
        handleRedisClientConnection serverState settingsRef logger metadata socket

-- | Running a TCP server with an accepted socket and its peer name. Lifted from https://www.stackage.org/haddock/lts-24.25/network-run-0.4.4/src/Network.Run.TCP.html#runTCPServer
runTCPServer :: Maybe HostName -> ServiceName -> Logger -> (Socket -> IO a) -> IO a
runTCPServer mhost port logger server = do
    addr <- resolve Stream mhost port [AI_PASSIVE] NE.head
    bracket (openTCPServerSocket addr) close $ \sock ->
        runTCPServerWithSocket sock logger server

{- | Running a TCP client with a connected socket for a given listen
socket.
-}
runTCPServerWithSocket ::
    Socket ->
    Logger ->
    -- | Called for each incoming connection, in a new thread
    (Socket -> IO a) ->
    IO a
runTCPServerWithSocket sock logger server = forever $ do
    bracketOnError (accept sock) (close . fst) $
        \(conn, _peer) ->
            void $
                forkFinally
                    (labelMe "TCP server" >> server conn)
                    ( \case
                        Left exception -> do
                            void $ Blammo.runLoggerLoggingT logger $ Blammo.logError $ "Connection closed with error" :# ["error" .= displayException exception]
                            gclose conn
                        Right _ -> gclose conn
                    )
  where
    gclose :: Socket -> IO ()
    gclose sock' = gracefulClose sock' 5000

    labelMe :: String -> IO ()
    labelMe name = do
        tid <- myThreadId
        labelThread tid name
