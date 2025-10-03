module Main (main) where

import Data.ByteString qualified as BS

import Control.Concurrent.STM (newTVarIO)
import Control.Monad (unless)
import Network.Run.TCP (runTCPServer)
import Network.Socket (Socket)
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
import Redis.Server (runServer)
import Redis.Server.Context (ServerContext (..))
import Redis.Server.Settings (ServerSettings, Settings (..), serverSettings)
import Redis.Server.Version (redisVersion)
import Redis.Store (StoreState, genInitialStore)
import System.IO (BufferMode (NoBuffering), hSetBuffering, stderr, stdout)

main :: IO ()
main = do
    -- Disable output buffering
    hSetBuffering stdout NoBuffering
    hSetBuffering stderr NoBuffering

    settings <- execParser serverSettingsParser
    putStrLn $ "Redis server listening on port " <> port
    initialStore <- newTVarIO genInitialStore
    runTCPServer Nothing port (handleRedisClientConnection initialStore settings.settingsFromCommandLine)
  where
    port :: String
    port = "6379"

    serverSettingsParser = info (serverSettings <**> helper <**> simpleVersioner redisVersion) (fullDesc <> progDesc "Redis server build in Haskell per CodeCrafters" <> header "A haskell redis server")

handleRedisClientConnection :: StoreState -> ServerSettings -> Socket -> IO ()
handleRedisClientConnection storeState settings s = do
    req <- recv s 1024
    unless (BS.null req) $ do
        runServer (handleCommandReq req) (ServerContext s storeState settings)
        handleRedisClientConnection storeState settings s
