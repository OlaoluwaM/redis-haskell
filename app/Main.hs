module Main (main) where

import Network.Run.TCP
import Network.Socket

import Data.ByteString qualified as BS

import Control.Concurrent.STM (newTVarIO)
import Control.Monad (unless)
import Network.Socket.ByteString (recv)
import Options.Applicative (
    execParser,
    fullDesc,
    header,
    helper,
    info,
    progDesc,
    (<**>),
 )
import Redis.Handler (handleCommandReq)
import Redis.Server (runServer)
import Redis.Server.Context (ServerContext (..))
import Redis.Server.Settings (ServerSettings, serverSettingsOptParser)
import Redis.Store (StoreState, genInitialStore)
import System.IO (BufferMode (NoBuffering), hSetBuffering, stderr, stdout)

main :: IO ()
main = do
    -- Disable output buffering
    hSetBuffering stdout NoBuffering
    hSetBuffering stderr NoBuffering

    putStrLn $ "Redis server listening on port " <> port
    initialStore <- newTVarIO genInitialStore
    serverSettings <- execParser opts
    runTCPServer Nothing port (handleRedisClientConnection initialStore serverSettings)
  where
    port :: String
    port = "6379"

    opts = info (serverSettingsOptParser <**> helper) (fullDesc <> progDesc "Redis server build in Haskell per CodeCrafters" <> header "A haskell redis server")

handleRedisClientConnection :: StoreState -> ServerSettings -> Socket -> IO ()
handleRedisClientConnection storeState serverSettings s = do
    req <- recv s 1024
    unless (BS.null req) $ do
        runServer (handleCommandReq req) (ServerContext s storeState serverSettings)
        handleRedisClientConnection storeState serverSettings s
