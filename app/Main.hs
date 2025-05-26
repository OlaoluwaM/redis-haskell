module Main (main) where

import Network.Run.TCP
import Network.Socket

import Data.ByteString qualified as BS

import Control.Concurrent.STM (newTVarIO)
import Control.Monad (unless)
import Data.Default (def)
import Network.Socket.ByteString (recv)
import Redis.Handler (handleCommandReq)
import Redis.Server (runServer)
import Redis.Server.Context (ServerContext (..))
import Redis.Store (StoreState, genInitialStore)
import System.IO (BufferMode (NoBuffering), hSetBuffering, stderr, stdout)

main :: IO ()
main = do
    -- Disable output buffering
    hSetBuffering stdout NoBuffering
    hSetBuffering stderr NoBuffering

    putStrLn $ "Redis server listening on port " <> port
    initialStore <- newTVarIO genInitialStore
    runTCPServer Nothing port (handleRedisClientConnection initialStore)
  where
    port :: String
    port = "6379"

handleRedisClientConnection :: StoreState -> Socket -> IO ()
handleRedisClientConnection storeState s = do
    req <- recv s 1024
    unless (BS.null req) $ do
        runServer (handleCommandReq req) (ServerContext s storeState def)
        handleRedisClientConnection storeState s
