module Main (main) where

import Control.Monad (unless)
import Data.ByteString qualified as BS
import Handler (handleCommandReq)
import Network.Run.TCP
import Network.Socket
import Network.Socket.ByteString (recv)
import Server (runServer)
import Server.Env (Env (..))
import System.IO (BufferMode (NoBuffering), hSetBuffering, stderr, stdout)

main :: IO ()
main = do
    -- Disable output buffering
    hSetBuffering stdout NoBuffering
    hSetBuffering stderr NoBuffering

    putStrLn $ "Redis server listening on port " ++ port
    runTCPServer Nothing port handleRedisClientConnection
  where
    port :: String
    port = "6379"

handleRedisClientConnection :: Socket -> IO ()
handleRedisClientConnection s = do
    req <- recv s 1024
    unless (BS.null req) $ do
        runServer (handleCommandReq req) (Env s ())
        handleRedisClientConnection s
