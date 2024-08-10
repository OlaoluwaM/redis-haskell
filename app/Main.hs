module Main where

import Network.Socket.ByteString

import Data.ByteString qualified as B

import Control.Monad (unless)
import Network.Run.TCP (runTCPServer)
import PyF (fmt)
import RESP (handleReq)

main :: IO ()
main = do
    let standardRedisServerPort = "6379"
    putStrLn ("Server listening on port " <> standardRedisServerPort)
    runTCPServer Nothing standardRedisServerPort handleServer
  where
    handleServer sock = do
        cmdReq <- recv sock 1024
        unless (B.null cmdReq) $ do
            let response = handleReq cmdReq
            print @String [fmt|Req: "{cmdReq}", and Resp: "{response}"|]
            sendAll sock response
            handleServer sock
