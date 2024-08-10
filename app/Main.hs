module Main where

import Network.Socket.ByteString

import Data.ByteString qualified as B

import Control.Monad (unless)
import Network.Run.TCP (runTCPServer)
import RESP (handleReq)

main :: IO ()
main = do
    let standardRedisServerPort = "6379"
    runTCPServer Nothing standardRedisServerPort handleServer
  where
    handleServer sock = do
        cmdReq <- recv sock 1024
        unless (B.null cmdReq) $ do
            let response = handleReq cmdReq
            sendAll sock response
            handleServer sock
