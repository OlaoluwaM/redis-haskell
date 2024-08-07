module Lib (main) where

import Control.Monad (forM_)
import Data.ByteString qualified as B
import Data.ByteString.Char8 qualified as BC
import Data.Maybe (fromMaybe)
import Network.Simple.TCP

main :: IO ()
main = do
    putStrLn "Logs from your program will appear here"

    let standardRedisServerPort = "6379"
    putStrLn $ "Redis server listening on port " ++ standardRedisServerPort
    serve HostAny standardRedisServerPort $ \(socket, address) -> do
        putStrLn $ "successfully connected client: " ++ show address
        receivedData <- fromMaybe "nothing" <$> recv socket 100
        let requests = BC.lines receivedData
        print requests
        forM_ requests $ \requestData ->
            if "PING" `B.isInfixOf` requestData || "ping" `B.isInfixOf` requestData
                then send socket "+PONG\r\n"
                else send socket "Bad Request\r\n"
