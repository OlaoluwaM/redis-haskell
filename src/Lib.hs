module Lib (main) where

import Data.ByteString (isInfixOf)
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
        if "PING" `isInfixOf` receivedData then send socket "+PONG\r\n" else send socket "Bad Request\r\n"
