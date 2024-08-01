module Stage1 (simpleServer) where

import Network.Simple.TCP (HostPreference (HostAny), closeSock, serve)

simpleServer :: IO ()
simpleServer = do
    -- You can use print statements as follows for debugging, they'll be visible when running tests.
    putStrLn "Logs from your program will appear here"

    -- Uncomment this block to pass stage 1
    let port = "6379"
    putStrLn $ "Redis server listening on port " ++ port
    serve HostAny port $ \(socket, address) -> do
        putStrLn $ "successfully connected client: " ++ show address
        closeSock socket