module Main where

-- import Network.Socket.ByteString

-- import Data.ByteString qualified as B

-- import Commands.Handler (handleCommandReq)
import Control.Concurrent.STM.TVar (newTVarIO)

-- import Control.Monad (unless)
import Network.Run.TCP (runTCPServer)

-- import Data.String.Interpolate ( i )
import Store.Operations (initialStore)

main :: IO ()
main = do
    let standardRedisServerPort = "6379"
    _ <- newTVarIO initialStore
    putStrLn ("Server listening on port " <> standardRedisServerPort)
    runTCPServer Nothing standardRedisServerPort handleServer
  where
    handleServer _ = do
        -- cmdReq <- recv sock 1024
        undefined

-- unless (B.null cmdReq) $ do
--     let response = handleCommandReq cmdReq
--     print @String [i|Req: "#{cmdReq}", and Resp: "#{response}"|]
--     sendAll sock response
--     handleServer sock
