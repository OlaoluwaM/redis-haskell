module Main where

import Network.Socket.ByteString

import Data.ByteString qualified as B

import Commands.Handler (handleCommandReq)
import Commands.Types (Env (Env))
import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TVar (newTVarIO)
import Control.Monad (unless)
import Control.Monad.Except (runExceptT)
import Control.Monad.Reader (ReaderT (runReaderT))
import Data.String.Interpolate (i)
import Data.Time (getCurrentTime)
import Network.Run.TCP (runTCPServer)
import RESP.Parser (serializeRESPDataType)
import Store.Operations (initialStore)
import Utils (convergeEither)

main :: IO ()
main = do
    let standardRedisServerPort = "6379"
    storeState <- newTVarIO initialStore
    putStrLn [i|Server listening on port #{standardRedisServerPort}|]
    runTCPServer Nothing standardRedisServerPort (handleServer storeState)
  where
    handleServer storeState sock = do
        cmdReq <- recv sock 1024
        unless (B.null cmdReq) $ do
            now <- getCurrentTime
            let storeOperation = fmap (convergeEither serializeRESPDataType) . runExceptT . runReaderT (handleCommandReq cmdReq) $ Env storeState now
            response <- atomically storeOperation
            -- Naive logging
            print @String [i|Req: "#{cmdReq}", and Resp: "#{response}"|]
            sendAll sock response
            handleServer storeState sock
