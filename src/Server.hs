{-# LANGUAGE FieldSelectors #-}

module Server (
    module X,
    runServer,
) where

import Blammo.Logging.Simple
import Control.Monad.Reader (ReaderT (runReaderT))
import Data.Text (Text)
import Server.Env as X (Env)
import Server.ServerT (ServerT (..))

type ServerM a = ServerT Env (LoggingT IO) a

runServer :: ServerM a -> Env -> IO a
runServer action = runSimpleLoggingT . withThreadContext ["app" .= ("redis-server-hs" :: Text)] . runReaderT (unServerT action)
