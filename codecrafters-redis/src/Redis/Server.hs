{-# LANGUAGE FieldSelectors #-}

module Redis.Server (
    module X,
    runServer,
) where

import Blammo.Logging.Simple
import Control.Monad.Reader (ReaderT (runReaderT))
import Data.Text (Text)
import Redis.Server.Context as X (ServerContext)
import Redis.Server.ServerT (ServerT (..))

type ServerM a = ServerT ServerContext (LoggingT IO) a

runServer :: ServerM () -> ServerContext -> IO ()
runServer action = runSimpleLoggingT . withThreadContext ["app" .= ("redis-server-hs" :: Text)] . runReaderT (unServerT action)
