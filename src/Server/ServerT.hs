{-# LANGUAGE FieldSelectors #-}

module Server.ServerT (ServerT (..)) where

import Blammo.Logging.Simple (MonadLogger, MonadLoggerIO)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Reader (MonadReader, ReaderT)
import Control.Monad.Trans.Class (MonadTrans)

newtype ServerT r m a = ServerT {unServerT :: ReaderT r m a}
    deriving newtype (Monad, Functor, Applicative, MonadIO, MonadTrans, MonadReader r, MonadLogger, MonadLoggerIO)
