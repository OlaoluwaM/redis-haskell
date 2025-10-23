module Effect.Time (
    MonadTime (..),
) where

import Data.Time qualified as Time
import Data.Time.Clock.POSIX qualified as Time

import Control.Monad.Reader (ReaderT, lift)
import Data.Time (UTCTime)
import Data.Time.Clock.POSIX (POSIXTime)

class MonadTime m where
    getCurrentTime :: m UTCTime
    getPosixTime :: m POSIXTime

instance MonadTime IO where
    getCurrentTime = Time.getCurrentTime
    getPosixTime = Time.getPOSIXTime

instance (MonadTime m, Monad m) => MonadTime (ReaderT r m) where
    getCurrentTime = lift getCurrentTime
    getPosixTime = lift getPosixTime
