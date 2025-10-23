module Effect.Filesystem.Handle (
    MonadHandle (..),
) where

import Data.ByteString qualified as BS
import System.IO qualified as SysIO

import Control.Monad.Reader (MonadTrans (lift), ReaderT (..))
import System.IO (Handle, IOMode)

class MonadHandle m where
    type Hndle m
    withBinaryFile :: FilePath -> IOMode -> (Hndle m -> m a) -> m a
    hGet :: Hndle m -> Int -> m BS.ByteString
    hPut :: Hndle m -> BS.ByteString -> m ()

instance MonadHandle IO where
    type Hndle IO = Handle
    withBinaryFile = SysIO.withBinaryFile
    hGet = BS.hGet
    hPut = BS.hPut

instance (MonadHandle m, Monad m) => MonadHandle (ReaderT r m) where
    type Hndle (ReaderT r m) = Hndle m
    withBinaryFile fp iomode f = ReaderT $ \r -> withBinaryFile fp iomode (\h -> runReaderT (f h) r)

    hGet h = lift . hGet h

    hPut h = lift . hPut h
