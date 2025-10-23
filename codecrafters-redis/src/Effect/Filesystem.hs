{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Effect.Filesystem (
    MonadFileSystem (..),
) where

import Data.ByteString.Lazy qualified as BSL

import Control.Monad.Reader (ReaderT (..), lift)
import Prelude hiding (readFile, writeFile)

class MonadFileSystem m where
    readFile :: FilePath -> m BSL.ByteString
    writeFile :: FilePath -> BSL.ByteString -> m ()

instance MonadFileSystem IO where
    readFile = BSL.readFile
    writeFile = BSL.writeFile

instance (MonadFileSystem m, Monad m) => MonadFileSystem (ReaderT e m) where
    readFile filePath = lift $ readFile filePath

    writeFile filePath fileData = lift $ writeFile filePath fileData
