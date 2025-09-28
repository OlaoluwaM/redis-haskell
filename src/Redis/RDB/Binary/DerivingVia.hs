module Redis.RDB.Binary.DerivingVia (
    BinaryFromRDBBinary (..),
) where

import Data.Binary
import Redis.RDB.Binary

import Control.Monad (void)
import Redis.RDB.Config (defaultRDBConfig)

newtype BinaryFromRDBBinary a = BinaryFromRDBBinary {unBinaryFromRDBBinary :: a}
    deriving stock (Show)

instance (RDBBinary a) => Binary (BinaryFromRDBBinary a) where
    put (BinaryFromRDBBinary x) = void . execRDBPut defaultRDBConfig . rdbPut $ x
    get = do
        (x, _) <- execRDBGet defaultRDBConfig rdbGet
        pure $ BinaryFromRDBBinary x
