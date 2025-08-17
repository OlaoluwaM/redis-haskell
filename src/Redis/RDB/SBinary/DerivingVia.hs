module Redis.RDB.SBinary.DerivingVia (
    BinaryFromSBinary (..)
) where

import Redis.RDB.SBinary
import Data.Binary 

import Control.Monad (void)

newtype BinaryFromSBinary a = BinaryFromSBinary {unBinaryFromSBinary :: a}
    deriving stock (Show)

instance (SBinary a) => Binary (BinaryFromSBinary a) where
    put (BinaryFromSBinary x) = void . execSPutWithChecksum . putWithChecksum $ x
    get = do
        (x, _) <- execSGetWithChecksum getWithChecksum
        pure $ BinaryFromSBinary x
