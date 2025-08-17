{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE ForeignFunctionInterface #-}

module Redis.RDB.CRC64 (
    crc64,
    CheckSum,
    initialChecksum,
    fromChecksum,
    toChecksum,
)
where

import Data.ByteString qualified as BS

import Data.ByteString (ByteString)
import Data.Word (Word64)
import Foreign.C (CString)
import Foreign.C.Types (CULLong (..))
import System.IO.Unsafe (unsafePerformIO)

foreign import ccall unsafe "crc64.h crc64_init" c_crc64_init :: IO ()
foreign import ccall unsafe "crc64.h crc64" c_crc64 :: CULLong -> CString -> CULLong -> CULLong

newtype CheckSum = CheckSum {getChecksum :: Word64}
    deriving stock (Show, Eq, Ord)
    deriving newtype (Num)

crc64 :: CheckSum -> ByteString -> CheckSum
crc64 seed bs = unsafePerformIO $ do
    -- ensure tables are initialized otherwise function will always return 0
    c_crc64_init
    BS.useAsCStringLen bs $ \(cs, len) -> do
        let cksum = c_crc64 (checksumIntegral seed) cs (fromIntegral len)
        pure (toChecksumFromIntegral cksum)

checksumIntegral :: (Num b) => CheckSum -> b
checksumIntegral = fromIntegral . (.getChecksum)

toChecksumFromIntegral :: (Integral a) => a -> CheckSum
toChecksumFromIntegral = CheckSum . fromIntegral @_ @Word64

fromChecksum :: CheckSum -> Word64
fromChecksum = (.getChecksum)

toChecksum :: Word64 -> CheckSum
toChecksum = CheckSum

initialChecksum :: CheckSum
initialChecksum = CheckSum 0
