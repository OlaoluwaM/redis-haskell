{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE ForeignFunctionInterface #-}

module Redis.RDB.CRC64 where

import Data.ByteString (ByteString, useAsCString)
import Data.ByteString qualified as BS
import Foreign
import Foreign.C (CString)
import Foreign.C.ConstPtr (ConstPtr (ConstPtr))
import Foreign.C.Types
import GHC.IO (unsafePerformIO)

foreign import capi "crc64.h crc64"
    c_crc64 :: CULong -> ConstPtr CChar -> CULong -> CULong

crc64 :: Word64 -> ByteString -> Word64
crc64 seed bs = fromIntegral $ c_crc64 (fromIntegral seed) (toConstPtr bs) (fromIntegral $ BS.length bs)
  where
    toConstPtr :: ByteString -> ConstPtr CChar
    toConstPtr bs' = ConstPtr . unsafePerformIO $ BS.useAsCString bs' pure
