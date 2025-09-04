{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# OPTIONS_GHC -fno-cse #-}

module Redis.RDB.LZF (
    lzfCompress,
    lzfDecompress,

    -- ** For Testing
    minimumByteStringLength
) where

import Data.ByteString qualified as BS
import Data.ByteString.Unsafe qualified as BSU

import Data.ByteString (ByteString)
import Foreign (allocaBytes, mallocBytes)
import Foreign.C (CString, CUInt (..))
import System.IO.Unsafe (unsafePerformIO)

foreign import capi "lzf.h lzf_compress" lzf_compress :: CString -> CUInt -> CString -> CUInt -> IO CUInt
foreign import capi "lzf.h lzf_decompress" lzf_decompress :: CString -> CUInt -> CString -> CUInt -> IO CUInt

{-# WARNING in "x-unsafe-internals" minimumByteStringLength "This value is exported for testing purposes only" #-}
minimumByteStringLength :: Int
minimumByteStringLength = 20 -- This is necessary to avoid issues with very small inputs that cannot be effectively compressed. Due to the way lzf compressions works, small inputs may not have enough data to compress and the end result may even be expanded due to the overhead of the compression algorithm. Input that is smaller than or equal to this threshold is unlikely to benefit from compression. This threshold was chosen via trial and error

-- Use of the unsafe function from Data.ByteString.Unsafe is fine here because neither we nor C mutates the underlying CString representation of the ByteString thus we avoid breaking referential transparency through mutation

-- The underlying C functions (lzf_compress & lzf_decompress) treat the bytestring input as immutable since the parameter for the input pointer has a const modifier on both the pointer and the data referenced by the pointer

--- lzf compression and decompression can fail, so we return Maybe ByteString to express that possibility. We _could_ return the bytestring as is, but there wouldn't be much use for it at that point

-- NOTE: lzf_compress is likely to expand small inputs due to compression metadata overhead, potentially no patterns to compress in small inputs

{-# NOINLINE lzfCompress #-}
lzfCompress :: ByteString -> Maybe ByteString
lzfCompress bs = unsafePerformIO $ do
    if BS.length bs <= minimumByteStringLength
        then pure Nothing -- Small inputs are unlikely to benefit from compression so we do not attempt to do so hence, Nothing
        else do
            BSU.unsafeUseAsCStringLen bs $ \(cstr, cstrLen) -> do
                let outputBufferSize = BS.length bs + 100 -- Apparently, it is possible for lzf compression to write more data to the output buffer than the input buffer size due to some overhead. This is particularly true for small inputs but may also happen, for some reason, with larger inputs. The (+ 100) is an arbitrary heuristic to account for this.

                -- We do not want to use mallocBytes or callocBytes here because it possible for the lzf_compress function to write more data to the output buffer than the input buffer size due to some compression overhead (poor compression ratio). We also don't want to use either of those functions because they will result in a memory leak if the number of bytes written to the output buffer is smaller than what we allocate. The excess memory will be leaked. With the combination of allocaBytes and packCStringLen, we avoid this issue because the memory allocated by allocaBytes is automatically freed when the function exits and packCStringLen will only use the portion of the buffer that was actually written to.
                allocaBytes outputBufferSize $ \outputBufferPointer -> do
                    numOfBytesWrittenToOutputBuffer <- lzf_compress cstr (fromIntegral cstrLen) outputBufferPointer (fromIntegral outputBufferSize)
                    if numOfBytesWrittenToOutputBuffer == 0
                        then pure Nothing
                        else Just <$> BS.packCStringLen (outputBufferPointer, fromIntegral numOfBytesWrittenToOutputBuffer)

{-# NOINLINE lzfDecompress #-}
lzfDecompress :: Int -> ByteString -> Maybe ByteString
lzfDecompress expectedDecompressedSize bs = unsafePerformIO $ do
    if BS.length bs <= minimumByteStringLength
        then pure Nothing -- For consistency with lzfCompress, if we do not attempt to compress small inputs, we also should not attempt to decompress them
        else do
            BSU.unsafeUseAsCStringLen bs $ \(cstr, cstrLen) -> do
                let outputBufferSize = expectedDecompressedSize
                outputBufferPointer <- mallocBytes outputBufferSize
                numOfDecompressedBytes <- lzf_decompress cstr (fromIntegral cstrLen) outputBufferPointer (fromIntegral outputBufferSize)
                if numOfDecompressedBytes == 0
                    then pure Nothing
                    else Just <$> BSU.unsafePackCStringLen (outputBufferPointer, fromIntegral numOfDecompressedBytes)
