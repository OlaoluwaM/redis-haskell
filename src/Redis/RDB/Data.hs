{-# LANGUAGE MultiWayIf #-}

module Redis.RDB.Data (
    RDBVal (..),
    RDBString (..),
    RDBShortString (..),
    RDBMediumString (..),
    RDBLongString (..),
    RDBInt8 (..),
    RDBInt16 (..),
    RDBInt32 (..),
    RDBLengthPrefix (..),
    RDBLengthPrefix6 (..),
    RDBLengthPrefix14 (..),
    RDBLengthPrefix32 (..),
    RDBUnixTimestampS (..),
    RDBUnixTimestampMS (..),
    toRDBString,
    fromRDBString,
    toRDBVal,
    fromRDBVal,
    toRDBLengthPrefix,
    fromRDBLengthPrefix,
    fromPosixTimeToRDBUnixTimestampMS,
    toPosixTimeFromRDBUnixTimestampMS,
    fromPosixTimeToRDBUnixTimestampS,
    toPosixTimeFromRDBUnixTimestampS,
) where

import Redis.RDB.Binary

import Data.Bits qualified as Bits
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as BSC

import Control.Applicative (Alternative (..), asum)
import Control.DeepSeq (NFData)
import Control.Monad.Reader (ask)
import Data.Binary.Get (
    getByteString,
    getInt16le,
    getInt32le,
    getWord32be,
    getWord32le,
    getWord64be,
    getWord64le,
 )
import Data.Binary.Put (
    putByteString,
    putInt16le,
    putInt32le,
    putWord32be,
    putWord32le,
    putWord64be,
    putWord64le,
 )
import Data.Fixed (Pico)
import Data.Int (Int16, Int32, Int8)
import Data.Maybe (isNothing)
import Data.String (IsString (fromString))
import Data.String.Interpolate (i)
import Data.Time.Clock (secondsToNominalDiffTime)
import Data.Time.Clock.POSIX (POSIXTime)
import Data.Word (Word16, Word32, Word64, Word8)
import GHC.Generics (Generic)
import Redis.RDB.Config (RDBConfig (..))
import Redis.RDB.LZF (LzfCompressedByteString (..), lzfCompress, lzfDecompress)
import Redis.Utils (millisecondsToSeconds, secondsToMilliseconds)

{-
    Okay, so the RDB spec describes what a variant of variable-length encoding that we shall refer to as rdb-variable-length encoding since this variant differs subtly from the standard variable-length encoding. Now, that said, the rdb-variable length encoding is mostly for the length of things to ensure that the length is stored smartly and efficiently, not using more space/bytes than necessary. However, Redis also uses this encoding/algorithm to sometimes encode integers hence we have the RDBLengthPrefix6, RDBLengthPrefix14, and RDBLengthPrefix32, types which encode integers as if they were a length of a string but without the accompany byte sequence you'd expect for a string.

    Note that these types differ from the RDBInt* types in that the RDBInt* types use a special encoding based off the rdb-variable-length encoding to encode integers as strings with a length as the first byte followed by a byte sequence representing the integer quantity. So in this case we aren't just encoding the length alone and considering it an integer during decoding, we encode the a special prefix byte followed by the encoded integer value
-}

-- When we refer to length-prefix we really mean length-prefix encoding as per the RDB variable length encoding scheme outlined here https://rdb.fnordig.de/file_format.html#length-encoding

-- RDB strings are encoded using the RDB length encoding scheme ([len][data]): https://github.com/redis/redis/blob/38d16a82eb4a8b0e393b51cc3e9144dc7b413fd1/src/rdb.c#L440

-- Supports strings with sizes up to 64 bits by virtue of there being support for length prefixes up to 64 bits: https://github.com/redis/redis/blob/38d16a82eb4a8b0e393b51cc3e9144dc7b413fd1/src/rdb.c#L157

newtype RDBShortString = RDBShortString {rdbShortString :: BS.ByteString}
    deriving stock (Show, Eq, Ord, Generic)
    deriving newtype (IsString)
    deriving anyclass (NFData)

newtype RDBMediumString = RDBMediumString {rdbMediumString :: BS.ByteString}
    deriving stock (Show, Eq, Ord, Generic)
    deriving newtype (IsString)
    deriving anyclass (NFData)

newtype RDBLongString = RDBLongString {rdbLongString :: BS.ByteString}
    deriving stock (Show, Eq, Ord, Generic)
    deriving newtype (IsString)
    deriving anyclass (NFData)

newtype RDBExtraLongString = RDBExtraLongString {rdbExtraLongString :: BS.ByteString}
    deriving stock (Show, Eq, Ord, Generic)
    deriving newtype (IsString)
    deriving anyclass (NFData)

-- NOTE: The rdb variable length encoding also includes special support for serializing integers as string as defined here https://rdb.fnordig.de/file_format.html#integers-as-string. The resulting byte sequence is in little-endian. Below are the types that represent integers to be serialized according to the special rdb variable - length encoding rules for integers

-- Only supports integers up to 32 bits: https://github.com/redis/redis/blob/38d16a82eb4a8b0e393b51cc3e9144dc7b413fd1/src/rdb.c#L251

newtype RDBInt8 = RDBInt8 {rdbInt8 :: Int8}
    deriving stock (Show, Eq, Ord, Generic)
    deriving anyclass (NFData)

newtype RDBInt16 = RDBInt16 {rdbInt16 :: Int16}
    deriving stock (Show, Eq, Ord, Generic)
    deriving anyclass (NFData)

newtype RDBInt32 = RDBInt32 {rdbInt32 :: Int32}
    deriving stock (Show, Eq, Ord, Generic)
    deriving anyclass (NFData)

-- As mentioned above, the rdb specification serializes strings using a length encoding (what we've opted to call rdb variable length encoding). For certain integral values, rather than serializing those as rdb string integers, redis will sometimes serializes integral data as a "length" as though it were doing a full length encoding, the difference is, there is no data, just an unsigned integer serialized as though it were part of a full length encoding. We call this part of the full encoding the length prefix and the types below represent unsigned integers serialized as length prefixes with no accompanying data. Big endian is assumed

-- Supports length prefix unsigned integer values up to 64 bits: https://github.com/redis/redis/blob/38d16a82eb4a8b0e393b51cc3e9144dc7b413fd1/src/rdb.c#L157

-- | 6-bit length prefix (0-63)
newtype RDBLengthPrefix6 = RDBLengthPrefix6 {rdbLengthPrefix8 :: Word8}
    deriving stock (Show, Eq, Ord)
    deriving newtype (Num, Enum, Real, Integral)

-- | 14-bit length prefix (0-16383)
newtype RDBLengthPrefix14 = RDBLengthPrefix14 {rdbLengthPrefix16 :: Word16}
    deriving stock (Show, Eq, Ord)
    deriving newtype (Num, Enum, Real, Integral)

-- | 32-bit length prefix (0-4294967295)
newtype RDBLengthPrefix32 = RDBLengthPrefix32 {rdbLengthPrefix32 :: Word32}
    deriving stock (Show, Eq, Ord)
    deriving newtype (Num, Enum, Real, Integral)

-- | 64-bit length prefix (0-18446744073709551615)
newtype RDBLengthPrefix64 = RDBLengthPrefix64 {rdbLengthPrefix64 :: Word64}
    deriving stock (Show, Eq, Ord)
    deriving newtype (Num, Enum, Real, Integral)

-- | RDB Unix timestamp types
newtype RDBUnixTimestampS = RDBUnixTimestampS {getRDBUnixTimestampS :: Word32}
    deriving stock (Show, Eq, Ord)

newtype RDBUnixTimestampMS = RDBUnixTimestampMS {getRDBUnixTimestampMS :: Word64}
    deriving stock (Show, Eq, Ord)

-- | Sum types that group the individual types defined above together
data RDBString
    = MkRDBShortString RDBShortString
    | MkRDBMediumString RDBMediumString
    | MkRDBLongString RDBLongString
    | MkRDBExtraLongString RDBExtraLongString
    deriving stock (Show, Eq)

data RDBLengthPrefix
    = MkRDBLengthPrefix6 RDBLengthPrefix6
    | MkRDBLengthPrefix14 RDBLengthPrefix14
    | MkRDBLengthPrefix32 RDBLengthPrefix32
    | MkRDBLengthPrefix64 RDBLengthPrefix64
    deriving stock (Show, Eq)

data RDBInt
    = MkRDBInt8 RDBInt8
    | MkRDBInt16 RDBInt16
    | MkRDBInt32 RDBInt32
    deriving stock (Show, Eq)

data RDBVal
    = MkRDBShortStringVal RDBShortString
    | MkRDBMediumStringVal RDBMediumString
    | MkRDBLongStringVal RDBLongString
    | MkRDBExtraLongStringVal RDBExtraLongString
    | MkRDBInt8Val RDBInt8
    | MkRDBInt16Val RDBInt16
    | MkRDBInt32Val RDBInt32
    deriving stock (Show, Eq, Generic)
    deriving anyclass (NFData)

instance RDBBinary RDBVal where
    rdbPut = \case
        (MkRDBInt8Val rdbInt8) -> rdbPut rdbInt8
        (MkRDBInt16Val rdbInt16) -> rdbPut rdbInt16
        (MkRDBInt32Val rdbInt32) -> rdbPut rdbInt32
        (MkRDBShortStringVal shortRDBString) -> rdbPut shortRDBString
        (MkRDBMediumStringVal mediumRDBString) -> rdbPut mediumRDBString
        (MkRDBLongStringVal longRDBString) -> rdbPut longRDBString
        (MkRDBExtraLongStringVal extraLongRDBString) -> rdbPut extraLongRDBString

    rdbGet =
        asum
            [ MkRDBInt8Val <$> rdbGet
            , MkRDBInt16Val <$> rdbGet
            , MkRDBInt32Val <$> rdbGet
            , MkRDBShortStringVal <$> rdbGet
            , MkRDBMediumStringVal <$> rdbGet
            , MkRDBLongStringVal <$> rdbGet
            , MkRDBExtraLongStringVal <$> rdbGet
            , fail "Failed to decode RDBVal"
            ]

instance RDBBinary RDBUnixTimestampS where
    rdbPut (RDBUnixTimestampS t) = genericRDBPutUsing (putWord32le t) -- Redis uses little-endian for timestamps

    rdbGet = RDBUnixTimestampS <$> genericRDBGetUsing getWord32le

instance RDBBinary RDBUnixTimestampMS where
    rdbPut (RDBUnixTimestampMS t) = genericRDBPutUsing (putWord64le t) -- Redis uses little-endian for timestamps

    rdbGet = RDBUnixTimestampMS <$> genericRDBGetUsing getWord64le

instance RDBBinary RDBString where
    rdbPut = \case
        MkRDBShortString shortStr -> rdbPut shortStr
        MkRDBMediumString mediumStr -> rdbPut mediumStr
        MkRDBLongString longStr -> rdbPut longStr
        MkRDBExtraLongString extraLongStr -> rdbPut extraLongStr

    rdbGet =
        asum
            [ MkRDBShortString <$> rdbGet
            , MkRDBMediumString <$> rdbGet
            , MkRDBLongString <$> rdbGet
            , MkRDBExtraLongString <$> rdbGet
            , fail "Failed to decode RDBString"
            ]

instance RDBBinary RDBLengthPrefix where
    rdbPut = \case
        MkRDBLengthPrefix6 rdbLengthPrefix8 -> rdbPut rdbLengthPrefix8
        MkRDBLengthPrefix14 rdbLengthPrefix16 -> rdbPut rdbLengthPrefix16
        MkRDBLengthPrefix32 rdbLengthPrefix132 -> rdbPut rdbLengthPrefix132
        MkRDBLengthPrefix64 rdbLengthPrefix164 -> rdbPut rdbLengthPrefix164

    rdbGet =
        asum
            [ MkRDBLengthPrefix6 <$> rdbGet
            , MkRDBLengthPrefix14 <$> rdbGet
            , MkRDBLengthPrefix32 <$> rdbGet
            , MkRDBLengthPrefix64 <$> rdbGet
            , fail "Failed to decode int val in rdb length prefix form"
            ]

instance RDBBinary RDBShortString where
    rdbPut = attemptLzfSerializationWithFallback (rdbVariableLengthEncodeShortString . RDBShortString) . (.rdbShortString)
    rdbGet = attemptLzfDeserializationWithFallback mkRDBShortString rdbVariableLengthDecodeShortString
      where
        mkRDBShortString :: (MonadFail m) => BS.ByteString -> m RDBShortString
        mkRDBShortString byteStr = do
            let len = BS.length byteStr
            if len < shortStringLenCutoff
                then pure (RDBShortString byteStr)
                else
                    fail $
                        "The value provided is too large to be encoded as an rdb short string. Length must be less than "
                            <> show @Int shortStringLenCutoff
                            <> " but got length: "
                            <> show len

instance RDBBinary RDBMediumString where
    rdbPut = attemptLzfSerializationWithFallback (rdbVariableLengthEncodeMediumString . RDBMediumString) . (.rdbMediumString)
    rdbGet = attemptLzfDeserializationWithFallback mkRDBMediumString rdbVariableLengthDecodeMediumString
      where
        mkRDBMediumString :: (MonadFail m) => BS.ByteString -> m RDBMediumString
        mkRDBMediumString byteStr = do
            let len = BS.length byteStr
            if len >= shortStringLenCutoff && len < mediumStringLenCutoff
                then pure (RDBMediumString byteStr)
                else
                    fail $
                        "The value provided was either too large or too small to be encoded as an RDB medium string. Length must be between "
                            <> show @Int shortStringLenCutoff
                            <> " (inclusive) and "
                            <> show @Int mediumStringLenCutoff
                            <> " (exclusive) but got length: "
                            <> show len

instance RDBBinary RDBLongString where
    rdbPut = attemptLzfSerializationWithFallback (rdbVariableLengthEncodeLongString . RDBLongString) . (.rdbLongString)
    rdbGet = attemptLzfDeserializationWithFallback mkRDBLongString rdbVariableLengthDecodeLongString
      where
        mkRDBLongString :: (MonadFail m) => BS.ByteString -> m RDBLongString
        mkRDBLongString byteStr = do
            let len = BS.length byteStr
            if len >= mediumStringLenCutoff && len < longStringLenCutoff
                then pure (RDBLongString byteStr)
                else
                    fail $
                        "The value provided was either too large or too small to be encoded as an RDB long string. Length must be between "
                            <> show @Int mediumStringLenCutoff
                            <> " (inclusive) and "
                            <> show @Int longStringLenCutoff
                            <> " (exclusive) but got length: "
                            <> show len

instance RDBBinary RDBExtraLongString where
    rdbPut = attemptLzfSerializationWithFallback (rdbVariableLengthEncodeExtraLongString . RDBExtraLongString) . (.rdbExtraLongString)
    rdbGet = attemptLzfDeserializationWithFallback mkRDBExtraLongString rdbVariableLengthDecodeExtraLongString
      where
        mkRDBExtraLongString :: (MonadFail m) => BS.ByteString -> m RDBExtraLongString
        mkRDBExtraLongString byteStr = do
            let len = BS.length byteStr
            if len >= longStringLenCutoff
                then pure (RDBExtraLongString byteStr)
                else
                    fail $
                        "The value provided is too small to be encoded as an RDB extra long string. Length must be at least "
                            <> show @Int longStringLenCutoff
                            <> " but got length: "
                            <> show len

instance RDBBinary RDBInt8 where
    rdbPut = rdbVariableLengthEncode8BitInt
    rdbGet = rdbVariableLengthDecode8BitInt

instance RDBBinary RDBInt16 where
    rdbPut = rdbVariableLengthEncode16BitInt
    rdbGet = rdbVariableLengthDecode16BitInt

instance RDBBinary RDBInt32 where
    rdbPut = rdbVariableLengthEncode32BitInt
    rdbGet = rdbVariableLengthDecode32BitInt

-- | Binary instances for length-prefixed integers
instance RDBBinary RDBLengthPrefix6 where
    rdbPut = rdbEncodeLengthPrefix8
    rdbGet = rdbDecodeLengthPrefix6

instance RDBBinary RDBLengthPrefix14 where
    rdbPut = rdbEncodeLengthPrefix14
    rdbGet = rdbDecodeLengthPrefix14

instance RDBBinary RDBLengthPrefix32 where
    rdbPut = rdbEncodeLengthPrefix32
    rdbGet = rdbDecodeLengthPrefix32

instance RDBBinary RDBLengthPrefix64 where
    rdbPut = rdbEncodeLengthPrefix64
    rdbGet = rdbDecodeLengthPrefix64

-- This logic of this function, particularly the bounds/limits in each case, is based off the `rdbSaveLen` redis function: https://github.com/redis/redis/blob/38d16a82eb4a8b0e393b51cc3e9144dc7b413fd1/src/rdb.c#L157
toRDBLengthPrefix :: Word -> RDBLengthPrefix
toRDBLengthPrefix num
    | num < shortStringLenCutoff = MkRDBLengthPrefix6 (RDBLengthPrefix6 . fromIntegral $ num)
    | num < mediumStringLenCutoff = MkRDBLengthPrefix14 (RDBLengthPrefix14 . fromIntegral $ num)
    | num < longStringLenCutoff = MkRDBLengthPrefix32 (RDBLengthPrefix32 . fromIntegral $ num)
    | otherwise = MkRDBLengthPrefix64 (RDBLengthPrefix64 . fromIntegral $ num)

shortStringLenCutoff :: (Num a) => a
shortStringLenCutoff = 64 -- Equivalent to (1<<6) in C

mediumStringLenCutoff :: (Num a) => a
mediumStringLenCutoff = 16384 -- Equivalent to (1<<14) in C

longStringLenCutoff :: (Num a) => a
longStringLenCutoff = 4294967295 -- Equivalent to UINT32_MAX in C

-- https://github.com/redis/redis/blob/unstable/src/rdb.c#L445, attempts integer encoding for potential integer strings
-- We do not need the random cut off at (len <= 11) since we can already check to determine whether a byte sequence is number by attempting to parse it as such. Arbitrary length cutoffs become redundant
toRDBVal :: BS.ByteString -> RDBVal
toRDBVal byteStr
    | Just num <- tryParseAsInt8 byteStr = MkRDBInt8Val (RDBInt8 num)
    | Just num <- tryParseAsInt16 byteStr = MkRDBInt16Val (RDBInt16 num)
    | Just num <- tryParseAsInt32 byteStr = MkRDBInt32Val (RDBInt32 num)
    | BS.length byteStr < shortStringLenCutoff = MkRDBShortStringVal (RDBShortString byteStr)
    | BS.length byteStr < mediumStringLenCutoff = MkRDBMediumStringVal (RDBMediumString byteStr)
    | BS.length byteStr < longStringLenCutoff = MkRDBLongStringVal (RDBLongString byteStr)
    | otherwise = MkRDBExtraLongStringVal (RDBExtraLongString byteStr)

toRDBString :: BS.ByteString -> RDBString
toRDBString byteStr
    | BS.length byteStr < shortStringLenCutoff = MkRDBShortString (RDBShortString byteStr)
    | BS.length byteStr < mediumStringLenCutoff = MkRDBMediumString (RDBMediumString byteStr)
    | BS.length byteStr < longStringLenCutoff = MkRDBLongString (RDBLongString byteStr)
    | otherwise = MkRDBExtraLongString (RDBExtraLongString byteStr)

fromRDBString :: RDBString -> BS.ByteString
fromRDBString (MkRDBShortString (RDBShortString byteStr)) = byteStr
fromRDBString (MkRDBMediumString (RDBMediumString byteStr)) = byteStr
fromRDBString (MkRDBLongString (RDBLongString byteStr)) = byteStr
fromRDBString (MkRDBExtraLongString (RDBExtraLongString byteStr)) = byteStr

fromRDBLengthPrefix :: (Num a) => RDBLengthPrefix -> a
fromRDBLengthPrefix = \case
    MkRDBLengthPrefix6 num -> fromIntegral num
    MkRDBLengthPrefix14 num -> fromIntegral num
    MkRDBLengthPrefix32 num -> fromIntegral num
    MkRDBLengthPrefix64 num -> fromIntegral num

tryParseAsInt8 :: BS.ByteString -> Maybe Int8
tryParseAsInt8 byteStr = do
    (num, restOfStr) <- BSC.readInt8 byteStr
    if BS.null restOfStr
        then Just num
        else Nothing

tryParseAsInt16 :: BS.ByteString -> Maybe Int16
tryParseAsInt16 byteStr = do
    (num, restOfStr) <- BSC.readInt16 byteStr
    if BS.null restOfStr
        then Just num
        else Nothing

tryParseAsInt32 :: BS.ByteString -> Maybe Int32
tryParseAsInt32 byteStr = do
    (num, restOfStr) <- BSC.readInt32 byteStr
    if BS.null restOfStr
        then Just num
        else Nothing

fromRDBVal :: RDBVal -> BS.ByteString
fromRDBVal (MkRDBShortStringVal (RDBShortString byteStr)) = byteStr
fromRDBVal (MkRDBMediumStringVal (RDBMediumString byteStr)) = byteStr
fromRDBVal (MkRDBLongStringVal (RDBLongString byteStr)) = byteStr
fromRDBVal (MkRDBExtraLongStringVal (RDBExtraLongString byteStr)) = byteStr
fromRDBVal (MkRDBInt8Val (RDBInt8 int8Val)) = fromString . show $ int8Val
fromRDBVal (MkRDBInt16Val (RDBInt16 int16Val)) = fromString . show $ int16Val
fromRDBVal (MkRDBInt32Val (RDBInt32 int32Val)) = fromString . show $ int32Val

toPosixTimeFromRDBUnixTimestampS :: RDBUnixTimestampS -> POSIXTime
toPosixTimeFromRDBUnixTimestampS = fromIntegral . (.getRDBUnixTimestampS)

fromPosixTimeToRDBUnixTimestampS :: POSIXTime -> RDBUnixTimestampS
fromPosixTimeToRDBUnixTimestampS = RDBUnixTimestampS . floor @_ @Word32

fromPosixTimeToRDBUnixTimestampMS :: POSIXTime -> RDBUnixTimestampMS
fromPosixTimeToRDBUnixTimestampMS = RDBUnixTimestampMS . floor @_ @Word64 . secondsToMilliseconds

toPosixTimeFromRDBUnixTimestampMS :: RDBUnixTimestampMS -> POSIXTime
toPosixTimeFromRDBUnixTimestampMS = secondsToNominalDiffTime . millisecondsToSeconds . fromIntegral @_ @Pico . (.getRDBUnixTimestampMS)

{- |
    Encode a short string using RDB's variable length encoding
    Format: [00XXXXXX] where XXXXXX is the string's 6-bit length followed by the actual byte string data
-}
rdbVariableLengthEncodeShortString :: RDBShortString -> RDBPut
rdbVariableLengthEncodeShortString (RDBShortString byteStr) = do
    let shortRdbByteStrLen = fromIntegral @_ @Word8 $ BS.length byteStr
    let lengthPrefix = RDBLengthPrefix6 shortRdbByteStrLen
    rdbEncodeLengthPrefix8 lengthPrefix
    genericRDBPutUsing $ putByteString byteStr

-- | Decode a short string using RDB's variable length encoding
rdbVariableLengthDecodeShortString :: RDBGet RDBShortString
rdbVariableLengthDecodeShortString = do
    RDBLengthPrefix6 shortByteStrLen <- rdbDecodeLengthPrefix6
    let shortByteStrLenInt = fromIntegral @_ @Int shortByteStrLen
    RDBShortString <$> genericRDBGetUsing (getByteString shortByteStrLenInt)

{- | Encode medium length string using RDB's variable length encoding
    Format: [01XXXXXX] [YYYYYYYY] where XXXXXXYYYYYYYY is the the string's 14-bit length followed by the actual byte string data
-}
rdbVariableLengthEncodeMediumString :: RDBMediumString -> RDBPut
rdbVariableLengthEncodeMediumString (RDBMediumString byteStr) = do
    let mediumRdbByteStrLen = fromIntegral @_ @Word16 $ BS.length byteStr
    let lengthPrefix = RDBLengthPrefix14 mediumRdbByteStrLen
    rdbEncodeLengthPrefix14 lengthPrefix
    genericRDBPutUsing $ putByteString byteStr

-- | Decode a medium length string using RDB's variable length encoding
rdbVariableLengthDecodeMediumString :: RDBGet RDBMediumString
rdbVariableLengthDecodeMediumString = do
    RDBLengthPrefix14 mediumByteStrLen <- rdbDecodeLengthPrefix14
    let mediumByteStrLenInt = fromIntegral @_ @Int mediumByteStrLen
    RDBMediumString <$> genericRDBGetUsing (getByteString mediumByteStrLenInt)

{- | Encode a long string using RDB's variable length encoding
    Format: [10000000] [XXXXXXXX] [XXXXXXXX] [XXXXXXXX] [XXXXXXXX] where X's represent the string's 32-bit length followed by the actual byte string data
-}
rdbVariableLengthEncodeLongString :: RDBLongString -> RDBPut
rdbVariableLengthEncodeLongString (RDBLongString byteStr) = do
    let longRdbStrLen = fromIntegral @_ @Word32 $ BS.length byteStr
    let lengthPrefix = RDBLengthPrefix32 longRdbStrLen
    rdbEncodeLengthPrefix32 lengthPrefix
    genericRDBPutUsing (putByteString byteStr)

-- | Decode a long string using RDB's variable length encoding
rdbVariableLengthDecodeLongString :: RDBGet RDBLongString
rdbVariableLengthDecodeLongString = do
    RDBLengthPrefix32 longByteStrLen <- rdbDecodeLengthPrefix32
    let longByteStrLenInt = fromIntegral @_ @Int longByteStrLen
    RDBLongString <$> genericRDBGetUsing (getByteString longByteStrLenInt)

{- | Encode an extra long string using RDB's variable length encoding
    Format: [10000001] [XXXXXXXX]...[XXXXXXXX] where X's represent the string's 64-bit length followed by the actual byte string data
-}
rdbVariableLengthEncodeExtraLongString :: RDBExtraLongString -> RDBPut
rdbVariableLengthEncodeExtraLongString (RDBExtraLongString byteStr) = do
    let extraLongRdbStrLen = fromIntegral @_ @Word64 $ BS.length byteStr
    let lengthPrefix = RDBLengthPrefix64 extraLongRdbStrLen
    rdbEncodeLengthPrefix64 lengthPrefix
    genericRDBPutUsing (putByteString byteStr)

-- | Decode an extra long string using RDB's variable length encoding
rdbVariableLengthDecodeExtraLongString :: RDBGet RDBExtraLongString
rdbVariableLengthDecodeExtraLongString = do
    RDBLengthPrefix64 extraLongByteStrLen <- rdbDecodeLengthPrefix64
    let extraLongByteStrLenInt = fromIntegral @_ @Int extraLongByteStrLen
    RDBExtraLongString <$> genericRDBGetUsing (getByteString extraLongByteStrLenInt)

-- Integer encoding using RDB's special integer length encoding scheme: https://rdb.fnordig.de/file_format.html#integers-as-string

{- | Encode an 8-bit integer using RDB's special integer length encoding scheme
    Format: [11000000] [XXXXXXXX] where XXXXXXXX is the 8-bit signed integer
-}
rdbVariableLengthEncode8BitInt :: RDBInt8 -> RDBPut
rdbVariableLengthEncode8BitInt (RDBInt8 intVal) = do
    genericRDBPut intPrefix8Bit
    genericRDBPut @Int8 intVal

-- | Decode an 8-bit integer using RDB's special integer length encoding scheme
rdbVariableLengthDecode8BitInt :: RDBGet RDBInt8
rdbVariableLengthDecode8BitInt = do
    prefixByte <- genericRDBGet @Word8
    let isPrefixByteValid = verifyPrefixByte prefixByte
    if not isPrefixByteValid
        then fail "Unexpected prefix byte while attempting to decode 8 bit integer. Expected the first two bits of the prefix byte to be 11 and the last 6 bits to equal 0"
        else RDBInt8 <$> genericRDBGet @Int8
  where
    -- Validate that prefix is correct and type identifier (last 6 bits) is 0
    verifyPrefixByte :: Word8 -> Bool
    verifyPrefixByte = (== intPrefix8Bit)

intPrefix8Bit :: Word8
intPrefix8Bit = 0b11000000

{- | Encode a 16-bit integer using RDB's special integer length encoding scheme
    Format: [11000001] [XXXXXXXX] [XXXXXXXX] where XXXXXXXXXXXXXXXX is the 16-bit signed integer
-}
rdbVariableLengthEncode16BitInt :: RDBInt16 -> RDBPut
rdbVariableLengthEncode16BitInt (RDBInt16 intVal) = do
    genericRDBPut intPrefix16Bit
    genericRDBPutUsing (putInt16le intVal)

-- | Decode a 16-bit integer using RDB's special integer length encoding scheme
rdbVariableLengthDecode16BitInt :: RDBGet RDBInt16
rdbVariableLengthDecode16BitInt = do
    prefixByte <- genericRDBGet @Word8
    let isPrefixByteValid = verifyPrefixByte prefixByte
    if not isPrefixByteValid
        then fail "Unexpected prefix byte while attempting to decode 16 bit integer. Expected the first two bits of the prefix byte to be 11 and the last 6 bits to equal 1"
        else RDBInt16 <$> genericRDBGetUsing getInt16le
  where
    -- Validate that prefix is correct and type identifier (last 6 bits) is 1
    verifyPrefixByte :: Word8 -> Bool
    verifyPrefixByte = (== intPrefix16Bit)

intPrefix16Bit :: Word8
intPrefix16Bit = 0b11000001

{- | Encode a 32-bit integer using RDB's special integer length encoding scheme
    Format: [11000010] [XXXXXXXX] [XXXXXXXX] [XXXXXXXX] [XXXXXXXX] where X's represent the 32-bit signed integer
-}
rdbVariableLengthEncode32BitInt :: RDBInt32 -> RDBPut
rdbVariableLengthEncode32BitInt (RDBInt32 intVal) = do
    genericRDBPut intPrefix32Bit
    genericRDBPutUsing (putInt32le intVal)

-- | Decode a 32-bit integer using RDB's special integer length encoding scheme
rdbVariableLengthDecode32BitInt :: RDBGet RDBInt32
rdbVariableLengthDecode32BitInt = do
    prefixByte <- genericRDBGet @Word8
    let isPrefixByteValid = verifyPrefixByte prefixByte
    if not isPrefixByteValid
        then fail "Unexpected prefix byte while attempting to decode 32 bit integer. Expected the first two bits of the prefix byte to be 11 and the last 6 bits to equal 2"
        else RDBInt32 <$> genericRDBGetUsing getInt32le
  where
    -- Validate that prefix is correct and type identifier (last 6 bits) is 2
    verifyPrefixByte :: Word8 -> Bool
    verifyPrefixByte = (== intPrefix32Bit)

intPrefix32Bit :: Word8
intPrefix32Bit = 0b11000010

-- | Length prefix encoding functions. These functions encode standalone length prefix values following the rdb variable length encoding, but without any accompanying data bytes.

{- | Encode a 6-bit length value
    Format: [00XXXXXX] where XXXXXX is the 6-bit length
-}
rdbEncodeLengthPrefix8 :: RDBLengthPrefix6 -> RDBPut
rdbEncodeLengthPrefix8 (RDBLengthPrefix6 lenVal) = do
    -- The value is already constrained to fit in 6 bits by the type system
    -- Top 2 bits are implicitly 00, so we can write the length directly
    genericRDBPut @Word8 lenVal

-- | Decode a 6-bit length value
rdbDecodeLengthPrefix6 :: RDBGet RDBLengthPrefix6
rdbDecodeLengthPrefix6 = do
    byte <- genericRDBGet @Word8
    -- Verify the prefix, the first two bits, is 00 (top 2 bits should be 0)
    let byteHasExpectedPrefix = (byte `Bits.shiftR` 6) == 0
    if not byteHasExpectedPrefix
        then fail "Unexpected prefix for 6-bit length. Expected first two bits to be 00"
        else pure $ RDBLengthPrefix6 byte

{- | Encode a 14-bit length prefix
    Format: [01XXXXXX][YYYYYYYY] where XXXXXXYYYYYYYY is the 14-bit length
-}
rdbEncodeLengthPrefix14 :: RDBLengthPrefix14 -> RDBPut
rdbEncodeLengthPrefix14 (RDBLengthPrefix14 lenVal) = do
    let prefixByte = bytePrefixFor14BitLengthPrefix
    let byteFromLenValToBeCombinedWithPrefix = fromIntegral @_ @Word8 (lenVal `Bits.shiftR` 8)
    let sixBitsMask = 0b00111111
    let byteFromLenValToBeCombinedWithPrefixWithFirstTwoBitsReset = byteFromLenValToBeCombinedWithPrefix Bits..&. sixBitsMask
    let prefixWithBitsFromLenVal = prefixByte Bits..|. byteFromLenValToBeCombinedWithPrefixWithFirstTwoBitsReset
    let remainingByteFromLenVal = fromIntegral @_ @Word8 lenVal

    genericRDBPut @Word8 prefixWithBitsFromLenVal
    genericRDBPut @Word8 remainingByteFromLenVal

-- | Decode a 14-bit length prefix
rdbDecodeLengthPrefix14 :: RDBGet RDBLengthPrefix14
rdbDecodeLengthPrefix14 = do
    firstByte <- genericRDBGet @Word8
    -- Mask to extract the first two bits of the first byte and reset the remaining six bits
    let prefixMask = 0b11000000
    let byteHasExpectedPrefix = (firstByte Bits..&. prefixMask) == bytePrefixFor14BitLengthPrefix
    if not byteHasExpectedPrefix
        then fail "Unexpected prefix for 14-bit length. Expected first two bits to be 01"
        else do
            secondByte <- genericRDBGet @Word8
            -- We know that the last six bits of the first byte and all eight bits of the second byte represent the length, so we want to extract the last six bits of the first byte and combine them with the second byte to form the full 14-bit length
            let sixBitsMask = 0b00111111
            let lastSixBitsFromFirstByte = fromIntegral @_ @Word16 (firstByte Bits..&. sixBitsMask)
            let remainingByte = fromIntegral @_ @Word16 secondByte -- To Word16 for combining
            let fullLength = (lastSixBitsFromFirstByte `Bits.shiftL` 8) Bits..|. remainingByte -- Combine to form full 14-bit length
            pure $ RDBLengthPrefix14 fullLength

bytePrefixFor14BitLengthPrefix :: Word8
bytePrefixFor14BitLengthPrefix = 0b01000000

{- | Encode a 32-bit length prefix
    Format: [10000000] [XXXXXXXX]...[XXXXXXXX] where X's represent the 32-bit length
-}
rdbEncodeLengthPrefix32 :: RDBLengthPrefix32 -> RDBPut
rdbEncodeLengthPrefix32 (RDBLengthPrefix32 lenVal) = do
    genericRDBPut @Word8 bytePrefixFor32BitLengthPrefix
    genericRDBPutUsing (putWord32be lenVal)

-- | Decode a 32-bit length prefix (0-4294967295)
rdbDecodeLengthPrefix32 :: RDBGet RDBLengthPrefix32
rdbDecodeLengthPrefix32 = do
    prefixByte <- genericRDBGet @Word8
    if prefixByte /= bytePrefixFor32BitLengthPrefix
        then fail "Unexpected prefix for 32-bit length. Expected 10000000"
        else RDBLengthPrefix32 <$> genericRDBGetUsing getWord32be

bytePrefixFor32BitLengthPrefix :: Word8
bytePrefixFor32BitLengthPrefix = 0b10000000

{- | Encode a 64-bit length prefix
    Format: [10000001] [XXXXXXXX]...[XXXXXXXX] where X's represent the 64-bit length
-}
rdbEncodeLengthPrefix64 :: RDBLengthPrefix64 -> RDBPut
rdbEncodeLengthPrefix64 (RDBLengthPrefix64 len) = do
    genericRDBPut @Word8 bytePrefixFor64BitLengthPrefix
    genericRDBPutUsing (putWord64be len)

-- | Decode a 64-bit length prefix (0-18446744073709551615)
rdbDecodeLengthPrefix64 :: RDBGet RDBLengthPrefix64
rdbDecodeLengthPrefix64 = do
    prefixByte <- genericRDBGet @Word8
    if prefixByte /= bytePrefixFor64BitLengthPrefix
        then fail "Unexpected prefix for 64-bit length. Expected 10000001"
        else RDBLengthPrefix64 <$> genericRDBGetUsing getWord64be

bytePrefixFor64BitLengthPrefix :: Word8
bytePrefixFor64BitLengthPrefix = 0b10000001

{-
    Encode LZF compressed data in the format of [prefix][compressedLen][originalLen][data]
    https://github.com/redis/redis/blob/81df8deca614e46cb113974dbac0b9ec3e6724e7/src/rdb.c#L362
-}
rdbEncodeLzfCompressedData :: LzfCompressedByteString -> RDBPut
rdbEncodeLzfCompressedData LzfCompressedByteString{lzfCompressedByteString, compressedLen, originalLen} = do
    genericRDBPut rdbLzfCompressedDataBinaryPrefix
    rdbPut . toRDBLengthPrefix . fromIntegral $ compressedLen
    rdbPut . toRDBLengthPrefix . fromIntegral $ originalLen
    genericRDBPutUsing (putByteString lzfCompressedByteString)

{-
    Decode LZF compressed data from an RDB file
    https://github.com/redis/redis/blob/81df8deca614e46cb113974dbac0b9ec3e6724e7/src/rdb.c#L383
-}
rdbDecodeLzfCompressedData :: RDBGet BS.ByteString
rdbDecodeLzfCompressedData = do
    prefixByte <- genericRDBGet @Word8
    if prefixByte /= rdbLzfCompressedDataBinaryPrefix
        then fail [i|Invalid prefix byte for LZF compressed datum. Expected prefix byte to be: #{show rdbLzfCompressedDataBinaryPrefix} but got: #{prefixByte}|]
        else do
            compressedLen <- fromRDBLengthPrefix @Int <$> rdbGet @RDBLengthPrefix
            originalLen <- fromRDBLengthPrefix @Int <$> rdbGet @RDBLengthPrefix
            lzfCompressedData <- genericRDBGetUsing (getByteString compressedLen)

            let mDecompressedData = lzfDecompress (LzfCompressedByteString lzfCompressedData compressedLen originalLen)

            maybe
                (fail [i|Failed to decompress LZF compressed data. Original length: #{originalLen}, Compressed length: #{compressedLen}|])
                pure
                mDecompressedData

rdbLzfCompressedDataBinaryPrefix :: Word8
rdbLzfCompressedDataBinaryPrefix = 0b11000011

attemptLzfSerializationWithFallback :: (BS.ByteString -> RDBPut) -> BS.ByteString -> RDBPut
attemptLzfSerializationWithFallback fallbackSerialization byteStr =
    do
        config <- ask
        let originalLen = BS.length byteStr
            mExpectedCompressionSize = calculateExpectedCompressionSize originalLen
            mCompressedData = lzfCompress byteStr mExpectedCompressionSize
        if
            | not config.useLzfCompression -> fallbackSerialization byteStr
            | originalLen <= minimumDataLengthForSufficientCompression -> fallbackSerialization byteStr
            | isNothing mExpectedCompressionSize -> fallbackSerialization byteStr
            | Just lzfCompressedData <- mCompressedData -> rdbEncodeLzfCompressedData lzfCompressedData
            | otherwise -> fallbackSerialization byteStr

-- The type signature is this way because we want `f` to only have the "ability" of MonadFail from RDBGet, not anything else
attemptLzfDeserializationWithFallback :: (MonadFail m, m a ~ RDBGet a) => (BS.ByteString -> m a) -> RDBGet a -> RDBGet a
attemptLzfDeserializationWithFallback f fallbackDeserialization = (rdbDecodeLzfCompressedData >>= f) <|> fallbackDeserialization

{-
    This is necessary to avoid issues with very small inputs that cannot be effectively compressed. Due to the way lzf compressions works, small inputs may not have enough data to compress and the end result may even be expanded due to the overhead of the compression algorithm. Input that is smaller than or equal to this threshold is unlikely to benefit from compression. This threshold was chosen per this code in the redis codebase https://github.com/redis/redis/blob/38d16a82eb4a8b0e393b51cc3e9144dc7b413fd1/src/rdb.c#L455
-}
{-# WARNING in "x-unsafe-internals" minimumDataLengthForSufficientCompression "This value is exported for testing purposes only" #-}
minimumDataLengthForSufficientCompression :: Int
minimumDataLengthForSufficientCompression = 20

-- For an lzf compression to be considered "sufficient/worth it" the compressed size ought to be 4 bytes smaller than the original (4 bytes compression at least)
-- As defined here: https://github.com/redis/redis/blob/38d16a82eb4a8b0e393b51cc3e9144dc7b413fd1/src/rdb.c#L368
-- As in the redis code, if we cannot get lzf compression to shave off at least 4 bytes from the original size, we don't want to do it
-- This function returns a Maybe to encapsulate the condition `val > 4` because as stated in the redis code "We require at least four bytes compression for this to be worth it". If we didn't return a Maybe, we'd have to do the `> 4` check wherever this function is used
{-# WARNING in "x-unsafe-internals" calculateExpectedCompressionSize "This value is exported for testing purposes only" #-}
calculateExpectedCompressionSize :: Int -> Maybe Int
calculateExpectedCompressionSize num =
    let val = num - 4
     in if val > 4 then Just val else Nothing
