module Redis.RDB.Data (
    RDBLengthPrefixedVal (..),
    RDBLengthPrefixedString (..),
    RDBLengthPrefixedShortString (..),
    RDBLengthPrefixedMediumString (..),
    RDBLengthPrefixedLongString (..),
    RDBLengthPrefixedExtraLongString (..),
    RDBLengthPrefixedInt8 (..),
    RDBLengthPrefixedInt16 (..),
    RDBLengthPrefixedInt32 (..),
    UIntValInRDBLengthPrefixForm (..),
    UInt8ValInRDBLengthPrefixForm (..),
    UInt16ValInRDBLengthPrefixForm (..),
    UInt32ValInRDBLengthPrefixForm (..),
    UInt64ValInRDBLengthPrefixForm (..),
    RDBUnixTimestampS (..),
    RDBUnixTimestampMS (..),
    toRDBVariableLengthPrefixedStr,
    toRDBLengthPrefixedStr,
    fromRDBLengthPrefixedStr,
    toRDBLengthPrefixedVal,
    toRDBLengthPrefixedValOptimizedToIntEncodingIfPossible,
    fromRDBLengthPrefixedVal,
    toUIntValInRDBLengthPrefixedForm,
    fromUIntValInRDBLengthPrefixedForm,
    fromPosixTimeToRDBUnixTimestampMS,
    toPosixTimeFromRDBUnixTimestampMS,
    fromPosixTimeToRDBUnixTimestampS,
    toPosixTimeFromRDBUnixTimestampS,
) where

import Data.Bits qualified as Bits
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as BSC

import Control.Applicative (asum)
import Data.Binary (Binary (..))
import Data.Binary.Get (
    Get,
    getByteString,
    getInt16le,
    getInt32le,
    getInt8,
    getWord32be,
    getWord32le,
    getWord64be,
    getWord64le,
    getWord8,
 )
import Data.Binary.Put (
    Put,
    putByteString,
    putInt16le,
    putInt32le,
    putInt8,
    putWord32be,
    putWord32le,
    putWord64be,
    putWord64le,
    putWord8,
 )
import Data.Fixed (Pico)
import Data.Int (Int16, Int32, Int8)
import Data.String (IsString (fromString))
import Data.Time.Clock (secondsToNominalDiffTime)
import Data.Time.Clock.POSIX (POSIXTime)
import Data.Word (Word16, Word32, Word64, Word8)
import Redis.Utils (millisecondsToSeconds, secondsToMilliseconds)

{-
    Okay, so the RDB spec describes what a variant of variable-length encoding that we shall refer to as rdb-variable-length encoding since this variant differs subtly from the standard variable-length encoding. Now, that said, the rdb-variable length encoding is mostly for the length of things to ensure that the length is stored smartly and efficiently, not using more space/bytes than necessary. However, Redis also uses this encoding/algorithm to sometimes encode integers hence we have the UInt8ValInRDBLengthPrefixForm, UInt16ValInRDBLengthPrefixForm, UInt32ValInRDBLengthPrefixForm, and UInt64ValInRDBLengthPrefixForm types which encode integers as if they were a length of a string but without the accompany byte sequence you'd expect for a string.

    Note that these types differ from the RDBInt* types in that the RDBInt* types use a special encoding based off the rdb-variable-length encoding to encode integers as strings with a length as the first byte followed by a byte sequence representing the integer quantity. So in this case we aren't just encoding the length alone and considering it an integer during decoding, we encode the a special prefix byte followed by the encoded integer value

    (Do not Change)
-}

-- Globally, when we refer to length-prefix we really mean length-prefix encoding as per the rdb-variable-length encoding scheme (Do not Change)

newtype RDBLengthPrefixedShortString = RDBLengthPrefixedShortString {getRDBLengthPrefixedShortString :: BS.ByteString}
    deriving stock (Show, Eq, Ord)
    deriving newtype (IsString)

newtype RDBLengthPrefixedMediumString = RDBLengthPrefixedMediumString {getRDBLengthPrefixedMediumString :: BS.ByteString}
    deriving stock (Show, Eq, Ord)
    deriving newtype (IsString)

newtype RDBLengthPrefixedLongString = RDBLengthPrefixedLongString {getRDBLengthPrefixedLongString :: BS.ByteString}
    deriving stock (Show, Eq, Ord)
    deriving newtype (IsString)

newtype RDBLengthPrefixedExtraLongString = RDBLengthPrefixedExtraLongString {getRDBLengthPrefixedExtraLongString :: BS.ByteString}
    deriving stock (Show, Eq, Ord)
    deriving newtype (IsString)

-- The encoding and decoding of length-prefixed integer types are done in little-endian (Do not Change)

newtype RDBLengthPrefixedInt8 = RDBLengthPrefixedInt8 {getRDBLengthPrefixedInt8 :: Int8}
    deriving stock (Show, Eq, Ord)

newtype RDBLengthPrefixedInt16 = RDBLengthPrefixedInt16 {getRDBLengthPrefixedInt16 :: Int16}
    deriving stock (Show, Eq, Ord)

newtype RDBLengthPrefixedInt32 = RDBLengthPrefixedInt32 {getRDBLengthPrefixedInt32 :: Int32}
    deriving stock (Show, Eq, Ord)

{- | Length-prefixed integer types for encoding standalone length values
These types use the same encoding prefixes as strings but encode only the length
value itself without any accompanying data bytes.
-}

-- The 'U' here stands for "Unsigned" as these are unsigned integers (Do not Change)
-- The following types represent unsigned integers that *can* be encoded in length prefix form. Encoding assumes big-endian (Do not Change)

-- | 6-bit length encoding (0-63)
newtype UInt8ValInRDBLengthPrefixForm = UInt8ValInRDBLengthPrefixForm {getUInt8ValInRDBLengthPrefixForm :: Word8}
    deriving stock (Show, Eq, Ord)
    deriving newtype (Num, Enum, Real, Integral)

-- | 14-bit length encoding (0-16383)
newtype UInt16ValInRDBLengthPrefixForm = UInt16ValInRDBLengthPrefixForm {getUInt16ValInRDBLengthPrefixForm :: Word16}
    deriving stock (Show, Eq, Ord)
    deriving newtype (Num, Enum, Real, Integral)

-- | 32-bit length encoding (0-4294967295)
newtype UInt32ValInRDBLengthPrefixForm = UInt32ValInRDBLengthPrefixForm {getUInt32ValInRDBLengthPrefixForm :: Word32}
    deriving stock (Show, Eq, Ord)
    deriving newtype (Num, Enum, Real, Integral)

-- | 64-bit length encoding (0-18446744073709551615)
newtype UInt64ValInRDBLengthPrefixForm = UInt64ValInRDBLengthPrefixForm {getUInt64ValInRDBLengthPrefixForm :: Word64}
    deriving stock (Show, Eq, Ord)
    deriving newtype (Num, Enum, Real, Integral)

-- | RDB Unix timestamp types
newtype RDBUnixTimestampS = RDBUnixTimestampS {getRDBUnixTimestampS :: Word32}
    deriving stock (Show, Eq, Ord)

newtype RDBUnixTimestampMS = RDBUnixTimestampMS {getRDBUnixTimestampMS :: Word64}
    deriving stock (Show, Eq, Ord)

-- | Sum types
data RDBLengthPrefixedString
    = MkRDBLengthPrefixedShortString RDBLengthPrefixedShortString
    | MkRDBLengthPrefixedMediumString RDBLengthPrefixedMediumString
    | MkRDBLengthPrefixedLongString RDBLengthPrefixedLongString
    | MkRDBLengthPrefixedExtraLongString RDBLengthPrefixedExtraLongString
    deriving stock (Show, Eq)

data UIntValInRDBLengthPrefixForm
    = MkUInt8ValInRDBLengthPrefixForm UInt8ValInRDBLengthPrefixForm
    | MkUInt16ValInRDBLengthPrefixForm UInt16ValInRDBLengthPrefixForm
    | MkUInt32ValInRDBLengthPrefixForm UInt32ValInRDBLengthPrefixForm
    | MkUInt64ValInRDBLengthPrefixForm UInt64ValInRDBLengthPrefixForm
    deriving stock (Show, Eq)

data RDBLengthPrefixedIntVal
    = RDBLengthPrefixedInt8Value RDBLengthPrefixedInt8
    | RDBLengthPrefixedInt16Value RDBLengthPrefixedInt16
    | RDBLengthPrefixedInt32Value RDBLengthPrefixedInt32
    deriving stock (Show, Eq)

data RDBLengthPrefixedVal
    = RDBLengthPrefixedShortStringVal RDBLengthPrefixedShortString
    | RDBLengthPrefixedMediumStringVal RDBLengthPrefixedMediumString
    | RDBLengthPrefixedLongStringVal RDBLengthPrefixedLongString
    | RDBLengthPrefixedExtraLongStringVal RDBLengthPrefixedExtraLongString
    | RDBLengthPrefixedInt8Val RDBLengthPrefixedInt8
    | RDBLengthPrefixedInt16Val RDBLengthPrefixedInt16
    | RDBLengthPrefixedInt32Val RDBLengthPrefixedInt32
    deriving stock (Show, Eq)

class (Integral a) => Unsigned a where
    toUIntValInRDBLengthPrefixedForm :: a -> UIntValInRDBLengthPrefixForm

instance Unsigned Word8 where
    toUIntValInRDBLengthPrefixedForm = MkUInt8ValInRDBLengthPrefixForm . UInt8ValInRDBLengthPrefixForm

instance Unsigned Word16 where
    toUIntValInRDBLengthPrefixedForm = MkUInt16ValInRDBLengthPrefixForm . UInt16ValInRDBLengthPrefixForm

instance Unsigned Word32 where
    toUIntValInRDBLengthPrefixedForm = MkUInt32ValInRDBLengthPrefixForm . UInt32ValInRDBLengthPrefixForm

instance Unsigned Word64 where
    toUIntValInRDBLengthPrefixedForm = MkUInt64ValInRDBLengthPrefixForm . UInt64ValInRDBLengthPrefixForm

instance Unsigned Word where
    toUIntValInRDBLengthPrefixedForm num
        | num >= fromIntegral (minBound @Word8) && num <= fromIntegral (maxBound @Word8) = MkUInt8ValInRDBLengthPrefixForm (UInt8ValInRDBLengthPrefixForm $ fromIntegral num)
        | num >= fromIntegral (minBound @Word32) && num <= fromIntegral (maxBound @Word32) = MkUInt32ValInRDBLengthPrefixForm (UInt32ValInRDBLengthPrefixForm $ fromIntegral num)
        | num >= fromIntegral (minBound @Word16) && num <= fromIntegral (maxBound @Word16) = MkUInt16ValInRDBLengthPrefixForm (UInt16ValInRDBLengthPrefixForm $ fromIntegral num)
        | otherwise = MkUInt64ValInRDBLengthPrefixForm (UInt64ValInRDBLengthPrefixForm $ fromIntegral num)

instance Binary RDBLengthPrefixedVal where
    put = \case
        (RDBLengthPrefixedInt8Val rdbLengthPrefixedInt8) -> rdbVariableLengthEncodeFor8BitIntAsStr rdbLengthPrefixedInt8
        (RDBLengthPrefixedInt16Val rdbLengthPrefixedInt16) -> rdbVariableLengthEncodeFor16BitIntAsStr rdbLengthPrefixedInt16
        (RDBLengthPrefixedInt32Val rdbLengthPrefixedInt32) -> rdbVariableLengthEncodeFor32BitIntAsStr rdbLengthPrefixedInt32
        (RDBLengthPrefixedShortStringVal shortRdbString) -> rdbVariableLengthEncodeStrOneByte shortRdbString
        (RDBLengthPrefixedMediumStringVal mediumRdbString) -> rdbVariableLengthEncodeStrTwoBytes mediumRdbString
        (RDBLengthPrefixedLongStringVal longRdbString) -> rdbVariableLengthEncodeStrFourBytes longRdbString
        (RDBLengthPrefixedExtraLongStringVal extraLongRdbString) -> rdbVariableLengthEncodeStrNineBytes extraLongRdbString
    get =
        asum
            [ RDBLengthPrefixedInt8Val <$> rdbVariableLengthDecode8BitIntFromStr
            , RDBLengthPrefixedInt16Val <$> rdbVariableLengthDecode16BitIntFromStr
            , RDBLengthPrefixedInt32Val <$> rdbVariableLengthDecode32BitIntFromStr
            , RDBLengthPrefixedShortStringVal <$> rdbVariableLengthDecodeStrOneByte
            , RDBLengthPrefixedMediumStringVal <$> rdbVariableLengthDecodeStrTwoBytes
            , RDBLengthPrefixedLongStringVal <$> rdbVariableLengthDecodeStrFourBytes
            , RDBLengthPrefixedExtraLongStringVal <$> rdbVariableLengthDecodeStrNineBytes
            , fail "Failed to decode RDBLengthPrefixedVal"
            ]

instance Binary RDBUnixTimestampS where
    put (RDBUnixTimestampS t) = putWord32le t -- Redis uses little-endian for timestamps

    get = RDBUnixTimestampS <$> getWord32le

instance Binary RDBUnixTimestampMS where
    put (RDBUnixTimestampMS t) = putWord64le t -- Redis uses little-endian for timestamps

    get = RDBUnixTimestampMS <$> getWord64le

instance Binary RDBLengthPrefixedString where
    put = \case
        MkRDBLengthPrefixedShortString shortStr -> rdbVariableLengthEncodeStrOneByte shortStr
        MkRDBLengthPrefixedMediumString mediumStr -> rdbVariableLengthEncodeStrTwoBytes mediumStr
        MkRDBLengthPrefixedLongString longStr -> rdbVariableLengthEncodeStrFourBytes longStr
        MkRDBLengthPrefixedExtraLongString extraLongStr -> rdbVariableLengthEncodeStrNineBytes extraLongStr

    get =
        asum
            [ MkRDBLengthPrefixedShortString <$> rdbVariableLengthDecodeStrOneByte
            , MkRDBLengthPrefixedMediumString <$> rdbVariableLengthDecodeStrTwoBytes
            , MkRDBLengthPrefixedLongString <$> rdbVariableLengthDecodeStrFourBytes
            , MkRDBLengthPrefixedExtraLongString <$> rdbVariableLengthDecodeStrNineBytes
            , fail "Failed to decode RDBLengthPrefixedString"
            ]

instance Binary UIntValInRDBLengthPrefixForm where
    put = \case
        MkUInt8ValInRDBLengthPrefixForm uint8Val -> rdbEncodeUInt8ToLengthPrefixedForm uint8Val
        MkUInt16ValInRDBLengthPrefixForm uint16Val -> rdbEncodeUInt16ToLengthPrefixedForm uint16Val
        MkUInt32ValInRDBLengthPrefixForm uint32Val -> rdbEncodeUInt32ToLengthPrefixedForm uint32Val
        MkUInt64ValInRDBLengthPrefixForm uint64Val -> rdbEncodeUInt64ToLengthPrefixedForm uint64Val

    get =
        asum
            [ MkUInt8ValInRDBLengthPrefixForm <$> rdbDecodeUInt8FromLengthPrefixForm
            , MkUInt16ValInRDBLengthPrefixForm <$> rdbDecodeUInt16FromLengthPrefixForm
            , MkUInt32ValInRDBLengthPrefixForm <$> rdbDecodeUInt32FromLengthPrefixForm
            , MkUInt64ValInRDBLengthPrefixForm <$> rdbDecodeUInt64FromLengthPrefixForm
            , fail "Failed to decode int val in rdb length prefix form"
            ]

instance Binary RDBLengthPrefixedShortString where
    put = rdbVariableLengthEncodeStrOneByte
    get = rdbVariableLengthDecodeStrOneByte

instance Binary RDBLengthPrefixedMediumString where
    put = rdbVariableLengthEncodeStrTwoBytes
    get = rdbVariableLengthDecodeStrTwoBytes

instance Binary RDBLengthPrefixedLongString where
    put = rdbVariableLengthEncodeStrFourBytes
    get = rdbVariableLengthDecodeStrFourBytes

instance Binary RDBLengthPrefixedInt8 where
    put = rdbVariableLengthEncodeFor8BitIntAsStr
    get = rdbVariableLengthDecode8BitIntFromStr

instance Binary RDBLengthPrefixedInt16 where
    put = rdbVariableLengthEncodeFor16BitIntAsStr
    get = rdbVariableLengthDecode16BitIntFromStr

instance Binary RDBLengthPrefixedInt32 where
    put = rdbVariableLengthEncodeFor32BitIntAsStr
    get = rdbVariableLengthDecode32BitIntFromStr

instance Binary RDBLengthPrefixedExtraLongString where
    put = rdbVariableLengthEncodeStrNineBytes
    get = rdbVariableLengthDecodeStrNineBytes

-- | Binary instances for length-prefixed integers
instance Binary UInt8ValInRDBLengthPrefixForm where
    put = rdbEncodeUInt8ToLengthPrefixedForm
    get = rdbDecodeUInt8FromLengthPrefixForm

instance Binary UInt16ValInRDBLengthPrefixForm where
    put = rdbEncodeUInt16ToLengthPrefixedForm
    get = rdbDecodeUInt16FromLengthPrefixForm

instance Binary UInt32ValInRDBLengthPrefixForm where
    put = rdbEncodeUInt32ToLengthPrefixedForm
    get = rdbDecodeUInt32FromLengthPrefixForm

instance Binary UInt64ValInRDBLengthPrefixForm where
    put = rdbEncodeUInt64ToLengthPrefixedForm
    get = rdbDecodeUInt64FromLengthPrefixForm

toRDBLengthPrefixedVal :: BS.ByteString -> RDBLengthPrefixedVal
toRDBLengthPrefixedVal byteStr
    | Just num <- tryParseAsInt8 byteStr = RDBLengthPrefixedInt8Val (RDBLengthPrefixedInt8 num)
    | Just num <- tryParseAsInt16 byteStr = RDBLengthPrefixedInt16Val (RDBLengthPrefixedInt16 num)
    | Just num <- tryParseAsInt32 byteStr = RDBLengthPrefixedInt32Val (RDBLengthPrefixedInt32 num)
    | BS.length byteStr <= shortStringLenCutoff = RDBLengthPrefixedShortStringVal (RDBLengthPrefixedShortString byteStr)
    | BS.length byteStr <= mediumStringLenCutoff = RDBLengthPrefixedMediumStringVal (RDBLengthPrefixedMediumString byteStr)
    | BS.length byteStr <= longStringLenCutoff = RDBLengthPrefixedLongStringVal (RDBLengthPrefixedLongString byteStr)
    | otherwise = RDBLengthPrefixedExtraLongStringVal (RDBLengthPrefixedExtraLongString byteStr)

toRDBVariableLengthPrefixedStr :: BS.ByteString -> RDBLengthPrefixedString
toRDBVariableLengthPrefixedStr byteStr
    | BS.length byteStr <= shortStringLenCutoff = MkRDBLengthPrefixedShortString (RDBLengthPrefixedShortString byteStr)
    | BS.length byteStr <= mediumStringLenCutoff = MkRDBLengthPrefixedMediumString (RDBLengthPrefixedMediumString byteStr)
    | BS.length byteStr <= longStringLenCutoff = MkRDBLengthPrefixedLongString (RDBLengthPrefixedLongString byteStr)
    | otherwise = MkRDBLengthPrefixedExtraLongString (RDBLengthPrefixedExtraLongString byteStr)

-- https://github.com/redis/redis/blob/unstable/src/rdb.c#L445, attempts integer encoding for strings with length <= 11
toRDBLengthPrefixedValOptimizedToIntEncodingIfPossible :: BS.ByteString -> RDBLengthPrefixedVal
toRDBLengthPrefixedValOptimizedToIntEncodingIfPossible byteStr
    | BS.length byteStr <= randomCutOffToAttemptIntParsing, Just num <- tryParseAsInt8 byteStr = RDBLengthPrefixedInt8Val (RDBLengthPrefixedInt8 num)
    | BS.length byteStr <= randomCutOffToAttemptIntParsing, Just num <- tryParseAsInt16 byteStr = RDBLengthPrefixedInt16Val (RDBLengthPrefixedInt16 num)
    | BS.length byteStr <= randomCutOffToAttemptIntParsing, Just num <- tryParseAsInt32 byteStr = RDBLengthPrefixedInt32Val (RDBLengthPrefixedInt32 num)
    | BS.length byteStr <= shortStringLenCutoff = RDBLengthPrefixedShortStringVal (RDBLengthPrefixedShortString byteStr)
    | BS.length byteStr <= mediumStringLenCutoff = RDBLengthPrefixedMediumStringVal (RDBLengthPrefixedMediumString byteStr)
    | BS.length byteStr <= longStringLenCutoff = RDBLengthPrefixedLongStringVal (RDBLengthPrefixedLongString byteStr)
    | otherwise = RDBLengthPrefixedExtraLongStringVal (RDBLengthPrefixedExtraLongString byteStr)

randomCutOffToAttemptIntParsing :: Int
randomCutOffToAttemptIntParsing = 11

toRDBLengthPrefixedStr :: BS.ByteString -> RDBLengthPrefixedString
toRDBLengthPrefixedStr byteStr
    | BS.length byteStr <= shortStringLenCutoff = MkRDBLengthPrefixedShortString (RDBLengthPrefixedShortString byteStr)
    | BS.length byteStr <= mediumStringLenCutoff = MkRDBLengthPrefixedMediumString (RDBLengthPrefixedMediumString byteStr)
    | BS.length byteStr <= longStringLenCutoff = MkRDBLengthPrefixedLongString (RDBLengthPrefixedLongString byteStr)
    | otherwise = MkRDBLengthPrefixedExtraLongString (RDBLengthPrefixedExtraLongString byteStr)

fromRDBLengthPrefixedStr :: RDBLengthPrefixedString -> BS.ByteString
fromRDBLengthPrefixedStr (MkRDBLengthPrefixedShortString (RDBLengthPrefixedShortString byteStr)) = byteStr
fromRDBLengthPrefixedStr (MkRDBLengthPrefixedMediumString (RDBLengthPrefixedMediumString byteStr)) = byteStr
fromRDBLengthPrefixedStr (MkRDBLengthPrefixedLongString (RDBLengthPrefixedLongString byteStr)) = byteStr
fromRDBLengthPrefixedStr (MkRDBLengthPrefixedExtraLongString (RDBLengthPrefixedExtraLongString byteStr)) = byteStr

{- | Choose appropriate unsigned integer type based on value range
toUIntValInRDBLengthPrefixedForm :: (Ord a, NumIsUnsigned a) => a -> UIntValInRDBLengthPrefixForm
toUIntValInRDBLengthPrefixedForm num
    | num >= fromIntegral (minBound @Word8) && num <= fromIntegral (maxBound @Word8) = MkUInt8ValInRDBLengthPrefixForm (UInt8ValInRDBLengthPrefixForm $ fromIntegral num)
    | num >= fromIntegral (minBound @Word16) && num <= fromIntegral (maxBound @Word16) = MkUInt16ValInRDBLengthPrefixForm (UInt16ValInRDBLengthPrefixForm $ fromIntegral num)
    | num >= fromIntegral (minBound @Word32) && num <= fromIntegral (maxBound @Word32) = MkUInt32ValInRDBLengthPrefixForm (UInt32ValInRDBLengthPrefixForm $ fromIntegral num)
    | otherwise = MkUInt64ValInRDBLengthPrefixForm (UInt64ValInRDBLengthPrefixForm $ fromIntegral num)
-}

-- | Extract value from unsigned integer wrapper type
fromUIntValInRDBLengthPrefixedForm :: (Num a) => UIntValInRDBLengthPrefixForm -> a
fromUIntValInRDBLengthPrefixedForm = \case
    MkUInt8ValInRDBLengthPrefixForm num -> fromIntegral num
    MkUInt16ValInRDBLengthPrefixForm num -> fromIntegral num
    MkUInt32ValInRDBLengthPrefixForm num -> fromIntegral num
    MkUInt64ValInRDBLengthPrefixForm num -> fromIntegral num

shortStringLenCutoff :: Int
shortStringLenCutoff = 63

mediumStringLenCutoff :: Int
mediumStringLenCutoff = 16383

longStringLenCutoff :: Int
longStringLenCutoff = 4294967295

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

fromRDBLengthPrefixedVal :: RDBLengthPrefixedVal -> BS.ByteString
fromRDBLengthPrefixedVal (RDBLengthPrefixedShortStringVal (RDBLengthPrefixedShortString byteStr)) = byteStr
fromRDBLengthPrefixedVal (RDBLengthPrefixedMediumStringVal (RDBLengthPrefixedMediumString byteStr)) = byteStr
fromRDBLengthPrefixedVal (RDBLengthPrefixedLongStringVal (RDBLengthPrefixedLongString byteStr)) = byteStr
fromRDBLengthPrefixedVal (RDBLengthPrefixedExtraLongStringVal (RDBLengthPrefixedExtraLongString byteStr)) = byteStr
fromRDBLengthPrefixedVal (RDBLengthPrefixedInt8Val (RDBLengthPrefixedInt8 int8Val)) = fromString . show $ int8Val
fromRDBLengthPrefixedVal (RDBLengthPrefixedInt16Val (RDBLengthPrefixedInt16 int16Val)) = fromString . show $ int16Val
fromRDBLengthPrefixedVal (RDBLengthPrefixedInt32Val (RDBLengthPrefixedInt32 int32Val)) = fromString . show $ int32Val

toPosixTimeFromRDBUnixTimestampS :: RDBUnixTimestampS -> POSIXTime
toPosixTimeFromRDBUnixTimestampS = fromIntegral . (.getRDBUnixTimestampS)

fromPosixTimeToRDBUnixTimestampS :: POSIXTime -> RDBUnixTimestampS
fromPosixTimeToRDBUnixTimestampS = RDBUnixTimestampS . floor @_ @Word32

fromPosixTimeToRDBUnixTimestampMS :: POSIXTime -> RDBUnixTimestampMS
fromPosixTimeToRDBUnixTimestampMS = RDBUnixTimestampMS . floor @_ @Word64 . secondsToMilliseconds

toPosixTimeFromRDBUnixTimestampMS :: RDBUnixTimestampMS -> POSIXTime
toPosixTimeFromRDBUnixTimestampMS = secondsToNominalDiffTime . millisecondsToSeconds . fromIntegral @_ @Pico . (.getRDBUnixTimestampMS)

{- | Encode a bytestring using RDB's 6-bit length prefix
Format: [00XXXXXX] where XXXXXX is the 6-bit length (0-63 bytes)
Used for strings up to 63 bytes
-}
rdbVariableLengthEncodeStrOneByte :: RDBLengthPrefixedShortString -> Put
rdbVariableLengthEncodeStrOneByte (RDBLengthPrefixedShortString byteStr) = do
    -- Extract the length as Word8 (max 63 for 6-bit encoding)
    let shortRdbByteStrLen = fromIntegral @_ @Word8 $ BS.length byteStr
    -- Use specialized integer length encoding function
    let lengthPrefix = UInt8ValInRDBLengthPrefixForm shortRdbByteStrLen
    -- Write length prefix followed by data
    rdbEncodeUInt8ToLengthPrefixedForm lengthPrefix <> putByteString byteStr

{- | Decode a bytestring using RDB's 6-bit length prefix
Format: [00XXXXXX] where XXXXXX is the 6-bit length (0-63 bytes)
-}
rdbVariableLengthDecodeStrOneByte :: Get RDBLengthPrefixedShortString
rdbVariableLengthDecodeStrOneByte = do
    -- Use specialized integer length decoding function
    UInt8ValInRDBLengthPrefixForm shortByteStrLen <- rdbDecodeUInt8FromLengthPrefixForm
    -- Convert to Int for reading data bytes
    let shortByteStrLenInt = fromIntegral @_ @Int shortByteStrLen
    -- Read the specified number of data bytes
    RDBLengthPrefixedShortString <$> getByteString shortByteStrLenInt

{- | Encode a bytestring using RDB's 14-bit length prefix
Format: [01XXXXXX] [YYYYYYYY] where XXXXXXYYYYYYYY is the 14-bit length (64-16383 bytes)
Used for strings from 64 to 16383 bytes
-}
rdbVariableLengthEncodeStrTwoBytes :: RDBLengthPrefixedMediumString -> Put
rdbVariableLengthEncodeStrTwoBytes (RDBLengthPrefixedMediumString byteStr) = do
    -- Get the length and convert to 16-bit value
    let mediumRdbByteStrLen = fromIntegral @_ @Word16 $ BS.length byteStr
    -- Use specialized integer length encoding function
    let lengthPrefix = UInt16ValInRDBLengthPrefixForm mediumRdbByteStrLen
    -- Write length prefix followed by data
    rdbEncodeUInt16ToLengthPrefixedForm lengthPrefix <> putByteString byteStr

{- | Decode a bytestring using RDB's 14-bit length prefix
Format: [01XXXXXX] [YYYYYYYY] where XXXXXXYYYYYYYY is the 14-bit length (64-16383 bytes)
-}
rdbVariableLengthDecodeStrTwoBytes :: Get RDBLengthPrefixedMediumString
rdbVariableLengthDecodeStrTwoBytes = do
    -- Use specialized integer length decoding function
    UInt16ValInRDBLengthPrefixForm mediumByteStrLen <- rdbDecodeUInt16FromLengthPrefixForm
    -- Convert to Int for reading data bytes
    let mediumByteStrLenInt = fromIntegral @_ @Int mediumByteStrLen
    -- Read the specified number of data bytes
    RDBLengthPrefixedMediumString <$> getByteString mediumByteStrLenInt

{- | Encode a bytestring using RDB's 32-bit length prefix
Format: [10000000] [XXXXXXXX] [XXXXXXXX] [XXXXXXXX] [XXXXXXXX] where X's represent the 32-bit length
Used for strings from 16384 bytes and above
-}
rdbVariableLengthEncodeStrFourBytes :: RDBLengthPrefixedLongString -> Put
rdbVariableLengthEncodeStrFourBytes (RDBLengthPrefixedLongString byteStr) = do
    -- Get the length and convert to 32-bit value
    let longRdbStrLen = fromIntegral @_ @Word32 $ BS.length byteStr
    -- Use specialized integer length encoding function
    let lengthPrefix = UInt32ValInRDBLengthPrefixForm longRdbStrLen
    -- Write length prefix followed by data
    rdbEncodeUInt32ToLengthPrefixedForm lengthPrefix <> putByteString byteStr

{- | Decode a bytestring using RDB's 32-bit length prefix
Format: [10000000] [XXXXXXXX] [XXXXXXXX] [XXXXXXXX] [XXXXXXXX] where X's represent the 32-bit length
-}
rdbVariableLengthDecodeStrFourBytes :: Get RDBLengthPrefixedLongString
rdbVariableLengthDecodeStrFourBytes = do
    -- Use specialized integer length decoding function
    UInt32ValInRDBLengthPrefixForm longByteStrLen <- rdbDecodeUInt32FromLengthPrefixForm
    -- Convert to Int for reading data bytes
    let longByteStrLenInt = fromIntegral @_ @Int longByteStrLen
    -- Read the specified number of data bytes
    RDBLengthPrefixedLongString <$> getByteString longByteStrLenInt

{- | Encode a bytestring using RDB's 64-bit length prefix
Format: [10000001] [XXXXXXXX]...[XXXXXXXX] where X's represent the 64-bit length
Used for strings from 2^32 bytes and above
-}
rdbVariableLengthEncodeStrNineBytes :: RDBLengthPrefixedExtraLongString -> Put
rdbVariableLengthEncodeStrNineBytes (RDBLengthPrefixedExtraLongString byteStr) = do
    -- Get the length and convert to 64-bit value
    let extraLongRdbStrLen = fromIntegral @_ @Word64 $ BS.length byteStr
    -- Use specialized integer length encoding function
    let lengthPrefix = UInt64ValInRDBLengthPrefixForm extraLongRdbStrLen
    -- Write length prefix followed by data
    rdbEncodeUInt64ToLengthPrefixedForm lengthPrefix <> putByteString byteStr

{- | Decode a bytestring using RDB's 64-bit length prefix
Format: [10000001] [XXXXXXXX]...[XXXXXXXX] where X's represent the 64-bit length
-}
rdbVariableLengthDecodeStrNineBytes :: Get RDBLengthPrefixedExtraLongString
rdbVariableLengthDecodeStrNineBytes = do
    -- Use specialized integer length decoding function
    UInt64ValInRDBLengthPrefixForm extraLongByteStrLen <- rdbDecodeUInt64FromLengthPrefixForm
    -- Convert to Int for reading data bytes
    let extraLongByteStrLenInt = fromIntegral @_ @Int extraLongByteStrLen
    -- Read the specified number of data bytes
    RDBLengthPrefixedExtraLongString <$> getByteString extraLongByteStrLenInt

{- | Encode an 8-bit integer using RDB's special integer format
Format: [11000000] [XXXXXXXX] where XXXXXXXX is the 8-bit signed integer
Used for integers that fit within 8 bits (-128 to 127)
The prefix 11000000 indicates special encoding with type identifier 0
-}
intPrefix8Bit :: Word8
intPrefix8Bit = 0b11000000

rdbVariableLengthEncodeFor8BitIntAsStr :: RDBLengthPrefixedInt8 -> Put
rdbVariableLengthEncodeFor8BitIntAsStr (RDBLengthPrefixedInt8 intVal) = do
    let prefixByte = intPrefix8Bit :: Word8
    -- Write the prefix byte followed by the 8-bit integer
    putWord8 prefixByte <> putInt8 intVal

{- | Decode an 8-bit integer using RDB's special integer format
Format: [11000000] [XXXXXXXX] where XXXXXXXX is the 8-bit signed integer
Expects prefix 11000000 with type identifier 0 in the last 6 bits
-}
rdbVariableLengthDecode8BitIntFromStr :: Get RDBLengthPrefixedInt8
rdbVariableLengthDecode8BitIntFromStr = do
    -- Read the prefix byte
    prefixByte <- getWord8
    -- Validate the prefix byte format and type identifier
    let isPrefixByteValid = verifyPrefixByte prefixByte
    if not isPrefixByteValid
        then fail "Unexpected prefix byte while attempting to decode 8 bit integer. Expected the first two bits of the prefix byte to be 11 and the last 6 bits to equal 0"
        else
            -- Read the 8-bit signed integer value
            RDBLengthPrefixedInt8 <$> getInt8
  where
    -- Validate that prefix is 11000000 and type identifier (last 6 bits) is 0
    verifyPrefixByte :: Word8 -> Bool
    verifyPrefixByte prefixByte =
        let expectedPrefixByte = intPrefix8Bit
         in prefixByte == expectedPrefixByte

{- | Encode a 16-bit integer using RDB's special integer format
Format: [11000001] [XXXXXXXX] [XXXXXXXX] where XXXXXXXXXXXXXXXX is the 16-bit signed integer
Used for integers that fit within 16 bits (-32768 to 32767)
The prefix 11000001 indicates special encoding with type identifier 1
-}
intPrefix16Bit :: Word8
intPrefix16Bit = 0b11000001

rdbVariableLengthEncodeFor16BitIntAsStr :: RDBLengthPrefixedInt16 -> Put
rdbVariableLengthEncodeFor16BitIntAsStr (RDBLengthPrefixedInt16 intVal) = do
    let prefixByte = intPrefix16Bit
    -- Write the prefix byte followed by the 16-bit integer in little-endian
    putWord8 prefixByte <> putInt16le intVal

{- | Decode a 16-bit integer using RDB's special integer format
Format: [11000001] [XXXXXXXX] [XXXXXXXX] where XXXXXXXXXXXXXXXX is the 16-bit signed integer
Expects prefix 11000001 with type identifier 1 in the last 6 bits
-}
rdbVariableLengthDecode16BitIntFromStr :: Get RDBLengthPrefixedInt16
rdbVariableLengthDecode16BitIntFromStr = do
    -- Read the prefix byte
    prefixByte <- getWord8
    -- Validate the prefix byte format and type identifier
    let isPrefixByteValid = verifyPrefixByte prefixByte
    if not isPrefixByteValid
        then fail "Unexpected prefix byte while attempting to decode 16 bit integer. Expected the first two bits of the prefix byte to be 11 and the last 6 bits to equal 1"
        else
            -- Read the 16-bit signed integer value in little-endian format
            RDBLengthPrefixedInt16 <$> getInt16le
  where
    -- Validate that prefix is 11000000 and type identifier (last 6 bits) is 1
    verifyPrefixByte :: Word8 -> Bool
    verifyPrefixByte prefixByte =
        let expectedPrefixByte = intPrefix16Bit
         in expectedPrefixByte == prefixByte

{- | Encode a 32-bit integer using RDB's special integer format
Format: [11000010] [XXXXXXXX] [XXXXXXXX] [XXXXXXXX] [XXXXXXXX] where X's represent the 32-bit signed integer
Used for integers that fit within 32 bits (-2147483648 to 2147483647)
The prefix 11000010 indicates special encoding with type identifier 2
-}
intPrefix32Bit :: Word8
intPrefix32Bit = 0b11000010

rdbVariableLengthEncodeFor32BitIntAsStr :: RDBLengthPrefixedInt32 -> Put
rdbVariableLengthEncodeFor32BitIntAsStr (RDBLengthPrefixedInt32 intVal) = do
    let prefixByte = intPrefix32Bit
    -- Write the prefix byte followed by the 32-bit integer in little-endian
    putWord8 prefixByte <> putInt32le intVal

{- | Decode a 32-bit integer using RDB's special integer format
Format: [11000010] [XXXXXXXX] [XXXXXXXX] [XXXXXXXX] [XXXXXXXX] where X's represent the 32-bit signed integer
Expects prefix 11000010 with type identifier 2 in the last 6 bits
-}
rdbVariableLengthDecode32BitIntFromStr :: Get RDBLengthPrefixedInt32
rdbVariableLengthDecode32BitIntFromStr = do
    -- Read the prefix byte
    prefixByte <- getWord8
    -- Validate the prefix byte format and type identifier
    let isPrefixByteValid = verifyPrefixByte prefixByte
    if not isPrefixByteValid
        then fail "Unexpected prefix byte while attempting to decode 32 bit integer. Expected the first two bits of the prefix byte to be 11 and the last 6 bits to equal 2"
        else
            -- Read the 32-bit signed integer value in little-endian format
            RDBLengthPrefixedInt32 <$> getInt32le
  where
    -- Validate that prefix is 11000000 and type identifier (last 6 bits) is 2
    verifyPrefixByte :: Word8 -> Bool
    verifyPrefixByte firstByte =
        let expectedPrefixByte = intPrefix32Bit
         in expectedPrefixByte == firstByte

{- | Length-prefixed integer encoding functions
These functions encode standalone length values using the same prefixes as strings,
but without any accompanying data bytes.
-}

{- | Encode a 6-bit length value (0-63)
Format: [00XXXXXX] where XXXXXX is the 6-bit length
-}
rdbEncodeUInt8ToLengthPrefixedForm :: UInt8ValInRDBLengthPrefixForm -> Put
rdbEncodeUInt8ToLengthPrefixedForm (UInt8ValInRDBLengthPrefixForm len) = do
    -- The value is already constrained to fit in 6 bits by the type system
    -- Top 2 bits are implicitly 00, so we can write the length directly
    putWord8 len

{- | Decode a 6-bit length value (0-63)
Format: [00XXXXXX] where XXXXXX is the 6-bit length
-}
rdbDecodeUInt8FromLengthPrefixForm :: Get UInt8ValInRDBLengthPrefixForm
rdbDecodeUInt8FromLengthPrefixForm = do
    byte <- getWord8
    -- Verify the prefix is 00 (top 2 bits should be 0)
    let byteHasExpectedPrefix = (byte `Bits.shiftR` 6) == 0
    if not byteHasExpectedPrefix
        then fail "Unexpected prefix for 6-bit length. Expected first two bits to be 00"
        else pure $ UInt8ValInRDBLengthPrefixForm byte

{- | Encode a 14-bit length value (0-16383)
Format: [01XXXXXX] [YYYYYYYY] where XXXXXXYYYYYYYY is the 14-bit length
-}
rdbEncodeUInt16ToLengthPrefixedForm :: UInt16ValInRDBLengthPrefixForm -> Put
rdbEncodeUInt16ToLengthPrefixedForm (UInt16ValInRDBLengthPrefixForm len) = do
    let prefix = 0b01000000 :: Word8
    -- Extract upper 6 bits from the 16-bit value
    let upperByte = fromIntegral @_ @Word8 (len `Bits.shiftR` 8)
    let sixBitsMask = 0b00111111
    let upperByteWithBitsReset = upperByte Bits..&. sixBitsMask
    let firstByteWithPrefix = prefix Bits..|. upperByteWithBitsReset
    -- Extract lower 8 bits
    let lowerByte = fromIntegral @_ @Word8 len
    putWord8 firstByteWithPrefix <> putWord8 lowerByte

{- | Decode a 14-bit length value (0-16383)
Format: [01XXXXXX] [YYYYYYYY] where XXXXXXYYYYYYYY is the 14-bit length
-}
rdbDecodeUInt16FromLengthPrefixForm :: Get UInt16ValInRDBLengthPrefixForm
rdbDecodeUInt16FromLengthPrefixForm = do
    firstByte <- getWord8
    -- Verify prefix is 01
    let prefixMask = 0b11000000
    let expectedPrefix = 0b01000000
    let hasExpectedPrefix = (firstByte Bits..&. prefixMask) == expectedPrefix
    if not hasExpectedPrefix
        then fail "Unexpected prefix for 14-bit length. Expected first two bits to be 01"
        else do
            secondByte <- getWord8
            -- Extract 6 bits from first byte and combine with second byte
            let sixBitsMask = 0b00111111
            let upperBits = fromIntegral @_ @Word16 (firstByte Bits..&. sixBitsMask)
            let lowerBits = fromIntegral @_ @Word16 secondByte
            let fullLength = (upperBits `Bits.shiftL` 8) Bits..|. lowerBits
            pure $ UInt16ValInRDBLengthPrefixForm fullLength

{- | Encode a 32-bit length value (0-4294967295)
Format: [10000000] [XXXXXXXX]...[XXXXXXXX] where X's represent the 32-bit length
-}
rdbEncodeUInt32ToLengthPrefixedForm :: UInt32ValInRDBLengthPrefixForm -> Put
rdbEncodeUInt32ToLengthPrefixedForm (UInt32ValInRDBLengthPrefixForm len) = do
    let prefixByte = 0b10000000 :: Word8
    putWord8 prefixByte <> putWord32be len

{- | Decode a 32-bit length value (0-4294967295)
Format: [10000000] [XXXXXXXX]...[XXXXXXXX] where X's represent the 32-bit length
-}
rdbDecodeUInt32FromLengthPrefixForm :: Get UInt32ValInRDBLengthPrefixForm
rdbDecodeUInt32FromLengthPrefixForm = do
    prefixByte <- getWord8
    let expectedPrefix = 0b10000000 :: Word8
    if prefixByte /= expectedPrefix
        then fail "Unexpected prefix for 32-bit length. Expected 10000000"
        else UInt32ValInRDBLengthPrefixForm <$> getWord32be

{- | Encode a 64-bit length value (0-18446744073709551615)
Format: [10000001] [XXXXXXXX]...[XXXXXXXX] where X's represent the 64-bit length
-}
rdbEncodeUInt64ToLengthPrefixedForm :: UInt64ValInRDBLengthPrefixForm -> Put
rdbEncodeUInt64ToLengthPrefixedForm (UInt64ValInRDBLengthPrefixForm len) = do
    let prefixByte = 0b10000001 :: Word8
    putWord8 prefixByte <> putWord64be len

{- | Decode a 64-bit length value (0-18446744073709551615)
Format: [10000001] [XXXXXXXX]...[XXXXXXXX] where X's represent the 64-bit length
-}
rdbDecodeUInt64FromLengthPrefixForm :: Get UInt64ValInRDBLengthPrefixForm
rdbDecodeUInt64FromLengthPrefixForm = do
    prefixByte <- getWord8
    let expectedPrefix = 0b10000001 :: Word8
    if prefixByte /= expectedPrefix
        then fail "Unexpected prefix for 64-bit length. Expected 10000001"
        else UInt64ValInRDBLengthPrefixForm <$> getWord64be
