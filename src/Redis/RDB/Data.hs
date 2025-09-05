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

import Data.Binary
import Redis.RDB.SBinary

import Data.Bits qualified as Bits
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as BSC

import Control.Applicative (asum)
import Control.DeepSeq (NFData)
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
import Data.String (IsString (fromString))
import Data.Time.Clock (secondsToNominalDiffTime)
import Data.Time.Clock.POSIX (POSIXTime)
import GHC.Generics (Generic)
import Redis.RDB.SBinary.DerivingVia (BinaryFromSBinary (..))
import Redis.Utils (millisecondsToSeconds, secondsToMilliseconds)

{-
    Okay, so the RDB spec describes what a variant of variable-length encoding that we shall refer to as rdb-variable-length encoding since this variant differs subtly from the standard variable-length encoding. Now, that said, the rdb-variable length encoding is mostly for the length of things to ensure that the length is stored smartly and efficiently, not using more space/bytes than necessary. However, Redis also uses this encoding/algorithm to sometimes encode integers hence we have the UInt8ValInRDBLengthPrefixForm, UInt16ValInRDBLengthPrefixForm, UInt32ValInRDBLengthPrefixForm, and UInt64ValInRDBLengthPrefixForm types which encode integers as if they were a length of a string but without the accompany byte sequence you'd expect for a string.

    Note that these types differ from the RDBInt* types in that the RDBInt* types use a special encoding based off the rdb-variable-length encoding to encode integers as strings with a length as the first byte followed by a byte sequence representing the integer quantity. So in this case we aren't just encoding the length alone and considering it an integer during decoding, we encode the a special prefix byte followed by the encoded integer value

    (Do not Change)
-}

-- Globally, when we refer to length-prefix we really mean length-prefix encoding as per the rdb-variable-length encoding scheme (Do not Change)

newtype RDBLengthPrefixedShortString = RDBLengthPrefixedShortString {getRDBLengthPrefixedShortString :: BS.ByteString}
    deriving stock (Show, Eq, Ord, Generic)
    deriving newtype (IsString)
    deriving anyclass (NFData)
    deriving (Binary) via (BinaryFromSBinary RDBLengthPrefixedShortString)

newtype RDBLengthPrefixedMediumString = RDBLengthPrefixedMediumString {getRDBLengthPrefixedMediumString :: BS.ByteString}
    deriving stock (Show, Eq, Ord, Generic)
    deriving newtype (IsString)
    deriving anyclass (NFData)
    deriving (Binary) via (BinaryFromSBinary RDBLengthPrefixedMediumString)

newtype RDBLengthPrefixedLongString = RDBLengthPrefixedLongString {getRDBLengthPrefixedLongString :: BS.ByteString}
    deriving stock (Show, Eq, Ord, Generic)
    deriving newtype (IsString)
    deriving anyclass (NFData)
    deriving (Binary) via (BinaryFromSBinary RDBLengthPrefixedLongString)

newtype RDBLengthPrefixedExtraLongString = RDBLengthPrefixedExtraLongString {getRDBLengthPrefixedExtraLongString :: BS.ByteString}
    deriving stock (Show, Eq, Ord, Generic)
    deriving newtype (IsString)
    deriving anyclass (NFData)
    deriving (Binary) via (BinaryFromSBinary RDBLengthPrefixedExtraLongString)

-- The encoding and decoding of length-prefixed integer types are done in little-endian (Do not Change)

newtype RDBLengthPrefixedInt8 = RDBLengthPrefixedInt8 {getRDBLengthPrefixedInt8 :: Int8}
    deriving stock (Show, Eq, Ord, Generic)
    deriving anyclass (NFData)
    deriving (Binary) via (BinaryFromSBinary RDBLengthPrefixedInt8)

newtype RDBLengthPrefixedInt16 = RDBLengthPrefixedInt16 {getRDBLengthPrefixedInt16 :: Int16}
    deriving stock (Show, Eq, Ord, Generic)
    deriving anyclass (NFData)
    deriving (Binary) via (BinaryFromSBinary RDBLengthPrefixedInt16)

newtype RDBLengthPrefixedInt32 = RDBLengthPrefixedInt32 {getRDBLengthPrefixedInt32 :: Int32}
    deriving stock (Show, Eq, Ord, Generic)
    deriving anyclass (NFData)
    deriving (Binary) via (BinaryFromSBinary RDBLengthPrefixedInt32)

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
    deriving (Binary) via (BinaryFromSBinary UInt8ValInRDBLengthPrefixForm)

-- | 14-bit length encoding (0-16383)
newtype UInt16ValInRDBLengthPrefixForm = UInt16ValInRDBLengthPrefixForm {getUInt16ValInRDBLengthPrefixForm :: Word16}
    deriving stock (Show, Eq, Ord)
    deriving newtype (Num, Enum, Real, Integral)
    deriving (Binary) via (BinaryFromSBinary UInt16ValInRDBLengthPrefixForm)

-- | 32-bit length encoding (0-4294967295)
newtype UInt32ValInRDBLengthPrefixForm = UInt32ValInRDBLengthPrefixForm {getUInt32ValInRDBLengthPrefixForm :: Word32}
    deriving stock (Show, Eq, Ord)
    deriving newtype (Num, Enum, Real, Integral)
    deriving (Binary) via (BinaryFromSBinary UInt32ValInRDBLengthPrefixForm)

-- | 64-bit length encoding (0-18446744073709551615)
newtype UInt64ValInRDBLengthPrefixForm = UInt64ValInRDBLengthPrefixForm {getUInt64ValInRDBLengthPrefixForm :: Word64}
    deriving stock (Show, Eq, Ord)
    deriving newtype (Num, Enum, Real, Integral)
    deriving (Binary) via (BinaryFromSBinary UInt64ValInRDBLengthPrefixForm)

-- | RDB Unix timestamp types
newtype RDBUnixTimestampS = RDBUnixTimestampS {getRDBUnixTimestampS :: Word32}
    deriving stock (Show, Eq, Ord)
    deriving (Binary) via (BinaryFromSBinary RDBUnixTimestampS)

newtype RDBUnixTimestampMS = RDBUnixTimestampMS {getRDBUnixTimestampMS :: Word64}
    deriving stock (Show, Eq, Ord)
    deriving (Binary) via (BinaryFromSBinary RDBUnixTimestampMS)

-- | Sum types
data RDBLengthPrefixedString
    = MkRDBLengthPrefixedShortString RDBLengthPrefixedShortString
    | MkRDBLengthPrefixedMediumString RDBLengthPrefixedMediumString
    | MkRDBLengthPrefixedLongString RDBLengthPrefixedLongString
    | MkRDBLengthPrefixedExtraLongString RDBLengthPrefixedExtraLongString
    deriving stock (Show, Eq)
    deriving (Binary) via (BinaryFromSBinary RDBLengthPrefixedString)

data UIntValInRDBLengthPrefixForm
    = MkUInt8ValInRDBLengthPrefixForm UInt8ValInRDBLengthPrefixForm
    | MkUInt16ValInRDBLengthPrefixForm UInt16ValInRDBLengthPrefixForm
    | MkUInt32ValInRDBLengthPrefixForm UInt32ValInRDBLengthPrefixForm
    | MkUInt64ValInRDBLengthPrefixForm UInt64ValInRDBLengthPrefixForm
    deriving stock (Show, Eq)
    deriving (Binary) via (BinaryFromSBinary UIntValInRDBLengthPrefixForm)

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
    deriving stock (Show, Eq, Generic)
    deriving anyclass (NFData)
    deriving (Binary) via (BinaryFromSBinary RDBLengthPrefixedVal)

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

instance SBinary RDBLengthPrefixedVal where
    putWithChecksum = \case
        (RDBLengthPrefixedInt8Val rdbLengthPrefixedInt8) -> putWithChecksum rdbLengthPrefixedInt8
        (RDBLengthPrefixedInt16Val rdbLengthPrefixedInt16) -> putWithChecksum rdbLengthPrefixedInt16
        (RDBLengthPrefixedInt32Val rdbLengthPrefixedInt32) -> putWithChecksum rdbLengthPrefixedInt32
        (RDBLengthPrefixedShortStringVal shortRdbString) -> putWithChecksum shortRdbString
        (RDBLengthPrefixedMediumStringVal mediumRdbString) -> putWithChecksum mediumRdbString
        (RDBLengthPrefixedLongStringVal longRdbString) -> putWithChecksum longRdbString
        (RDBLengthPrefixedExtraLongStringVal extraLongRdbString) -> putWithChecksum extraLongRdbString
    getWithChecksum =
        asum
            [ RDBLengthPrefixedInt8Val <$> getWithChecksum
            , RDBLengthPrefixedInt16Val <$> getWithChecksum
            , RDBLengthPrefixedInt32Val <$> getWithChecksum
            , RDBLengthPrefixedShortStringVal <$> getWithChecksum
            , RDBLengthPrefixedMediumStringVal <$> getWithChecksum
            , RDBLengthPrefixedLongStringVal <$> getWithChecksum
            , RDBLengthPrefixedExtraLongStringVal <$> getWithChecksum
            , fail "Failed to decode RDBLengthPrefixedVal"
            ]

instance SBinary RDBUnixTimestampS where
    putWithChecksum (RDBUnixTimestampS t) = genericPutWithChecksumUsing (putWord32le t) -- Redis uses little-endian for timestamps

    getWithChecksum = RDBUnixTimestampS <$> genericGetWithChecksumUsing getWord32le

instance SBinary RDBUnixTimestampMS where
    putWithChecksum (RDBUnixTimestampMS t) = genericPutWithChecksumUsing (putWord64le t) -- Redis uses little-endian for timestamps

    getWithChecksum = RDBUnixTimestampMS <$> genericGetWithChecksumUsing getWord64le

instance SBinary RDBLengthPrefixedString where
    putWithChecksum = \case
        MkRDBLengthPrefixedShortString shortStr -> putWithChecksum shortStr
        MkRDBLengthPrefixedMediumString mediumStr -> putWithChecksum mediumStr
        MkRDBLengthPrefixedLongString longStr -> putWithChecksum longStr
        MkRDBLengthPrefixedExtraLongString extraLongStr -> putWithChecksum extraLongStr

    getWithChecksum =
        asum
            [ MkRDBLengthPrefixedShortString <$> getWithChecksum
            , MkRDBLengthPrefixedMediumString <$> getWithChecksum
            , MkRDBLengthPrefixedLongString <$> getWithChecksum
            , MkRDBLengthPrefixedExtraLongString <$> getWithChecksum
            , fail "Failed to decode RDBLengthPrefixedString"
            ]

instance SBinary UIntValInRDBLengthPrefixForm where
    putWithChecksum = \case
        MkUInt8ValInRDBLengthPrefixForm uint8Val -> putWithChecksum uint8Val
        MkUInt16ValInRDBLengthPrefixForm uint16Val -> putWithChecksum uint16Val
        MkUInt32ValInRDBLengthPrefixForm uint32Val -> putWithChecksum uint32Val
        MkUInt64ValInRDBLengthPrefixForm uint64Val -> putWithChecksum uint64Val

    getWithChecksum =
        asum
            [ MkUInt8ValInRDBLengthPrefixForm <$> getWithChecksum
            , MkUInt16ValInRDBLengthPrefixForm <$> getWithChecksum
            , MkUInt32ValInRDBLengthPrefixForm <$> getWithChecksum
            , MkUInt64ValInRDBLengthPrefixForm <$> getWithChecksum
            , fail "Failed to decode int val in rdb length prefix form"
            ]

instance SBinary RDBLengthPrefixedShortString where
    putWithChecksum = rdbVariableLengthEncodeStrOneByte
    getWithChecksum = rdbVariableLengthDecodeStrOneByte

instance SBinary RDBLengthPrefixedMediumString where
    putWithChecksum = rdbVariableLengthEncodeStrTwoBytes
    getWithChecksum = rdbVariableLengthDecodeStrTwoBytes

instance SBinary RDBLengthPrefixedLongString where
    putWithChecksum = rdbVariableLengthEncodeStrFourBytes
    getWithChecksum = rdbVariableLengthDecodeStrFourBytes

instance SBinary RDBLengthPrefixedInt8 where
    putWithChecksum = rdbVariableLengthEncodeFor8BitIntAsStr
    getWithChecksum = rdbVariableLengthDecode8BitIntFromStr

instance SBinary RDBLengthPrefixedInt16 where
    putWithChecksum = rdbVariableLengthEncodeFor16BitIntAsStr
    getWithChecksum = rdbVariableLengthDecode16BitIntFromStr

instance SBinary RDBLengthPrefixedInt32 where
    putWithChecksum = rdbVariableLengthEncodeFor32BitIntAsStr
    getWithChecksum = rdbVariableLengthDecode32BitIntFromStr

instance SBinary RDBLengthPrefixedExtraLongString where
    putWithChecksum = rdbVariableLengthEncodeStrNineBytes
    getWithChecksum = rdbVariableLengthDecodeStrNineBytes

-- | Binary instances for length-prefixed integers
instance SBinary UInt8ValInRDBLengthPrefixForm where
    putWithChecksum = rdbEncodeUInt8ToLengthPrefixedForm
    getWithChecksum = rdbDecodeUInt8FromLengthPrefixForm

instance SBinary UInt16ValInRDBLengthPrefixForm where
    putWithChecksum = rdbEncodeUInt16ToLengthPrefixedForm
    getWithChecksum = rdbDecodeUInt16FromLengthPrefixForm

instance SBinary UInt32ValInRDBLengthPrefixForm where
    putWithChecksum = rdbEncodeUInt32ToLengthPrefixedForm
    getWithChecksum = rdbDecodeUInt32FromLengthPrefixForm

instance SBinary UInt64ValInRDBLengthPrefixForm where
    putWithChecksum = rdbEncodeUInt64ToLengthPrefixedForm
    getWithChecksum = rdbDecodeUInt64FromLengthPrefixForm

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
rdbVariableLengthEncodeStrOneByte :: RDBLengthPrefixedShortString -> SPut
rdbVariableLengthEncodeStrOneByte (RDBLengthPrefixedShortString byteStr) = do
    -- Extract the length as Word8 (max 63 for 6-bit encoding)
    let shortRdbByteStrLen = fromIntegral @_ @Word8 $ BS.length byteStr
    -- Use specialized integer length encoding function
    let lengthPrefix = UInt8ValInRDBLengthPrefixForm shortRdbByteStrLen
    -- Write length prefix followed by data
    rdbEncodeUInt8ToLengthPrefixedForm lengthPrefix
    genericPutWithChecksumUsing $ putByteString byteStr

{- | Decode a bytestring using RDB's 6-bit length prefix
Format: [00XXXXXX] where XXXXXX is the 6-bit length (0-63 bytes)
-}
rdbVariableLengthDecodeStrOneByte :: SGet RDBLengthPrefixedShortString
rdbVariableLengthDecodeStrOneByte = do
    -- Use specialized integer length decoding function
    UInt8ValInRDBLengthPrefixForm shortByteStrLen <- rdbDecodeUInt8FromLengthPrefixForm
    -- Convert to Int for reading data bytes
    let shortByteStrLenInt = fromIntegral @_ @Int shortByteStrLen
    -- Read the specified number of data bytes
    RDBLengthPrefixedShortString <$> genericGetWithChecksumUsing (getByteString shortByteStrLenInt)

{- | Encode a bytestring using RDB's 14-bit length prefix
Format: [01XXXXXX] [YYYYYYYY] where XXXXXXYYYYYYYY is the 14-bit length (64-16383 bytes)
Used for strings from 64 to 16383 bytes
-}
rdbVariableLengthEncodeStrTwoBytes :: RDBLengthPrefixedMediumString -> SPut
rdbVariableLengthEncodeStrTwoBytes (RDBLengthPrefixedMediumString byteStr) = do
    -- Get the length and convert to 16-bit value
    let mediumRdbByteStrLen = fromIntegral @_ @Word16 $ BS.length byteStr
    -- Use specialized integer length encoding function
    let lengthPrefix = UInt16ValInRDBLengthPrefixForm mediumRdbByteStrLen
    -- Write length prefix followed by data
    rdbEncodeUInt16ToLengthPrefixedForm lengthPrefix
    genericPutWithChecksumUsing $ putByteString byteStr

{- | Decode a bytestring using RDB's 14-bit length prefix
Format: [01XXXXXX] [YYYYYYYY] where XXXXXXYYYYYYYY is the 14-bit length (64-16383 bytes)
-}
rdbVariableLengthDecodeStrTwoBytes :: SGet RDBLengthPrefixedMediumString
rdbVariableLengthDecodeStrTwoBytes = do
    -- Use specialized integer length decoding function
    UInt16ValInRDBLengthPrefixForm mediumByteStrLen <- rdbDecodeUInt16FromLengthPrefixForm
    -- Convert to Int for reading data bytes
    let mediumByteStrLenInt = fromIntegral @_ @Int mediumByteStrLen
    -- Read the specified number of data bytes
    RDBLengthPrefixedMediumString <$> genericGetWithChecksumUsing (getByteString mediumByteStrLenInt)

{- | Encode a bytestring using RDB's 32-bit length prefix
Format: [10000000] [XXXXXXXX] [XXXXXXXX] [XXXXXXXX] [XXXXXXXX] where X's represent the 32-bit length
Used for strings from 16384 bytes and above
-}
rdbVariableLengthEncodeStrFourBytes :: RDBLengthPrefixedLongString -> SPut
rdbVariableLengthEncodeStrFourBytes (RDBLengthPrefixedLongString byteStr) = do
    -- Get the length and convert to 32-bit value
    let longRdbStrLen = fromIntegral @_ @Word32 $ BS.length byteStr
    -- Use specialized integer length encoding function
    let lengthPrefix = UInt32ValInRDBLengthPrefixForm longRdbStrLen
    -- Write length prefix followed by data
    rdbEncodeUInt32ToLengthPrefixedForm lengthPrefix
    genericPutWithChecksumUsing (putByteString byteStr)

{- | Decode a bytestring using RDB's 32-bit length prefix
Format: [10000000] [XXXXXXXX] [XXXXXXXX] [XXXXXXXX] [XXXXXXXX] where X's represent the 32-bit length
-}
rdbVariableLengthDecodeStrFourBytes :: SGet RDBLengthPrefixedLongString
rdbVariableLengthDecodeStrFourBytes = do
    -- Use specialized integer length decoding function
    UInt32ValInRDBLengthPrefixForm longByteStrLen <- rdbDecodeUInt32FromLengthPrefixForm
    -- Convert to Int for reading data bytes
    let longByteStrLenInt = fromIntegral @_ @Int longByteStrLen
    -- Read the specified number of data bytes
    RDBLengthPrefixedLongString <$> genericGetWithChecksumUsing (getByteString longByteStrLenInt)

{- | Encode a bytestring using RDB's 64-bit length prefix
Format: [10000001] [XXXXXXXX]...[XXXXXXXX] where X's represent the 64-bit length
Used for strings from 2^32 bytes and above
-}
rdbVariableLengthEncodeStrNineBytes :: RDBLengthPrefixedExtraLongString -> SPut
rdbVariableLengthEncodeStrNineBytes (RDBLengthPrefixedExtraLongString byteStr) = do
    -- Get the length and convert to 64-bit value
    let extraLongRdbStrLen = fromIntegral @_ @Word64 $ BS.length byteStr
    -- Use specialized integer length encoding function
    let lengthPrefix = UInt64ValInRDBLengthPrefixForm extraLongRdbStrLen
    -- Write length prefix followed by data
    rdbEncodeUInt64ToLengthPrefixedForm lengthPrefix
    genericPutWithChecksumUsing (putByteString byteStr)

{- | Decode a bytestring using RDB's 64-bit length prefix
Format: [10000001] [XXXXXXXX]...[XXXXXXXX] where X's represent the 64-bit length
-}
rdbVariableLengthDecodeStrNineBytes :: SGet RDBLengthPrefixedExtraLongString
rdbVariableLengthDecodeStrNineBytes = do
    -- Use specialized integer length decoding function
    UInt64ValInRDBLengthPrefixForm extraLongByteStrLen <- rdbDecodeUInt64FromLengthPrefixForm
    -- Convert to Int for reading data bytes
    let extraLongByteStrLenInt = fromIntegral @_ @Int extraLongByteStrLen
    -- Read the specified number of data bytes
    RDBLengthPrefixedExtraLongString <$> genericGetWithChecksumUsing (getByteString extraLongByteStrLenInt)

{- | Encode an 8-bit integer using RDB's special integer format
Format: [11000000] [XXXXXXXX] where XXXXXXXX is the 8-bit signed integer
Used for integers that fit within 8 bits (-128 to 127)
The prefix 11000000 indicates special encoding with type identifier 0
-}
intPrefix8Bit :: Word8
intPrefix8Bit = 0b11000000

rdbVariableLengthEncodeFor8BitIntAsStr :: RDBLengthPrefixedInt8 -> SPut
rdbVariableLengthEncodeFor8BitIntAsStr (RDBLengthPrefixedInt8 intVal) = do
    let prefixByte = intPrefix8Bit :: Word8
    -- Write the prefix byte followed by the 8-bit integer
    genericPutWithChecksum @Word8 prefixByte
    genericPutWithChecksum @Int8 intVal

{- | Decode an 8-bit integer using RDB's special integer format
Format: [11000000] [XXXXXXXX] where XXXXXXXX is the 8-bit signed integer
Expects prefix 11000000 with type identifier 0 in the last 6 bits
-}
rdbVariableLengthDecode8BitIntFromStr :: SGet RDBLengthPrefixedInt8
rdbVariableLengthDecode8BitIntFromStr = do
    -- Read the prefix byte
    prefixByte <- genericGetWithChecksum @Word8
    -- Validate the prefix byte format and type identifier
    let isPrefixByteValid = verifyPrefixByte prefixByte
    if not isPrefixByteValid
        then fail "Unexpected prefix byte while attempting to decode 8 bit integer. Expected the first two bits of the prefix byte to be 11 and the last 6 bits to equal 0"
        else
            -- Read the 8-bit signed integer value
            RDBLengthPrefixedInt8 <$> genericGetWithChecksum @Int8
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

rdbVariableLengthEncodeFor16BitIntAsStr :: RDBLengthPrefixedInt16 -> SPut
rdbVariableLengthEncodeFor16BitIntAsStr (RDBLengthPrefixedInt16 intVal) = do
    let prefixByte = intPrefix16Bit
    -- Write the prefix byte followed by the 16-bit integer in little-endian
    genericPutWithChecksum @Word8 prefixByte
    genericPutWithChecksumUsing (putInt16le intVal)

{- | Decode a 16-bit integer using RDB's special integer format
Format: [11000001] [XXXXXXXX] [XXXXXXXX] where XXXXXXXXXXXXXXXX is the 16-bit signed integer
Expects prefix 11000001 with type identifier 1 in the last 6 bits
-}
rdbVariableLengthDecode16BitIntFromStr :: SGet RDBLengthPrefixedInt16
rdbVariableLengthDecode16BitIntFromStr = do
    -- Read the prefix byte
    prefixByte <- genericGetWithChecksum @Word8
    -- Validate the prefix byte format and type identifier
    let isPrefixByteValid = verifyPrefixByte prefixByte
    if not isPrefixByteValid
        then fail "Unexpected prefix byte while attempting to decode 16 bit integer. Expected the first two bits of the prefix byte to be 11 and the last 6 bits to equal 1"
        else
            -- Read the 16-bit signed integer value in little-endian format
            RDBLengthPrefixedInt16 <$> genericGetWithChecksumUsing getInt16le
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

rdbVariableLengthEncodeFor32BitIntAsStr :: RDBLengthPrefixedInt32 -> SPut
rdbVariableLengthEncodeFor32BitIntAsStr (RDBLengthPrefixedInt32 intVal) = do
    let prefixByte = intPrefix32Bit
    -- Write the prefix byte followed by the 32-bit integer in little-endian
    genericPutWithChecksum @Word8 prefixByte
    genericPutWithChecksumUsing (putInt32le intVal)

{- | Decode a 32-bit integer using RDB's special integer format
Format: [11000010] [XXXXXXXX] [XXXXXXXX] [XXXXXXXX] [XXXXXXXX] where X's represent the 32-bit signed integer
Expects prefix 11000010 with type identifier 2 in the last 6 bits
-}
rdbVariableLengthDecode32BitIntFromStr :: SGet RDBLengthPrefixedInt32
rdbVariableLengthDecode32BitIntFromStr = do
    -- Read the prefix byte
    prefixByte <- genericGetWithChecksum @Word8
    -- Validate the prefix byte format and type identifier
    let isPrefixByteValid = verifyPrefixByte prefixByte
    if not isPrefixByteValid
        then fail "Unexpected prefix byte while attempting to decode 32 bit integer. Expected the first two bits of the prefix byte to be 11 and the last 6 bits to equal 2"
        else
            -- Read the 32-bit signed integer value in little-endian format
            RDBLengthPrefixedInt32 <$> genericGetWithChecksumUsing getInt32le
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
rdbEncodeUInt8ToLengthPrefixedForm :: UInt8ValInRDBLengthPrefixForm -> SPut
rdbEncodeUInt8ToLengthPrefixedForm (UInt8ValInRDBLengthPrefixForm len) = do
    -- The value is already constrained to fit in 6 bits by the type system
    -- Top 2 bits are implicitly 00, so we can write the length directly
    genericPutWithChecksum @Word8 len

{- | Decode a 6-bit length value (0-63)
Format: [00XXXXXX] where XXXXXX is the 6-bit length
-}
rdbDecodeUInt8FromLengthPrefixForm :: SGet UInt8ValInRDBLengthPrefixForm
rdbDecodeUInt8FromLengthPrefixForm = do
    byte <- genericGetWithChecksum @Word8
    -- Verify the prefix is 00 (top 2 bits should be 0)
    let byteHasExpectedPrefix = (byte `Bits.shiftR` 6) == 0
    if not byteHasExpectedPrefix
        then fail "Unexpected prefix for 6-bit length. Expected first two bits to be 00"
        else pure $ UInt8ValInRDBLengthPrefixForm byte

{- | Encode a 14-bit length value (0-16383)
Format: [01XXXXXX] [YYYYYYYY] where XXXXXXYYYYYYYY is the 14-bit length
-}
rdbEncodeUInt16ToLengthPrefixedForm :: UInt16ValInRDBLengthPrefixForm -> SPut
rdbEncodeUInt16ToLengthPrefixedForm (UInt16ValInRDBLengthPrefixForm len) = do
    let prefix = 0b01000000 :: Word8
    -- Extract upper 6 bits from the 16-bit value
    let upperByte = fromIntegral @_ @Word8 (len `Bits.shiftR` 8)
    let sixBitsMask = 0b00111111
    let upperByteWithBitsReset = upperByte Bits..&. sixBitsMask
    let firstByteWithPrefix = prefix Bits..|. upperByteWithBitsReset
    -- Extract lower 8 bits
    let lowerByte = fromIntegral @_ @Word8 len

    genericPutWithChecksum @Word8 firstByteWithPrefix
    genericPutWithChecksum @Word8 lowerByte

{- | Decode a 14-bit length value (0-16383)
Format: [01XXXXXX] [YYYYYYYY] where XXXXXXYYYYYYYY is the 14-bit length
-}
rdbDecodeUInt16FromLengthPrefixForm :: SGet UInt16ValInRDBLengthPrefixForm
rdbDecodeUInt16FromLengthPrefixForm = do
    firstByte <- genericGetWithChecksum @Word8
    -- Verify prefix is 01
    let prefixMask = 0b11000000
    let expectedPrefix = 0b01000000
    let hasExpectedPrefix = (firstByte Bits..&. prefixMask) == expectedPrefix
    if not hasExpectedPrefix
        then fail "Unexpected prefix for 14-bit length. Expected first two bits to be 01"
        else do
            secondByte <- genericGetWithChecksum @Word8
            -- Extract 6 bits from first byte and combine with second byte
            let sixBitsMask = 0b00111111
            let upperBits = fromIntegral @_ @Word16 (firstByte Bits..&. sixBitsMask)
            let lowerBits = fromIntegral @_ @Word16 secondByte
            let fullLength = (upperBits `Bits.shiftL` 8) Bits..|. lowerBits
            pure $ UInt16ValInRDBLengthPrefixForm fullLength

{- | Encode a 32-bit length value (0-4294967295)
Format: [10000000] [XXXXXXXX]...[XXXXXXXX] where X's represent the 32-bit length
-}
rdbEncodeUInt32ToLengthPrefixedForm :: UInt32ValInRDBLengthPrefixForm -> SPut
rdbEncodeUInt32ToLengthPrefixedForm (UInt32ValInRDBLengthPrefixForm len) = do
    let prefixByte = 0b10000000 :: Word8
    genericPutWithChecksum @Word8 prefixByte
    genericPutWithChecksumUsing (putWord32be len)

{- | Decode a 32-bit length value (0-4294967295)
Format: [10000000] [XXXXXXXX]...[XXXXXXXX] where X's represent the 32-bit length
-}
rdbDecodeUInt32FromLengthPrefixForm :: SGet UInt32ValInRDBLengthPrefixForm
rdbDecodeUInt32FromLengthPrefixForm = do
    prefixByte <- genericGetWithChecksum @Word8
    let expectedPrefix = 0b10000000 :: Word8
    if prefixByte /= expectedPrefix
        then fail "Unexpected prefix for 32-bit length. Expected 10000000"
        else UInt32ValInRDBLengthPrefixForm <$> genericGetWithChecksumUsing getWord32be

{- | Encode a 64-bit length value (0-18446744073709551615)
Format: [10000001] [XXXXXXXX]...[XXXXXXXX] where X's represent the 64-bit length
-}
rdbEncodeUInt64ToLengthPrefixedForm :: UInt64ValInRDBLengthPrefixForm -> SPut
rdbEncodeUInt64ToLengthPrefixedForm (UInt64ValInRDBLengthPrefixForm len) = do
    let prefixByte = 0b10000001 :: Word8
    genericPutWithChecksum @Word8 prefixByte
    genericPutWithChecksumUsing (putWord64be len)

{- | Decode a 64-bit length value (0-18446744073709551615)
Format: [10000001] [XXXXXXXX]...[XXXXXXXX] where X's represent the 64-bit length
-}
rdbDecodeUInt64FromLengthPrefixForm :: SGet UInt64ValInRDBLengthPrefixForm
rdbDecodeUInt64FromLengthPrefixForm = do
    prefixByte <- genericGetWithChecksum @Word8
    let expectedPrefix = 0b10000001 :: Word8
    if prefixByte /= expectedPrefix
        then fail "Unexpected prefix for 64-bit length. Expected 10000001"
        else UInt64ValInRDBLengthPrefixForm <$> genericGetWithChecksumUsing getWord64be
