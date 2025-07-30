module Redis.RDB.DataSpec where

import Redis.RDB.Data
import Test.Hspec
import Test.Tasty

import Data.ByteString.Lazy qualified as BSL
import Data.ByteString.Lazy.Char8 qualified as BSLC
import Hedgehog qualified as H
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range

import Data.Binary (Binary (..))
import Data.Binary.Get (runGet)
import Data.Binary.Put (runPut)
import Data.String (IsString (fromString))
import Hedgehog (MonadGen)
import Redis.Helper (encodeThenDecode)
import Test.Tasty.Hedgehog (testProperty)

genWord :: (MonadGen m) => m Word
genWord = Gen.word (Range.linearBounded @Word)

genByteSequence :: (MonadGen m) => m BSL.ByteString
genByteSequence = do
    intByteSeq <- fromString . show <$> Gen.int (Range.linear (-1000000) 10000000)
    arbitraryByteSeq <- Gen.bytes (Range.linear 0 100000)
    BSL.fromStrict <$> Gen.element [arbitraryByteSeq, intByteSeq]

genLargeByteSequence :: (MonadGen m) => m BSL.ByteString
genLargeByteSequence = do
    -- Generate a large byte sequence of length between 1 and 512 MB
    arbitraryByteSeq <- Gen.bytes (Range.linear 0 maxSize)
    return $ BSL.fromStrict arbitraryByteSeq

-- This is set to 512 MB
maxSize :: (Num a) => a
maxSize = 512 * 1024 * 1024

test_rdb_data_binary_serialization_prop_tests :: [TestTree]
test_rdb_data_binary_serialization_prop_tests =
    [ testProperty "unsigned int values length prefixed form encoding roundtrips" validateUnsignedIntVaLengthPrefixedEncoding
    , testProperty "RDB length prefixed value encoding roundtrips" validateRDBLengthPrefixedValEncoding
    -- TODO This property test causes the program to hang, figure out why
    -- , testProperty "RDB length prefixed value encoding for large byte sequences roundtrips" validateRDBLengthPrefixedValEncodingForLargeByteSequences
    ]

validateUnsignedIntVaLengthPrefixedEncoding :: H.Property
validateUnsignedIntVaLengthPrefixedEncoding = H.property $ do
    wordVal <- H.forAll genWord
    let uInt = toUIntValInRDBLengthPrefixedForm wordVal
    encodeThenDecode uInt H.=== uInt

validateRDBLengthPrefixedValEncoding :: H.Property
validateRDBLengthPrefixedValEncoding = H.property $ do
    byteString <- H.forAll genByteSequence
    let rdbString = toRDBLengthPrefixedVal byteString
    encodeThenDecode rdbString H.=== rdbString

validateRDBLengthPrefixedValEncodingForLargeByteSequences :: H.Property
validateRDBLengthPrefixedValEncodingForLargeByteSequences = H.property $ do
    byteString <- H.forAll genLargeByteSequence
    let rdbString = toRDBLengthPrefixedVal byteString
    encodeThenDecode rdbString H.=== rdbString

spec_RDB_data_binary_serialization_unit_tests :: Spec
spec_RDB_data_binary_serialization_unit_tests = do
    describe "Boundary Value Tests" $ do
        it "handles encoding of val at exact 6-bit length boundary (63 bytes)" $ do
            let str63 = BSLC.replicate 63 'a'
                shortString = RDBLengthPrefixedShortString str63
            encodeThenDecode shortString `shouldBe` shortString

        it "handles encoding of val just over 6-bit length boundary (64 bytes)" $ do
            let str64 = BSLC.replicate 64 'a'
                mediumString = RDBLengthPrefixedMediumString str64
            encodeThenDecode mediumString `shouldBe` mediumString

        it "handles encoding of val at maximum 14-bit length (16383 bytes)" $ do
            let str16383 = BSLC.replicate 16383 'b'
                mediumString = RDBLengthPrefixedMediumString str16383
            encodeThenDecode mediumString `shouldBe` mediumString

        it "handles encoding of val just over 14-bit length (16384 bytes)" $ do
            let str16384 = BSLC.replicate 16384 'c'
                longString = RDBLengthPrefixedLongString str16384
            encodeThenDecode longString `shouldBe` longString

        it "handles encoding of val that is max-size" $ do
            let strMax = BSLC.replicate maxSize 'a'
                val = toRDBLengthPrefixedVal strMax
            encodeThenDecode val `shouldBe` val

        it "handles encoding of minimum and maximum 8-bit integers" $ do
            let minInt8 = RDBLengthPrefixedInt8 (-128)
                maxInt8 = RDBLengthPrefixedInt8 127
                encodedMin = runPut (put minInt8)
                encodedMax = runPut (put maxInt8)
                decodedMin = runGet get encodedMin
                decodedMax = runGet get encodedMax
            decodedMin `shouldBe` minInt8
            decodedMax `shouldBe` maxInt8

        it "handles encoding of minimum and maximum 16-bit integers" $ do
            let minInt16 = RDBLengthPrefixedInt16 (-32768)
                maxInt16 = RDBLengthPrefixedInt16 32767
            encodeThenDecode minInt16 `shouldBe` minInt16
            encodeThenDecode maxInt16 `shouldBe` maxInt16

        it "handles encoding of minimum and maximum 32-bit integers" $ do
            let minInt32 = RDBLengthPrefixedInt32 (-2147483648)
                maxInt32 = RDBLengthPrefixedInt32 2147483647
            encodeThenDecode minInt32 `shouldBe` minInt32
            encodeThenDecode maxInt32 `shouldBe` maxInt32

    describe "Edge Cases and Corner Cases" $ do
        it "handles empty strings" $ do
            let emptyString = RDBLengthPrefixedShortString BSL.empty
            encodeThenDecode emptyString `shouldBe` emptyString

        it "handles zero values" $ do
            let zeroInt8 = RDBLengthPrefixedInt8 0
                zeroInt16 = RDBLengthPrefixedInt16 0
                zeroInt32 = RDBLengthPrefixedInt32 0
                zeroTimestampS = RDBUnixTimestampS 0
                zeroTimestampMS = RDBUnixTimestampMS 0
            encodeThenDecode zeroInt8 `shouldBe` zeroInt8
            encodeThenDecode zeroInt16 `shouldBe` zeroInt16
            encodeThenDecode zeroInt32 `shouldBe` zeroInt32
            encodeThenDecode zeroTimestampS `shouldBe` zeroTimestampS
            encodeThenDecode zeroTimestampMS `shouldBe` zeroTimestampMS

        it "handles Unicode and special characters in strings" $ do
            let unicodeStr = "héllo 世界 🚀 \n\t\r"
                unicodeString = toRDBLengthPrefixedStr unicodeStr
            encodeThenDecode unicodeString `shouldBe` unicodeString

        it "handles binary data in strings" $ do
            let binaryData = BSLC.pack ['\0', '\1', '\255', '\127']
                binaryString = RDBLengthPrefixedShortString binaryData
            encodeThenDecode binaryString `shouldBe` binaryString

        it "handles single character strings" $ do
            let singleChar = RDBLengthPrefixedShortString "x"
            encodeThenDecode singleChar `shouldBe` singleChar

        it "handles string-like numeric values that don't optimize to integers" $ do
            let floatStr = "123.45"
                sciStr = "1e10"
                hexStr = "0xFF"
                vals = map toRDBLengthPrefixedValOptimizedToIntEncodingIfPossible [floatStr, sciStr, hexStr]
            mapM_ (\val -> encodeThenDecode val `shouldBe` val) vals

        it "handles timestamp edge cases" $ do
            let maxSeconds = RDBUnixTimestampS 2147483647 -- Max 32-bit timestamp (2038)
                maxMillis = RDBUnixTimestampMS 9223372036854775807 -- Max 64-bit timestamp
                minTimestamp = RDBUnixTimestampS 1 -- Minimal valid timestamp
            encodeThenDecode maxSeconds `shouldBe` maxSeconds
            encodeThenDecode maxMillis `shouldBe` maxMillis
            encodeThenDecode minTimestamp `shouldBe` minTimestamp

        it "handles mixed type polymorphic value sequences" $ do
            let values =
                    [ toRDBLengthPrefixedVal "short"
                    , toRDBLengthPrefixedVal (BSLC.replicate 100 'a')
                    , toRDBLengthPrefixedVal (BSLC.replicate 1000 'b')
                    , toRDBLengthPrefixedValOptimizedToIntEncodingIfPossible "42"
                    , toRDBLengthPrefixedValOptimizedToIntEncodingIfPossible "-999"
                    ]
                testVal val = encodeThenDecode val `shouldBe` val
            mapM_ testVal values

        it "maintains precision for timestamp edge cases and leap seconds" $ do
            -- Test precision around known problematic timestamps
            let criticalTimestampsS =
                    [ RDBUnixTimestampS 0 -- Unix epoch
                    , RDBUnixTimestampS 946684800 -- Y2K
                    , RDBUnixTimestampS 1234567890 -- Common test timestamp
                    , RDBUnixTimestampS 1577836800 -- 2020-01-01 00:00:00 UTC
                    , RDBUnixTimestampS 1640995200 -- 2022-01-01 00:00:00 UTC
                    ]
                criticalTimestampsMS =
                    [ RDBUnixTimestampMS 0
                    , RDBUnixTimestampMS 1577836800000 -- 2020-01-01 00:00:00 UTC
                    , RDBUnixTimestampMS 1640995200000 -- 2022-01-01 00:00:00 UTC
                    ]
                testTimestamp ts = runGet get (runPut (put ts)) `shouldBe` ts
            mapM_ testTimestamp criticalTimestampsS
            mapM_ testTimestamp criticalTimestampsMS
