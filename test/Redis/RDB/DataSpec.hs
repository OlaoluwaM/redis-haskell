module Redis.RDB.DataSpec where

import Redis.RDB.Data
import Test.Hspec
import Test.Hspec.Hedgehog
import Test.Tasty

import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as BSC
import Hedgehog qualified as H
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range

import Data.String (IsString (fromString))
import Redis.Helper (encodeThenDecodeRDBBinary)
import Redis.RDB.TestConfig (genRDBConfig)
import Test.Tasty.Hedgehog (testProperty)

genWord :: (MonadGen m) => m Word
genWord = Gen.word (Range.linearBounded @Word)

genByteSequence :: (MonadGen m) => m BS.ByteString
genByteSequence = do
    intByteSeq <- fromString . show <$> Gen.int (Range.linear (-1000000) 10000000)
    arbitraryByteSeq <- Gen.bytes (Range.linear 0 100000)
    Gen.element [arbitraryByteSeq, intByteSeq]

test_rdb_data_binary_serialization_prop_tests :: [TestTree]
test_rdb_data_binary_serialization_prop_tests =
    [ testProperty "unsigned int values length prefixed form encoding roundtrips" validateUnsignedIntVaLengthPrefixedEncoding
    , testProperty "RDB length prefixed value encoding roundtrips" validateRDBValEncoding
    ]

validateUnsignedIntVaLengthPrefixedEncoding :: H.Property
validateUnsignedIntVaLengthPrefixedEncoding = H.property $ do
    wordVal <- H.forAll genWord
    rdbConfig <- H.forAll genRDBConfig
    let lengthPrefixVal = toRDBLengthPrefix wordVal
    encodeThenDecodeRDBBinary rdbConfig lengthPrefixVal H.=== lengthPrefixVal

validateRDBValEncoding :: H.Property
validateRDBValEncoding = H.property $ do
    byteString <- H.forAll genByteSequence
    let rdbVal = toRDBVal byteString
    rdbConfig <- H.forAll genRDBConfig
    encodeThenDecodeRDBBinary rdbConfig rdbVal H.=== rdbVal

spec_RDB_data_binary_serialization_unit_tests :: Spec
spec_RDB_data_binary_serialization_unit_tests = do
    describe "Boundary Value Tests" $ do
        it "handles encoding of val at exact 6-bit length boundary (63 bytes)" $ hedgehog $ do
            let str63 = BSC.replicate 63 'a'
                shortString = RDBShortString str63
            rdbConfig <- H.forAll genRDBConfig
            -- As an example of how'd we'd go about using hspec assertions with Hedgehog
            evalIO $ encodeThenDecodeRDBBinary rdbConfig shortString `shouldBe` shortString

        it "handles encoding of val just over 6-bit length boundary (64 bytes)" $ hedgehog $ do
            let str64 = BSC.replicate 64 'a'
                mediumString = RDBMediumString str64
            rdbConfig <- H.forAll genRDBConfig
            encodeThenDecodeRDBBinary rdbConfig mediumString H.=== mediumString

        it "handles encoding of val at maximum 14-bit length (16383 bytes)" $ hedgehog $ do
            let str16383 = BSC.replicate 16383 'b'
                mediumString = RDBMediumString str16383
            rdbConfig <- H.forAll genRDBConfig
            encodeThenDecodeRDBBinary rdbConfig mediumString H.=== mediumString

        it "handles encoding of val just over 14-bit length (16384 bytes)" $ hedgehog $ do
            let str16384 = BSC.replicate 16384 'c'
                longString = RDBLongString str16384
            rdbConfig <- H.forAll genRDBConfig
            encodeThenDecodeRDBBinary rdbConfig longString H.=== longString

        -- This test is resource-intensive; limit to a few runs
        modifyMaxSuccess (const 5) $ it "handles encoding of val that is max-size" $ hedgehog $ do
            -- This is set to 512 MB
            let maxSize = 512 * 1024 * 1024
            let strMax = BSC.replicate maxSize 'a'
                val = toRDBString strMax
            rdbConfig <- H.forAll genRDBConfig
            encodeThenDecodeRDBBinary rdbConfig val H.=== val

    describe "Edge Cases and Corner Cases" $ do
        it "handles empty strings" $ hedgehog $ do
            let emptyString = RDBShortString BS.empty
            rdbConfig <- H.forAll genRDBConfig
            encodeThenDecodeRDBBinary rdbConfig emptyString H.=== emptyString

        it "handles zero values" $ hedgehog $ do
            let zeroInt8 = RDBInt8 0
                zeroInt16 = RDBInt16 0
                zeroInt32 = RDBInt32 0
                zeroTimestampS = RDBUnixTimestampS 0
                zeroTimestampMS = RDBUnixTimestampMS 0
            rdbConfig <- H.forAll genRDBConfig
            encodeThenDecodeRDBBinary rdbConfig zeroInt8 H.=== zeroInt8
            encodeThenDecodeRDBBinary rdbConfig zeroInt16 H.=== zeroInt16
            encodeThenDecodeRDBBinary rdbConfig zeroInt32 H.=== zeroInt32
            encodeThenDecodeRDBBinary rdbConfig zeroTimestampS H.=== zeroTimestampS
            encodeThenDecodeRDBBinary rdbConfig zeroTimestampMS H.=== zeroTimestampMS

        it "handles Unicode and special characters in strings" $ hedgehog $ do
            let unicodeStr = "héllo 世界 🚀 \n\t\r"
                unicodeString = toRDBString unicodeStr
            rdbConfig <- H.forAll genRDBConfig
            encodeThenDecodeRDBBinary rdbConfig unicodeString H.=== unicodeString

        it "handles binary data in strings" $ hedgehog $ do
            let binaryData = BSC.pack ['\0', '\1', '\255', '\127']
                binaryString = RDBShortString binaryData
            rdbConfig <- H.forAll genRDBConfig
            encodeThenDecodeRDBBinary rdbConfig binaryString H.=== binaryString

        it "handles single character strings" $ hedgehog $ do
            let singleChar = RDBShortString "x"
            rdbConfig <- H.forAll genRDBConfig
            encodeThenDecodeRDBBinary rdbConfig singleChar H.=== singleChar

        it "handles string-like numeric values that don't optimize to integers" $ hedgehog $ do
            let floatStr = "123.45"
                sciStr = "1e10"
                hexStr = "0xFF"
                vals = map toRDBVal [floatStr, sciStr, hexStr]
            rdbConfig <- H.forAll genRDBConfig
            mapM_ (\val -> encodeThenDecodeRDBBinary rdbConfig val H.=== val) vals

        it "handles timestamp edge cases" $ hedgehog $ do
            let maxSeconds = RDBUnixTimestampS 2147483647 -- Max 32-bit timestamp (2038)
                maxMillis = RDBUnixTimestampMS 9223372036854775807 -- Max 64-bit timestamp
                minTimestamp = RDBUnixTimestampS 1 -- Minimal valid timestamp
            rdbConfig <- H.forAll genRDBConfig
            encodeThenDecodeRDBBinary rdbConfig maxSeconds H.=== maxSeconds
            encodeThenDecodeRDBBinary rdbConfig maxMillis H.=== maxMillis
            encodeThenDecodeRDBBinary rdbConfig minTimestamp H.=== minTimestamp

        it "handles mixed type polymorphic value sequences" $ hedgehog $ do
            rdbConfig <- H.forAll genRDBConfig
            let values =
                    [ toRDBVal "short"
                    , toRDBVal (BSC.replicate 100 'a')
                    , toRDBVal (BSC.replicate 1000 'b')
                    , toRDBVal "42"
                    , toRDBVal "-999"
                    ]
                testVal val = encodeThenDecodeRDBBinary rdbConfig val H.=== val
            mapM_ testVal values

        it "maintains precision for timestamp edge cases and leap seconds" $ hedgehog $ do
            -- Test precision around known problematic timestamps
            rdbConfig <- H.forAll genRDBConfig
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
                testTimestampS ts = encodeThenDecodeRDBBinary rdbConfig ts H.=== ts
                testTimestampMS ts = encodeThenDecodeRDBBinary rdbConfig ts H.=== ts
            mapM_ testTimestampS criticalTimestampsS
            mapM_ testTimestampMS criticalTimestampsMS
