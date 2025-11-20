module Redis.RDB.DataSpec where

import Redis.RDB.Data
import Test.Hspec
import Test.Hspec.Hedgehog
import Test.Tasty

import Hedgehog qualified as H
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Data.ByteString qualified as BSC

import Data.Int (Int16, Int32, Int64, Int8)
import Data.Word (Word32, Word64)
import Redis.RDB.Binary (decodeOrFail, encode)
import Redis.RDB.TestConfig (genRDBConfig,)
import Test.Tasty.Hedgehog (testProperty)

test_rdb_data_binary_serialization_prop_tests :: [TestTree]
test_rdb_data_binary_serialization_prop_tests =
    [ testProperty "Length prefix encoding roundtrips" roundTripRDBLengthPrefixEncoding
    , testProperty "RDB string encoding roundtrips" roundTripRDBStringEncoding
    , testProperty "RDB int8 encoding roundtrips" roundTripRDBInt8Encoding
    , testProperty "RDB int16 encoding roundtrips" roundTripRDBInt16Encoding
    , testProperty "RDB int32 encoding roundtrips" roundTripRDBInt32Encoding
    , testProperty "RDB unix timestamp in seconds encoding roundtrips" roundTripRDBUnixTimestampSEncoding
    , testProperty "RDB unix timestamp in milliseconds encoding roundtrips" roundTripRDBUnixTimestampMSEncoding
    , testProperty "RDB float encoding roundtrips" roundTripRDBFloatEncoding
    , testProperty "RDB double encoding roundtrips" roundTripRDBDoubleEncoding
    ]

roundTripRDBLengthPrefixEncoding :: H.Property
roundTripRDBLengthPrefixEncoding = H.property $ do
    lengthPrefixVal <- toRDBLengthPrefix <$> H.forAll (Gen.word Range.linearBounded)
    rdbConfig <- H.forAll genRDBConfig
    H.tripping lengthPrefixVal (encode rdbConfig) (decodeOrFail rdbConfig)

-- Trial and error puts 30 tests as the sweet spot
roundTripRDBStringEncoding :: H.Property
roundTripRDBStringEncoding = withTests 20 $ H.property $ do
    rdbString <- H.forAll (Gen.bytes (Range.exponential 0 (fromIntegral (maxBound @Int64))))
    rdbConfig <- H.forAll genRDBConfig
    H.tripping (toRDBString rdbString) (encode rdbConfig) (decodeOrFail rdbConfig)

roundTripRDBInt8Encoding :: H.Property
roundTripRDBInt8Encoding = H.property $ do
    rdbInt <- RDBInt8 <$> H.forAll (Gen.int8 (Range.linearBounded @Int8))
    rdbConfig <- H.forAll genRDBConfig
    H.tripping rdbInt (encode rdbConfig) (decodeOrFail rdbConfig)

roundTripRDBInt16Encoding :: H.Property
roundTripRDBInt16Encoding = H.property $ do
    rdbInt <- RDBInt16 <$> H.forAll (Gen.int16 (Range.linearBounded @Int16))
    rdbConfig <- H.forAll genRDBConfig
    H.tripping rdbInt (encode rdbConfig) (decodeOrFail rdbConfig)

roundTripRDBInt32Encoding :: H.Property
roundTripRDBInt32Encoding = H.property $ do
    rdbInt <- RDBInt32 <$> H.forAll (Gen.int32 (Range.linearBounded @Int32))
    rdbConfig <- H.forAll genRDBConfig
    H.tripping rdbInt (encode rdbConfig) (decodeOrFail rdbConfig)

roundTripRDBUnixTimestampSEncoding :: H.Property
roundTripRDBUnixTimestampSEncoding = H.property $ do
    timestampS <- RDBUnixTimestampS <$> H.forAll (Gen.word32 (Range.linearBounded @Word32))
    rdbConfig <- H.forAll genRDBConfig
    H.tripping timestampS (encode rdbConfig) (decodeOrFail rdbConfig)

roundTripRDBUnixTimestampMSEncoding :: H.Property
roundTripRDBUnixTimestampMSEncoding = H.property $ do
    timestampMS <- RDBUnixTimestampMS <$> H.forAll (Gen.word64 (Range.linearBounded @Word64))
    rdbConfig <- H.forAll genRDBConfig
    H.tripping timestampMS (encode rdbConfig) (decodeOrFail rdbConfig)

roundTripRDBFloatEncoding :: H.Property
roundTripRDBFloatEncoding = H.property $ do
    rdbFloat <- RDBFloat <$> H.forAll (Gen.float (Range.linearFrac (-1.0e10) 1.0e10))
    rdbConfig <- H.forAll genRDBConfig
    H.tripping rdbFloat (encode rdbConfig) (decodeOrFail rdbConfig)

roundTripRDBDoubleEncoding :: H.Property
roundTripRDBDoubleEncoding = H.property $ do
    rdbDouble <- RDBDouble <$> H.forAll (Gen.double (Range.linearFrac (-1.0e10) 1.0e10))
    rdbConfig <- H.forAll genRDBConfig
    H.tripping rdbDouble (encode rdbConfig) (decodeOrFail rdbConfig)

spec_RDB_data_binary_serialization_unit_tests :: Spec
spec_RDB_data_binary_serialization_unit_tests = do
    modifyMaxSuccess (const 10) $ it "Test on 512mb of data" $ hedgehog $ do
        let sampleString = toRDBString $ BSC.replicate 67108864 97
        rdbConfig <- H.forAll genRDBConfig
        H.tripping sampleString (encode rdbConfig) (decodeOrFail rdbConfig)

    describe "Edge Cases and Corner Cases" $ do
        it "handles Unicode and special characters in strings" $ hedgehog $ do
            let unicodeStr = "héllo 世界 🚀 \n\t\r"
                unicodeString = toRDBString unicodeStr
            rdbConfig <- H.forAll genRDBConfig
            H.tripping unicodeString (encode rdbConfig) (decodeOrFail rdbConfig)

        -- Not sure if this test is necessary, but meh, I'll allow it
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
                testTimestampS ts = H.tripping ts (encode rdbConfig) (decodeOrFail rdbConfig)
                testTimestampMS ts = H.tripping ts (encode rdbConfig) (decodeOrFail rdbConfig)

            mapM_ testTimestampS criticalTimestampsS
            mapM_ testTimestampMS criticalTimestampsMS
