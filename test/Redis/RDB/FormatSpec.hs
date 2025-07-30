module Redis.RDB.FormatSpec where

import Redis.RDB.Data
import Redis.RDB.Format
import Test.Hspec

import Data.ByteString.Lazy.Char8 qualified as BSLC
import Redis.Helper (encodeThenDecode)

spec_RDB_format_binary_serialization :: Spec
spec_RDB_format_binary_serialization = do
    describe "Roundtrip serialization of RDB structuure components" $ do
        it "roundtrip encodes and decodes magic string correctly" $ do
            let magicString = Redis
            encodeThenDecode magicString `shouldBe` magicString

        it "roundtrip encodes and decodes version strings correctly" $ do
            let version = RDBVersion "0009"
            encodeThenDecode version `shouldBe` version

        it "roundtrip encodes and decodes EOF marker correctly" $ do
            encodeThenDecode EOF `shouldBe` EOF

        it "roundtrip encodes and decodes expiry in milliseconds correctly" $ do
            let timestampMS = RDBUnixTimestampMS 1640995200000
                valueType = Str
                key = toRDBLengthPrefixedVal "testkey"
                value = toRDBLengthPrefixedVal "testvalue"
                keyVal = KeyValWithExpiryInMS timestampMS valueType key value
                opCode = FCOpCode keyVal
            encodeThenDecode opCode `shouldBe` opCode

        it "roundtrip encodes and decodes expiry in seconds correctly" $ do
            let timestampS = RDBUnixTimestampS 1640995200
                valueType = Str
                key = toRDBLengthPrefixedVal "testkey"
                value = toRDBLengthPrefixedVal "testvalue"
                keyVal = KeyValWithExpiryInS timestampS valueType key value
                opCode = FDOpcode keyVal
            encodeThenDecode opCode `shouldBe` opCode

        it "roundtrip encodes and decodes no expiry correctly" $ do
            let valueType = Str
                key = toRDBLengthPrefixedVal "testkey"
                value = toRDBLengthPrefixedVal "testvalue"
                keyVal = KeyValWithNoExpiryInfo valueType key value
                opCode = KeyValOpCode keyVal
            encodeThenDecode opCode `shouldBe` opCode

        it "roundtrip encodes and decodes Redis version auxiliary field correctly" $ do
            let redisVer = RedisVersion (RDBLengthPrefixedShortString (BSLC.pack "7.0.0"))
                auxField = AuxFieldRedisVer redisVer
            encodeThenDecode auxField `shouldBe` auxField

        it "roundtrip encodes and decodes Redis bits auxiliary field correctly" $ do
            let redisBits = RedisBits64
                auxField = AuxFieldRedisBits redisBits
            encodeThenDecode auxField `shouldBe` auxField

        it "roundtrip encodes and decodes creation time auxiliary field correctly" $ do
            let ctime = CTime (RDBUnixTimestampS 1640995200)
                auxField = AuxFieldCTime ctime
            encodeThenDecode auxField `shouldBe` auxField

        it "roundtrip encodes and decodes used memory auxiliary field correctly" $ do
            let usedMem = UsedMem 1073741824 -- 1GB
                auxField = AuxFieldUsedMem usedMem
            encodeThenDecode auxField `shouldBe` auxField

        it "roundtrip encodes and decodes database selection correctly" $ do
            let dbSelect = SelectDB 20
            encodeThenDecode dbSelect `shouldBe` dbSelect

        it "roundtrip encodes and decodes string value type correctly" $ do
            let valueType = Str
            encodeThenDecode valueType `shouldBe` valueType

        describe "Boundary Value and Edge Case Tests" $ do
            it "handles zero-length key-value pairs" $ do
                let emptyKey = toRDBLengthPrefixedVal BSLC.empty
                    emptyValue = toRDBLengthPrefixedVal BSLC.empty
                    keyVal = KeyValWithNoExpiryInfo Str emptyKey emptyValue
                    opCode = KeyValOpCode keyVal
                encodeThenDecode opCode `shouldBe` opCode

            it "handles very long keys and values" $ do
                let longKey = toRDBLengthPrefixedVal (BSLC.pack (replicate 1000 'k'))
                    longValue = toRDBLengthPrefixedVal (BSLC.pack (replicate 10000 'v'))
                    keyVal = KeyValWithNoExpiryInfo Str longKey longValue
                    opCode = KeyValOpCode keyVal
                encodeThenDecode opCode `shouldBe` opCode

            it "handles edge case timestamps" $ do
                let epochStart = RDBUnixTimestampS 0
                    year2038 = RDBUnixTimestampS 2147483647
                    epochStartMS = RDBUnixTimestampMS 0
                    farFutureMS = RDBUnixTimestampMS 9223372036854775807

                    keyVal1 =
                        KeyValWithExpiryInS
                            epochStart
                            Str
                            (toRDBLengthPrefixedVal "epoch")
                            (toRDBLengthPrefixedVal "start")
                    keyVal2 =
                        KeyValWithExpiryInS
                            year2038
                            Str
                            (toRDBLengthPrefixedVal "y2038")
                            (toRDBLengthPrefixedVal "problem")
                    keyVal3 =
                        KeyValWithExpiryInMS
                            epochStartMS
                            Str
                            (toRDBLengthPrefixedVal "epoch_ms")
                            (toRDBLengthPrefixedVal "start")
                    keyVal4 =
                        KeyValWithExpiryInMS
                            farFutureMS
                            Str
                            (toRDBLengthPrefixedVal "far_future")
                            (toRDBLengthPrefixedVal "ms")

                    opCodes = [FDOpcode keyVal1, FDOpcode keyVal2, FCOpCode keyVal3, FCOpCode keyVal4]
                mapM_ (\op -> encodeThenDecode op `shouldBe` op) opCodes
