module Redis.RDB.FormatSpec where

-- import Redis.RDB.Data
-- import Redis.RDB.Format
-- import Redis.RDB.TestConfig (genRDBConfig)
-- import Test.Hspec
-- import Test.Hspec.Hedgehog

-- import Data.ByteString.Char8 qualified as BSC
-- import Hedgehog qualified as H
-- import Hedgehog.Gen qualified as Gen
-- import Refined.Unsafe (reallyUnsafeRefine)

-- import Redis.Helper (encodeThenDecodeRDBBinary)

-- -- NOTE For why we use reallyUnsafeRefine here, see note in Redis/RDB/Format.hs

-- spec_RDB_format_binary_serialization :: Spec
-- spec_RDB_format_binary_serialization = do
--     describe "Roundtrip serialization of RDB structure components" $ do
--         it "roundtrip encodes and decodes magic string correctly" $ hedgehog $ do
--             let magicString = Redis
--             rdbConfig <- H.forAll genRDBConfig
--             encodeThenDecodeRDBBinary rdbConfig magicString H.=== magicString

--         it "roundtrip encodes and decodes version strings correctly" $ hedgehog $ do
--             version <- H.forAll $ Gen.enumBounded @_ @RDBVersion
--             rdbConfig <- H.forAll genRDBConfig
--             encodeThenDecodeRDBBinary rdbConfig version H.=== version

--         it "roundtrip encodes and decodes EOF marker correctly" $ hedgehog $ do
--             let eofMarker = EOF
--             rdbConfig <- H.forAll genRDBConfig
--             encodeThenDecodeRDBBinary rdbConfig eofMarker H.=== eofMarker

--         it "roundtrip encodes and decodes expiry in milliseconds correctly" $ hedgehog $ do
--             let timestampMS = RDBUnixTimestampMS 1640995200000
--                 valueType = Str
--                 key = toRDBStringOrIntVal "testkey"
--                 value = toRDBStringOrIntVal "testvalue"
--                 keyVal = KeyValWithExpiryInMS timestampMS valueType key value
--                 opCode = FCOpCode keyVal
--             rdbConfig <- H.forAll genRDBConfig
--             encodeThenDecodeRDBBinary rdbConfig opCode H.=== opCode

--         it "roundtrip encodes and decodes expiry in seconds correctly" $ hedgehog $ do
--             let timestampS = RDBUnixTimestampS 1640995200
--                 valueType = Str
--                 key = toRDBStringOrIntVal "testkey"
--                 value = toRDBStringOrIntVal "testvalue"
--                 keyVal = KeyValWithExpiryInS timestampS valueType key value
--                 opCode = FDOpcode keyVal
--             rdbConfig <- H.forAll genRDBConfig
--             encodeThenDecodeRDBBinary rdbConfig opCode H.=== opCode

--         it "roundtrip encodes and decodes no expiry correctly" $ hedgehog $ do
--             let valueType = Str
--                 key = toRDBStringOrIntVal "testkey"
--                 value = toRDBStringOrIntVal "testvalue"
--                 keyVal = KeyValWithNoExpiryInfo valueType key value
--                 opCode = KeyValOpCode keyVal
--             rdbConfig <- H.forAll genRDBConfig
--             encodeThenDecodeRDBBinary rdbConfig opCode H.=== opCode

--         it "roundtrip encodes and decodes Redis version auxiliary field correctly" $ hedgehog $ do
--             let redisVer = RedisVersion (reallyUnsafeRefine "7.0.0")
--                 auxField = AuxFieldRedisVer redisVer
--             rdbConfig <- H.forAll genRDBConfig
--             encodeThenDecodeRDBBinary rdbConfig auxField H.=== auxField

--         it "roundtrip encodes and decodes Redis bits auxiliary field correctly" $ hedgehog $ do
--             let redisBits = RedisBits64
--                 auxField = AuxFieldRedisBits redisBits
--             rdbConfig <- H.forAll genRDBConfig
--             encodeThenDecodeRDBBinary rdbConfig auxField H.=== auxField

--         it "roundtrip encodes and decodes creation time auxiliary field correctly" $ hedgehog $ do
--             let ctime = CTime (RDBUnixTimestampS 1640995200)
--                 auxField = AuxFieldCTime ctime
--             rdbConfig <- H.forAll genRDBConfig
--             encodeThenDecodeRDBBinary rdbConfig auxField H.=== auxField

--         it "roundtrip encodes and decodes used memory auxiliary field correctly" $ hedgehog $ do
--             let usedMem = UsedMem 1073741824 -- 1GB
--                 auxField = AuxFieldUsedMem usedMem
--             rdbConfig <- H.forAll genRDBConfig
--             encodeThenDecodeRDBBinary rdbConfig auxField H.=== auxField

--         it "roundtrip encodes and decodes database selection correctly" $ hedgehog $ do
--             let dbSelect = SelectDB 20
--             rdbConfig <- H.forAll genRDBConfig
--             encodeThenDecodeRDBBinary rdbConfig dbSelect H.=== dbSelect

--         it "roundtrip encodes and decodes string value type correctly" $ hedgehog $ do
--             let valueType = Str
--             rdbConfig <- H.forAll genRDBConfig
--             encodeThenDecodeRDBBinary rdbConfig valueType H.=== valueType

--         describe "Boundary Value and Edge Case Tests" $ do
--             it "handles zero-length key-value pairs" $ hedgehog $ do
--                 let emptyKey = toRDBStringOrIntVal BSC.empty
--                     emptyValue = toRDBStringOrIntVal BSC.empty
--                     keyVal = KeyValWithNoExpiryInfo Str emptyKey emptyValue
--                     opCode = KeyValOpCode keyVal
--                 rdbConfig <- H.forAll genRDBConfig
--                 encodeThenDecodeRDBBinary rdbConfig opCode H.=== opCode

--             it "handles very long keys and values" $ hedgehog $ do
--                 let longKey = toRDBStringOrIntVal (BSC.pack (replicate 1000 'k'))
--                     longValue = toRDBStringOrIntVal (BSC.pack (replicate 10000 'v'))
--                     keyVal = KeyValWithNoExpiryInfo Str longKey longValue
--                     opCode = KeyValOpCode keyVal
--                 rdbConfig <- H.forAll genRDBConfig
--                 encodeThenDecodeRDBBinary rdbConfig opCode H.=== opCode

--             it "handles edge case timestamps" $ hedgehog $ do
--                 let epochStart = RDBUnixTimestampS 0
--                     year2038 = RDBUnixTimestampS 2147483647
--                     epochStartMS = RDBUnixTimestampMS 0
--                     farFutureMS = RDBUnixTimestampMS 9223372036854775807

--                     keyVal1 =
--                         KeyValWithExpiryInS
--                             epochStart
--                             Str
--                             (toRDBStringOrIntVal "epoch")
--                             (toRDBStringOrIntVal "start")
--                     keyVal2 =
--                         KeyValWithExpiryInS
--                             year2038
--                             Str
--                             (toRDBStringOrIntVal "y2038")
--                             (toRDBStringOrIntVal "problem")
--                     keyVal3 =
--                         KeyValWithExpiryInMS
--                             epochStartMS
--                             Str
--                             (toRDBStringOrIntVal "epoch_ms")
--                             (toRDBStringOrIntVal "start")
--                     keyVal4 =
--                         KeyValWithExpiryInMS
--                             farFutureMS
--                             Str
--                             (toRDBStringOrIntVal "far_future")
--                             (toRDBStringOrIntVal "ms")

--                     opCodes = [FDOpcode keyVal1, FDOpcode keyVal2, FCOpCode keyVal3, FCOpCode keyVal4]
--                 rdbConfig <- H.forAll genRDBConfig
--                 mapM_ (\op -> encodeThenDecodeRDBBinary rdbConfig op H.=== op) opCodes
