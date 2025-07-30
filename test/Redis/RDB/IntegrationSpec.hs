module Redis.RDB.IntegrationSpec where

import Redis.RDB.Data
import Redis.RDB.Format
import Test.Hspec

import Data.Binary qualified as Binary

import Control.Exception (SomeException, try)
import Data.Foldable (for_)
import Data.Time.Clock.POSIX (POSIXTime, getPOSIXTime)
import System.Directory (doesFileExist)
import System.FilePath ((</>))

createSampleRDBStructure :: POSIXTime -> RDBFile
createSampleRDBStructure currentTime =
    RDBFile
        { magicString = Redis
        , version = RDBVersion "0003"
        , auxFieldEntries =
            [ AuxFieldRedisVer $ RedisVersion $ RDBLengthPrefixedShortString "7.0.0"
            , AuxFieldRedisBits RedisBits64
            , AuxFieldCustom (toRDBLengthPrefixedStr "custom-key") (toRDBLengthPrefixedVal "custom-value")
            , AuxFieldCTime $ CTime (fromPosixTimeToRDBUnixTimestampS currentTime)
            , AuxFieldCustom (toRDBLengthPrefixedStr "another-key") (toRDBLengthPrefixedVal "another-value")
            , AuxFieldUsedMem $ UsedMem 1048576 -- 1MB
            ]
        , dbEntries =
            [ RDbEntry
                { entryId = SelectDB 0 -- Database 0
                , resizeDBEntry = ResizeDB 4 3 -- 4 keys total, 3 with expiry
                , keyValEntries =
                    [ FCOpCode $
                        KeyValWithExpiryInMS
                            { expiryTimeMs = fromPosixTimeToRDBUnixTimestampMS $ currentTime + 300 -- Expires in 5 minutes
                            , valueType = Str
                            , encodedKey = toRDBLengthPrefixedVal "key1"
                            , encodedValue = toRDBLengthPrefixedVal "Hello World"
                            }
                    , FDOpcode $
                        KeyValWithExpiryInS
                            { expiryTimeS = fromPosixTimeToRDBUnixTimestampS $ currentTime + 3600 -- Expires in 1 hour
                            , valueType = Str
                            , encodedKey = toRDBLengthPrefixedVal "counter"
                            , encodedValue = toRDBLengthPrefixedVal "42"
                            }
                    , FCOpCode $
                        KeyValWithExpiryInMS
                            { expiryTimeMs = fromPosixTimeToRDBUnixTimestampMS $ currentTime + 1800 -- Expires in 30 minutes
                            , valueType = Str
                            , encodedKey = toRDBLengthPrefixedVal "123"
                            , encodedValue = toRDBLengthPrefixedVal "John Doe"
                            }
                    , KeyValOpCode $
                        KeyValWithNoExpiryInfo
                            { valueType = Str
                            , encodedKey = toRDBLengthPrefixedVal "-439284723"
                            , encodedValue = toRDBLengthPrefixedVal "Negative 32 bit integer"
                            }
                    ]
                }
            ]
        , checksum = Nothing -- No checksum for this example
        }

emptyRDBFile :: RDBFile
emptyRDBFile =
    RDBFile
        { magicString = Redis
        , version = RDBVersion "0009"
        , auxFieldEntries = []
        , dbEntries = []
        , checksum = Nothing
        }

rdbFileWithOnlyAuxFields :: RDBFile
rdbFileWithOnlyAuxFields =
    RDBFile
        { magicString = Redis
        , version = RDBVersion "0009"
        , auxFieldEntries =
            [ AuxFieldRedisVer $ RedisVersion $ RDBLengthPrefixedShortString "7.2.0"
            , AuxFieldRedisBits RedisBits64
            , AuxFieldUsedMem $ UsedMem 2048
            ]
        , dbEntries = []
        , checksum = Nothing
        }

multiDatabaseRDBStructure :: RDBFile
multiDatabaseRDBStructure =
    RDBFile
        { magicString = Redis
        , version = RDBVersion "0009"
        , auxFieldEntries = []
        , dbEntries =
            [ RDbEntry
                { entryId = SelectDB 0
                , resizeDBEntry = ResizeDB 1 0
                , keyValEntries =
                    [ KeyValOpCode $
                        KeyValWithNoExpiryInfo
                            { valueType = Str
                            , encodedKey = toRDBLengthPrefixedVal "db0_key"
                            , encodedValue = toRDBLengthPrefixedVal "db0_value"
                            }
                    ]
                }
            , RDbEntry
                { entryId = SelectDB 1
                , resizeDBEntry = ResizeDB 1 0
                , keyValEntries =
                    [ KeyValOpCode $
                        KeyValWithNoExpiryInfo
                            { valueType = Str
                            , encodedKey = toRDBLengthPrefixedVal "db1_key"
                            , encodedValue = toRDBLengthPrefixedVal "db1_value"
                            }
                    ]
                }
            , RDbEntry
                { entryId = SelectDB 2
                , resizeDBEntry = ResizeDB 1 0
                , keyValEntries =
                    [ KeyValOpCode $
                        KeyValWithNoExpiryInfo
                            { valueType = Str
                            , encodedKey = toRDBLengthPrefixedVal "db2_key"
                            , encodedValue = toRDBLengthPrefixedVal "db2_value"
                            }
                    ]
                }
            ]
        , checksum = Nothing
        }

-- | List of example RDB files to test
exampleRDBFiles :: [String]
exampleRDBFiles =
    [ "empty_database.rdb"
    , "expiration.rdb"
    , "integer_keys.rdb"
    , "keys_with_expiry.rdb"
    , "multiple_databases.rdb"
    , "non_ascii_values.rdb"
    , "rdb_version_5_with_checksum.rdb"
    -- , "easily_compressible_string_key.rdb" -- Commented because it requires lzf compression which we do not yet support
    ]

encodeThenDecodeRDBStructure :: RDBFile -> RDBFile
encodeThenDecodeRDBStructure rdbFile =
    let encoded = Binary.encode rdbFile
        decoded = Binary.decode encoded
     in decoded

spec_RDB_integration_tests :: Spec
spec_RDB_integration_tests = do
    describe "RDB File Integration Tests" $ do
        describe "RDB Creation and Roundtrip Tests" $ do
            it "Encodes/decodes a sample RDB file successfully" $ do
                currentTime <- getPOSIXTime
                let rdbStructure = createSampleRDBStructure currentTime
                encodeThenDecodeRDBStructure rdbStructure `shouldBe` rdbStructure

            it "Encodes/decodes an empty RDB file successfully" $ do
                encodeThenDecodeRDBStructure emptyRDBFile `shouldBe` emptyRDBFile

            it "Encodes/decodes an auxiliary-only RDB file successfully" $ do
                encodeThenDecodeRDBStructure rdbFileWithOnlyAuxFields `shouldBe` rdbFileWithOnlyAuxFields

            it "handles multiple databases correctly" $ do
                encodeThenDecodeRDBStructure multiDatabaseRDBStructure `shouldBe` multiDatabaseRDBStructure

            it "encodes large RDB files efficiently" $ do
                currentTime <- getPOSIXTime
                let largeRDB = createLargeRDB currentTime
                encodeThenDecodeRDBStructure largeRDB `shouldBe` largeRDB

        describe "Decoding sample RDB Files" $ do
            for_ exampleRDBFiles $ \filename -> do
                it ("successfully decodes " <> filename) $ do
                    let filepath = "test/input/example-rdb-files" </> filename
                    fileExists <- doesFileExist filepath
                    if fileExists
                        then do
                            result <- try (Binary.decodeFile filepath) :: IO (Either SomeException RDBFile)
                            case result of
                                Left err -> expectationFailure $ "Failed to decode " <> filename <> ": " <> show err
                                Right decodedRDB -> do
                                    decodedRDB.magicString `shouldBe` Redis
                                    case decodedRDB.version of
                                        RDBVersion _ -> pure ()
                        else expectationFailure $ "File not found: " <> filepath

-- | Create a large RDB for performance testing
createLargeRDB :: POSIXTime -> RDBFile
createLargeRDB currentTime =
    RDBFile
        { magicString = Redis
        , version = RDBVersion "0009"
        , auxFieldEntries =
            [ AuxFieldRedisVer $ RedisVersion $ RDBLengthPrefixedShortString "7.0.0"
            , AuxFieldRedisBits RedisBits64
            ]
        , dbEntries =
            [ RDbEntry
                { entryId = SelectDB 0
                , resizeDBEntry = ResizeDB 50 25 -- 50 keys, 25 with expiry
                , keyValEntries = generateLargeKeyValList currentTime 50
                }
            ]
        , checksum = Nothing
        }

-- | Generate a large list of key-value entries
generateLargeKeyValList :: POSIXTime -> Int -> [KeyValueOpCode]
generateLargeKeyValList currentTime count =
    take count $
        cycle
            [ KeyValOpCode $
                KeyValWithNoExpiryInfo
                    { valueType = Str
                    , encodedKey = toRDBLengthPrefixedVal "persistent_key"
                    , encodedValue = toRDBLengthPrefixedVal "persistent_value"
                    }
            , FCOpCode $
                KeyValWithExpiryInMS
                    { expiryTimeMs = fromPosixTimeToRDBUnixTimestampMS $ currentTime + 3600
                    , valueType = Str
                    , encodedKey = toRDBLengthPrefixedVal "expiring_key_ms"
                    , encodedValue = toRDBLengthPrefixedVal "expiring_value_ms"
                    }
            , FDOpcode $
                KeyValWithExpiryInS
                    { expiryTimeS = fromPosixTimeToRDBUnixTimestampS $ currentTime + 7200
                    , valueType = Str
                    , encodedKey = toRDBLengthPrefixedVal "expiring_key_s"
                    , encodedValue = toRDBLengthPrefixedVal "expiring_value_s"
                    }
            ]
