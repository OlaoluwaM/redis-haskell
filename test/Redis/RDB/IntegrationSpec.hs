module Redis.RDB.IntegrationSpec where

import Redis.RDB.Data
import Redis.RDB.Format
import Test.Hspec
import Test.Hspec.Hedgehog

import Hedgehog qualified as H
import Redis.RDB.Binary qualified as Binary

import Control.Exception (SomeException, try)
import Data.Foldable (for_)
import Data.Time.Clock.POSIX (POSIXTime, getPOSIXTime)
import Redis.Helper (encodeThenDecodeRDBBinary)
import Redis.RDB.Config (RDBConfig (..))
import Redis.RDB.TestConfig (defaultRDBConfig, genRDBConfig)
import System.Directory (doesFileExist)
import System.FilePath ((</>))

createSampleRDBStructure :: POSIXTime -> RDBFile
createSampleRDBStructure currentTime =
    RDBFile
        { magicString = Redis
        , version = RDBv7
        , auxFieldEntries =
            [ AuxFieldRedisVer $ RedisVersion $ RDBShortString "7.0.0"
            , AuxFieldRedisBits RedisBits64
            , AuxFieldCustom (toRDBString "custom-key") (toRDBVal "custom-value")
            , AuxFieldCTime $ CTime (fromPosixTimeToRDBUnixTimestampS currentTime)
            , AuxFieldCustom (toRDBString "another-key") (toRDBVal "another-value")
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
                            , encodedKey = toRDBVal "key1"
                            , encodedValue = toRDBVal "Hello World"
                            }
                    , FDOpcode $
                        KeyValWithExpiryInS
                            { expiryTimeS = fromPosixTimeToRDBUnixTimestampS $ currentTime + 3600 -- Expires in 1 hour
                            , valueType = Str
                            , encodedKey = toRDBVal "counter"
                            , encodedValue = toRDBVal "42"
                            }
                    , FCOpCode $
                        KeyValWithExpiryInMS
                            { expiryTimeMs = fromPosixTimeToRDBUnixTimestampMS $ currentTime + 1800 -- Expires in 30 minutes
                            , valueType = Str
                            , encodedKey = toRDBVal "123"
                            , encodedValue = toRDBVal "John Doe"
                            }
                    , KeyValOpCode $
                        KeyValWithNoExpiryInfo
                            { valueType = Str
                            , encodedKey = toRDBVal "-439284723"
                            , encodedValue = toRDBVal "Negative 32 bit integer"
                            }
                    ]
                }
            ]
        }

emptyRDBFile :: RDBFile
emptyRDBFile =
    RDBFile
        { magicString = Redis
        , version = RDBv7
        , auxFieldEntries = []
        , dbEntries = []
        }

rdbFileWithOnlyAuxFields :: RDBFile
rdbFileWithOnlyAuxFields =
    RDBFile
        { magicString = Redis
        , version = RDBv7
        , auxFieldEntries =
            [ AuxFieldRedisVer $ RedisVersion $ RDBShortString "7.2.0"
            , AuxFieldRedisBits RedisBits64
            , AuxFieldUsedMem $ UsedMem 2048
            ]
        , dbEntries = []
        }

multiDatabaseRDBStructure :: RDBFile
multiDatabaseRDBStructure =
    RDBFile
        { magicString = Redis
        , version = RDBv7
        , auxFieldEntries = []
        , dbEntries =
            [ RDbEntry
                { entryId = SelectDB 0
                , resizeDBEntry = ResizeDB 1 0
                , keyValEntries =
                    [ KeyValOpCode $
                        KeyValWithNoExpiryInfo
                            { valueType = Str
                            , encodedKey = toRDBVal "db0_key"
                            , encodedValue = toRDBVal "db0_value"
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
                            , encodedKey = toRDBVal "db1_key"
                            , encodedValue = toRDBVal "db1_value"
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
                            , encodedKey = toRDBVal "db2_key"
                            , encodedValue = toRDBVal "db2_value"
                            }
                    ]
                }
            ]
        }

-- | List of example RDB files to test
exampleRDBFiles :: [String]
exampleRDBFiles =
    [ "rdb_version_5_with_checksum.rdb"
    , "empty_database.rdb"
    , "expiration.rdb"
    , "integer_keys.rdb"
    , "keys_with_expiry.rdb"
    , "multiple_databases.rdb"
    , "non_ascii_values.rdb"
    , "easily_compressible_string_key.rdb"
    ]

spec_RDB_integration_tests :: Spec
spec_RDB_integration_tests = do
    describe "RDB File Integration Tests" $ do
        describe "RDB Creation and Roundtrip Tests" $ do
            it "Encodes/decodes a sample RDB file successfully (without checksum)" $ do
                currentTime <- getPOSIXTime
                let rdbStructure = createSampleRDBStructure currentTime
                let rdbConfig = defaultRDBConfig{generateChecksum = False, skipChecksumValidation = True}
                encodeThenDecodeRDBBinary rdbConfig rdbStructure `shouldBe` rdbStructure

            it "Encodes/decodes a sample RDB file successfully (with checksum)" $ do
                currentTime <- getPOSIXTime
                let rdbStructure = createSampleRDBStructure currentTime
                let rdbConfig = defaultRDBConfig{generateChecksum = True, skipChecksumValidation = False}
                let encoded = Binary.encode rdbConfig rdbStructure
                let decoded = Binary.decode @RDBFile rdbConfig encoded

                decoded `shouldBe` rdbStructure

            it "Encodes/decodes an empty RDB file successfully" $ hedgehog $ do
                rdbConfig <- H.forAll genRDBConfig
                evalIO $ encodeThenDecodeRDBBinary rdbConfig emptyRDBFile `shouldBe` emptyRDBFile

            it "Encodes/decodes an auxiliary-only RDB file successfully" $ hedgehog $ do
                rdbConfig <- H.forAll genRDBConfig
                evalIO $ encodeThenDecodeRDBBinary rdbConfig rdbFileWithOnlyAuxFields `shouldBe` rdbFileWithOnlyAuxFields

            it "handles multiple databases correctly" $ hedgehog $ do
                rdbConfig <- H.forAll genRDBConfig
                evalIO $ encodeThenDecodeRDBBinary rdbConfig multiDatabaseRDBStructure `shouldBe` multiDatabaseRDBStructure

            it "encodes large RDB files efficiently" $ hedgehog $ do
                currentTime <- evalIO getPOSIXTime
                let largeRDB = createLargeRDB currentTime
                rdbConfig <- H.forAll genRDBConfig
                evalIO $ encodeThenDecodeRDBBinary rdbConfig largeRDB `shouldBe` largeRDB

        describe "Decoding sample RDB Files" $ do
            for_ exampleRDBFiles $ \filename -> do
                it ("successfully decodes " <> filename) $ do
                    let filepath = "test/input/example-rdb-files" </> filename
                    fileExists <- doesFileExist filepath
                    let rdbConfig =
                            defaultRDBConfig
                                { useLzfCompression = True
                                , skipChecksumValidation = False
                                , generateChecksum = True
                                }

                    if fileExists
                        then do
                            result <- try (Binary.decodeFile rdbConfig filepath) :: IO (Either SomeException RDBFile)
                            case result of
                                Left err -> expectationFailure $ "Failed to decode " <> filename <> ": " <> show err
                                Right decodedRDB -> do
                                    decodedRDB.magicString `shouldBe` Redis
                                    decodedRDB.version `shouldSatisfy` (`elem` [minBound .. maxBound])
                        else expectationFailure $ "File not found: " <> filepath

-- | Create a large RDB for performance testing
createLargeRDB :: POSIXTime -> RDBFile
createLargeRDB currentTime =
    RDBFile
        { magicString = Redis
        , version = RDBv7
        , auxFieldEntries =
            [ AuxFieldRedisVer $ RedisVersion $ RDBShortString "7.0.0"
            , AuxFieldRedisBits RedisBits64
            ]
        , dbEntries =
            [ RDbEntry
                { entryId = SelectDB 0
                , resizeDBEntry = ResizeDB 50 25 -- 50 keys, 25 with expiry
                , keyValEntries = generateLargeKeyValList currentTime 50
                }
            ]
        }

-- | Generate a large list of key-value entries
generateLargeKeyValList :: POSIXTime -> Int -> [KeyValueOpCode]
generateLargeKeyValList currentTime count =
    take count $
        cycle
            [ KeyValOpCode $
                KeyValWithNoExpiryInfo
                    { valueType = Str
                    , encodedKey = toRDBVal "persistent_key"
                    , encodedValue = toRDBVal "persistent_value"
                    }
            , FCOpCode $
                KeyValWithExpiryInMS
                    { expiryTimeMs = fromPosixTimeToRDBUnixTimestampMS $ currentTime + 3600
                    , valueType = Str
                    , encodedKey = toRDBVal "expiring_key_ms"
                    , encodedValue = toRDBVal "expiring_value_ms"
                    }
            , FDOpcode $
                KeyValWithExpiryInS
                    { expiryTimeS = fromPosixTimeToRDBUnixTimestampS $ currentTime + 7200
                    , valueType = Str
                    , encodedKey = toRDBVal "expiring_key_s"
                    , encodedValue = toRDBVal "expiring_value_s"
                    }
            ]
