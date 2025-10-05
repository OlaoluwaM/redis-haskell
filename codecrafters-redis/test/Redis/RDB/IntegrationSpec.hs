{-# LANGUAGE TemplateHaskell #-}

module Redis.RDB.IntegrationSpec where

import Redis.RDB.Data
import Redis.RDB.Format
import Test.Hspec
import Test.Hspec.Hedgehog hiding (assert)

import Hedgehog qualified as H
import Redis.RDB.Binary qualified as Binary

import Control.Exception (SomeException, try)
import Data.Time.Clock.POSIX (POSIXTime, getPOSIXTime)
import Data.Traversable (for)
import Path (addExtension, parseRelFile, reldir, toFilePath, (</>))
import Redis.Helper (encodeThenDecodeUsingRDBBinary)
import Redis.RDB.Config (RDBConfig (..))
import Redis.RDB.TestConfig (defaultRDBConfig, genRDBConfig)
import Rerefined.Refine.TH (refineTH)
import System.Directory (doesFileExist)
import Test.Tasty (TestTree)
import Test.Tasty.Golden (goldenVsFile)
import Test.Tasty.HUnit (assertFailure)

createSampleRDBStructure :: POSIXTime -> RDBFile
createSampleRDBStructure currentTime =
    RDBFile
        { magicString = Redis
        , version = RDBv7
        , auxFieldEntries =
            [ AuxFieldRedisVer . RedisVersion $ $$(refineTH "7.0.0")
            , AuxFieldRedisBits RedisBits64
            , AuxFieldCustom (toRDBStringOrIntVal "custom-key") (toRDBStringOrIntVal "custom-value")
            , AuxFieldCTime $ CTime (fromPosixTimeToRDBUnixTimestampS currentTime)
            , AuxFieldCustom (toRDBStringOrIntVal "another-key") (toRDBStringOrIntVal "another-value")
            , AuxFieldUsedMem $ UsedMem 1048576 -- 1MB
            ]
        , dbEntries =
            [ RDbEntry
                { entryId = SelectDB $$(refineTH 0) -- Database 0
                , resizeDBEntry = ResizeDB 4 3 -- 4 keys total, 3 with expiry
                , keyValEntries =
                    [ FCOpCode $
                        KeyValWithExpiryInMS
                            { expiryTimeMs = fromPosixTimeToRDBUnixTimestampMS $ currentTime + 300 -- Expires in 5 minutes
                            , valueType = Str
                            , encodedKey = toRDBStringOrIntVal "key1"
                            , encodedValue = toRDBStringOrIntVal "Hello World"
                            }
                    , FDOpcode $
                        KeyValWithExpiryInS
                            { expiryTimeS = fromPosixTimeToRDBUnixTimestampS $ currentTime + 3600 -- Expires in 1 hour
                            , valueType = Str
                            , encodedKey = toRDBStringOrIntVal "counter"
                            , encodedValue = toRDBStringOrIntVal "42"
                            }
                    , FCOpCode $
                        KeyValWithExpiryInMS
                            { expiryTimeMs = fromPosixTimeToRDBUnixTimestampMS $ currentTime + 1800 -- Expires in 30 minutes
                            , valueType = Str
                            , encodedKey = toRDBStringOrIntVal "123"
                            , encodedValue = toRDBStringOrIntVal "John Doe"
                            }
                    , KeyValOpCode $
                        KeyValWithNoExpiryInfo
                            { valueType = Str
                            , encodedKey = toRDBStringOrIntVal "-439284723"
                            , encodedValue = toRDBStringOrIntVal "Negative 32 bit integer"
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
            [ AuxFieldRedisVer . RedisVersion $ $$(refineTH "7.2.0")
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
                { entryId = SelectDB $$(refineTH 0)
                , resizeDBEntry = ResizeDB 1 0
                , keyValEntries =
                    [ KeyValOpCode $
                        KeyValWithNoExpiryInfo
                            { valueType = Str
                            , encodedKey = toRDBStringOrIntVal "db0_key"
                            , encodedValue = toRDBStringOrIntVal "db0_value"
                            }
                    ]
                }
            , RDbEntry
                { entryId = SelectDB $$(refineTH 1)
                , resizeDBEntry = ResizeDB 1 0
                , keyValEntries =
                    [ KeyValOpCode $
                        KeyValWithNoExpiryInfo
                            { valueType = Str
                            , encodedKey = toRDBStringOrIntVal "db1_key"
                            , encodedValue = toRDBStringOrIntVal "db1_value"
                            }
                    ]
                }
            , RDbEntry
                { entryId = SelectDB $$(refineTH 2)
                , resizeDBEntry = ResizeDB 1 0
                , keyValEntries =
                    [ KeyValOpCode $
                        KeyValWithNoExpiryInfo
                            { valueType = Str
                            , encodedKey = toRDBStringOrIntVal "db2_key"
                            , encodedValue = toRDBStringOrIntVal "db2_value"
                            }
                    ]
                }
            ]
        }

-- | List of example RDB files to test
exampleRDBFiles :: [String]
exampleRDBFiles =
    [ "rdb_version_5_with_checksum"
    , "empty_database"
    , "expiration"
    , "integer_keys"
    , "keys_with_expiry"
    , "multiple_databases"
    , "non_ascii_values"
    , "easily_compressible_string_key"
    ]

spec_RDB_integration_tests :: Spec
spec_RDB_integration_tests = do
    describe "RDB File Integration Tests" $ do
        describe "RDB Creation and Roundtrip Tests" $ do
            it "Encodes/decodes a sample RDB file successfully (without checksum)" $ do
                currentTime <- getPOSIXTime
                let rdbStructure = createSampleRDBStructure currentTime
                let rdbConfig = defaultRDBConfig{generateChecksum = False, skipChecksumValidation = True}
                encodeThenDecodeUsingRDBBinary rdbConfig rdbStructure `shouldBe` rdbStructure

            it "Encodes/decodes a sample RDB file successfully (with checksum)" $ do
                currentTime <- getPOSIXTime
                let rdbStructure = createSampleRDBStructure currentTime
                let rdbConfig = defaultRDBConfig{generateChecksum = True, skipChecksumValidation = False}
                let encoded = Binary.encode rdbConfig rdbStructure
                let decoded = Binary.decode @RDBFile rdbConfig encoded

                decoded `shouldBe` rdbStructure

            it "Encodes/decodes an empty RDB file successfully" $ hedgehog $ do
                rdbConfig <- H.forAll genRDBConfig
                evalIO $ encodeThenDecodeUsingRDBBinary rdbConfig emptyRDBFile `shouldBe` emptyRDBFile

            it "Encodes/decodes an auxiliary-only RDB file successfully" $ hedgehog $ do
                rdbConfig <- H.forAll genRDBConfig
                evalIO $ encodeThenDecodeUsingRDBBinary rdbConfig rdbFileWithOnlyAuxFields `shouldBe` rdbFileWithOnlyAuxFields

            it "handles multiple databases correctly" $ hedgehog $ do
                rdbConfig <- H.forAll genRDBConfig
                evalIO $ encodeThenDecodeUsingRDBBinary rdbConfig multiDatabaseRDBStructure `shouldBe` multiDatabaseRDBStructure

            it "encodes large RDB files efficiently" $ hedgehog $ do
                currentTime <- evalIO getPOSIXTime
                let largeRDB = createLargeRDB currentTime
                rdbConfig <- H.forAll genRDBConfig
                evalIO $ encodeThenDecodeUsingRDBBinary rdbConfig largeRDB `shouldBe` largeRDB

test_Decoding_sample_rdb_file :: IO [TestTree]
test_Decoding_sample_rdb_file = do
    for exampleRDBFiles $ \rawTestFilename -> do
        testFilename <- parseRelFile rawTestFilename
        filepath <- addExtension ".rdb" $ [reldir|test/Redis/RDB/input|] </> testFilename
        fileExists <- doesFileExist (toFilePath filepath)
        let rdbConfig =
                defaultRDBConfig
                    { useLzfCompression = True
                    , skipChecksumValidation = False
                    , generateChecksum = True
                    }

        if fileExists
            then do
                goldenPath <- addExtension ".rdb" $ [reldir|test/Redis/RDB/golden|] </> testFilename
                outputPath <- addExtension ".rdb" $ [reldir|test/Redis/RDB/output|] </> testFilename
                result <- try (Binary.decodeFile rdbConfig (toFilePath filepath)) :: IO (Either SomeException RDBFile)
                case result of
                    Left err -> assertFailure ("Failed to decode " <> show filepath <> ": " <> show err)
                    Right decodedRDB ->
                        pure $
                            goldenVsFile ("Golden test for " <> rawTestFilename) (toFilePath goldenPath) (toFilePath outputPath) (Binary.encodeFile rdbConfig (toFilePath outputPath) decodedRDB)
            else assertFailure ("File not found: " <> show filepath)

-- | Create a large RDB for performance testing
createLargeRDB :: POSIXTime -> RDBFile
createLargeRDB currentTime =
    RDBFile
        { magicString = Redis
        , version = RDBv7
        , auxFieldEntries =
            [ AuxFieldRedisVer . RedisVersion $ $$(refineTH "7.0.0")
            , AuxFieldRedisBits RedisBits64
            ]
        , dbEntries =
            [ RDbEntry
                { entryId = SelectDB $$(refineTH 0)
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
                    , encodedKey = toRDBStringOrIntVal "persistent_key"
                    , encodedValue = toRDBStringOrIntVal "persistent_value"
                    }
            , FCOpCode $
                KeyValWithExpiryInMS
                    { expiryTimeMs = fromPosixTimeToRDBUnixTimestampMS $ currentTime + 3600
                    , valueType = Str
                    , encodedKey = toRDBStringOrIntVal "expiring_key_ms"
                    , encodedValue = toRDBStringOrIntVal "expiring_value_ms"
                    }
            , FDOpcode $
                KeyValWithExpiryInS
                    { expiryTimeS = fromPosixTimeToRDBUnixTimestampS $ currentTime + 7200
                    , valueType = Str
                    , encodedKey = toRDBStringOrIntVal "expiring_key_s"
                    , encodedValue = toRDBStringOrIntVal "expiring_value_s"
                    }
            ]
