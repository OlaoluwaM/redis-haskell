module Redis.RDB.TestConfig (
    genRDBConfig,
    RDBConfig (..),
    defaultRDBConfig,
) where

import Hedgehog.Gen qualified as Gen

import Hedgehog (MonadGen)
import Redis.RDB.Config (RDBConfig (..), defaultRDBConfig)

genRDBConfig :: (MonadGen m) => m RDBConfig
genRDBConfig = do
    useLzfCompression <- Gen.bool
    generateChecksum <- Gen.bool
    skipChecksumValidation <- Gen.bool
    pure RDBConfig{useLzfCompression, generateChecksum, skipChecksumValidation}
