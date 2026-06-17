module Redis.Server.Settings.Get (
    getRDBDumpFilePathFromSettings,
    genRDBConfigFromSettings,
    getRedisPortFromSettings,
) where

import Data.HashMap.Strict qualified as HashMap
import Data.Maybe (fromMaybe)
import Data.Text qualified as T
import Path
import Redis.RDB.Config (MkRDBConfigArg (..), RDBConfig, defaultRDBConfig, mkRDBConfig)
import Redis.Server.Settings
import Data.String (IsString, fromString)

getRDBDumpFilePathFromSettings :: ServerSettings -> SomeBase File
getRDBDumpFilePathFromSettings (ServerSettings settings) = fromMaybe defaultRdbDumpFilePath $ do
    rdbFileDirSettingVal <- HashMap.lookup rdbFileDirectorySettingKey settings
    rdbFileNameSettingVal <- HashMap.lookup rdbFilenameSettingKey settings
    case (rdbFileDirSettingVal, rdbFileNameSettingVal) of
        (DirPathVal (Abs dir), FilePathVal (Rel file)) -> pure $ Abs $ dir </> file
        (DirPathVal (Rel dir), FilePathVal (Rel file)) -> pure $ Rel $ dir </> file
        _ -> Nothing
  where
    defaultRdbDumpFilePath :: SomeBase File
    defaultRdbDumpFilePath = Rel $ defaultRDBFileDirectory </> defaultRDBFilename

genRDBConfigFromSettings :: ServerSettings -> RDBConfig
genRDBConfigFromSettings (ServerSettings settings) = fromMaybe defaultRDBConfig $ do
    rdbCompressionSetting <- HashMap.lookup rdbCompressionSettingKey settings
    rdbChecksumSetting <- HashMap.lookup rdbChecksumSettingKey settings

    case (rdbCompressionSetting, rdbChecksumSetting) of
        (BoolVal useCompression, BoolVal genChecksum) ->
            pure $
                mkRDBConfig $
                    MkRDBConfigArg
                        { useCompression
                        , generateChecksum = genChecksum
                        }
        _ -> Nothing

getRedisPortFromSettings :: IsString a => ServerSettings -> a
getRedisPortFromSettings (ServerSettings settings) = fromMaybe (fromString $ show redisDefaultPort) $ do
    redisPortVal <- HashMap.lookup redisPortSettingKey settings

    case redisPortVal of
        (TextVal portStr) -> pure . fromString $ T.unpack portStr
        _ -> Nothing
