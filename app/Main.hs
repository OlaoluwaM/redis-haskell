module Main (main) where

import Network.Run.TCP
import Network.Socket

import Data.Binary qualified as Binary
import Data.ByteString qualified as BS

import Control.Concurrent.STM (newTVarIO)
import Control.Exception (SomeException, try)
import Control.Monad (unless)
import Data.Time.Clock.POSIX (POSIXTime, getPOSIXTime)
import Network.Socket.ByteString (recv)
import Options.Applicative (
    execParser,
    fullDesc,
    header,
    helper,
    info,
    progDesc,
    (<**>),
 )
import Redis.Handler (handleCommandReq)
import Redis.RDB.Data
import Redis.RDB.Format
import Redis.Server (runServer)
import Redis.Server.Context (ServerContext (..))
import Redis.Server.Settings (ServerSettings, serverSettingsOptParser)
import Redis.Store (StoreState, genInitialStore)
import System.IO (BufferMode (NoBuffering), hSetBuffering, stderr, stdout)
import Text.Ascii (ascii)
import Text.Pretty.Simple (pPrint)

mainServer :: IO ()
mainServer = do
    -- Disable output buffering
    hSetBuffering stdout NoBuffering
    hSetBuffering stderr NoBuffering

    putStrLn $ "Redis server listening on port " <> port
    initialStore <- newTVarIO genInitialStore
    serverSettings <- execParser opts
    runTCPServer Nothing port (handleRedisClientConnection initialStore serverSettings)
  where
    port :: String
    port = "6379"

    opts = info (serverSettingsOptParser <**> helper) (fullDesc <> progDesc "Redis server build in Haskell per CodeCrafters" <> header "A haskell redis server")

handleRedisClientConnection :: StoreState -> ServerSettings -> Socket -> IO ()
handleRedisClientConnection storeState serverSettings s = do
    req <- recv s 1024
    unless (BS.null req) $ do
        runServer (handleCommandReq req) (ServerContext s storeState serverSettings)
        handleRedisClientConnection storeState serverSettings s

-- | Create a sample RDB structure for testing
createSampleRDB :: POSIXTime -> RDBFile
createSampleRDB currentTime =
    RDBFile
        { magicString = Redis
        , version = RDBVersion [ascii|"0003"|]
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
                , resizeDBEntry = ResizeDB 3 3 -- 3 keys, 3 with expiry (updated count)
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

-- | Generate a sample RDB file for testing
generateSampleRDB :: IO ()
generateSampleRDB = do
    currentTime <- getPOSIXTime
    let sampleRDB = createSampleRDB currentTime

    Binary.encodeFile "test.rdb" sampleRDB
    putStrLn "✅ Generated test.rdb successfully!"
    putStrLn "📄 File contains:"
    putStrLn "   - Magic string: REDIS"
    putStrLn "   - Version: 0003"
    putStrLn "   - Auxiliary field: redis-ver = 7.0.0"
    putStrLn "   - Key-value: key1 = Hello World (expires in 5 minutes, millisecond precision)"
    putStrLn "   - Key-value: counter = 42 (expires in 1 hour, second precision)"
    putStrLn "   - Key-value: user:1000:name = John Doe (expires in 30 minutes, millisecond precision)"
    putStrLn "   - EOF marker"

-- | Alternative main function that generates RDB file and exits
main :: IO ()
main = do
    -- Disable output buffering
    hSetBuffering stdout NoBuffering
    hSetBuffering stderr NoBuffering

    putStrLn "🚀 Generating sample RDB file..."
    currentTime <- getPOSIXTime
    let originalSampleRDB = createSampleRDB currentTime

    -- Write the RDB file
    Binary.encodeFile "./test.rdb" originalSampleRDB
    putStrLn "✅ Generated test.rdb successfully!"

    putStrLn "🔍 Reading back RDB file to verify..."
    result <- try (Binary.decodeFile "./easily_compressible_string_key.rdb" :: IO RDBFile) :: IO (Either SomeException RDBFile)
    case result of
        Left err -> do
            putStrLn $ "❌ Failed to decode RDB file: " ++ show err
            putStrLn "🔍 This suggests an issue with the Binary instance for RDBFile"
        Right decodedRDB -> do
            pPrint decodedRDB

-- if decodedRDB == originalSampleRDB
--     then do
--         putStrLn "✅ Verification successful! Decoded RDB matches original structure."
--         putStrLn "✨ Done! Use this as your test.rdb file."
--     else do
--         putStrLn "❌ Verification failed! Decoded RDB does not match original structure."
--         putStrLn "🔍 Original structure:"
--         pPrint originalSampleRDB
--         putStrLn "\n🔍 Decoded structure:"
--         pPrint decodedRDB
