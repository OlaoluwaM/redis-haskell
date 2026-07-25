module Redis.Server.Config.Conf where

import Effectful.FileSystem qualified as Eff
import Effectful.FileSystem.IO.ByteString qualified as Eff
import Path qualified
import Redis.Server.Config.Conf.Lexer qualified as Lexer
import Redis.Server.Config.Readers qualified as Readers
import Redis.Server.Config.Types qualified as Config

import Blammo.Logging (Message (..), (.=))
import Control.Applicative (Const (..))
import Control.Exception (Exception (displayException))
import Control.Monad.Except (liftEither, runExceptT)
import Data.Attoparsec.Text (parseOnly)
import Data.Bifunctor (second)
import Data.Maybe (mapMaybe)
import Data.Monoid (Last (..))
import Data.String (IsString (fromString))
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding (decodeUtf8')
import Data.Text.Encoding.Error (UnicodeException)
import Effectful (Eff, (:>))
import Options.Applicative (
    ReadM,
 )
import Redis.Effect.Logging (logError, logInfo)
import Redis.Effects (Logging)
import Redis.Server.Config.Conf.Lexer (RawRedisConfEntry (..))
import Redis.Server.Config.Defaults (emptyPartialRedisConfig)
import Redis.Server.Config.Types (RedisConfigF (..), getConfigFieldName)
import Redis.Server.Metadata (RedisConfFilePath (..))
import Redis.Utils (catEithers, mapLeft, runReadM)

newtype RedisConfDocument = RedisConfDocument Config.PartialRedisConfig
    deriving newtype (Eq, Show)

type ConfigFieldSpecTable = Config.RedisConfigF (Const (Text, ReadM Config.PartialRedisConfig))

newtype FieldValue = FieldValue {fieldValue :: String}

data LoadConfigDocumentError = UTF8DecodeError UnicodeException | LexerError String | ParserErrors [String]

loadRedisConfDocument ::
    forall es.
    ( Eff.FileSystem :> es
    , Logging :> es
    ) =>
    RedisConfFilePath -> Eff es (Either String RedisConfDocument)
loadRedisConfDocument (RedisConfFilePath confFilePath) = do
    let redisConfPath = Path.toFilePath confFilePath

    confFileExists <- Eff.doesFileExist redisConfPath
    if not confFileExists
        then do
            let msg = "No configuration file found at " <> redisConfPath <> "; falling back to default settings."
            logInfo (fromString msg)
            pure . Right . RedisConfDocument $ emptyPartialRedisConfig
        else do
            decodedConfFileContentsResult <- mapLeft UTF8DecodeError . decodeUtf8' <$> Eff.readFile redisConfPath
            confDocumentParseResult <- runExceptT $ do
                decodedConfFileContents <- liftEither decodedConfFileContentsResult
                parsedRawConfEntries <- liftEither . mapLeft LexerError $ parseOnly Lexer.confDocumentParser decodedConfFileContents
                let combinedConfigFieldReadersM = mapMaybe lookupFieldReader parsedRawConfEntries
                let parseFields = uncurry (runReadM @String) . second (.fieldValue)
                -- NOTE: We could probably parallelize this operation
                parsedPartialConfigsPerEntry <- liftEither . mapLeft ParserErrors . catEithers . map parseFields $ combinedConfigFieldReadersM
                pure $ mconcat parsedPartialConfigsPerEntry

            case confDocumentParseResult of
                Left (UTF8DecodeError err) -> do
                    let msg = "Could not decode config file at path " <> redisConfPath <> ". It seems to contain some non-UTF8 encoded text? "
                    logError $ fromString msg :# ["Error" .= displayException err]
                    returnErrMsg msg
                Left (LexerError err) -> do
                    let msg = "An error occured while attempting to parse the config at " <> redisConfPath
                    logError $ fromString msg :# ["Error" .= err]
                    returnErrMsg msg
                Left (ParserErrors errs) -> do
                    let msg = "Failed to parse configuration values in the config file at " <> redisConfPath
                    logError $ fromString msg :# ["Errors" .= errs]
                    returnErrMsg msg
                Right conf -> pure . Right . RedisConfDocument $ conf
  where
    returnErrMsg = pure . Left

configFieldSpecs :: RedisConfigF (Const (Text, ReadM Config.PartialRedisConfig))
configFieldSpecs =
    RedisConfigF
        { rdbFileDirPath =
            Const (getConfigFieldName @Config.RDBFileDir, (\v -> emptyPartialRedisConfig{rdbFileDirPath = Last (Just v)}) <$> Readers.rdbFileDirReader)
        , rdbFilenamePath =
            Const (getConfigFieldName @Config.RDBFilename, (\v -> emptyPartialRedisConfig{rdbFilenamePath = Last (Just v)}) <$> Readers.rdbFilenameReader)
        , useRDBCompression =
            Const (getConfigFieldName @Config.UseRDBCompression, (\v -> emptyPartialRedisConfig{useRDBCompression = Last (Just v)}) <$> Readers.rdbCompressionReader)
        , genRdbChecksum =
            Const (getConfigFieldName @Config.GenRDBChecksum, (\v -> emptyPartialRedisConfig{genRdbChecksum = Last (Just v)}) <$> Readers.rdbChecksumReader)
        , port =
            Const (getConfigFieldName @Config.RedisPort, (\v -> emptyPartialRedisConfig{port = Last (Just v)}) <$> Readers.portReader)
        }

lookupFieldReader :: RawRedisConfEntry -> Maybe (ReadM Config.PartialRedisConfig, FieldValue)
lookupFieldReader RawRedisConfEntry{key, value} = (,FieldValue . T.unpack $ value) <$> lookup key (Config.collectFieldSpecs configFieldSpecs)
