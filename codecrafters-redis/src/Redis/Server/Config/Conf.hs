module Redis.Server.Config.Conf (
    loadRedisConfFile,
    RedisConfigFromConfigFile (..),
    LoadConfigFileError (..)
) where

import Prettyprinter

import Effectful.FileSystem qualified as Eff
import Effectful.FileSystem.IO.ByteString qualified as Eff
import Path qualified
import Redis.Server.Config.Conf.Lexer qualified as Lexer
import Redis.Server.Config.Readers qualified as Readers
import Redis.Server.Config.Types qualified as Config

import Control.Applicative (Const (..))
import Control.Exception (Exception (displayException))
import Control.Monad.Except (liftEither, runExceptT)
import Data.Attoparsec.Text (parseOnly)
import Data.Bifunctor (second)
import Data.Maybe (mapMaybe)
import Data.Monoid (Last (..))
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding (decodeUtf8')
import Data.Text.Encoding.Error (UnicodeException)
import Effectful (Eff, (:>))
import Options.Applicative (
    ReadM,
 )
import Redis.Server.Config.Conf.Lexer (RawRedisConfEntry (..))
import Redis.Server.Config.Defaults (emptyPartialRedisConfig)
import Redis.Server.Config.Types (RedisConfigF (..), getConfigFieldName)
import Redis.Server.Metadata (RedisConfFilePath (..))
import Redis.Utils (catEithers, mapLeft, runReadM)

newtype RedisConfigFromConfigFile = RedisConfigFromConfigFile Config.PartialRedisConfig
    deriving newtype (Eq, Show)

newtype FieldValue = FieldValue {fieldValue :: String}

data ConfigFileParseError = UTF8DecodeError UnicodeException | LexerError String | ParserErrors [String]

data LoadConfigFileError = LoadConfigFileError {
    errMsg :: String,
    errMetadata :: [String]
}
    deriving stock (Show)

instance Pretty LoadConfigFileError where
    pretty LoadConfigFileError {errMsg, errMetadata} = vsep ["Error while loading config:", indent 4 . vsep . map pretty $ [errMsg] <> errMetadata]

loadRedisConfFile ::
    forall es.
    (Eff.FileSystem :> es) =>
    Maybe RedisConfFilePath -> Eff es (Either LoadConfigFileError RedisConfigFromConfigFile)
loadRedisConfFile Nothing = pure . Right . RedisConfigFromConfigFile $ emptyPartialRedisConfig
loadRedisConfFile (Just (RedisConfFilePath confFilePath)) = do
    let redisConfPath = Path.toFilePath confFilePath

    confFileExists <- Eff.doesFileExist redisConfPath
    if not confFileExists
        then pure . Right . RedisConfigFromConfigFile $ emptyPartialRedisConfig
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
                    returnErr $ LoadConfigFileError msg [displayException err]
                Left (LexerError err) -> do
                    let msg = "An error occured while attempting to parse the config at " <> redisConfPath
                    returnErr $ LoadConfigFileError msg [err]
                Left (ParserErrors errs) -> do
                    let msg = "Failed to parse configuration values in the config file at " <> redisConfPath
                    returnErr $ LoadConfigFileError msg errs
                Right conf -> pure . Right . RedisConfigFromConfigFile $ conf
  where
    returnErr = pure . Left

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
