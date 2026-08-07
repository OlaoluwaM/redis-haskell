module Redis.Server.Config (
    loadRedisConfig,
    commandlineConfigParser,
    CommandLineConfig,
    commandLineConfigFilePath,
    RedisConfig,
    RedisConfigF (..),

    -- ** For testing
    mkCompleteRedisConfig,
    parseRedisConfFilePath,
) where

import Path

import Effectful qualified as Eff
import Effectful.Error.Static qualified as Eff
import Effectful.FileSystem qualified as Eff
import Redis.Server.Config.Conf qualified as Conf
import Redis.Server.Config.Defaults qualified as Defaults

import Redis.Server.Config.Types qualified as Config

import Data.Bifunctor (Bifunctor (bimap))
import Data.Bool (bool)
import Data.Maybe (fromMaybe)
import Data.Monoid (Last (..))
import Effectful (Eff, (:>))
import GHC.Generics (Generic (..))
import Options.Applicative (
    Parser,
    ParserInfo,
    ReadM,
    argument,
    fullDesc,
    header,
    help,
    helper,
    info,
    metavar,
    optional,
    progDesc,
    simpleVersioner,
    (<**>),
 )
import Options.Applicative.Types (readerAsk)
import Redis.Server.Config.CommandLine (
    RedisConfigFromCommandLine (..),
    parserForCommandLineConfig,
 )
import Redis.Server.Config.Conf (LoadConfigFileError (..), RedisConfigFromConfigFile (..))
import Redis.Server.Config.Defaults (DefaultRedisConfig (..))
import Redis.Server.Config.Types (RedisConfig, RedisConfigF (..), gZipWith)
import Redis.Server.Metadata (RedisConfFilePath (..))
import Redis.Server.Version (redisVersion)

data CommandLineConfig = CommandLineConfig (Maybe RedisConfFilePath) RedisConfigFromCommandLine

commandLineConfigFilePath :: CommandLineConfig -> Maybe RedisConfFilePath
commandLineConfigFilePath (CommandLineConfig confFilePath _) = confFilePath

loadRedisConfig :: CommandLineConfig -> IO (Either LoadConfigFileError RedisConfig)
loadRedisConfig commandlineConfig = Eff.runEff . Eff.runErrorNoCallStack . Eff.runFileSystem $ loadRedisConfig_ commandlineConfig

loadRedisConfig_ ::
    forall es.
    ( Eff.FileSystem :> es
    , Eff.Error LoadConfigFileError :> es
    ) =>
    CommandLineConfig -> Eff es Config.RedisConfig
loadRedisConfig_ (CommandLineConfig confFilePath cliConfig) = mkCompleteRedisConfig cliConfig <$> Conf.loadRedisConfFile confFilePath

{-# WARNING in "x-unsafe-internals" mkCompleteRedisConfig "This value is exported for testing purposes only" #-}
mkCompleteRedisConfig :: RedisConfigFromCommandLine -> RedisConfigFromConfigFile -> Config.RedisConfig
mkCompleteRedisConfig (RedisConfigFromCommandLine configFromCli) (RedisConfigFromConfigFile configFromFile) =
    let (DefaultRedisConfig defaults) = Defaults.defaultRedisConfig
        partiallyMergedConfig = configFromFile <> configFromCli
     in to (gZipWith fromLast (from defaults) (from partiallyMergedConfig))
  where
    fromLast :: a -> Last a -> a
    fromLast defaultVal = fromMaybe defaultVal . (.getLast)

commandlineConfigParser :: FilePath -> ParserInfo CommandLineConfig
commandlineConfigParser cwd =
    info
        (parser <**> helper <**> simpleVersioner redisVersion)
        (fullDesc <> progDesc "Redis server built in Haskell" <> header "A haskell redis server")
  where
    parser =
        CommandLineConfig
            <$> optional (parserForRedisConfigFromConfigFilePathArgument cwd)
            <*> parserForCommandLineConfig

parserForRedisConfigFromConfigFilePathArgument :: FilePath -> Parser RedisConfFilePath
parserForRedisConfigFromConfigFilePathArgument cwd =
    argument
        (parseRedisConfFilePath cwd)
        (metavar "REDIS_CONFIG_FILE" <> help "Path to redis config file")

{-# WARNING in "x-unsafe-internals" parseRedisConfFilePath "This value is exported for testing purposes only" #-}
parseRedisConfFilePath :: FilePath -> ReadM RedisConfFilePath
parseRedisConfFilePath cwd = do
    rawPath <- readerAsk
    parseResult <-
        maybe
            (fail "Path provided for redis config file is not an absolute file path")
            pure
            $ parseSomeFile @Maybe rawPath

    either fail (pure . RedisConfFilePath) $ resolveRedisConfFilePath cwd parseResult

resolveRedisConfFilePath :: FilePath -> SomeBase File -> Either String (Path Abs File)
resolveRedisConfFilePath cwd parseResult = case parseResult of
    (Abs absPath) ->
        bool
            (Left "The file provided is not named redis.conf")
            (Right absPath)
            (hasValidConfigFilename absPath)
    (Rel relativePath) -> do
        relPath <-
            bool
                (Left "The file provided is not named redis.conf")
                (Right relativePath)
                (hasValidConfigFilename relativePath)
        bimap (const @String $ "Invalid cwd: " <> cwd) (</> relPath) $ parseAbsDir cwd
  where
    hasValidConfigFilename = (== [relfile|redis.conf|]) . filename

