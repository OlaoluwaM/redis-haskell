module Redis.Server.Config (
    loadRedisConfig,
    commandlineConfigParser,
    CommandLineConfig,
    commandLineConfigFilePath,
    RedisConfig,
    RedisConfigF (..),

    -- ** For testing
    mkCompleteRedisConfig,
) where

import Path

import Effectful qualified as Eff
import Effectful.Error.Static qualified as Eff
import Effectful.FileSystem qualified as Eff
import Redis.Server.Config.Conf qualified as Conf
import Redis.Server.Config.Defaults qualified as Defaults

import Redis.Server.Config.Types qualified as Config

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

commandlineConfigParser :: ParserInfo CommandLineConfig
commandlineConfigParser =
    info
        (parser <**> helper <**> simpleVersioner redisVersion)
        (fullDesc <> progDesc "Redis server built in Haskell" <> header "A haskell redis server")
  where
    parser = CommandLineConfig <$> optional parserForRedisConfigFromConfigFilePathArgument <*> parserForCommandLineConfig

parserForRedisConfigFromConfigFilePathArgument :: Parser RedisConfFilePath
parserForRedisConfigFromConfigFilePathArgument =
    argument
        parseRedisConfFilePath
        (metavar "REDIS_CONFIG_FILE" <> help "Path to redis config file")

parseRedisConfFilePath :: ReadM RedisConfFilePath
parseRedisConfFilePath = do
    rawPath <- readerAsk
    path <-
        maybe
            (fail "Path provided for redis config file is not an absolute file path")
            pure
            $ parseAbsFile @Maybe rawPath

    if filename path == [relfile|redis.conf|]
        then pure (RedisConfFilePath path)
        else fail "The file provided is not named redis.conf"
