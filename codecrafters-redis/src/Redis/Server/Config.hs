module Redis.Server.Config (
    loadRedisConfig,
    commandlineConfigParser,

    -- ** For testing
    mkCompleteRedisConfig,
) where

import Path

import Effectful.FileSystem qualified as Eff
import Redis.Server.Config.Conf qualified as Conf
import Redis.Server.Config.Defaults qualified as Defaults
import Redis.Server.Config.Types qualified as Config

import Blammo.Logging (Message (..), (.=))
import Data.Maybe (fromMaybe)
import Data.Monoid (Last (..))
import Data.String (fromString)
import Effectful (Eff, (:>))
import Effectful.Fail (Fail)
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
import Redis.Effect.Logging (Logging, logError)
import Redis.Server.Config.CommandLine (
    RedisConfigFromCommandLine (..),
    parserForCommandLineConfig,
 )
import Redis.Server.Config.Conf (LoadConfigFileError (..), RedisConfigFromConfigFile (..))
import Redis.Server.Config.Defaults (DefaultRedisConfig (..))
import Redis.Server.Config.Types (gZipWith)
import Redis.Server.Metadata (RedisConfFilePath (..))
import Redis.Server.Version (redisVersion)

data CommandLineConfig = CommandLineConfig (Maybe RedisConfFilePath) RedisConfigFromCommandLine

loadRedisConfig ::
    forall es.
    ( Eff.FileSystem :> es
    , Logging :> es
    , Fail :> es
    ) =>
    CommandLineConfig -> Eff es Config.RedisConfig
loadRedisConfig (CommandLineConfig confFilePath cliConfig) = do
    redisConfFileLoadResultE <- Conf.loadRedisConfFile confFilePath
    case redisConfFileLoadResultE of
        Left err -> do
            logError $ fromString err.errMsg :# ["Error" .= err.errMetadata]
            fail "Error occurred while attempting to load redis config file"
        Right redisConfigFromConfigFile -> pure $ mkCompleteRedisConfig redisConfigFromConfigFile cliConfig

{-# WARNING in "x-unsafe-internals" mkCompleteRedisConfig "This value is exported for testing purposes only" #-}
mkCompleteRedisConfig :: RedisConfigFromConfigFile -> RedisConfigFromCommandLine -> Config.RedisConfig
mkCompleteRedisConfig (RedisConfigFromConfigFile configFromFile) (RedisConfigFromCommandLine configFromCli) =
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
