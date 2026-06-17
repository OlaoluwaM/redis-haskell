module Redis.Server.Metadata (
    ServerMetadata (..),
    RedisConfFilePath (..),
    Environment (..),
    loadEnvironment,
) where

import Env qualified

import Data.Char (toLower)
import Data.Time (UTCTime)
import Path (Abs, File, Path)

data ServerMetadata = ServerMetadata
    { startTime :: UTCTime
    , configFilePath :: Maybe RedisConfFilePath
    , environment :: Environment
    }

newtype RedisConfFilePath = RedisConfFilePath {redisConfFilePath :: Path Abs File}
    deriving stock (Eq, Show)

data Environment = DEV | TEST | PROD
    deriving stock (Eq, Show)

loadEnvironment :: IO Environment
loadEnvironment = Env.parse (Env.header "Redis Haskell Server Environment") $ Env.var (Env.eitherReader parseEnvVar) "ENV" (Env.def DEV)

parseEnvVar :: String -> Either String Environment
parseEnvVar str =
    let normalizedStr = map toLower str
     in case normalizedStr of
            "dev" -> Right DEV
            "prod" -> Right PROD
            "test" -> Right TEST
            "testing" -> Right TEST
            "production" -> Right PROD
            "development" -> Right DEV
            _ -> Left $ "Invalid environment option: " <> normalizedStr
