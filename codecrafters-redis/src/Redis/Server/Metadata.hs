module Redis.Server.Metadata (
    ServerMetadata (..),
    RedisConfFilePath (..),
    Environment (..),
    loadEnvironment,
    ReplId,
    genReplId,
    parseReplId,
    replIdToText,
    ReplIdRef,

    -- ** For testing
    genReplId_,
    replIdLength,
) where

import Redis.Effect.Random

import Data.Text qualified as T
import Data.Vector qualified as V
import Env qualified

import Control.Concurrent.STM (TVar)
import Control.Monad (guard, replicateM)
import Data.Char (toLower)
import Data.Text (Text)
import Data.Time (UTCTime)
import Effectful (Eff, (:>))
import Effectful qualified as Eff
import Path (Abs, File, Path)

data ServerMetadata = ServerMetadata
    { startTime :: UTCTime
    , configFilePath :: Maybe RedisConfFilePath
    , environment :: Environment
    , replIdRef :: ReplIdRef
    }

type ReplIdRef = TVar ReplId

newtype RedisConfFilePath = RedisConfFilePath {redisConfFilePath :: Path Abs File}
    deriving stock (Eq, Show)

newtype ReplId = ReplId Text
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

{-# WARNING in "x-unsafe-internals" replIdLength "This value is exported for testing purposes only" #-}
replIdLength :: Int
replIdLength = 40

parseReplId :: Text -> Maybe ReplId
parseReplId rawText = do
    guard (T.length rawText == replIdLength)
    pure . ReplId $ rawText

genReplId :: IO ReplId
genReplId = Eff.runEff . runRandomIO . (ReplId <$>) $ genReplId_

{-# WARNING in "x-unsafe-internals" genReplId_ "This value is exported for testing purposes only" #-}
genReplId_ :: (Random :> es) => Eff es Text
genReplId_ = do
    let hexDigits = V.fromList ['0', '1', '2', '3', '4', '5', '6', '7', '8', '9', 'a', 'b', 'c', 'd', 'e', 'f']
    let indexOfLastHexDigit = length hexDigits - 1

    hexChars <- replicateM replIdLength $ (hexDigits V.!) <$> genRandomR @Int (0, indexOfLastHexDigit)

    pure . T.pack $ hexChars

replIdToText :: ReplId -> Text
replIdToText (ReplId txt) = txt
