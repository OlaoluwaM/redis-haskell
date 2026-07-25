module Redis.Server.Config.ConfSpec where

import Test.Hspec

import Blammo.Logging.Logger qualified as Blammo
import Effectful qualified as Eff
import Effectful.FileSystem qualified as Eff
import Redis.Effect.Logging qualified as Eff
import Redis.Server.Config.Conf qualified as Conf

import Blammo.Logging.LogSettings (defaultLogSettings)
import Data.Foldable (for_)
import Data.Monoid (Last (..))
import Path (
    SomeBase (..),
    absdir,
    parseAbsDir,
    parseRelFile,
    reldir,
    relfile,
    (</>),
 )
import Redis.Server.Config.Conf (RedisConfDocument (..))
import Redis.Server.Config.Defaults (emptyPartialRedisConfig)
import Redis.Server.Config.Types (PartialRedisConfig, RedisConfigF (..))
import Redis.Server.Metadata (RedisConfFilePath (..))
import System.Directory (getCurrentDirectory)

{- | Sample redis.conf files under test/Redis/Server/Config/Conf/input, paired with the
'PartialRedisConfig' we expect 'Conf.loadRedisConfDocument' to produce from each.
-}
exampleConfFiles :: [(String, PartialRedisConfig)]
exampleConfFiles =
    [ ("minimal_port", emptyPartialRedisConfig{port = Last (Just 7000)})
    ,
        ( "rdb_persistence"
        , emptyPartialRedisConfig
            { rdbFileDirPath = Last (Just (Abs [absdir|/var/lib/redis|]))
            , rdbFilenamePath = Last (Just [relfile|dump.rdb|])
            , useRDBCompression = Last (Just True)
            , genRdbChecksum = Last (Just True)
            }
        )
    , ("unsupported_directives_only", emptyPartialRedisConfig)
    ,
        ( "mixed_full"
        , emptyPartialRedisConfig
            { port = Last (Just 6380)
            , rdbFileDirPath = Last (Just (Rel [reldir|data|]))
            , rdbFilenamePath = Last (Just [relfile|custom.rdb|])
            , useRDBCompression = Last (Just False)
            , genRdbChecksum = Last (Just False)
            }
        )
    ,
        ( "yes_no_booleans"
        , emptyPartialRedisConfig
            { rdbFilenamePath = Last (Just [relfile|yesno.rdb|])
            , useRDBCompression = Last (Just True)
            , genRdbChecksum = Last (Just False)
            }
        )
    ,
        ( "yes_no_case_insensitive"
        , emptyPartialRedisConfig
            { useRDBCompression = Last (Just True)
            , genRdbChecksum = Last (Just False)
            }
        )
    ]

spec_redis_conf_document_loading :: Spec
spec_redis_conf_document_loading = do
    describe "loadRedisConfDocument against sample redis.conf files" $ do
        for_ exampleConfFiles $ \(sampleName, expectedConfig) ->
            it ("Parses " <> sampleName <> ".conf into the expected PartialRedisConfig") $ do
                confFilePath <- mkSampleConfFilePath sampleName
                result <- runLoadRedisConfDocument confFilePath
                case result of
                    Left err -> expectationFailure ("loadRedisConfDocument failed for " <> sampleName <> ", with error: " <> err)
                    Right (RedisConfDocument actualConfig) -> actualConfig `shouldBe` expectedConfig

mkSampleConfFilePath :: String -> IO RedisConfFilePath
mkSampleConfFilePath sampleName = do
    projectRoot <- parseAbsDir =<< getCurrentDirectory
    relativeSamplePath <- parseRelFile ("test/Redis/Server/Config/Conf/input/" <> sampleName <> ".conf")
    pure . RedisConfFilePath $ projectRoot </> relativeSamplePath

runLoadRedisConfDocument :: RedisConfFilePath -> IO (Either String RedisConfDocument)
runLoadRedisConfDocument confFilePath = do
    logger <- Blammo.newTestLogger defaultLogSettings
    Eff.runEff . Eff.runLoggingWithLogger logger . Eff.runFileSystem $ Conf.loadRedisConfDocument confFilePath
