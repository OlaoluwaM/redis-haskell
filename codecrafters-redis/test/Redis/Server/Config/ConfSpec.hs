{-# LANGUAGE TemplateHaskell #-}

module Redis.Server.Config.ConfSpec where

import Test.Hspec

import Effectful qualified as Eff
import Effectful.Error.Static qualified as Eff
import Effectful.FileSystem qualified as Eff
import Redis.Server.Config.Conf qualified as Conf

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
import Prettyprinter (pretty)
import Redis.Server.Config.Conf (LoadConfigFileError (..), RedisConfigFromConfigFile (..))
import Redis.Server.Config.Defaults (emptyPartialRedisConfig)
import Redis.Server.Config.Types (
    HostPortRec (..),
    PartialRedisConfig,
    Port,
    RedisConfigF (..),
    ReplicaOfType (..),
 )
import Redis.Server.Metadata (RedisConfFilePath (..))
import Rerefined.Refine.TH (refineTH)
import System.Directory (getCurrentDirectory)

{- | Sample redis.conf files under test/Redis/Server/Config/Conf/input, paired with the
'PartialRedisConfig' we expect 'Conf.loadRedisConfFile' to produce from each.
-}
exampleConfFiles :: [(String, PartialRedisConfig)]
exampleConfFiles =
    [
        ( "minimal_port"
        , emptyPartialRedisConfig
            { port = Last (Just ($$(refineTH 7000) :: Port))
            , replicaOf = Last (Just NO_ONE)
            }
        )
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
            { port = Last (Just ($$(refineTH 6380) :: Port))
            , rdbFileDirPath = Last (Just (Rel [reldir|data|]))
            , rdbFilenamePath = Last (Just [relfile|custom.rdb|])
            , useRDBCompression = Last (Just False)
            , genRdbChecksum = Last (Just False)
            , replicaOf = Last (Just (HostPort $ HostPortRec "localhost" ($$(refineTH 6381) :: Port)))
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
    , -- Every directive is repeated; the last occurrence in the file must win

        ( "duplicate_keys"
        , emptyPartialRedisConfig
            { port = Last (Just ($$(refineTH 6381) :: Port))
            , rdbFileDirPath = Last (Just (Abs [absdir|/data/redis|]))
            , rdbFilenamePath = Last (Just [relfile|dump.rdb|])
            , useRDBCompression = Last (Just False)
            , genRdbChecksum = Last (Just True)
            , replicaOf = Last (Just (HostPort $ HostPortRec "10.0.0.2" ($$(refineTH 6379) :: Port)))
            }
        )
    ]

spec_redis_conf_document_loading :: Spec
spec_redis_conf_document_loading = do
    describe "loadRedisConfFile against sample redis.conf files" $ do
        for_ exampleConfFiles $ \(sampleName, expectedConfig) ->
            it ("Can Parse " <> sampleName <> ".conf into the expected PartialRedisConfig") $ do
                confFilePath <- mkSampleConfFilePath sampleName
                result <- runloadRedisConfFile confFilePath
                case result of
                    Left err -> expectationFailure ("loadRedisConfFile failed for " <> sampleName <> ", with error: " <> (show . pretty $ err))
                    Right (RedisConfigFromConfigFile actualConfig) -> actualConfig `shouldBe` expectedConfig

mkSampleConfFilePath :: String -> IO (Maybe RedisConfFilePath)
mkSampleConfFilePath sampleName = do
    projectRoot <- parseAbsDir =<< getCurrentDirectory
    relativeSamplePath <- parseRelFile ("test/Redis/Server/Config/Conf/input/" <> sampleName <> ".conf")
    pure . Just . RedisConfFilePath $ projectRoot </> relativeSamplePath

runloadRedisConfFile :: Maybe RedisConfFilePath -> IO (Either LoadConfigFileError RedisConfigFromConfigFile)
runloadRedisConfFile = Eff.runEff . Eff.runErrorNoCallStack . Eff.runFileSystem . Conf.loadRedisConfFile
