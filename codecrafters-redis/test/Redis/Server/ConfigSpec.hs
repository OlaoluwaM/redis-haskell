{-# OPTIONS_GHC -Wno-x-unsafe-internals #-}

module Redis.Server.ConfigSpec where

import Test.Hspec

import Path (SomeBase (..), absdir, relfile)
import Redis.Server.Config (mkCompleteRedisConfig)
import Redis.Server.Config.CommandLine (RedisConfigFromCommandLine (..))
import Redis.Server.Config.Conf (RedisConfigFromConfigFile (..))
import Redis.Server.Config.Defaults (
    DefaultRedisConfig (..),
    defaultRedisConfig,
    emptyPartialRedisConfig,
 )
import Redis.Server.Config.Types (RedisConfig, RedisConfigF (..))

defaults :: RedisConfig
defaults = defaultRedisConfig.redisConf

spec_mk_complete_redis_config :: Spec
spec_mk_complete_redis_config = do
    describe "mkCompleteRedisConfig" $ do
        it "Returns default config if neither config file or cli configs are available" $
            mkCompleteRedisConfig (RedisConfigFromCommandLine emptyPartialRedisConfig) (RedisConfigFromConfigFile emptyPartialRedisConfig) `shouldBe` defaults

        it "Takes a config value from the config file if there is no corresponding command line override" $
            let expectedPort = 7000
             in mkCompleteRedisConfig (RedisConfigFromCommandLine emptyPartialRedisConfig) (RedisConfigFromConfigFile emptyPartialRedisConfig{port = pure expectedPort})
                    `shouldBe` defaults{port = expectedPort}

        it "Takes a config value from the command line if the config file leaves it unset" $
            let expectedPort = 6380
             in mkCompleteRedisConfig (RedisConfigFromCommandLine emptyPartialRedisConfig{port = pure expectedPort}) (RedisConfigFromConfigFile emptyPartialRedisConfig)
                    `shouldBe` defaults{port = expectedPort}

        it "Command line config values override those from the config file" $ do
            let fromConfigFile =
                    RedisConfigF
                        { rdbFileDirPath = pure (Abs [absdir|/var/lib/redis|])
                        , rdbFilenamePath = pure [relfile|from-file.rdb|]
                        , useRDBCompression = pure False
                        , genRdbChecksum = pure True
                        , port = pure 7000
                        }
                fromCommandLine =
                    RedisConfigF
                        { rdbFileDirPath = pure (Abs [absdir|/data|])
                        , rdbFilenamePath = pure [relfile|from-cli.rdb|]
                        , useRDBCompression = pure True
                        , genRdbChecksum = pure False
                        , port = pure 6380
                        }
            mkCompleteRedisConfig (RedisConfigFromCommandLine fromCommandLine) (RedisConfigFromConfigFile fromConfigFile)
                `shouldBe` RedisConfigF
                    { rdbFileDirPath = Abs [absdir|/data|]
                    , rdbFilenamePath = [relfile|from-cli.rdb|]
                    , useRDBCompression = True
                    , genRdbChecksum = False
                    , port = 6380
                    }

        it "Resolves each field independently across all three sources" $ do
            let fromConfigFile =
                    emptyPartialRedisConfig
                        { rdbFileDirPath = pure (Abs [absdir|/var/lib/redis|])
                        , rdbFilenamePath = pure [relfile|from-file.rdb|]
                        , port = pure 7000
                        }
                fromCommandLine =
                    emptyPartialRedisConfig
                        { port = pure 6380
                        , useRDBCompression = pure True
                        }
            mkCompleteRedisConfig (RedisConfigFromCommandLine fromCommandLine) (RedisConfigFromConfigFile fromConfigFile)
                `shouldBe` defaults
                    { rdbFileDirPath = Abs [absdir|/var/lib/redis|]
                    , rdbFilenamePath = [relfile|from-file.rdb|]
                    , port = 6380
                    , useRDBCompression = True
                    }

        it "Uses no defaults when every field is overridden" $ do
            let fromCommandLine =
                    emptyPartialRedisConfig
                        { rdbFileDirPath = pure (Abs [absdir|/data|])
                        , rdbFilenamePath = pure [relfile|override.rdb|]
                        , useRDBCompression = pure True
                        , genRdbChecksum = pure False
                        , port = pure 9999
                        }
            mkCompleteRedisConfig (RedisConfigFromCommandLine fromCommandLine) (RedisConfigFromConfigFile emptyPartialRedisConfig)
                `shouldBe` RedisConfigF
                    { rdbFileDirPath = Abs [absdir|/data|]
                    , rdbFilenamePath = [relfile|override.rdb|]
                    , useRDBCompression = True
                    , genRdbChecksum = False
                    , port = 9999
                    }
