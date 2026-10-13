{-# OPTIONS_GHC -Wno-x-unsafe-internals #-}

module Redis.Server.ConfigSpec where

import Test.Hspec

import Path (SomeBase (..), absdir, absfile, relfile)
import Redis.Server.Config (mkCompleteRedisConfig, parseRedisConfFilePath)
import Redis.Server.Config.CommandLine (RedisConfigFromCommandLine (..))
import Redis.Server.Config.Conf (RedisConfigFromConfigFile (..))
import Redis.Server.Config.Defaults (
    DefaultRedisConfig (..),
    defaultRedisConfig,
    emptyPartialRedisConfig,
 )
import Redis.Server.Config.Types (RedisConfig, RedisConfigF (..))
import Redis.Server.Metadata (RedisConfFilePath (..))
import Redis.Utils (runReadM)

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

spec_parse_redis_conf_file_path :: Spec
spec_parse_redis_conf_file_path = do
    describe "parseRedisConfFilePath" $ do
        it "accepts an absolute redis.conf path as-is" $
            runReadM @String (parseRedisConfFilePath "/tmp/redis") "/etc/redis/redis.conf" `shouldBe` Right (RedisConfFilePath [absfile|/etc/redis/redis.conf|])

        it "rejects an absolute config path that is not named redis.conf" $
            runReadM @String (parseRedisConfFilePath "/tmp/redis") "/etc/redis/not-redis.conf" `shouldBe` Left "The file provided is not named redis.conf"

        it "resolves a relative redis.conf path against the provided cwd" $
            runReadM @String (parseRedisConfFilePath "/tmp/redis") "redis.conf" `shouldBe` Right (RedisConfFilePath [absfile|/tmp/redis/redis.conf|])

        it "resolves a nested relative redis.conf path against the provided cwd" $
            runReadM @String (parseRedisConfFilePath "/tmp/redis") "conf/redis.conf" `shouldBe` Right (RedisConfFilePath [absfile|/tmp/redis/conf/redis.conf|])

        it "rejects a relative config path that is not named redis.conf" $
            runReadM @String (parseRedisConfFilePath "/tmp/redis") "not-redis.conf" `shouldBe` Left "The file provided is not named redis.conf"

        it "rejects a relative redis.conf path when the cwd is not an absolute directory" $
            runReadM @String (parseRedisConfFilePath "not-an-absolute-dir") "redis.conf" `shouldBe` Left "Invalid cwd: not-an-absolute-dir"
