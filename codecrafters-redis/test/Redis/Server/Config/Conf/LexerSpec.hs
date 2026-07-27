{-# OPTIONS_GHC -Wno-x-unsafe-internals #-}

module Redis.Server.Config.Conf.LexerSpec where

import Test.Hspec

import Data.Attoparsec.Text qualified as AT

import Data.Either (isLeft)
import Data.String (fromString)
import Data.Text (Text)
import Redis.Server.Config.Conf.Lexer (RawRedisConfEntry (..), confDocumentParser, parseConfEntry)

sampleConf :: Text
sampleConf =
    fromString $
        unlines
            [ "# Redis uses default hardened security configuration directives to reduce the"
            , "# attack surface on innocent users. Therefore, several sensitive configuration"
            , "# directives are immutable, and some potentially-dangerous commands are blocked."
            , "#"
            , "# Configuration directives that control files that Redis writes to (e.g., 'dir'"
            , "# and 'dbfilename') and that aren't usually modified during runtime"
            , "# are protected by making them immutable."
            , "#"
            , "# Commands that can increase the attack surface of Redis and that aren't usually"
            , "# called by users are blocked by default."
            , "#"
            , "# These can be exposed to either all connections or just local ones by setting"
            , "# each of the configs listed below to either of these values:"
            , "#"
            , "# no    - Block for any connection (remain immutable)"
            , "# yes   - Allow for any connection (no protection)"
            , "# local - Allow only for local connections. Ones originating from the"
            , "#         IPv4 address (127.0.0.1), IPv6 address (::1) or Unix domain sockets."
            , "#"
            , ""
            , ""
            , "# enable-protected-configs no"
            , "# enable-debug-command no"
            , "# enable-module-command no"
            , "replicaof 127.0.0.1 6478 \"dwe nhbgb\" 'adcce referf erfer' # frer"
            , "jumbo yes jfr 1.2.34"
            , "# freiofnerifnerfienr"
            , "# frererre"
            , "port 8473"
            , "dir C:\\Program Files\\Redis\\data"
            , "dir2 \"C:\\Program Files\\Redis\\data\""
            , "dbfilename dump.rdb"
            , "# Hellooo"
            ]

spec_conf_entry_lexer_unit_tests :: Spec
spec_conf_entry_lexer_unit_tests = do
    describe "redis.conf entry lexer" do
        it "Succeeds when provided a simple key value entry" do
            let result = AT.parseOnly parseConfEntry "port 8473\n"
            result `shouldBe` Right (RawRedisConfEntry{key = "port", value = "8473"})

        it "Succeeds when provided an entry with multiple values" do
            let result = AT.parseOnly parseConfEntry "replicaof 127.0.0.1 6478\n"
            result `shouldBe` Right (RawRedisConfEntry{key = "replicaof", value = "127.0.0.1 6478"})

        it "Succeeds when provided an entry with a double quoted value containing spaces" do
            let result = AT.parseOnly parseConfEntry "dir \"C:\\Program Files\\Redis\\data\"\n"
            result `shouldBe` Right (RawRedisConfEntry{key = "dir", value = "\"C:\\Program Files\\Redis\\data\""})

        it "Succeeds when provided an entry with a single quoted value containing spaces" do
            let result = AT.parseOnly parseConfEntry "requirepass 'my secret pass'\n"
            result `shouldBe` Right (RawRedisConfEntry{key = "requirepass", value = "'my secret pass'"})

        it "Succeeds when provided an entry with a mix of quoted and unquoted values" do
            let result = AT.parseOnly parseConfEntry "replicaof 127.0.0.1 6478 \"dwe nhbgb\" 'adcce referf erfer'\n"
            result `shouldBe` Right (RawRedisConfEntry{key = "replicaof", value = "127.0.0.1 6478 \"dwe nhbgb\" 'adcce referf erfer'"})

        it "Succeeds when provided an entry with a trailing comment, discarding the comment" do
            let result = AT.parseOnly parseConfEntry "dbfilename dump.rdb # the rdb file's name\n"
            result `shouldBe` Right (RawRedisConfEntry{key = "dbfilename", value = "dump.rdb"})

        it "Fails when provided an entry without a value" do
            let result = AT.parseOnly parseConfEntry "port\n"
            result `shouldSatisfy` isLeft

        it "Fails when provided an empty input" do
            let result = AT.parseOnly parseConfEntry ""
            result `shouldSatisfy` isLeft

spec_conf_document_lexer_unit_tests :: Spec
spec_conf_document_lexer_unit_tests = do
    describe "redis.conf document lexer" do
        it "Extracts every config entry from the sample conf, skipping comment lines" do
            let result = AT.parseOnly confDocumentParser sampleConf
            let expected =
                    [ RawRedisConfEntry{key = "replicaof", value = "127.0.0.1 6478 \"dwe nhbgb\" 'adcce referf erfer'"}
                    , RawRedisConfEntry{key = "jumbo", value = "yes jfr 1.2.34"}
                    , RawRedisConfEntry{key = "port", value = "8473"}
                    , RawRedisConfEntry{key = "dir", value = "C:\\Program Files\\Redis\\data"}
                    , RawRedisConfEntry{key = "dir2", value = "\"C:\\Program Files\\Redis\\data\""}
                    , RawRedisConfEntry{key = "dbfilename", value = "dump.rdb"}
                    ]
            result `shouldBe` Right expected

        it "Extracts entries separated by a completely empty line" do
            let result = AT.parseOnly confDocumentParser "port 6379\n\nport 6380\n"
            let expected =
                    [ RawRedisConfEntry{key = "port", value = "6379"}
                    , RawRedisConfEntry{key = "port", value = "6380"}
                    ]
            result `shouldBe` Right expected

        it "Extracts entries separated by a line containing a single space" do
            let result = AT.parseOnly confDocumentParser "port 6379\n \nport 6380\n"
            let expected =
                    [ RawRedisConfEntry{key = "port", value = "6379"}
                    , RawRedisConfEntry{key = "port", value = "6380"}
                    ]
            result `shouldBe` Right expected

        it "Extracts entries separated by a line containing multiple spaces" do
            let result = AT.parseOnly confDocumentParser "port 6379\n   \nport 6380\n"
            let expected =
                    [ RawRedisConfEntry{key = "port", value = "6379"}
                    , RawRedisConfEntry{key = "port", value = "6380"}
                    ]
            result `shouldBe` Right expected

        it "Extracts entries separated by a line containing a single tab" do
            let result = AT.parseOnly confDocumentParser "port 6379\n\t\nport 6380\n"
            let expected =
                    [ RawRedisConfEntry{key = "port", value = "6379"}
                    , RawRedisConfEntry{key = "port", value = "6380"}
                    ]
            result `shouldBe` Right expected

        it "Extracts entries separated by multiple consecutive blank lines" do
            let result = AT.parseOnly confDocumentParser "port 6379\n\n\n   \nport 6380\n"
            let expected =
                    [ RawRedisConfEntry{key = "port", value = "6379"}
                    , RawRedisConfEntry{key = "port", value = "6380"}
                    ]
            result `shouldBe` Right expected
