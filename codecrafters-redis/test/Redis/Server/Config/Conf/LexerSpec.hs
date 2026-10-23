{-# OPTIONS_GHC -Wno-x-unsafe-internals #-}

module Redis.Server.Config.Conf.LexerSpec where

import Test.Hspec

import Data.Attoparsec.Text qualified as AT
import Data.HashMap.Strict qualified as HashMap
import Data.List.NonEmpty qualified as NE

import Data.Either (isLeft)
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.String (fromString)
import Data.Text (Text)
import Redis.Server.Config.Conf.Lexer (RawRedisConfEntriesMap (..), confDocumentParser, parseConfEntry)

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

{- |
    A conf where several keys are repeated, spread out across the document with comments and blank lines in between.
    Every repeated key has a distinct value per occurrence so a test can tell which one ended up where.
    Only directives the server currently supports are used. The occurrence order, top to bottom, is:

        port:            6379, 6380, 6381
        replicaof:       10.0.0.1 6379, 10.0.0.2 6379
        dir:             /var/lib/redis, /tmp/redis, /data/redis
        rdbcompression:  yes, no
        rdbchecksum:     no, yes
        dbfilename:      dump.rdb (single occurrence, as a control)
-}
duplicateKeysConf :: Text
duplicateKeysConf =
    fromString $
        unlines
            [ "# Baseline settings"
            , "port 6379"
            , "dir /var/lib/redis"
            , "dbfilename dump.rdb"
            , "rdbcompression yes"
            , ""
            , "# Replication"
            , "replicaof 10.0.0.1 6379"
            , "rdbchecksum no"
            , ""
            , "# Overrides added later by an operator"
            , "port 6380 # moved off the default"
            , "dir /tmp/redis"
            , "rdbcompression no"
            , ""
            , ""
            , "# Final overrides, these should win"
            , "replicaof 10.0.0.2 6379"
            , "dir /data/redis"
            , "rdbchecksum yes"
            , "port 6381"
            , "# trailing comment"
            ]

mkRawRedisConfEntriesMap :: [(Text, NonEmpty Text)] -> RawRedisConfEntriesMap
mkRawRedisConfEntriesMap = RawRedisConfEntriesMap . HashMap.fromList

spec_conf_file_entry_lexer_unit_tests :: Spec
spec_conf_file_entry_lexer_unit_tests = do
    describe "redis.conf config entry lexer" do
        it "Can parse a simple key value entry" do
            let result = AT.parseOnly parseConfEntry "port 8473\n"
            result `shouldBe` Right ("port", "8473")

        it "Can parse an entry with multiple values" do
            let result = AT.parseOnly parseConfEntry "replicaof 127.0.0.1 6478\n"
            result `shouldBe` Right ("replicaof", "127.0.0.1 6478")

        it "Can parse an entry with a double quoted value containing spaces" do
            let result = AT.parseOnly parseConfEntry "dir \"C:\\Program Files\\Redis\\data\"\n"
            result `shouldBe` Right ("dir", "C:\\Program Files\\Redis\\data")

        it "Can parse an entry with a single quoted value containing spaces" do
            let result = AT.parseOnly parseConfEntry "requirepass 'my secret pass'\n"
            result `shouldBe` Right ("requirepass", "my secret pass")

        it "Can parse an entry with values containing a mix of quoted and unquoted values" do
            let result = AT.parseOnly parseConfEntry "replicaof 127.0.0.1 6478 \"dwe nhbgb\" 'adcce referf erfer'\n"
            result `shouldBe` Right ("replicaof", "127.0.0.1 6478 dwe nhbgb adcce referf erfer")

        it "Can parse an entry with a trailing comment, discarding the comment" do
            let result = AT.parseOnly parseConfEntry "dbfilename dump.rdb # the rdb file's name\n"
            result `shouldBe` Right ("dbfilename", "dump.rdb")

        it "Doesn't parse an entry without a value" do
            let result = AT.parseOnly parseConfEntry "port\n"
            result `shouldSatisfy` isLeft

        it "Doesn't parse an empty string as an entry" do
            let result = AT.parseOnly parseConfEntry ""
            result `shouldSatisfy` isLeft

spec_conf_document_lexer_unit_tests :: Spec
spec_conf_document_lexer_unit_tests = do
    describe "Lexer on mock redis.conf document" do
        it "Can extract every config entry from the sample conf, skipping comment lines" do
            let result = AT.parseOnly confDocumentParser sampleConf
            let expected =
                    mkRawRedisConfEntriesMap
                        [ ("replicaof", NE.singleton "127.0.0.1 6478 dwe nhbgb adcce referf erfer")
                        , ("jumbo", NE.singleton "yes jfr 1.2.34")
                        , ("port", NE.singleton "8473")
                        , ("dir", NE.singleton "C:\\Program Files\\Redis\\data")
                        , ("dir2", NE.singleton "C:\\Program Files\\Redis\\data")
                        , ("dbfilename", NE.singleton "dump.rdb")
                        ]
            result `shouldBe` Right expected

        it "Produces an empty map for an empty document" do
            let result = AT.parseOnly confDocumentParser ""
            result `shouldBe` Right (mkRawRedisConfEntriesMap [])

        it "Produces an empty map for a document containing only comments and blank lines" do
            let result = AT.parseOnly confDocumentParser "# just a comment\n\n   \n# another one\n"
            result `shouldBe` Right (mkRawRedisConfEntriesMap [])

        it "Collects duplicate keys under one entry, latest instance first" do
            let result = AT.parseOnly confDocumentParser "port 6379\nport 6380\nport 6381\n"
            let expected = mkRawRedisConfEntriesMap [("port", "6381" :| ["6380", "6379"])]
            result `shouldBe` Right expected

        it "Keeps duplicates of different keys separate" do
            let result = AT.parseOnly confDocumentParser "port 6379\nsave 3600 1\nport 6380\nsave 300 100\n"
            let expected =
                    mkRawRedisConfEntriesMap
                        [ ("port", "6380" :| ["6379"])
                        , ("save", "300 100" :| ["3600 1"])
                        ]
            result `shouldBe` Right expected

        it "Can extract entries separated by a completely empty line" do
            let result = AT.parseOnly confDocumentParser "port 6379\n\nport 6380\n"
            let expected = mkRawRedisConfEntriesMap [("port", "6380" :| ["6379"])]
            result `shouldBe` Right expected

        it "Can extract entries separated by an empty line followed by a single space" do
            let result = AT.parseOnly confDocumentParser "port 6379\n \nport 6380\n"
            let expected = mkRawRedisConfEntriesMap [("port", "6380" :| ["6379"])]
            result `shouldBe` Right expected

        it "Can extract entries separated by an empty line followed by multiple spaces" do
            let result = AT.parseOnly confDocumentParser "port 6379\n   \nport 6380\n"
            let expected = mkRawRedisConfEntriesMap [("port", "6380" :| ["6379"])]
            result `shouldBe` Right expected

        it "Can extract entries separated by an empty line followed by a single tab" do
            let result = AT.parseOnly confDocumentParser "port 6379\n\t\nport 6380\n"
            let expected = mkRawRedisConfEntriesMap [("port", "6380" :| ["6379"])]
            result `shouldBe` Right expected

        it "Can extract entries separated by multiple consecutive empty lines" do
            let result = AT.parseOnly confDocumentParser "port 6379\n\n\n   \nport 6380\n"
            let expected = mkRawRedisConfEntriesMap [("port", "6380" :| ["6379"])]
            result `shouldBe` Right expected

        it "Can parse out duplicate entries in order from latest instance to oldest" do
            let result = AT.parseOnly confDocumentParser duplicateKeysConf
            let expected =
                    mkRawRedisConfEntriesMap
                        [ ("port", "6381" :| ["6380", "6379"])
                        , ("replicaof", "10.0.0.2 6379" :| ["10.0.0.1 6379"])
                        , ("dir", "/data/redis" :| ["/tmp/redis", "/var/lib/redis"])
                        , ("rdbcompression", "no" :| ["yes"])
                        , ("rdbchecksum", "yes" :| ["no"])
                        , ("dbfilename", NE.singleton "dump.rdb")
                        ]
            result `shouldBe` Right expected
