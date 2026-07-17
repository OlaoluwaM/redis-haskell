module Redis.Server.Config.Conf.Lexer where

import Data.Attoparsec.Text qualified as AT
import Data.Vector qualified as V
import Path

import Control.Applicative (Alternative ((<|>)), many, optional, some)
import Control.Applicative.Combinators (between, sepBy1, someTill)
import Control.Monad (void)
import Data.Attoparsec.Text (Parser)
import Data.List.Extra (trim)
import Data.String (IsString (fromString))
import Data.Text (Text)
import Data.Vector (Vector)

-- The purpose of this module is to add in redis.conf support (not redis-full.conf) to our server, at least partial support to complete the Settings system. Have it function like a proper redis server, being able to accept both arguments and a conf and reconcile overlaps as expected

-- TODO: Typed parsing. Have the settings be typed and not an untyped hashmap

data RawRedisConfig = RawRedisConfig
    { key :: Text
    , value :: Vector Text
    }

type ConfKey = Text
type ConfVal = Vector Text

parseConfEntry :: Parser RawRedisConfig
parseConfEntry = do
    key <- parseConfKey
    val <- parseConfVal <* optional (spaceOrTab *> skipComment)
    AT.endOfLine
    pure $ RawRedisConfig{key, value = val}
  where
    parseConfKey :: Parser ConfKey
    parseConfKey = fromString . trim <$> someTill (anyCharExcept "\n") spaceOrTab

    parseConfVal :: Parser ConfVal
    parseConfVal = V.fromList . fmap (fromString . trim) <$> (doubleQuotedConfVal <|> singleQuotedConfVal <|> unquotedConfVal) `sepBy1` spaceOrTab

    doubleQuotedConfVal :: Parser String
    doubleQuotedConfVal = between (AT.char '"') (AT.char '"') (some (anyCharExcept "\n#\""))

    singleQuotedConfVal :: Parser String
    singleQuotedConfVal = between (AT.char '\'') (AT.char '\'') (some (anyCharExcept "\n#\'"))

    unquotedConfVal :: Parser String
    unquotedConfVal = some (anyCharExcept "\n #")

skipComment :: Parser ()
skipComment = void $ AT.char '#' *> AT.takeWhile (anyCharExceptFn "\n")

anyCharExcept :: [Char] -> Parser Char
anyCharExcept chars = AT.satisfy (anyCharExceptFn chars)

anyCharExceptFn :: [Char] -> (Char -> Bool)
anyCharExceptFn xs c = c `notElem` xs

spaceOrTab :: Parser Char
spaceOrTab = AT.satisfy isSpace
  where
    isSpace :: Char -> Bool
    isSpace c = c == ' ' || c == '\t'

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
            , "# enable-protected-configs no"
            , "# enable-debug-command no"
            , "# enable-module-command no"
            , "replicaof 127.0.0.1 6478 \"dwe nhbgb\" 'adcce referf erfer' # frer"
            , "# freiofnerifnerfienr"
            , "# frererre"
            , "port 8473"
            , "dir \"C:\\Program Files\\Redis\\data\""
            , "dbfilename dump.rdb"
            , "# Hellooo"
            ]

--- >>> AT.parseOnly (many (fmap (const Nothing) (skipComment <* AT.endOfLine) <|> fmap Just parseConfEntry)) sampleConf
-- Right [Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Just ("replicaof",["127.0.0.1","6478","dwe nhbgb","adcce referf erfer"]),Nothing,Nothing,Just ("port",["8473"]),Just ("dir",["C:\\Program Files\\Redis\\data"]),Just ("dbfilename",["dump.rdb"]),Nothing]
