module Redis.Server.Config.Conf.Lexer (
    RawRedisConfEntry (..),
    confDocumentParser,

    -- ** For testing
    parseConfEntry,
) where

import Data.Attoparsec.Text qualified as AT
import Data.Text qualified as T

import Control.Applicative (Alternative ((<|>)), many, optional, some)
import Control.Applicative.Combinators (sepBy1, someTill)
import Control.Monad (void)
import Data.Attoparsec.Text (Parser)
import Data.List.Extra (trim)
import Data.Maybe (catMaybes)
import Data.String (IsString (fromString))
import Data.Text (Text)
import Redis.Helpers (withCustomError)

-- The purpose of this module is to add in redis.conf support (not redis-full.conf) to our server, at least partial support to complete the Settings system. Have it function like a proper redis server, being able to accept both arguments and a conf and reconcile overlaps as expected

type EntryKey = Text
type EntryVal = Text

data RawRedisConfEntry = RawRedisConfEntry
    { key :: EntryKey
    , value :: EntryVal
    }
    deriving stock (Show, Eq)

confDocumentParser :: Parser [RawRedisConfEntry]
confDocumentParser =
    let skipOverComment = Nothing <$ (comment <* AT.endOfLine)
        blankLine = Nothing <$ (AT.takeWhile (noneExceptFn [' ', '\t']) <* AT.endOfLine)
        parseConfigEntry = Just <$> parseConfEntry
     in -- The 'many' here makes this parser very permissive, allowing for zero or more configuration entries interspersed with comments
        catMaybes <$> many (skipOverComment <|> parseConfigEntry <|> blankLine)

{-# WARNING in "x-unsafe-internals" parseConfEntry "This value is exported for testing purposes only" #-}
parseConfEntry :: Parser RawRedisConfEntry
parseConfEntry = do
    key <- withCustomError parseConfKey "Failed to parse configuration key"
    val <- parseConfVal <* optional (spaceOrTab *> comment)
    withCustomError AT.endOfLine "Failed to parse end of line after configuration entry"
    pure $ RawRedisConfEntry{key, value = val}
  where
    parseConfKey :: Parser EntryKey
    parseConfKey = fromString . trim <$> someTill (anyCharExcept "\n") spaceOrTab

    parseConfVal :: Parser EntryVal
    parseConfVal = T.intercalate " " . fmap T.strip <$> (doubleQuotedConfVal <|> singleQuotedConfVal <|> unquotedConfVal) `sepBy1` spaceOrTab

    doubleQuotedConfVal :: Parser Text
    doubleQuotedConfVal = let parser = doubleQuotedText (fromString <$> some (anyCharExcept "\n#\"")) in withCustomError parser "Failed to parse double quoted configuration value"

    singleQuotedConfVal :: Parser Text
    singleQuotedConfVal = let parser = singleQuotedText (fromString <$> some (anyCharExcept "\n#\'")) in withCustomError parser "Failed to parse single quoted configuration value"

    unquotedConfVal :: Parser Text
    unquotedConfVal = let parser = fromString <$> some (anyCharExcept "\n #") in withCustomError parser "Failed to parse unquoted configuration value"

comment :: Parser ()
comment = void $ AT.char '#' *> AT.takeWhile (anyCharExceptFn "\n")

noneExcept :: [Char] -> Parser Char
noneExcept chars = AT.satisfy (`elem` chars)

noneExceptFn :: [Char] -> (Char -> Bool)
noneExceptFn chars c = c `elem` chars

anyCharExcept :: [Char] -> Parser Char
anyCharExcept chars = AT.satisfy (anyCharExceptFn chars)

anyCharExceptFn :: [Char] -> (Char -> Bool)
anyCharExceptFn xs c = c `notElem` xs

doubleQuotedText :: Parser Text -> Parser Text
doubleQuotedText p = (\startQuote content endQuote -> startQuote <> content <> endQuote) <$> AT.string "\"" <*> p <*> AT.string "\""

singleQuotedText :: Parser Text -> Parser Text
singleQuotedText p = (\startQuote content endQuote -> startQuote <> content <> endQuote) <$> AT.string "'" <*> p <*> AT.string "'"

spaceOrTab :: Parser Char
spaceOrTab = AT.satisfy isSpace
  where
    isSpace :: Char -> Bool
    isSpace c = c == ' ' || c == '\t'
