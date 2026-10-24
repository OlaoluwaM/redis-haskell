module Redis.Server.Config.Conf.Lexer (
    confDocumentParser,
    RawRedisConfEntriesMap (..),

    -- ** For testing
    parseConfEntry,
) where

import Data.Attoparsec.Text qualified as AT
import Data.HashMap.Strict qualified as HashMap
import Data.List.NonEmpty qualified as NE
import Data.Text qualified as T

import Control.Applicative (Alternative ((<|>)), many, optional, some)
import Control.Applicative.Combinators (sepBy1, someTill)
import Control.Monad (void)
import Data.Attoparsec.Text (Parser)
import Data.HashMap.Strict (HashMap)
import Data.List (foldl')
import Data.List.Extra (trim)
import Data.List.NonEmpty (NonEmpty)
import Data.Maybe (catMaybes)
import Data.String (IsString (fromString))
import Data.Text (Text)
import Redis.Helpers (withCustomError)

-- The purpose of this module is to add in redis.conf support (not redis-full.conf) to our server, at least partial support to complete the Settings system. Have it function like a proper redis server, being able to accept both arguments and a conf and reconcile overlaps as expected

{- |
    Ok, you may be wondering, why are we structuring this as a map? Well, originally we had a list that we'd just look up values in, but duplicates became a problem, specifically handling duplicates deterministically, as in, in the face of duplicates always use the last instance: this is how redis does it at least.

    However, to throw a wrench into things, redis has some config entries like `include`, `rename-command`, `user`, `save`, loadmodule`, with different semantics when it comes to duplicates. In those cases it doesn't just take the last instance, so having a map where we collect all the values for duplicate instances of a given key (latest/last instance first, oldest/first instance last) allows us to better handle these different semantics later on
-}
newtype RawRedisConfEntriesMap = RawRedisConfEntriesMap
    { hmap :: HashMap Text (NonEmpty Text)
    }
    deriving stock (Show, Eq)

confDocumentParser :: Parser RawRedisConfEntriesMap
confDocumentParser =
    let skipOverComment = Nothing <$ (comment <* AT.endOfLine)
        blankLine = Nothing <$ (AT.takeWhile (noneExceptFn [' ', '\t']) <* AT.endOfLine)
        parseConfigEntry = Just <$> parseConfEntry
        -- The 'many' here makes this parser very permissive, allowing for zero or more configuration entries interspersed with comments
        parseConfigEntries = catMaybes <$> many (skipOverComment <|> parseConfigEntry <|> blankLine)
        collectConfEntries hmap (confEntryKey, latestInstance) =
            HashMap.alter
                ( \case
                    Nothing -> Just $ NE.singleton latestInstance
                    Just olderInstances -> Just $ latestInstance NE.<| olderInstances
                )
                confEntryKey
                hmap
     in RawRedisConfEntriesMap . foldl' collectConfEntries HashMap.empty
            <$> parseConfigEntries

{-# WARNING in "x-unsafe-internals" parseConfEntry "This value is exported for testing purposes only" #-}
parseConfEntry :: Parser (Text, Text)
parseConfEntry = do
    key <- withCustomError parseConfKey "Failed to parse configuration key"
    val <- parseConfVal <* optional (spaceOrTab *> comment)
    withCustomError AT.endOfLine "Failed to parse end of line after configuration entry"
    pure (key, val)
  where
    parseConfKey :: Parser Text
    parseConfKey = fromString . trim <$> someTill (anyCharExcept "\n") spaceOrTab

    parseConfVal :: Parser Text
    parseConfVal = T.intercalate " " . fmap T.strip <$> (doubleQuotedConfVal <|> singleQuotedConfVal <|> unquotedConfVal) `sepBy1` spaceOrTab

    doubleQuotedConfVal :: Parser Text
    doubleQuotedConfVal = let parser = doubleQuotedText (fromString <$> some (anyCharExcept "\n#\"")) in withCustomError parser "Failed to parse double quoted configuration value"

    singleQuotedConfVal :: Parser Text
    singleQuotedConfVal = let parser = singleQuotedText (fromString <$> some (anyCharExcept "\n#\'")) in withCustomError parser "Failed to parse single quoted configuration value"

    unquotedConfVal :: Parser Text
    unquotedConfVal = let parser = fromString <$> some (anyCharExcept "\n #") in withCustomError parser "Failed to parse unquoted configuration value"

comment :: Parser ()
comment = void $ AT.char '#' *> AT.takeWhile (anyCharExceptFn "\n")

noneExceptFn :: [Char] -> (Char -> Bool)
noneExceptFn chars c = c `elem` chars

anyCharExcept :: [Char] -> Parser Char
anyCharExcept chars = AT.satisfy (anyCharExceptFn chars)

anyCharExceptFn :: [Char] -> (Char -> Bool)
anyCharExceptFn xs c = c `notElem` xs

doubleQuotedText :: Parser Text -> Parser Text
doubleQuotedText p = AT.string "\"" *> p <* AT.string "\""

singleQuotedText :: Parser Text -> Parser Text
singleQuotedText p = AT.string "'" *> p <* AT.string "'"

spaceOrTab :: Parser Char
spaceOrTab = AT.satisfy isSpace
  where
    isSpace :: Char -> Bool
    isSpace c = c == ' ' || c == '\t'
