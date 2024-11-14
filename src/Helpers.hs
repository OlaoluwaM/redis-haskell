module Helpers where

import RESP.Types

import Data.Attoparsec.ByteString.Char8 qualified as AC
import Data.ByteString.Char8 qualified as BS

import Control.Applicative ((<|>))
import Data.Attoparsec.Types (Parser)
import Data.ByteString (ByteString)
import Data.Char (toLower, toUpper)
import Data.String (IsString (..))
import Data.String.Interpolate (i)

withCustomError :: Parser i a -> String -> Parser i a
withCustomError p s = p <|> fail s

toOptionString :: [BulkString] -> ByteString
toOptionString = BS.intercalate " " . map bulkStringToByteString
  where
    bulkStringToByteString NullBulkString = ""
    bulkStringToByteString (BulkString bytes) = bytes

-- Match the lowercase or uppercase form of 'c'

caseInsensitiveChar :: Char -> Parser ByteString Char
caseInsensitiveChar c = AC.char (toLower c) <|> AC.char (toUpper c)

-- Match the string 's', accepting either lowercase or uppercase form of each character
caseInsensitiveString :: (IsString a) => String -> Parser ByteString a
caseInsensitiveString s = fromString <$> (mapM caseInsensitiveChar s AC.<?> [i|Case insensitive parser for #{s}|])
