module Helpers where

import RESP.Types

import Data.ByteString.Char8 qualified as BS

import Control.Applicative ((<|>))
import Data.Attoparsec.Types (Parser)
import Data.ByteString (ByteString)

withCustomError :: Parser i a -> String -> Parser i a
withCustomError p s = p <|> fail s

toOptionString :: [BulkString] -> ByteString
toOptionString = BS.intercalate " " . map bulkStringToByteString
  where
    bulkStringToByteString NullBulkString = ""
    bulkStringToByteString (BulkString bytes) = bytes
