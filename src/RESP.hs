module RESP (
    RESPDataType (..),
    mkNonNullBulkString,
    serializeRESPDataType,
    BulkString (..),
    bulkStringParser,
    terminatorSeqParser,
    mkNonNullRESPArray,
    toOptionString,
) where

import Data.Attoparsec.ByteString.Char8 qualified as AC
import Data.ByteString (ByteString)
import Data.ByteString.Char8 qualified as BS
import Data.Text (Text)
import Data.Vector (Vector)
import Data.Vector qualified as V

import Control.Monad (mfilter, when)
import Control.Monad.Trans.Class (MonadTrans (lift))
import Control.Monad.Trans.Except (runExceptT, throwE)
import Data.Attoparsec.ByteString (Parser)
import Data.String.Interpolate (i)
import Helpers (withCustomError)
import Utils (fromEither)

-- Spec: https://redis.io/docs/latest/develop/reference/protocol-spec/

-- All supported RESP data types
data RESPDataType = SimpleString Text | MkBulkStringResponse BulkString | MkArrayResponse Array | RESPInteger Int | Null
    deriving stock (Eq, Show)

data BulkString = BulkString ByteString | NullBulkString
    deriving stock (Eq, Show)

-- This type represents an RESP data type array as a response value, NOT as a carrier for a command/request.
data Array = Array (Vector RESPDataType) | NullArray
    deriving stock (Eq, Show)

-- NOTE: We only need parsers for the things coming FROM the client (excluding arrays) bulk strings
terminatorSeqParser :: Parser ()
terminatorSeqParser = AC.endOfLine

bulkStringParser :: Parser BulkString
bulkStringParser = (fromEither <$>) $ runExceptT $ do
    len <- lift $ withCustomError (AC.char '$' *> AC.signed AC.decimal <* terminatorSeqParser) "Invalid RESP BulkString string"
    when (len < 0) $ throwE NullBulkString

    isEndOfInput <- lift AC.atEnd

    lift $ case (len == 0, isEndOfInput) of
        (False, True) -> fail "BulkString is not supposed to be empty"
        _ -> pure ()

    mainStr <- lift $ withCustomError ((BS.all notTerminatorSeq `mfilter` AC.take len) <* terminatorSeqParser) (genLenMismatchErr len)
    -- lift $ withCustomError terminatorSeqParser (genLenMismatchErr len)
    pure $ BulkString mainStr
  where
    notTerminatorSeq :: Char -> Bool
    notTerminatorSeq c = not $ c == '\r' || c == '\n'

    genLenMismatchErr :: Int -> String
    genLenMismatchErr len' = [i|There seems to be mismatch between the purported BulkString length (#{len'}) and it's actual length or perhaps the contents of your bulk string contain a sequence terminator such as '\r' or '\n'|]

serializeRESPDataType :: RESPDataType -> ByteString
serializeRESPDataType = \case
    SimpleString txt -> [i|+#{txt}#{seqTerminator}|]
    MkBulkStringResponse (BulkString bytes) -> let len = BS.length bytes in [i|$#{len}#{seqTerminator}#{bytes}#{seqTerminator}|]
    MkBulkStringResponse NullBulkString -> [i|$-1#{seqTerminator}|]
    RESPInteger int -> [i|:#{int}#{seqTerminator}|]
    Null -> [i|_#{seqTerminator}|]
    MkArrayResponse NullArray -> [i|*-1#{seqTerminator}|]
    MkArrayResponse (Array otherResTypes) ->
        let arrLen = V.length otherResTypes
            serializedElems = V.foldr (\a b -> serializeRESPDataType a <> b) "" otherResTypes
         in [i|*#{arrLen}#{seqTerminator}#{serializedElems}|]

seqTerminator :: ByteString
seqTerminator = "\r\n"

mkNonNullBulkString :: ByteString -> RESPDataType
mkNonNullBulkString = MkBulkStringResponse . BulkString

mkNonNullRESPArray :: [RESPDataType] -> RESPDataType
mkNonNullRESPArray = MkArrayResponse . Array . V.fromList

toOptionString :: [BulkString] -> ByteString
toOptionString = BS.intercalate " " . map bulkStringToByteString
  where
    bulkStringToByteString NullBulkString = ""
    bulkStringToByteString (BulkString bytes) = bytes
