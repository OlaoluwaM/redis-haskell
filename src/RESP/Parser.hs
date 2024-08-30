module RESP.Parser where

import RESP.Types

import Data.Attoparsec.ByteString.Char8 qualified as AC
import Data.ByteString.Char8 qualified as BS
import Data.Vector qualified as V

import Control.Monad (mfilter, when)
import Control.Monad.Trans.Class (MonadTrans (lift))
import Control.Monad.Trans.Except
import Data.Attoparsec.ByteString (Parser)
import Data.ByteString (ByteString)
import Helpers (withCustomError)
import Data.String.Interpolate ( i )
import Utils (fromEither)

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

    mainStr <- lift $ withCustomError (BS.all notTerminatorSeq `mfilter` AC.take len) (genLenMismatchErr len)
    lift $ withCustomError terminatorSeqParser (genLenMismatchErr len)
    pure $ BulkString mainStr
  where
    notTerminatorSeq c = not $ c == '\r' || c == '\n'
    genLenMismatchErr len' =
        [i|There seems to be mismatch between the purported BulkString length (#{len'}) and it's actual length or perhaps the contents of your bulk string contain a sequence terminator such as '\r' or '\n'|]

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
