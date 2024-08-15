module RESP.Parser where

import Control.Applicative.Combinators
import RESP.Types

import Data.Attoparsec.ByteString.Char8 qualified as AC
import Data.Text qualified as T
import Data.Vector qualified as V

import Control.Monad (when)
import Control.Monad.Except (MonadError (throwError), MonadTrans (lift), runExceptT)
import Data.Attoparsec.ByteString (Parser)
import Data.ByteString (ByteString)
import Helpers (withCustomError)
import PyF (fmt)
import Utils (fromEither)

-- TODO Add tests for this module
-- NOTE: We only need parsers for the things coming FROM the client (excluding arrays) bulk strings

terminatorSeqParser :: Parser ()
terminatorSeqParser = AC.endOfLine

bulkStringParser :: Parser BulkString
bulkStringParser = (fromEither <$>) $ runExceptT $ do
    strLength <- lift $ withCustomError (AC.char '$' *> AC.signed AC.decimal <* terminatorSeqParser) "Invalid RESP BulkString string"
    when (strLength < 0) $ throwError NullBulkString

    isEndOfInput <- lift AC.atEnd

    case (strLength == 0, isEndOfInput) of
        (False, True) -> fail "BulkString is not supposed to be empty"
        (True, False) -> fail "BulkString is supposed to be empty"
        _ -> pure ()

    mainStr <- lift $ withCustomError (count strLength AC.anyChar) [fmt|There seems to be mismatch between the purported BulkString length ({strLength}) and it's actual length|]
    lift terminatorSeqParser
    pure $ BulkString $ T.pack mainStr

serializeRESPDataType :: RESPDataType -> ByteString
serializeRESPDataType = \case
    SimpleString txt -> [fmt|+{txt}{seqTerminator}|]
    MkBulkStringResponse (BulkString txt) -> let len = T.length txt in [fmt|${len}{seqTerminator}{txt}{seqTerminator}|]
    MkBulkStringResponse NullBulkString -> [fmt|$-1{seqTerminator}|]
    RESPInteger int ->
        if int >= 0
            then [fmt|:{int}{seqTerminator}|]
            else [fmt|:-{int}{seqTerminator}|]
    MkArrayResponse NullArray -> [fmt|*-1{seqTerminator}|]
    MkArrayResponse (Array otherResTypes) ->
        let arrLen = V.length otherResTypes
            serializedElems = V.foldr (\a b -> serializeRESPDataType a <> b) "" otherResTypes
         in [fmt|*{arrLen}{seqTerminator}{serializedElems}|]
  where
    seqTerminator :: ByteString
    seqTerminator = "\r\n"
