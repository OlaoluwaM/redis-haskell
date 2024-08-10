module RESP (handleReq) where

import Control.Applicative.Combinators

import Data.Attoparsec.ByteString qualified as A
import Data.Attoparsec.ByteString.Char8 qualified as AC
import Data.List.NonEmpty qualified as NE
import Data.Text qualified as T
import Data.Vector qualified as V

import Control.Monad (when)
import Control.Monad.Except (MonadError (throwError), MonadTrans (lift), liftEither, runExceptT)
import Data.Attoparsec.ByteString (Parser)
import Data.ByteString (ByteString)
import Data.List.NonEmpty (NonEmpty)
import Data.Text (Text)
import Data.Vector (Vector)
import Helpers (withCustomError)
import PyF (fmt)
import Utils (fromEither, mapLeft)

-- SPEC: https://redis.io/docs/latest/develop/reference/protocol-spec/#arrays
-- Info on commands: https://redis.io/docs/latest/commands/

type RawCommandRequest = ByteString
type RawCommandResponse = ByteString

data ParsedCommandRequest = ParsedCommandRequest {command :: BulkString, args :: [BulkString]} deriving (Eq, Show)

-- all the command types
-- Each command that we support is a constructor of this type
data Command = Ping (Maybe Text) | Echo Text | InvalidCommand Text deriving (Eq, Show)

-- all the response types
data Response = MkSimpleStringResponse SimpleString | MkBulkStringResponse BulkString | MkArrayResponse Array | MkIntegerResponse RESPInteger deriving (Eq, Show)

-- Individual RESP data types
-- Smart constructors might be necessary for some of these types
newtype SimpleString = SimpleString Text deriving (Eq, Show)
data BulkString = BulkString Text | NullBulkString deriving (Eq, Show)
newtype RESPInteger = RESPInteger Int deriving (Eq, Show)

-- This type represents an RESP array as a response value, NOT as a carrier for a command/request.
data Array = Array (Vector Response) | NullArray deriving (Eq, Show)

handleReq :: RawCommandRequest -> RawCommandResponse
handleReq = serializeResponse . execCommand . either InvalidCommand id . mapLeft T.pack . A.parseOnly parseReqToCommand

execCommand :: Command -> Response
execCommand (Ping (Just txt)) = MkBulkStringResponse $ BulkString txt
execCommand (Ping Nothing) = MkSimpleStringResponse $ SimpleString "PONG"
execCommand (Echo txt) = MkBulkStringResponse $ BulkString txt
execCommand (InvalidCommand msg) = MkBulkStringResponse $ BulkString [fmt|Invalid Command: {msg}|]

-- NOTE: For now, we're working with the assumption that a request can only have one command at any given time even when pipelining
parseReqToCommand :: Parser Command
parseReqToCommand = (either InvalidCommand id <$>) $ runExceptT $ do
    arrLength <- lift $ withCustomError (AC.char '*' *> (AC.signed @Int) AC.decimal <* terminatorSeqParser) "Invalid RESP Command"
    isEndOfInput <- lift AC.atEnd

    case (arrLength == 0, isEndOfInput) of
        (False, True) -> lift $ fail @Parser "RESP command string input ended too early"
        (True, False) -> lift $ fail @Parser "RESP command string input ended too early"
        (True, True) -> lift $ fail @Parser "RESP command string should not be empty"
        _ -> pure ()

    commandInfo <- lift (withCustomError (NE.fromList <$> count arrLength parseBulkString) [fmt|Mismatch between purported RESP array length ({arrLength}) and actual length|]) >>= liftEither . toParsedCommandReq
    liftEither $ toCommand commandInfo
  where
    toParsedCommandReq :: NonEmpty BulkString -> Either Text ParsedCommandRequest
    toParsedCommandReq (NullBulkString NE.:| _) = Left "A null bulk string is an invalid command"
    toParsedCommandReq (bulkStr NE.:| bulkStrs)
        | NullBulkString `elem` bulkStrs = Left "Command arguments cannot contain null bulk strings"
        | otherwise = Right $ ParsedCommandRequest bulkStr bulkStrs

toCommand :: ParsedCommandRequest -> Either Text Command
toCommand (ParsedCommandRequest (BulkString "PING") []) = Right $ Ping Nothing
toCommand (ParsedCommandRequest (BulkString "PING") [BulkString arg]) = Right $ Ping $ Just arg
toCommand (ParsedCommandRequest (BulkString "PING") _) = Left "PING command does not currently support multiple arguments"
toCommand (ParsedCommandRequest (BulkString "ECHO") [BulkString arg]) = Right $ Echo arg
toCommand (ParsedCommandRequest (BulkString "ECHO") []) = Left "ECHO command requires an argument. None was provided"
toCommand (ParsedCommandRequest (BulkString "ECHO") _) = Left "ECHO command requires only 1 argument. Too many were passed"
toCommand (ParsedCommandRequest NullBulkString _) = Left "A null bulk string is not a valid command"
toCommand (ParsedCommandRequest (BulkString unimplementedCommandStr) _) =
    let validButNotImplementedCmds = []
     in if unimplementedCommandStr `elem` validButNotImplementedCmds
            then Left [fmt|The command '{unimplementedCommandStr}' has not yet been implemented|]
            else Left [fmt|The command '{unimplementedCommandStr}' is invalid|]

terminatorSeqParser :: Parser ()
terminatorSeqParser = AC.endOfLine

-- NOTE: We only need parsers for the things coming FROM the client (excluding arrays) bulk strings
parseBulkString :: Parser BulkString
parseBulkString = (fromEither <$>) $ runExceptT $ do
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

serializeResponse :: Response -> ByteString
serializeResponse (MkSimpleStringResponse (SimpleString txt)) = [fmt|+{txt}{seqTerminator}|]
serializeResponse (MkBulkStringResponse (BulkString txt)) = let len = T.length txt in [fmt|${len}{seqTerminator}{txt}{seqTerminator}|]
serializeResponse (MkBulkStringResponse NullBulkString) = [fmt|$-1{seqTerminator}|]
serializeResponse (MkIntegerResponse (RESPInteger int)) = if int >= 0 then [fmt|:{int}{seqTerminator}|] else [fmt|:-{int}{seqTerminator}|]
serializeResponse (MkArrayResponse NullArray) = [fmt|*-1{seqTerminator}|]
serializeResponse (MkArrayResponse (Array otherResTypes)) =
    let arrLen = V.length otherResTypes
        serializedElems = V.foldr (\a b -> serializeResponse a <> b) "" otherResTypes
     in [fmt|*{arrLen}{seqTerminator}{serializedElems}|]

seqTerminator :: ByteString
seqTerminator = "\r\n"
