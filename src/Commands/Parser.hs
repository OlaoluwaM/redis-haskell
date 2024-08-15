module Commands.Parser where

import Commands.Types
import Control.Applicative.Combinators
import RESP.Types

import Data.Attoparsec.ByteString.Char8 qualified as AC
import Data.List.NonEmpty qualified as NE

import Control.Monad.Except (MonadTrans (lift), liftEither, runExceptT)
import Data.Attoparsec.ByteString (Parser)
import Data.List.NonEmpty (NonEmpty)
import Data.Text (Text)
import Helpers (withCustomError)
import PyF (fmt)
import RESP.Parser (bulkStringParser, terminatorSeqParser)

-- NOTE: For now, we're working with the assumption that a request can only have one command at any given time even when pipelining
commandParser :: Parser Command
commandParser = (either InvalidCommand id <$>) $ runExceptT $ do
    arrLength <- lift $ withCustomError (AC.char '*' *> (AC.signed @Int) AC.decimal <* terminatorSeqParser) "Invalid RESP Command"
    isEndOfInput <- lift AC.atEnd

    case (arrLength == 0, isEndOfInput) of
        (False, True) -> lift $ fail @Parser "RESP command string input ended too early"
        (True, False) -> lift $ fail @Parser "RESP command string input ended too early"
        (True, True) -> lift $ fail @Parser "RESP command string should not be empty"
        _ -> pure ()

    commandInfo <- lift (withCustomError (NE.fromList <$> count arrLength bulkStringParser) [fmt|Mismatch between purported RESP array length ({arrLength}) and actual length|]) >>= liftEither . toParsedCommandReq
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
        let validButNotImplementedCmds = ["SET", "GET"]
         in if unimplementedCommandStr `elem` validButNotImplementedCmds
                then Left [fmt|The command '{unimplementedCommandStr}' has not yet been implemented|]
                else Left [fmt|The command '{unimplementedCommandStr}' is invalid|]
