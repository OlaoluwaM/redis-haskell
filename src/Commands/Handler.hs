{-# LANGUAGE PatternSynonyms #-}

module Commands.Handler (
    handleCommandReq,

    -- * For Testing
    commandParser,
    Command (..),
) where

import Commands.String.Get
import Commands.String.Set
import RESP.Types

import Data.Attoparsec.ByteString.Char8 qualified as AC
import Data.ByteString.Char8 qualified as BS
import Data.List.NonEmpty qualified as NE
import Data.Text qualified as T

import Commands.Connection.Echo (mkEchoCmdRunner)
import Commands.Connection.Ping (mkPingCmdRunner)
import Commands.Types (CmdRunner)
import Control.Applicative.Combinators (count)
import Control.Monad.Except (MonadError (throwError), liftEither, runExceptT)
import Control.Monad.Trans (MonadTrans (lift))
import Data.Attoparsec.ByteString (Parser, parseOnly)
import Data.ByteString (ByteString)
import Data.Char (toUpper)
import Data.Default (def)
import Data.List.NonEmpty (NonEmpty)
import Data.String.Interpolate (i)
import Data.Text (Text)
import Helpers (toOptionString, withCustomError)
import RESP.Helpers (mkNonNullBulkString)
import RESP.Parser (bulkStringParser, terminatorSeqParser)
import Utils (fromEither, mapLeft)

type RawCommandRequest = ByteString

data ParsedCommandRequest = ParsedCommandRequest {command :: BulkString, args :: [BulkString]} deriving (Eq, Show)

data Command = Ping (Maybe ByteString) | Echo ByteString | Set SetCmd | Get GetCmd | InvalidCommand Text deriving (Eq, Show)

handleCommandReq :: RawCommandRequest -> CmdRunner
handleCommandReq cmdReq = do
    let command = fromEither . mapLeft (InvalidCommand . T.pack) . parseOnly commandParser $ cmdReq
    mkCmdRunner command

mkCmdRunner :: Command -> CmdRunner
mkCmdRunner (Ping arg) = mkPingCmdRunner arg
mkCmdRunner (Echo arg) = mkEchoCmdRunner arg
mkCmdRunner (Get (GetCmd key)) = mkGetCmdRunner key
mkCmdRunner (Set (SetCmd key val cmdOpts)) = mkSetCmdRunner key val cmdOpts
mkCmdRunner (InvalidCommand msg) = pure . mkNonNullBulkString $ [i|Invalid Command: #{msg}|]
mkCmdRunner _ = undefined

-- NOTE: For now, we're working with the assumption that a request can only have one command at any given time even when pipelining
commandParser :: Parser Command
commandParser = (either InvalidCommand id <$>) $ runExceptT $ do
    arrLength <- lift $ withCustomError (AC.char '*' *> (AC.signed @Int) AC.decimal <* terminatorSeqParser) "Invalid RESP Command"
    isEndOfInput <- lift AC.atEnd

    lift $ case (arrLength == 0, isEndOfInput) of
        (False, True) -> fail "RESP command string input ended too early"
        (True, False) -> fail "RESP command string input ended too early"
        (True, True) -> fail "RESP command string should not be empty"
        _ -> pure ()

    commandInfo <- lift (withCustomError (NE.fromList <$> count arrLength bulkStringParser) [i|Mismatch between purported RESP array length (#{arrLength}) and actual length|]) >>= toParsedCommandReq
    toCommand commandInfo

toParsedCommandReq :: (MonadError Text m) => NonEmpty BulkString -> m ParsedCommandRequest
toParsedCommandReq (NullBulkString NE.:| _) = throwError "A null bulk string is an invalid command"
toParsedCommandReq (bulkStr NE.:| bulkStrs)
    | NullBulkString `elem` bulkStrs = throwError "Command arguments cannot contain null bulk strings"
    | otherwise = pure $ ParsedCommandRequest bulkStr bulkStrs

toCommand :: (MonadError Text m) => ParsedCommandRequest -> m Command
toCommand (ParsedCommandRequest NullBulkString _) = throwError "A null bulk string is not a valid command"
toCommand (ParsedCommandRequest (BulkString rawCmdStr) parsedArgs) =
    let normalizedCmdStr = BS.map toUpper rawCmdStr
     in case (normalizedCmdStr, parsedArgs) of
            -- PING
            ("PING", []) -> pure . Ping $ Nothing
            ("PING", [BulkString arg]) -> pure . Ping . Just $ arg
            ("PING", _) -> throwError "PING command does accept multiple arguments"
            -- ECHO
            ("ECHO", []) -> throwError "ECHO command requires at least 1 argument. None were provided"
            ("ECHO", [BulkString arg]) -> pure . Echo $ arg
            ("ECHO", _) -> throwError "ECHO command requires only 1 argument"
            -- GET
            ("GET", []) -> throwError "GET command requires at least 1 argument. None were provided"
            ("GET", [BulkString key]) -> Get <$> mkGetCmd key
            ("GET", _) -> throwError "GET command requires only 1 argument"
            -- SET
            ("SET", args) | length args < 2 -> throwError "SET command requires at least 2 arguments. None were provided"
            ("SET", [BulkString key, BulkString val]) -> Set <$> mkSetCmd key val (def @SetCmdOpts)
            ("SET", BulkString key : BulkString val : otherArgs) -> (liftEither . mapLeft T.pack . AC.parseOnly parseSetCmdOptions . toOptionString $ otherArgs) >>= fmap Set . mkSetCmd key val
            (unimplementedCommandStr, _) -> handleUnimplementedCmd unimplementedCommandStr

handleUnimplementedCmd :: (MonadError Text m) => ByteString -> m Command
handleUnimplementedCmd unimplementedCommandStr =
    let validButNotImplementedCmds = []
     in if unimplementedCommandStr `elem` validButNotImplementedCmds
            then throwError [i|The command '#{unimplementedCommandStr}' has not yet been implemented|]
            else throwError [i|The command '#{unimplementedCommandStr}' is invalid|]
