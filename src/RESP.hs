module RESP where

import Data.Attoparsec.ByteString (Parser)
import Data.ByteString (ByteString)
import Data.Text (Text)
import Data.Vector (Vector)

import Control.Applicative.Combinators
import Control.Monad (when)
import Data.Attoparsec.ByteString.Char8 qualified as AC
import Data.Char qualified as Char
import Data.Text qualified as T
import Data.Vector qualified as V
import Utils (myTrace, myTraceM)

-- SPEC: https://redis.io/docs/latest/develop/reference/protocol-spec/#arrays
-- Info on commands: https://redis.io/docs/latest/commands/

type Request = ByteString

-- all the command types
data Command = MkPingCommand PING | MkHelloCommand Hello deriving (Eq, Show)

-- all the response types
data Response = MkSimpleStringResponse SimpleString | MkBulkStringResponse BulkString | MkArrayResponse Array | MkIntegerResponse RESPInteger deriving (Eq, Show)

-- Individual Command types
-- We want type for each command we wish to support
newtype PING = PING ByteString deriving (Eq, Show)
newtype Hello = Hello ByteString deriving (Eq, Show)

-- Individual RESP data types
-- We want types for each response type for our commands
-- Smart constructors will be necessary here for proper formatting when creating terms for these types
newtype SimpleString = SimpleString Text deriving (Eq, Show)
newtype BulkString = BulkString Text deriving (Eq, Show)

-- We use ByteString here because an array can contain elements of different RESP value types so we don't want to parse them yet, maybe?
-- Maybe it should be a vector of Response?
-- This type represents an RESP array as a return value, NOT as a carrier for a command/request. For that, we need no type and the parsing semantics would be slightly different
newtype Array = Array (Vector Response) deriving (Eq, Show)

-- TODO We want to be able to parse RESP integers so we can test with "*5\r\n:1\r\n:2\r\n:3\r\n:4\r\n$5\r\nhello\r\n" and "*2\r\n*3\r\n:1\r\n:2\r\n:3\r\n*2\r\n+Hello\r\n-World\r\n"
newtype RESPInteger = RESPInteger Int deriving (Eq, Show)

-- Here is how I think we should go about parsing a request. We'll use the following bytestring as an example: *2\r\n$4\r\nLLEN\r\n$6\r\nmylist\r\n
-- We know from the spec that requests come in the form of RESP arrays with elements as bulk strings. We know that the the sequence of control characters, "\r\n", are the protocol's terminator, so they separate parts
-- We know that a request will only have a single command as the first-ish element of the array. The arguments for the request, if any, come after.
-- The numbers we see in the example request also serve a purpose. The 2 at the start tells us that the request array has two elements, the second number 4, tells us the length of the command bulk string, while the 6 is the length of the command's argument (also a bulk string). I think these numbers could be useful to validate parsing
-- Now that I think about it, I think it would make sense to always parse a request type (ByteString) to an RESPArray type

-- Req parser type should look something like `Request -> Command`
-- We will need parsers for the arguments of each command type

-- This is where we will do, where X is some alternative to show
-- instance X Array where

-- * 2\r\n$4\r\nLLEN\r\n$6\r\nmylist\r\n

-- I think for now, let's work with the assumption that a request can only have one command at any given time even when pipelining
-- parseCommand :: Parser Command

parseArray :: Parser Array
parseArray = do
    -- We want to perform validation for when the array might be empty. We have two pieces of data to draw a conclusion from, arrLength being 0 and AC.atEnd being true.
    -- How can we check in a straightforward manner, not requiring too many ternaries? Pattern matching perhaps case (arrLength == 0, AC.atEnd) of ...
    -- After emptiness checks then perhaps checks for nesting?
    arrLength <- AC.char '*' *> AC.decimal <* terminatorSeqParser
    isEndOfInput <- AC.atEnd

    let shouldBeEmptyArray = arrLength == 0
    case (shouldBeEmptyArray, isEndOfInput) of
        (False, True) -> fail "Array is not supposed to be empty"
        (True, False) -> fail "Array is supposed to be empty"
        _ -> pure ()

    -- TODO we need to support null array
    arrElems <- count arrLength parseResponse
    -- when (shouldBeEmptyArray && isEndOfInput) $ pure (Array V.empty)
    -- carrt on parsing
    -- terminatorSeqParser
    pure (Array (V.fromList arrElems))

terminatorSeqParser :: Parser ()
terminatorSeqParser = AC.endOfLine

parseResponse :: Parser Response
parseResponse = (MkBulkStringResponse <$> parseBulkString) <|> (MkSimpleStringResponse <$> parseSimpleString) <|> (MkArrayResponse <$> parseArray)

parseBulkStringOnly :: Parser BulkString
parseBulkStringOnly = parseBulkString <* AC.endOfInput

-- TODO We need to support null bulk strings
parseBulkString :: Parser BulkString
parseBulkString = do
    strLength <- AC.char '$' *> AC.decimal
    terminatorSeqParser
    mainStr <- count strLength AC.anyChar
    terminatorSeqParser
    pure $ BulkString $ T.pack mainStr

parseSimpleStringOnly :: Parser SimpleString
parseSimpleStringOnly = parseSimpleString <* AC.endOfInput

parseSimpleString :: Parser SimpleString
parseSimpleString = do
    mainStr <- AC.char '+' *> many (AC.satisfy Char.isAlpha)
    terminatorSeqParser
    pure $ SimpleString $ T.pack mainStr
