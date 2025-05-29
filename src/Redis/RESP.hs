module Redis.RESP (
    RESPDataType (..),
    mkNonNullBulkString,
    serializeRESPDataType,
    mkNonNullRESPArray,
    nullBulkString,
    Array (..),

    -- * Re-exports from Redis.RESP.BulkString
    BulkString (..),
    bulkStringParser,
    mapBulkString,
    decodeUtf8BulkString',
    toOptionString,

    -- ** Testing
    terminatorSeqParser,
    seqTerminator,
    notTerminatorSeq,
) where

import Data.ByteString qualified as BS
import Data.Vector qualified as V

import Data.ByteString (ByteString)
import Data.String.Interpolate (i)
import Data.Text (Text)
import Data.Vector (Vector)
import Redis.RESP.BulkString (
    BulkString (..),
    bulkStringParser,
    decodeUtf8BulkString',
    mapBulkString,
    notTerminatorSeq,
    seqTerminator,
    terminatorSeqParser,
    toOptionString,
 )

-- Spec: https://redis.io/docs/latest/develop/reference/protocol-spec/

-- All supported RESP data types
data RESPDataType = SimpleString Text | MkBulkStringResponse BulkString | MkArrayResponse Array | RESPInteger Int | Null
    deriving stock (Eq, Show)

-- This type represents an RESP data type array as a response value, NOT as a carrier for a command/request.
-- This type represents an RESP data type array as a response value, NOT as a carrier for a command/request.
data Array = Array (Vector RESPDataType) | NullArray
    deriving stock (Eq, Show)

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

mkNonNullBulkString :: ByteString -> RESPDataType
mkNonNullBulkString = MkBulkStringResponse . BulkString

nullBulkString :: RESPDataType
nullBulkString = MkBulkStringResponse NullBulkString

mkNonNullRESPArray :: [RESPDataType] -> RESPDataType
mkNonNullRESPArray = MkArrayResponse . Array . V.fromList
