module Commands.String.Helpers where

import RESP.Helpers (mkNonNullBulkString)
import RESP.Types (RESPDataType (SimpleString))
import Store.Types

serializeStoreValueForStringCmds :: RedisDataType -> RESPDataType
serializeStoreValueForStringCmds (Str str) = mkNonNullBulkString str
serializeStoreValueForStringCmds _ = stringCmdTypeMismatchError

stringCmdTypeMismatchError :: RESPDataType
stringCmdTypeMismatchError = SimpleString "(error) WRONGTYPE Operation against a key holding the wrong kind of value"
