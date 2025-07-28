module Redis.RDB.Serialize where

import Redis.RDB.Format
import Redis.Store
import Redis.Store.Data

import Data.Binary qualified as Binary
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as BSC
import Data.ByteString.Lazy qualified as BSL

import Control.Monad ((>=>))
import Data.Binary (Binary (..))
import Data.Binary.Get
import Data.Binary.Put
import Data.Char (chr, ord)
import Data.Maybe (mapMaybe)
import Data.String (IsString (..))
import Data.Word (Word8)
import Numeric (readHex, showHex)
import Optics.Core (review, view)

rdbFileMagicString :: BSL.ByteString
rdbFileMagicString = Binary.encode @BSL.ByteString "REDIS"

rdbFileVersion :: BSL.ByteString
rdbFileVersion = Binary.encode @BSL.ByteString "REDIS"

--- >>> Binary.encode (ByteStringFoo "HELLO")
-- "\ENQHELLO"

-- encode :: ByteString -> StoreValue -> OpCode
-- encode key StoreValue{value = (MkRedisStr (RedisStr str)), ttlTimestamp = Nothing} = KeyValOpCode $ KeyVal {encodedValue=Str, encodedKey=_encodedKey, valueType=_valueType}
