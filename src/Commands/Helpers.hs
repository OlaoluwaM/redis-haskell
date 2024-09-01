module Commands.Helpers where

import Data.Data (Data (toConstr))
import Data.Function (on)
import Store.Types (RedisDataType (..))

-- Redis has data type specific set commands it seems, for instance, SET can only work on string-based keys while LPUSH can only operate on list based keys
-- You cannot use SET on a key with a list value and vice versa
isCmdDataTypeValidForKey :: (Monoid a) => (a -> RedisDataType) -> Maybe RedisDataType -> Bool
isCmdDataTypeValidForKey _ Nothing = True
isCmdDataTypeValidForKey expectedCmdRedisDTConstructor (Just keyRedisDT) = on (==) toConstr keyRedisDT (expectedCmdRedisDTConstructor mempty)
