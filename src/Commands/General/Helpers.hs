module Commands.General.Helpers where

import Data.Data (Data (toConstr))
import Data.Function (on)
import Store.Types (RedisDataType (..))

-- Redis has data type specific set commands it seems, for instance, SET can only work on string-based keys while LPUSH can only operate on list based keys
-- You cannot use SET on a key with a list value and vice versa
isCmdValidForKeyDataType :: (Monoid a) => (a -> RedisDataType) -> Maybe RedisDataType -> Bool
isCmdValidForKeyDataType _ Nothing = True
isCmdValidForKeyDataType expectedCmdRedisDTConstructor (Just keyRedisDT) = on (==) toConstr keyRedisDT (expectedCmdRedisDTConstructor mempty)
