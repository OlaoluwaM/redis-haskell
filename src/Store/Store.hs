module Store.Store (Store, RedisDataType (..), initialStore, retrieveItem, setItem, mkStoreValue) where

import Data.HashMap.Strict (HashMap)
import Data.Text (Text)

import Data.HashMap.Strict qualified as HashMap
import Data.Time (UTCTime)

-- We can split this module into Store/Types & Store/Operations
-- We would rename "Internal" to "Store"
-- Once this module gets a bit bigger or something

-- Weighted pair will require a custom Eq and Ord instance
-- Two weighted pairs are the same if their snd items are the same
-- newtype WeightedPair = WeightedPair (Int, Text)
-- data SortedSet = OSet WeightedPair

-- There are other value types as listed, but these will suffice for now
-- https://redis.io/docs/latest/develop/data-types/
-- Might require smart constructors
data RedisDataType = Str Text | List [Text] | Hash (HashMap Text Text) deriving (Eq, Show)

-- TODO: remove these maybes once we begin implementing expiry in earnest
data StoreValue = StoreValue {ttl :: Maybe UTCTime, insertDate :: Maybe UTCTime, value :: RedisDataType} deriving (Eq, Show)

type Store = HashMap Text StoreValue

storeValueWithNoExpiryInfo :: StoreValue
storeValueWithNoExpiryInfo = StoreValue{ttl = Nothing, insertDate = Nothing, value = Str ""}

initialStore :: Store
initialStore = HashMap.empty

retrieveItem :: Text -> Store -> Maybe StoreValue
retrieveItem = HashMap.lookup

setItem :: Text -> StoreValue -> Store -> Store
setItem = HashMap.insert

-- This signature and implementation are temporary until we begin implementing expiry
mkStoreValue :: RedisDataType -> StoreValue
mkStoreValue newVal = storeValueWithNoExpiryInfo{value = newVal}
