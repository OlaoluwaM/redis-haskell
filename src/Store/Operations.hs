module Store.Operations (initialStore, retrieveItem, setItem, mkStoreValue) where

import Store.Types

import Data.HashMap.Strict qualified as HashMap

import Data.ByteString (ByteString)

storeValueWithNoExpiryInfo :: StoreValue
storeValueWithNoExpiryInfo = StoreValue{ttl = Nothing, insertTime = Nothing, value = Str ""}

initialStore :: Store
initialStore = HashMap.empty

retrieveItem :: ByteString -> Store -> Maybe StoreValue
retrieveItem = HashMap.lookup

setItem :: ByteString -> StoreValue -> Store -> Store
setItem = HashMap.insert

-- This signature and implementation are temporary until we begin implementing expiry
mkStoreValue :: RedisDataType -> StoreValue
mkStoreValue newVal = storeValueWithNoExpiryInfo{value = newVal}
