module Store.Operations where

import Store.Types

import Data.HashMap.Strict qualified as HashMap

import Data.ByteString (ByteString)
import Data.Time (UTCTime)

initialStore :: Store
initialStore = HashMap.empty

retrieveItem :: ByteString -> Store -> Maybe StoreValue
retrieveItem = HashMap.lookup

setItem :: ByteString -> StoreValue -> Store -> Store
setItem = HashMap.insert

mkStoreValue :: RedisDataType -> UTCTime -> UTCTime -> StoreValue
mkStoreValue = InternalStoreValue
