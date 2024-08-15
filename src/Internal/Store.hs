module Internal.Store where

import Data.Map (Map)
import Data.Text (Text)

import Data.Map qualified as M
import Data.Time (UTCTime)

-- Weighted pair will require a custom Eq and Ord instance
-- Two weighted pairs are the same if their snd items are the same
-- newtype WeightedPair = WeightedPair (Int, Text)
-- data SortedSet = OSet WeightedPair

-- There are other value types as listed, but these will suffice for now
-- https://redis.io/docs/latest/develop/data-types/
data ValueTypes = Str Text | List [Text] | Hash (Map Text Text) deriving (Eq, Show)

-- TODO: remove these maybes once we begin implementing expiry in earnest
data StoreValue = StoreValue {value :: ValueTypes, ttl :: Maybe UTCTime, insertDate :: Maybe UTCTime} deriving (Eq, Show)

type Store = Map Text StoreValue

storeMap :: Store
storeMap = M.empty
