{-@ LIQUID "--no-positivity-check" @-}

module Store.Types where

import Data.HashMap.Strict (HashMap)
import Data.Text (Text)
import Data.Time (UTCTime)

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
