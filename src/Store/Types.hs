{-# LANGUAGE DeriveDataTypeable #-}

module Store.Types where

import Data.Data

import Data.ByteString (ByteString)
import Data.HashMap.Strict (HashMap)
import Data.Time (UTCTime)

-- Weighted pair will require a custom Eq and Ord instance
-- Two weighted pairs are the same if their snd items are the same
-- newtype WeightedPair = WeightedPair (Int, Text)
-- data SortedSet = OSet WeightedPair

-- There are other value types as listed, but these will suffice for now
-- https://redis.io/docs/latest/develop/data-types/
-- Might require smart constructors
data RedisDataType = Str ByteString | List [ByteString] | Hash (HashMap ByteString ByteString) deriving (Eq, Show, Typeable, Data)

data StoreValue = InternalStoreValue {value :: RedisDataType, insertTime :: UTCTime, ttlTimestamp :: UTCTime} deriving (Eq, Show)

type Store = HashMap ByteString StoreValue
