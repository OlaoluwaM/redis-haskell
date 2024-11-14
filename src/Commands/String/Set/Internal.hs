module Commands.String.Set.Internal where

import Data.ByteString (ByteString)
import Data.Default (Default (def))

data SetCmd = InternalSetCmd {key :: ByteString, val :: ByteString, opts :: SetCmdOpts} deriving (Eq, Show)

data SetCmdOpts = SetCmdOpts {setCondition :: SetCondition, returnOldVal :: Bool, ttlOption :: TTLOption} deriving (Eq, Show)

-- "Any previous time to live associated with the key is discarded on successful SET operation". That is from the redis documentation on the SET command
-- I personally take this to mean that prev ttls will be replaced by the DEFAULT ttl
data TTLOption = EX Seconds | PX MilliSeconds | EXAT UnixTimeSeconds | PXAT UnixTimeMilliSeconds | KeepTTL | DiscardAndReplaceWithDefaultTTL deriving (Eq, Show, Ord)

-- Determines how the SET command works. Does it a) set the target key to the specified value regardless of whether the key already exists (Always, the default)
-- b) only set if the target key exists (OnlyIfKeyExists)
-- c) only set if the target key does not exist (OnlyIfKeyDoesNotExist)
data SetCondition = Always | OnlyIfKeyExists | OnlyIfKeyDoesNotExist deriving (Eq, Show)

newtype Seconds = Seconds {unSeconds :: Int} deriving (Eq, Show, Ord)
newtype MilliSeconds = MilliSeconds {unMilliseconds :: Int} deriving (Eq, Show, Ord)
newtype UnixTimeSeconds = UnixTimeSeconds {unUnixTimeStampSeconds :: Int} deriving (Eq, Show, Ord)
newtype UnixTimeMilliSeconds = UnixTimeMilliSeconds {unUnixTimeStampMilliseconds :: Int} deriving (Eq, Show, Ord)

instance Default SetCmdOpts where
    def = SetCmdOpts{setCondition = Always, returnOldVal = False, ttlOption = def @TTLOption}

instance Default TTLOption where
    def = DiscardAndReplaceWithDefaultTTL

mkSetCmdOpts :: SetCondition -> Bool -> TTLOption -> Either String SetCmdOpts
mkSetCmdOpts setCond shouldReturnOldVal ttlOption
    | getTTL ttlOption < minAcceptedTTL = Left "(error) invalid expire time (ttlOption) in 'set' command. Specified TTL is too short"
    | otherwise = Right $ SetCmdOpts setCond shouldReturnOldVal ttlOption
  where
    minAcceptedTTL = 5

    getTTL (EX t) = t.unSeconds
    getTTL (PX t) = t.unMilliseconds
    getTTL (EXAT t) = t.unUnixTimeStampSeconds
    getTTL (PXAT t) = t.unUnixTimeStampMilliseconds
    -- validation is skipped for KeepTTL | DiscardAndReplaceWithDefaultTTL expiry options
    getTTL _ = 100
