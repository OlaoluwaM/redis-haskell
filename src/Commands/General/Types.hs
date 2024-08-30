module Commands.General.Types where

import RESP.Types

import Control.Concurrent.STM (TVar, STM)
import Data.ByteString (ByteString)
import Data.Default (Default (..))
import Data.Text (Text)
import Data.Time (UTCTime)
import Store.Types (Store)
import Control.Monad.Reader (ReaderT)
import Control.Monad.Except (ExceptT)

type RawCommandRequest = ByteString
type RawCommandResponse = ByteString

type StoreStateTVar = TVar Store
data Env = Env {storeState :: StoreStateTVar, time :: UTCTime} deriving (Eq)

-- We are using ExceptT to gain the ability to short-circuit
type CmdRunner = ReaderT Env (ExceptT RESPDataType STM) RESPDataType

data ParsedCommandRequest = ParsedCommandRequest {command :: BulkString, args :: [BulkString]} deriving (Eq, Show)

-- All the commands
-- https://redis.io/docs/latest/commands/
data Command = Ping (Maybe ByteString) | Echo ByteString | Set SetCmd | Get GetCmd | InvalidCommand Text deriving (Eq, Show)

newtype GetCmd = GetCmd {key :: ByteString} deriving (Eq, Show)

data SetCmd = SetCmd {key :: ByteString, val :: ByteString, opts :: SetCmdOpts} deriving (Eq, Show)

data SetCmdOpts = SetCmdOpts {setCondition :: SetCondition, returnOldVal :: Bool, expiry :: Expiry} deriving (Eq, Show)

-- "Any previous time to live associated with the key is discarded on successful SET operation". That is from the redis documentation on the SET command
-- I personally take this to mean that prev ttls will be replaced by the DEFAULT ttl
data Expiry = EX Seconds | PX MilliSeconds | EXAT UnixTimeSeconds | PXAT UnixTimeMilliSeconds | KeepTTL | DiscardAndReplaceWithDefaultTTL deriving (Eq, Show, Ord)

-- Determines how the SET command works. Does it a) set the target key to the specified value regardless of whether the key already exists (Always, the default)
-- b) only set if the target key exists (OnlyIfKeyExists)
-- c) only set if the target key does not exist (OnlyIfKeyDoesNotExist)
data SetCondition = Always | OnlyIfKeyExists | OnlyIfKeyDoesNotExist deriving (Eq, Show)

newtype Seconds = Seconds {unSeconds :: Int} deriving (Eq, Show, Ord)
newtype MilliSeconds = MilliSeconds {unMilliseconds :: Int} deriving (Eq, Show, Ord)
newtype UnixTimeSeconds = UnixTimeSeconds {unUnixTimeStampSeconds :: Int} deriving (Eq, Show, Ord)
newtype UnixTimeMilliSeconds = UnixTimeMilliSeconds {unUnixTimeStampMilliseconds :: Int} deriving (Eq, Show, Ord)

instance Default SetCmdOpts where
    def = SetCmdOpts{setCondition = Always, returnOldVal = False, expiry = def @Expiry}

instance Default Expiry where
    def = DiscardAndReplaceWithDefaultTTL
