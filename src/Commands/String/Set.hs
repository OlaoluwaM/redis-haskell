module Commands.String.Set (
  mkSetCmdRunner,
  parseSetCmdOptions,
  SetCmd(..),

  -- * For Testing
  mkSetCmdOpts,
  -- ** The type itself is part of the public API for this module, but the constructors and field selectors are for testing only
  SetCmdOpts(..),
  SetCondition (..),
  Expiry (..),
  Seconds (..),
  MilliSeconds (..),
  UnixTimeMilliSeconds (..),
  UnixTimeSeconds (..),
  ) where

import Data.Attoparsec.ByteString.Char8 qualified as AC

import Commands.Helpers (isCmdValidForKeyDataType)
import Commands.String.Helpers (serializeStoreValueForStringCmds, stringCmdTypeMismatchError)
import Commands.Types (CmdRunner, Env (..), StoreStateTVar)
import Control.Applicative (Alternative (some, (<|>)))
import Control.Applicative.Combinators (skipSomeTill)
import Control.Applicative.Permutations (
    intercalateEffect,
    toPermutationWithDefault,
 )
import Control.Concurrent.STM (STM, modifyTVar, readTVar)
import Control.Monad (unless)
import Control.Monad.Except (throwError)
import Control.Monad.Reader (ask)
import Control.Monad.Trans (MonadTrans (lift))
import Data.Attoparsec.ByteString (Parser)
import Data.ByteString (ByteString)
import Data.Default (Default (..))
import Data.Functor (($>))
import Data.Maybe (isJust)
import Data.String (IsString (fromString))
import RESP.Helpers (mkNonNullBulkString)
import RESP.Types (RESPDataType (Null, SimpleString))
import Store.Operations (mkStoreValue, retrieveItem, setItem)
import Store.Types (RedisDataType (Str), StoreValue (value))

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

parseSetCmdOptions :: Parser SetCmdOpts
parseSetCmdOptions = intercalateEffect (some AC.space) (mkSetCmdOpts <$> toPermutationWithDefault Always nxOrXXParser <*> toPermutationWithDefault False getOptionParser <*> toPermutationWithDefault DiscardAndReplaceWithDefaultTTL expiryOptionParser) >>= either (fail . fromString) pure
  where
    getOptionParser = AC.string "GET" $> True

    nxOrXXParser = (AC.string "NX" $> OnlyIfKeyDoesNotExist) <|> (AC.string "XX" $> OnlyIfKeyExists)

    individualExpiryOptionWithArgParser expiryOptionWithTimeArg = AC.string expiryOptionWithTimeArg *> skipSomeTill AC.space AC.decimal
    exOptionParser = EX . Seconds <$> individualExpiryOptionWithArgParser "EX"
    pxOptionParser = PX . MilliSeconds <$> individualExpiryOptionWithArgParser "PX"
    exatOptionParser = EXAT . UnixTimeSeconds <$> individualExpiryOptionWithArgParser "EXAT"
    pxatOptionParser = PXAT . UnixTimeMilliSeconds <$> individualExpiryOptionWithArgParser "PXAT"
    keepTTLFlagParser = KeepTTL <$ AC.string "KEEPTTL"

    expiryOptionParser = exOptionParser <|> pxOptionParser <|> exatOptionParser <|> pxatOptionParser <|> keepTTLFlagParser

mkSetCmdOpts :: SetCondition -> Bool -> Expiry -> Either String SetCmdOpts
mkSetCmdOpts setCond shouldReturnOldVal expiryTimeOffset
    | getExpiryTimeOffset expiryTimeOffset < minimumAllowedExpiryTimeOffset = Left "(error) invalid expire time in 'set' command"
    | otherwise = Right $ SetCmdOpts setCond shouldReturnOldVal expiryTimeOffset
  where
    minimumAllowedExpiryTimeOffset = 5

    getExpiryTimeOffset (EX t) = t.unSeconds
    getExpiryTimeOffset (PX t) = t.unMilliseconds
    getExpiryTimeOffset (EXAT t) = t.unUnixTimeStampSeconds
    getExpiryTimeOffset (PXAT t) = t.unUnixTimeStampMilliseconds
    -- validation is skipped for KeepTTL | DiscardAndReplaceWithDefaultTTL expiry options
    getExpiryTimeOffset _ = 100

mkSetCmdRunner :: ByteString -> ByteString -> SetCmdOpts -> CmdRunner
mkSetCmdRunner key val cmdOpts = do
    (Env storeStateTVar _) <- ask
    let liftCompletely = lift . lift
    currentStoreState <- liftCompletely . readTVar $ storeStateTVar

    let keyDataTypeIsAStringValue = isCmdValidForKeyDataType Str
    let mCurrentStoreVal = retrieveItem key currentStoreState

    unless (keyDataTypeIsAStringValue ((.value) <$> mCurrentStoreVal)) $ throwError stringCmdTypeMismatchError

    let performSetCmdOperation = mkSetCmdHandler mCurrentStoreVal
    let keyAlreadyHasVal = isJust mCurrentStoreVal

    liftCompletely $ case (keyAlreadyHasVal, cmdOpts.setCondition) of
        (True, OnlyIfKeyDoesNotExist) -> pure Null
        (False, OnlyIfKeyDoesNotExist) -> performSetCmdOperation storeStateTVar
        (True, OnlyIfKeyExists) -> performSetCmdOperation storeStateTVar
        (False, OnlyIfKeyExists) -> pure Null
        (_, Always) -> performSetCmdOperation storeStateTVar
  where
    mkSetCmdHandler :: Maybe StoreValue -> StoreStateTVar -> STM RESPDataType
    mkSetCmdHandler mOldVal storeStateTVar = do
        let newStoreVal = mkStoreValue (Str val)
        modifyTVar storeStateTVar (setItem key newStoreVal)
        case (cmdOpts.returnOldVal, mOldVal) of
            (True, Nothing) -> pure . mkNonNullBulkString $ val
            (True, Just oldVal) -> pure . serializeStoreValueForStringCmds $ oldVal.value
            (False, _) -> pure . SimpleString $ "OK"
