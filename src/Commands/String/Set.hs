module Commands.String.Set (
    mkSetCmdRunner,
    parseSetCmdOptions,
    SetCmd (..),
    SetCmdOpts,
) where

import Commands.String.Set.Internal

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
import Data.Default (Default (def))
import Data.Functor (($>))
import Data.Maybe (isJust)
import Data.String (IsString (fromString))
import RESP.Helpers (mkNonNullBulkString)
import RESP.Types (RESPDataType (Null, SimpleString))
import Store.Operations (mkStoreValue, retrieveItem, setItem)
import Store.Types (RedisDataType (Str), StoreValue (value))

parseSetCmdOptions :: Parser SetCmdOpts
parseSetCmdOptions =
    let defaultCmdOpts = def @SetCmdOpts
     in intercalateEffect
            (some AC.space)
            ( mkSetCmdOpts
                <$> toPermutationWithDefault defaultCmdOpts.setCondition nxOrXXParser
                <*> toPermutationWithDefault defaultCmdOpts.returnOldVal getOptionParser
                <*> toPermutationWithDefault defaultCmdOpts.ttl ttlOptionParser
            )
            >>= either (fail . fromString) pure
  where
    getOptionParser = AC.string "GET" $> True

    nxOrXXParser = (AC.string "NX" $> OnlyIfKeyDoesNotExist) <|> (AC.string "XX" $> OnlyIfKeyExists)

    individualExpiryOptionWithArgParser ttlOptionWithTimeArg = AC.string ttlOptionWithTimeArg *> skipSomeTill AC.space AC.decimal
    exOptionParser = EX . Seconds <$> individualExpiryOptionWithArgParser "EX"
    pxOptionParser = PX . MilliSeconds <$> individualExpiryOptionWithArgParser "PX"
    exatOptionParser = EXAT . UnixTimeSeconds <$> individualExpiryOptionWithArgParser "EXAT"
    pxatOptionParser = PXAT . UnixTimeMilliSeconds <$> individualExpiryOptionWithArgParser "PXAT"
    keepTTLFlagParser = KeepTTL <$ AC.string "KEEPTTL"

    ttlOptionParser = exOptionParser <|> pxOptionParser <|> exatOptionParser <|> pxatOptionParser <|> keepTTLFlagParser

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
