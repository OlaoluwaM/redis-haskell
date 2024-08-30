module Commands.String.Set where

import Commands.General.Types
import Control.Applicative
import Control.Concurrent.STM
import RESP.Types
import Store.Operations
import Store.Types

import Data.Attoparsec.ByteString.Char8 qualified as AC

import Control.Monad.Reader (ask)
import Control.Monad.Trans (MonadTrans (lift))
import Data.Attoparsec.ByteString (Parser)
import Data.ByteString (ByteString)
import Data.Default (Default (def))
import Data.Maybe (fromMaybe, isJust)
import Commands.General.Helpers (isCmdValidForKeyDataType)
import Control.Monad.Except (throwError)
import Control.Monad (unless)
import RESP.Helpers (mkNonNullBulkString)
import Commands.String.Helpers (serializeStoreValueForStringCmds, stringCmdTypeMismatchError)

parseSetCmdOptions :: Parser SetCmdOpts
parseSetCmdOptions = do
    let defaultCmdOpts = def @SetCmdOpts
    setCondition <- fromMaybe defaultCmdOpts.setCondition <$> optional (nxOrXXParser <* AC.space)
    shouldReturnOldVal <- isJust <$> optional (AC.string "GET" <* AC.space)
    expiryOption <- fromMaybe defaultCmdOpts.expiry <$> optional expiryOptionParser
    pure $ SetCmdOpts setCondition shouldReturnOldVal expiryOption
  where
    nxOrXXParser = do
        nxOrXX <- optional $ AC.string "NX" <|> AC.string "XX"
        pure $ case nxOrXX of
            (Just "NX") -> OnlyIfKeyDoesNotExist
            (Just "XX") -> OnlyIfKeyExists
            _ -> Always

    individualExpiryOptionWithArgParser expiryOptionWithTimeArg = AC.string expiryOptionWithTimeArg *> AC.space *> AC.decimal

    exOptionParser = EX . Seconds <$> individualExpiryOptionWithArgParser "EX"
    pxOptionParser = PX . MilliSeconds <$> individualExpiryOptionWithArgParser "PX"
    exatOptionParser = EXAT . UnixTimeSeconds <$> individualExpiryOptionWithArgParser "EXAT"
    pxatOptionParser = PXAT . UnixTimeMilliSeconds <$> individualExpiryOptionWithArgParser "PXAT"
    keepTTLFlagParser = KeepTTL <$ AC.string "KEEPTTL"

    expiryOptionParser = do
        v <- optional (exOptionParser <|> pxOptionParser <|> exatOptionParser <|> pxatOptionParser <|> keepTTLFlagParser)
        pure $ fromMaybe DiscardAndReplaceWithDefaultTTL v

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
