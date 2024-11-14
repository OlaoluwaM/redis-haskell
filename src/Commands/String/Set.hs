{-# LANGUAGE PatternSynonyms #-}

module Commands.String.Set (
    mkSetCmdRunner,
    parseSetCmdOptions,
    mkSetCmd,
    SetCmd,
    pattern SetCmd,
    SetCmdOpts,
) where

import Commands.String.Set.Internal

import Data.Attoparsec.ByteString.Char8 qualified as AC
import Data.ByteString qualified as BS

import Commands.Helpers (isCmdDataTypeValidForKey)
import Commands.String.Helpers (serializeStoreValueForStringCmds, stringCmdTypeMismatchError)
import Commands.Types (CmdRunner, Env (..), StoreStateTVar)
import Control.Applicative (Alternative (some, (<|>)), asum)
import Control.Applicative.Combinators (skipSomeTill)
import Control.Applicative.Permutations (
    intercalateEffect,
    toPermutationWithDefault,
 )
import Control.Concurrent.STM (STM, modifyTVar, readTVar)
import Control.Monad.Except (MonadError, throwError)
import Control.Monad.Reader (MonadTrans (lift), asks)
import Data.Attoparsec.ByteString (Parser)
import Data.Attoparsec.Combinator ((<?>))
import Data.ByteString (ByteString)
import Data.Default (Default (def))
import Data.Functor (($>), (<&>))
import Data.Maybe (isJust)
import Data.String (IsString (fromString))
import Data.Text (Text)
import Data.Time (UTCTime, addUTCTime)
import Helpers (caseInsensitiveString)
import RESP.Helpers (mkNonNullBulkString)
import RESP.Types (RESPDataType (Null, SimpleString))
import Store.Operations (mkStoreValue, retrieveItem, setItem)
import Store.Types (RedisDataType (Str), StoreValue (value))
import Utils (millisecondsToSeconds)

-- Allows us use `SetCmd` for pattern matching but not for constructing values since we have the smart constructor `mkSetCmd` for that
pattern SetCmd :: ByteString -> ByteString -> SetCmdOpts -> SetCmd
pattern SetCmd key val opts <- InternalSetCmd key val opts

parseSetCmdOptions :: Parser SetCmdOpts
parseSetCmdOptions =
    let defaultCmdOpts = def @SetCmdOpts
     in intercalateEffect
            (some AC.space)
            ( mkSetCmdOpts
                <$> toPermutationWithDefault defaultCmdOpts.setCondition nxOrXXParser
                <*> toPermutationWithDefault defaultCmdOpts.returnOldVal getOptionParser
                <*> toPermutationWithDefault defaultCmdOpts.ttlOption ttlOptionParser
            )
            >>= either (fail . fromString) pure
  where
    getOptionParser = caseInsensitiveString "GET" $> True <?> "GET option parser"
    nxOrXXParser = (caseInsensitiveString "NX" $> OnlyIfKeyDoesNotExist) <|> (caseInsensitiveString "XX" $> OnlyIfKeyExists) <?> "NX || XX option parser"

    ttlOptionParser =
        asum
            [ numberedTTLOptionParser "EX" <&> EX . Seconds
            , numberedTTLOptionParser "PX" <&> PX . MilliSeconds
            , numberedTTLOptionParser "EXAT" <&> EXAT . UnixTimeSeconds
            , numberedTTLOptionParser "PXAT" <&> PXAT . UnixTimeMilliSeconds
            , KeepTTL <$ caseInsensitiveString "KEEPTTL"
            ]
            <?> "TTL option parser"

    numberedTTLOptionParser numberedTTLOptionStr = caseInsensitiveString numberedTTLOptionStr *> skipSomeTill AC.space AC.decimal

mkSetCmd :: (MonadError Text m) => ByteString -> ByteString -> SetCmdOpts -> m SetCmd
mkSetCmd key val opts
    | BS.null key = throwError "Error: cannot use SET command without providing a key"
    | otherwise = pure $ InternalSetCmd key val opts

mkSetCmdRunner :: ByteString -> ByteString -> SetCmdOpts -> CmdRunner
mkSetCmdRunner key val cmdOpts = do
    storeStateTVar <- asks (.storeState)
    currentTime <- asks (.currentTime)
    let liftCompletely = lift . lift
    currentStoreState <- liftCompletely . readTVar $ storeStateTVar

    let mCurrentStoreVal = retrieveItem key currentStoreState
    let keyAlreadyHasVal = isJust mCurrentStoreVal

    liftCompletely $ case (keyAlreadyHasVal, cmdOpts.setCondition) of
        (True, OnlyIfKeyDoesNotExist) -> pure Null
        (False, OnlyIfKeyExists) -> pure Null
        _ -> performSetCmdOperation mCurrentStoreVal storeStateTVar
  where
    performSetCmdOperation :: Maybe StoreValue -> StoreStateTVar -> STM RESPDataType
    performSetCmdOperation mOldVal storeStateTVar = do
        -- let setTTL = cmdOpts.ttl
        -- need a function from TTL -> UTCTime
        -- We will also need to know the prev ttl for the KeepTTL option
        let newStoreVal = mkStoreValue (Str val) undefined undefined
        modifyTVar storeStateTVar (setItem key newStoreVal)
        case (cmdOpts.returnOldVal, mOldVal) of
            (True, Nothing) -> pure . mkNonNullBulkString $ val
            (True, Just oldVal) -> pure . serializeStoreValueForStringCmds $ oldVal.value
            (False, _) -> pure . SimpleString $ "OK"

-- calcTTlTimestamp :: UTCTime -> UTCTime -> TTL -> UTCTime
-- calcTTlTimestamp prevTTLTimestamp currentTime = \case
--     (EX (Seconds s)) -> addUTCTime (realToFrac s) currentTime
--     (PX (MilliSeconds ms)) -> addUTCTime (realToFrac . millisecondsToSeconds $ ms) currentTime

--  where
-- defaultTTLInSecs = 180 -- 3 minutes
