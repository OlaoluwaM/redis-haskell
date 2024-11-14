module Commands.String.Get (
    mkGetCmd,
    mkGetCmdRunner,
    GetCmd,
    pattern GetCmd,
) where

import Data.ByteString qualified as BS

import Commands.Helpers (isCmdDataTypeValidForKey)
import Commands.String.Helpers (serializeStoreValueForStringCmds, stringCmdTypeMismatchError)
import Commands.Types (CmdRunner, Env (..))
import Control.Concurrent.STM (readTVar)
import Control.Monad (unless)
import Control.Monad.Except (MonadError (throwError))
import Control.Monad.Reader (asks)
import Control.Monad.Trans (MonadTrans (lift))
import Data.ByteString (ByteString)
import Data.Text (Text)
import RESP.Types (RESPDataType (Null))
import Store.Operations (retrieveItem)
import Store.Types (RedisDataType (..), StoreValue (..))

pattern GetCmd :: ByteString -> GetCmd
pattern GetCmd k <- InternalGetCmd k

newtype GetCmd = InternalGetCmd {key :: ByteString} deriving (Eq, Show)

mkGetCmd :: (MonadError Text m) => ByteString -> m GetCmd
mkGetCmd key
    | BS.null key = throwError "Error: cannot use GET command with an empty key"
    | otherwise = pure $ InternalGetCmd key

mkGetCmdRunner :: ByteString -> CmdRunner
mkGetCmdRunner key = do
    storeStateTVar <- asks (.storeState)
    let liftCompletely = lift . lift
    currentStoreState <- liftCompletely . readTVar $ storeStateTVar
    let mCurrentStoreVal = (.value) <$> retrieveItem key currentStoreState
    unless (isCmdDataTypeValidForKey Str mCurrentStoreVal) $ throwError stringCmdTypeMismatchError
    let returnVal = maybe Null serializeStoreValueForStringCmds mCurrentStoreVal
    pure returnVal
