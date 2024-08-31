module Commands.Types where

import RESP.Types (RESPDataType)
import Control.Concurrent.STM (STM, TVar)
import Control.Monad.Except (ExceptT)
import Control.Monad.Reader (ReaderT)
import Data.Time (UTCTime)
import Store.Types (Store)

type StoreStateTVar = TVar Store

data Env = Env {storeState :: StoreStateTVar, currentTime :: UTCTime} deriving (Eq)

-- We are using ExceptT to gain the ability to short-circuit in our computations
type CmdRunner = ReaderT Env (ExceptT RESPDataType STM) RESPDataType
