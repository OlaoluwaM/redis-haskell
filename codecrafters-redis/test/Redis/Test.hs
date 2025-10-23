{-# LANGUAGE FieldSelectors #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module Redis.Test (
    TestM,
    runTestM,
    runTestM',
    PassableTestContext (..),
) where

import Data.List.NonEmpty qualified as NE

import Blammo.Logging (LogLevel (LevelError), Logger, MonadLogger (..), MonadLoggerIO)
import Blammo.Logging.LogSettings.LogLevels (newLogLevels)
import Blammo.Logging.Simple (HasLogger (..), WithLogger (..), defaultLogSettings, setLogSettingsLevels, withLogger)
import Control.Concurrent.STM (newTVarIO)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Reader (MonadReader, ReaderT (runReaderT))
import Data.ByteString (ByteString)
import Data.Default (Default (def))
import Data.Maybe (fromMaybe)
import Control.Monad.Catch (MonadCatch, MonadThrow)
import GHC.Generics (Generic)
import Network.Socket (
    AddrInfo (addrFamily, addrFlags, addrProtocol, addrSocketType),
    AddrInfoFlag (..),
    Socket,
    SocketType (..),
    defaultHints,
    getAddrInfo,
    socket,
 )
import Optics (makeFieldLabelsNoPrefix)
import Redis.Server.ServerT (MonadSocket (..))
import Redis.Server.Settings (ServerSettings)
import Redis.Store (StoreState, genInitialStore)

data TestContext = TestContext
    { clientSocket :: Socket
    , store :: StoreState
    , settings :: ServerSettings
    , logger :: Logger
    }
    deriving stock (Generic)

makeFieldLabelsNoPrefix ''TestContext

data PassableTestContext = PassableTestContext
    { storeState :: Maybe StoreState
    , settings :: Maybe ServerSettings
    }
    deriving stock (Generic)

instance HasLogger TestContext where
    loggerL f (TestContext clientSocket store settings logger) = TestContext clientSocket store settings <$> f logger

newtype TestM a = TestM {unTestM :: ReaderT TestContext IO a}
    deriving newtype (Functor, Applicative, Monad, MonadIO, MonadReader TestContext, MonadThrow, MonadCatch)
    deriving (MonadLogger, MonadLoggerIO) via (WithLogger TestContext IO)

instance MonadSocket TestM ByteString where
    sendThroughSocket _ = pure

instance MonadSocket TestM () where
    sendThroughSocket _ _ = pure ()


runTestM :: TestM a -> Maybe StoreState -> IO a
runTestM action storeM = do
    loopbackSocket <- mkLoopbackSocket
    initialStoreState <- newTVarIO genInitialStore
    let kvStoreState = fromMaybe initialStoreState storeM
    withLogger (setLogSettingsLevels (newLogLevels LevelError []) defaultLogSettings) $ \logger ->
        runReaderT
            (unTestM action)
            TestContext
                { clientSocket = loopbackSocket
                , store = kvStoreState
                , logger = logger
                , settings = def
                }

-- TODO: This should the replace the above API
runTestM' :: TestM a -> PassableTestContext -> IO a
runTestM' action testContext = do
    loopbackSocket <- mkLoopbackSocket
    initialStoreState <- newTVarIO genInitialStore
    let kvStoreState = fromMaybe initialStoreState testContext.storeState
    let serverSettings = fromMaybe def testContext.settings
    withLogger (setLogSettingsLevels (newLogLevels LevelError []) defaultLogSettings) $ \logger ->
        runReaderT
            (unTestM action)
            TestContext
                { clientSocket = loopbackSocket
                , store = kvStoreState
                , logger = logger
                , settings = serverSettings
                }

mkLoopbackSocket :: IO Socket
mkLoopbackSocket = do
    let hints = defaultHints{addrFlags = [AI_NUMERICHOST, AI_NUMERICSERV], addrSocketType = Stream}
    addr <- NE.head <$> getAddrInfo (Just hints) (Just "127.0.0.1") (Just "5000")
    socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)
