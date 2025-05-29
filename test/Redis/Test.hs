{-# LANGUAGE FieldSelectors #-}

module Redis.Test (
    TestM,
    runTestM,
    runTestM',
) where

import Data.List.NonEmpty qualified as NE

import Blammo.Logging (LogLevel (LevelError), Logger, MonadLogger (..), MonadLoggerIO)
import Blammo.Logging.LogSettings.LogLevels (newLogLevels)
import Blammo.Logging.Simple (HasLogger (..), WithLogger (..), defaultLogSettings, setLogSettingsLevels, withLogger)
import Control.Concurrent.STM (newTVarIO)
import Control.Lens (lens)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Reader (MonadReader, ReaderT (runReaderT))
import Data.ByteString (ByteString)
import Data.Default (Default (def))
import Data.Maybe (fromMaybe)
import Network.Socket (AddrInfo (addrFamily, addrFlags, addrProtocol, addrSocketType), AddrInfoFlag (..), Socket, SocketType (..), defaultHints, getAddrInfo, socket)
import Redis.Server.Context (HasClientSocket (..), HasStore (..))
import Redis.Server.ServerT (MonadSocket (..))
import Redis.Server.Settings (ServerSettings)
import Redis.Store (StoreState, genInitialStore)

data TestContext = TestContext
    { testContextClientSocket :: Socket
    , testContextStore :: StoreState
    , testContextLogger :: Logger
    , testContextSetting :: ServerSettings
    }

data PassableTestContext = PassableTestContext
    { storeState :: Maybe StoreState
    , settings :: Maybe ServerSettings
    }

instance HasClientSocket TestContext Socket where
    clientSocket = lens testContextClientSocket $ \x y -> x{testContextClientSocket = y}

instance HasStore TestContext StoreState where
    store = lens testContextStore $ \x y -> x{testContextStore = y}

instance HasLogger TestContext where
    loggerL = lens testContextLogger $ \x y -> x{testContextLogger = y}

newtype TestM a = TestM {unTestM :: ReaderT TestContext IO a}
    deriving newtype (Functor, Applicative, Monad, MonadIO, MonadReader TestContext)
    deriving (MonadLogger, MonadLoggerIO) via (WithLogger TestContext IO)

instance MonadSocket TestM ByteString where
    sendThroughSocket _ = pure

runTestM :: TestM a -> Maybe StoreState -> IO a
runTestM action storeM = do
    loopbackSocket <- mkLoopbackSocket
    initialStoreState <- newTVarIO genInitialStore
    let kvStoreState = fromMaybe initialStoreState storeM
    withLogger (setLogSettingsLevels (newLogLevels LevelError []) defaultLogSettings) $ \logger -> runReaderT (unTestM action) TestContext{testContextClientSocket = loopbackSocket, testContextStore = kvStoreState, testContextLogger = logger, testContextSetting = def}

-- TODO: This should the replace the above API
runTestM' :: TestM a -> PassableTestContext -> IO a
runTestM' action testContext = do
    loopbackSocket <- mkLoopbackSocket
    initialStoreState <- newTVarIO genInitialStore
    let kvStoreState = fromMaybe initialStoreState testContext.storeState
    let settings = fromMaybe def testContext.settings
    withLogger (setLogSettingsLevels (newLogLevels LevelError []) defaultLogSettings) $ \logger -> runReaderT (unTestM action) TestContext{testContextClientSocket = loopbackSocket, testContextStore = kvStoreState, testContextLogger = logger, testContextSetting = settings}

mkLoopbackSocket :: IO Socket
mkLoopbackSocket = do
    let hints = defaultHints{addrFlags = [AI_NUMERICHOST, AI_NUMERICSERV], addrSocketType = Stream}
    addr <- NE.head <$> getAddrInfo (Just hints) (Just "127.0.0.1") (Just "5000")
    socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)
