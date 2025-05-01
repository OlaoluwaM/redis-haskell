{-# LANGUAGE FieldSelectors #-}

module Redis.Test (
    TestM,
    runTestM,
) where

import Blammo.Logging (LogLevel (LevelError), Logger, MonadLogger (..), MonadLoggerIO)
import Blammo.Logging.LogSettings.LogLevels (newLogLevels)
import Blammo.Logging.Simple (HasLogger (..), WithLogger (..), defaultLogSettings, setLogSettingsLevels, withLogger)
import Control.Lens (lens)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Reader (MonadReader, ReaderT (runReaderT))
import Data.ByteString (ByteString)
import Data.List.NonEmpty qualified as NE
import Data.Maybe (fromMaybe)
import Network.Socket (AddrInfo (addrFamily, addrFlags, addrProtocol, addrSocketType), AddrInfoFlag (..), Socket, SocketType (..), defaultHints, getAddrInfo, socket)
import Redis.Server.Env (HasClientSocket (..))
import Redis.Server.ServerT (MonadSocket (..))

data TestEnv = TestEnv
    { testEnvClientSocket :: Socket
    , testEnvStore :: ()
    , testEnvLogger :: Logger
    }

instance HasClientSocket TestEnv Socket where
    clientSocket = lens testEnvClientSocket $ \x y -> x{testEnvClientSocket = y}

instance HasLogger TestEnv where
    loggerL = lens testEnvLogger $ \x y -> x{testEnvLogger = y}

newtype TestM a = TestM {unTestM :: ReaderT TestEnv IO a}
    deriving newtype (Functor, Applicative, Monad, MonadIO, MonadReader TestEnv)
    deriving (MonadLogger, MonadLoggerIO) via (WithLogger TestEnv IO)

instance MonadSocket TestM ByteString where
    sendThroughSocket _ = pure

runTestM :: TestM a -> Maybe () -> IO a
runTestM action storeM = do
    loopbackSocket <- mkLoopbackSocket
    let store = fromMaybe () storeM
    withLogger (setLogSettingsLevels (newLogLevels LevelError []) defaultLogSettings) $ \logger -> runReaderT (unTestM action) TestEnv{testEnvClientSocket = loopbackSocket, testEnvStore = store, testEnvLogger = logger}

mkLoopbackSocket :: IO Socket
mkLoopbackSocket = do
    let hints = defaultHints{addrFlags = [AI_NUMERICHOST, AI_NUMERICSERV], addrSocketType = Stream}
    addr <- NE.head <$> getAddrInfo (Just hints) (Just "127.0.0.1") (Just "5000")
    socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)
