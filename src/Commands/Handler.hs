module Commands.Handler (handleCommandReq) where

import Commands.Types
import Control.Monad.Reader
import RESP.Parser
import RESP.Types

import Data.Text qualified as T

import Commands.Parser (commandParser)
import Control.Concurrent.STM (STM, TVar)
import Data.Attoparsec.ByteString (parseOnly)
import Data.Text (Text)
import PyF (fmt)
import Store.Store (Store)
import Utils (fromEither, mapLeft)

type StoreState = TVar Store
type CmdRunner = ReaderT StoreState STM RESPDataType

handleCommandReq :: RawCommandRequest -> ReaderT StoreState STM RawCommandResponse
handleCommandReq cmdReq = do
    let command = fromEither $ mapLeft (InvalidCommand . T.pack) $ parseOnly commandParser cmdReq
    serializeRESPDataType <$> mkCmdRunner command

mkCmdRunner :: Command -> CmdRunner
mkCmdRunner (Ping args) = pingCmdRunner args
mkCmdRunner (Echo args) = echoCmdRunner args
mkCmdRunner (InvalidCommand msg) = pure $ MkBulkStringResponse $ BulkString [fmt|Invalid Command: {msg}|]

pingCmdRunner :: Maybe Text -> CmdRunner
pingCmdRunner (Just txt) = pure $ MkBulkStringResponse $ BulkString txt
pingCmdRunner Nothing = pure $ SimpleString "PONG"

echoCmdRunner :: Text -> CmdRunner
echoCmdRunner = pure . MkBulkStringResponse . BulkString
