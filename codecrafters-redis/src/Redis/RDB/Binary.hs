{-# LANGUAGE DefaultSignatures #-}

module Redis.RDB.Binary (
    execRDBPut,
    execRDBGet,
    RDBPut,
    RDBGet,
    RDBBinary (..),
    genericRDBPutUsing,
    genericRDBGetUsing,
    genericRDBPut,
    genericRDBGet,
    encode,
    decode,
    decodeOrFail,
    encodeFile,
    decodeFile,
    RDBError (..),
    RDBErrorType (..),
    MonadRDBError (..),
    RDBErrorArg (..),
) where

import Prettyprinter

import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BSL
import Data.ByteString.Lazy.Internal qualified as BSL
import Data.Sequence qualified as Seq

import Control.Applicative (Alternative (..), (<|>))
import Control.Monad (when)
import Control.Monad.Except (ExceptT (..), MonadError, runExceptT, throwError)
import Control.Monad.Reader (MonadReader, ReaderT (..), ask)
import Control.Monad.State.Strict (MonadState, StateT (..), execStateT, modify)
import Control.Monad.Trans (MonadTrans (..), lift)
import Data.Bifunctor (Bifunctor (bimap))
import Data.Binary (Binary (get, put))
import Data.Binary.Get (
    ByteOffset,
    Decoder (..),
    Get,
    bytesRead,
    getByteString,
    getLazyByteString,
    lookAhead,
    runGet,
    runGetIncremental,
    runGetOrFail,
 )
import Data.Binary.Get.Internal (lookAheadE)
import Data.Binary.Put (PutM, putLazyByteString, runPut, runPutM)
import Data.Foldable (Foldable (toList))
import Data.Sequence (Seq)
import Data.Text (Text)
import GHC.IO.Handle.FD (withBinaryFile)
import GHC.IO.IOMode (IOMode (..))
import Redis.RDB.CRC64 (CheckSum, crc64, initialChecksum)
import Redis.RDB.Config (RDBConfig (..))

-- ReaderT so we may be able to perform serialization based on some configuration
-- StateT to maintain the checksum state during serialization/deserialization

newtype RDBPutT r m a = RDBPutT (ReaderT r (StateT CheckSum m) a)
    deriving newtype (Monad, Functor, Applicative, MonadState CheckSum, MonadReader r)

instance MonadTrans (RDBPutT e) where
    lift = RDBPutT . lift . lift

newtype RDBGetT r e m a = RDBGetT (ReaderT r (StateT CheckSum (ExceptT e m)) a)
    deriving newtype (Monad, Functor, Applicative, MonadState CheckSum, MonadReader r, MonadError e)

instance MonadTrans (RDBGetT e r) where
    lift = RDBGetT . lift . lift . lift

instance (Monoid e) => Alternative (RDBGetT r e Get) where
    empty = RDBGetT . lift . lift $ empty
    RDBGetT ma <|> RDBGetT mb = RDBGetT $ ReaderT $ \r -> StateT $ \s -> ExceptT $ do
        ex <- lookAheadE $ runExceptT $ runStateT (runReaderT ma r) s
        case ex of
            Left e -> fmap (either (Left . (e <>)) Right) (runExceptT $ runStateT (runReaderT mb r) s)
            Right x -> pure (Right x)

type RDBPut = RDBPutT RDBConfig PutM ()
type RDBGet a = RDBGetT RDBConfig (Seq RDBError) Get a

data RDBError = RDBError
    { bytesOffset :: Maybe ByteOffset
    -- ^ Byte position where error occurred in the input stream
    , errType :: RDBErrorType
    -- ^ Classification of the error type
    , context :: Text
    -- ^ Description of where in the parsing process this error occurred
    , msg :: Text
    -- ^ Human-readable description of what went wrong
    }
    deriving stock (Show)

data RDBErrorType = ValidationError | ParseError | RefinementError
    deriving stock (Show)

instance Pretty RDBError where
    pretty RDBError{bytesOffset, errType, context, msg} =
        let offsetInfo = maybe mempty (\offset -> " at position" <+> pretty offset) bytesOffset
         in vsep
                [ "RDB Error" <> offsetInfo <> ":"
                , indent 4 $
                    vsep
                        [ "Type:" <+> viaShow errType
                        , "Context:" <+> pretty context
                        , "Details:" <+> pretty msg
                        ]
                ]

    prettyList = (\items -> line <> "Errors:" <+> line <> indent 2 items) . align . vsep . map pretty

data RDBErrorArg = MKRDBErrorArg
    { rdbErrorType :: RDBErrorType
    , rdbErrorContext :: Text
    , rdbErrorMsg :: Text
    }

class (MonadError (Seq RDBError) m) => MonadRDBError m where
    throwRDBError :: RDBErrorArg -> m a

instance MonadRDBError (RDBGetT r (Seq RDBError) Get) where
    throwRDBError (MKRDBErrorArg errType context msg) = do
        currentOffset <- lift bytesRead
        throwError (Seq.singleton (RDBError (Just currentOffset) errType context msg))

class RDBBinary a where
    rdbPut :: a -> RDBPut
    default rdbPut :: (Binary a) => a -> RDBPut
    rdbPut x = do
        config <- ask
        let putResult = runPut (put x)
        when config.generateChecksum $ updateChecksum putResult
        lift $ putLazyByteString putResult

    rdbGet :: RDBGet a
    default rdbGet :: (Binary a) => RDBGet a
    rdbGet = genericRDBGetUsing get

execRDBPut :: RDBConfig -> RDBPut -> PutM CheckSum
execRDBPut config = flip execStateT initialChecksum . flip runReaderT config . runRDBPut
  where
    runRDBPut :: RDBPut -> ReaderT RDBConfig (StateT CheckSum PutM) ()
    runRDBPut (RDBPutT comp) = comp

execRDBGet :: RDBConfig -> RDBGet a -> Get (a, CheckSum)
execRDBGet config getComp = do
    resultE <- runExceptT . flip runStateT initialChecksum . flip runReaderT config . runRDBGet $ getComp
    case resultE of
        Right val -> pure val
        Left errs -> fail . show . prettyList . toList $ errs
  where
    runRDBGet :: RDBGet a -> ReaderT RDBConfig (StateT CheckSum (ExceptT (Seq RDBError) Get)) a
    runRDBGet (RDBGetT comp) = comp

updateChecksum :: (MonadState CheckSum m) => BSL.ByteString -> m ()
updateChecksum bs = modify (\currentChecksum -> BSL.foldl' (\currChkSum byte -> crc64 currChkSum (BS.singleton byte)) currentChecksum bs)

genericRDBPut :: (Binary a) => a -> RDBPut
genericRDBPut x = do
    config <- ask
    let putResult = runPut (put x)
    when config.generateChecksum $ updateChecksum putResult
    lift $ putLazyByteString putResult

genericRDBGet :: (Binary a) => RDBGet a
genericRDBGet = genericRDBGetUsing Data.Binary.get

genericRDBPutUsing :: PutM a -> RDBPut
genericRDBPutUsing encoder = do
    config <- ask
    let (_, putResult) = runPutM encoder
    when config.generateChecksum $ updateChecksum putResult
    lift $ putLazyByteString putResult

genericRDBGetUsing :: Get a -> RDBGet a
genericRDBGetUsing decoder = do
    config <- ask
    (result, consumedInput) <- lift $ getWithConsumedInput' decoder
    when config.generateChecksum $ updateChecksum (BSL.fromStrict consumedInput)
    pure result

-- | Get a value and the consumed input as a strict ByteString
getWithConsumedInput' :: Get a -> Get (a, BS.ByteString)
getWithConsumedInput' decoder = do
    -- lookAhead will parse then backtrack, so this is our first pass to get the value and the size of the consumed input
    (size, value) <- lookAhead do
        before <- bytesRead
        value <- decoder
        after <- bytesRead
        pure (after - before, value)
    -- Here we actually consume the input but this time to get the byte sequence that would have been consumed in the first pass
    consumedInput <- getByteString (fromIntegral size)
    pure (value, consumedInput)

-- | Get a value and the consumed input as a lazy ByteString
getWithConsumedInput :: Get a -> Get (a, BSL.ByteString)
getWithConsumedInput decoder = do
    (size, value) <- lookAhead do
        before <- bytesRead
        value <- decoder
        after <- bytesRead
        pure (after - before, value)
    consumedInput <- getLazyByteString size
    pure (value, consumedInput)

encode :: (RDBBinary a) => RDBConfig -> a -> BSL.ByteString
encode config = snd . runPutM . execRDBPut config . rdbPut

decode :: (RDBBinary a) => RDBConfig -> BSL.ByteString -> a
decode config = fst . runGet (execRDBGet config rdbGet)

decodeOrFail :: (RDBBinary a) => RDBConfig -> BSL.ByteString -> Either String a
decodeOrFail config = bimap (\(_, _, x) -> x) (\(_, _, x) -> x) . decodeOrFailWithValueAndMetadata config

decodeOrFailWithValueAndMetadata ::
    (RDBBinary a) => RDBConfig -> BSL.ByteString -> Either (BSL.ByteString, ByteOffset, String) (BSL.ByteString, ByteOffset, a)
decodeOrFailWithValueAndMetadata config = runGetOrFail (fst <$> execRDBGet config rdbGet)

encodeFile :: (RDBBinary a) => RDBConfig -> FilePath -> a -> IO ()
encodeFile config filePath v = BSL.writeFile filePath (encode config v)

decodeFile :: (RDBBinary a) => RDBConfig -> FilePath -> IO a
decodeFile config filePath = do
    result <- decodeFileOrFail config filePath
    case result of
        Right x -> return x
        Left (_, str) -> error str

decodeFileOrFail :: (RDBBinary a) => RDBConfig -> FilePath -> IO (Either (ByteOffset, String) a)
decodeFileOrFail config filePath =
    withBinaryFile filePath ReadMode $ \h -> do
        feed (runGetIncremental (fst <$> execRDBGet config rdbGet)) h
  where
    feed (Done _ _ x) _ = return (Right x)
    feed (Fail _ pos str) _ = return (Left (pos, str))
    feed (Partial k) h = do
        chunk <- BS.hGet h BSL.defaultChunkSize
        case BS.length chunk of
            0 -> feed (k Nothing) h
            _ -> feed (k (Just chunk)) h

-- Failed attempt to implement runAndCaptureInput which does what getWithConsumedInput does but in a single pass. We'd end up needing to do two passes like in getWithConsumedInput, so there isn't much point
-- runAndCaptureInput :: Get a -> Get (a, BS.ByteString)
-- runAndCaptureInput g = do
--     (decoder, bs) <- runAndKeepTrack $ do
--         before <- bytesRead
--         value <- g
--         after <- bytesRead
--         pure (fromIntegral $ after - before, value)
--     case decoder of
--         Done _ (bytesConsumed, a) -> do
--             let fullBS = BS.concat bs
--             pushBack fullBS
--             consumedInput <- getByteString bytesConsumed
--             -- let (consumed, unConsumed) = BS.splitAt bytesConsumed fullBS
--             pure (a, consumedInput)
--         Fail inp s -> g{runCont = \_ _ -> Fail inp s}
--         _ -> error "Binary: impossible"
--   where
--     pushBack :: BS.ByteString -> Get ()
--     pushBack bs
--         | BS.null bs = g{runCont = \inp ks -> ks inp ()}
--         | otherwise = g{runCont = \inp ks -> ks (inp <> bs) ()}

-- runAndKeepTrack :: Get a -> Get (Decoder a, [BS.ByteString])
-- runAndKeepTrack g =
--     g
--         { runCont = \inp ks ->
--             let r0 = runCont g inp Done
--                 go !acc r = do
--                     case r of
--                         Done inp' a -> ks inp (Done inp' a, reverse acc)
--                         Partial k -> Partial $ \minp -> do
--                             go (maybe acc (: acc) minp) (k minp)
--                         Fail inp' s -> ks inp (Fail inp' s, reverse acc)
--                         BytesRead unused k -> BytesRead unused (go acc . k)
--              in go [] r0
--         }
-- {-# INLINE runAndKeepTrack #-}
