{-# LANGUAGE DefaultSignatures #-}

module Redis.RDB.SBinary (
    execSPutWithChecksum,
    execSGetWithChecksum,
    SPut,
    SGet,
    SBinary (..),
    genericPutWithChecksumUsing,
    genericGetWithChecksumUsing,
    genericPutWithChecksum,
    genericGetWithChecksum,
) where

import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BSL

import Control.Applicative (Alternative)
import Control.Monad.State.Strict (MonadState, StateT (runStateT), execStateT, modify)
import Control.Monad.Trans (MonadTrans (..), lift)
import Data.Binary (Binary (get, put))
import Data.Binary.Get (
    Get,
    bytesRead,
    getByteString,
    getLazyByteString,
    lookAhead,
 )
import Data.Binary.Put (PutM, putLazyByteString, runPut, runPutM)
import Redis.RDB.CRC64 (CheckSum, crc64, initialChecksum)

newtype SPutT m a = SPutT (StateT CheckSum m a)
    deriving newtype (Monad, Functor, Applicative, MonadState CheckSum, MonadTrans)

newtype SGetT m a = SGetT (StateT CheckSum m a)
    deriving newtype (Monad, Functor, Applicative, MonadState CheckSum, MonadTrans, Alternative, MonadFail)

type SPut = SPutT PutM ()
type SGet a = SGetT Get a

class SBinary a where
    putWithChecksum :: a -> SPut
    default putWithChecksum :: (Binary a) => a -> SPut
    putWithChecksum x = do
        let putResult = runPut (put x)
        updateChecksum putResult
        lift $ putLazyByteString putResult

    getWithChecksum :: SGet a
    default getWithChecksum :: (Binary a) => SGet a
    getWithChecksum = genericGetWithChecksumUsing Data.Binary.get

execSPutWithChecksum :: SPut -> PutM CheckSum
execSPutWithChecksum = flip execStateT initialChecksum . runSPut
  where
    runSPut :: SPut -> StateT CheckSum PutM ()
    runSPut (SPutT comp) = comp

execSGetWithChecksum :: SGet a -> Get (a, CheckSum)
execSGetWithChecksum = flip runStateT initialChecksum . runSGet
  where
    runSGet :: SGet a -> StateT CheckSum Get a
    runSGet (SGetT comp) = comp

updateChecksum :: (MonadState CheckSum m) => BSL.ByteString -> m ()
updateChecksum bs = modify (\currentChecksum -> BSL.foldl' (\currChkSum byte -> crc64 currChkSum (BS.singleton byte)) currentChecksum bs)

updateChecksum' :: (MonadState CheckSum m) => BSL.ByteString -> m ()
updateChecksum' bs = modify (`crc64` BSL.toStrict bs)

genericPutWithChecksum :: (Binary a) => a -> SPut
genericPutWithChecksum x = do
    let putResult = runPut (put x)
    updateChecksum putResult
    lift $ putLazyByteString putResult

genericGetWithChecksum :: (Binary a) => SGet a
genericGetWithChecksum = genericGetWithChecksumUsing Data.Binary.get

genericPutWithChecksumUsing :: PutM a -> SPut
genericPutWithChecksumUsing encoder = do
    let (_, putResult) = runPutM encoder
    updateChecksum putResult
    lift $ putLazyByteString putResult

genericGetWithChecksumUsing :: Get a -> SGet a
genericGetWithChecksumUsing decoder = do
    (result, consumedInput) <- lift $ getWithConsumedInput' decoder
    updateChecksum (BSL.fromStrict consumedInput)
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
