{-# LANGUAGE FieldSelectors #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module Redis.Server (
    module X,
    runServer,
) where

import Effectful qualified as Eff
import Effectful.Concurrent.STM qualified as Eff
import Effectful.FileSystem qualified as Eff
import Effectful.Reader.Static qualified as ReaderEff
import Redis.Effect.Communication qualified as Eff
import Redis.Effect.Logging qualified as Eff
import Redis.Effect.Time qualified as Eff
import Redis.Server.Context as X (ServerContext)

import Blammo.Logging (Logger)
import Effectful (Eff)
import Redis.Effects (ServerEffects)

runServer :: ServerContext -> Logger -> Eff (ServerEffects ServerContext) () -> IO ()
runServer env logger =
    Eff.runEff
        . Eff.runCommunicationIO
        . Eff.runLoggingWithLogger logger
        . Eff.runGetTimeIO
        . Eff.runConcurrent
        . Eff.runFileSystem
        . ReaderEff.runReader env
