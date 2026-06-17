{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Redis.Effect.Logging where

import Blammo.Logging.Simple qualified as Blammo
import Effectful.Dispatch.Dynamic qualified as Eff
import Effectful.TH qualified as Eff

import Effectful (Eff, Effect, IOE, (:>))

data Logging :: Effect where
    LogInfo :: Blammo.Message -> Logging m ()
    LogDebug :: Blammo.Message -> Logging m ()
    LogWarn :: Blammo.Message -> Logging m ()
    LogError :: Blammo.Message -> Logging m ()

Eff.makeEffect ''Logging

runLoggingWithLogger :: (IOE :> es) => Blammo.Logger -> Eff (Logging ': es) a -> Eff es a
runLoggingWithLogger logger = Eff.interpret_ $ \case
    LogInfo msg -> Blammo.runLoggerLoggingT logger $ Blammo.logInfo msg
    LogDebug msg -> Blammo.runLoggerLoggingT logger $ Blammo.logDebug msg
    LogWarn msg -> Blammo.runLoggerLoggingT logger $ Blammo.logWarn msg
    LogError msg -> Blammo.runLoggerLoggingT logger $ Blammo.logError msg

-- TODO: Can we add a pure interperter?
