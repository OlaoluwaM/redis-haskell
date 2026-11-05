{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Redis.Effect.Random where

import Effectful.Dispatch.Dynamic qualified as Eff
import Effectful.State.Static.Local qualified as EffState
import Effectful.TH qualified as Eff
import System.Random.Stateful qualified as Random

import Control.Monad.IO.Class (liftIO)
import Effectful (Eff, Effect, IOE, (:>))

data Random :: Effect where
    GenRandom :: (Random.Uniform a) => Random m a
    GenRandomR :: (Random.UniformRange a) => (a, a) -> Random m a

Eff.makeEffect ''Random

runRandomIO :: (IOE :> es) => Eff (Random ': es) a -> Eff es a
runRandomIO = Eff.interpret_ $ \case
    GenRandom -> liftIO $ Random.uniformM Random.globalStdGen
    GenRandomR range -> liftIO $ Random.uniformRM range Random.globalStdGen

runRandomPure :: forall g a es. (Random.RandomGen g) => g -> Eff (Random ': es) a -> Eff es a
runRandomPure gen = Eff.reinterpret (EffState.evalState gen) $ \_ -> \case
    GenRandom -> EffState.state @g Random.uniform
    GenRandomR range -> EffState.state @g (Random.uniformR range)
