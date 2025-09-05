module Main (main) where

import Criterion.Main

import LZF qualified
import SBinary qualified

main :: IO ()
main = defaultMain $ LZF.benchmarks <> SBinary.benchmarks
