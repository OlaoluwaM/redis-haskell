module Main (main) where

import Criterion.Main

import LZF qualified
import RDBBinary qualified

main :: IO ()
main = defaultMain $ LZF.benchmarks <> RDBBinary.benchmarks
