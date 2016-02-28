module Main
  ( main -- :: IO ()
  ) where

import           Criterion.Main (bgroup, defaultMain)

-- Import our benchmark suites
import qualified Mini      as Mini
import qualified Macro     as Macro
import qualified Micro     as Micro
import qualified Instances as Inst

import           Utils         (prepBenchmarkFiles)


-- A simple driver, for running every set of benchmarks.
main :: IO ()
main = prepBenchmarkFiles >> defaultMain
  [ bgroup "instance" Inst.benchmarks
  , bgroup "micro"    Micro.benchmarks
  , bgroup "mini"     Mini.benchmarks
  , bgroup "macro"    Macro.benchmarks
  ]
