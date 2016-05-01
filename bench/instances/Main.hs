module Main
  ( main -- :: IO ()
  ) where

import           Criterion.Main (bgroup, defaultMain)

import qualified Instances.Time   as Time
import qualified Instances.Vector as Vector

--------------------------------------------------------------------------------

main :: IO ()
main = defaultMain
  [ bgroup "time"   Time.benchmarks
  , bgroup "vector" Vector.benchmarks
  ]
