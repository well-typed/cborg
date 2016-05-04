module Main
  ( main -- :: IO ()
  ) where

import           Criterion.Main (bgroup, defaultMain)

import qualified Instances.Vector as Vector

--------------------------------------------------------------------------------

main :: IO ()
main = defaultMain
  [ bgroup "vector" Vector.benchmarks
  ]
