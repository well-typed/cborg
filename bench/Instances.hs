{-# LANGUAGE ScopedTypeVariables #-}

module Instances
  ( benchmarks -- :: [Benchmark]
  ) where

import           Criterion.Main

import qualified Instances.Vector as Vector
import qualified Instances.Time as Time

benchmarks :: [Benchmark]
benchmarks =
  [ bgroup "vector" Vector.benchmarks
  , bgroup "time" Time.benchmarks
  ]
