{-# LANGUAGE ScopedTypeVariables #-}

module Instances
  ( benchmarks -- :: [Benchmark]
  ) where

import           Criterion.Main

import qualified Instances.Vector as Vector

benchmarks :: [Benchmark]
benchmarks =
  [ bgroup "vector" Vector.benchmarks
  ]
