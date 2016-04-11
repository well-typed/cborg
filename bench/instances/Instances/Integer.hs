{-# LANGUAGE ScopedTypeVariables #-}

module Instances.Integer
  ( benchmarks -- :: [Benchmark]
  ) where

import           Criterion.Main
import qualified Data.ByteString.Lazy   as BS
import qualified Data.Vector            as Vector

import           Data.Binary.Serialise.CBOR

benchmarks :: [Benchmark]
benchmarks =
  [ bench "small" (nf go integerDataSmall)
  , bench "large" (nf go integerDataLarge)
  ]
  where
    go = BS.length . serialise
    integerDataSmall = Vector.replicate (100 :: Int) (10 :: Integer)
    integerDataLarge = Vector.replicate (100 :: Int) ((2 :: Integer)^(200 :: Integer))

