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
  [ bgroup "serialise"
    [ bench "small" (nf goSerialise integerDataSmall)
    , bench "large" (nf goSerialise integerDataLarge)
    ]
  , bgroup "deserialise"
    [ bench "small" (nf goDeserialise integerDataSerialisedSmall)
    , bench "large" (nf goDeserialise integerDataSerialisedLarge)
    ]
  ]
  where
    goSerialise = BS.length . serialise
    goDeserialise :: BS.ByteString -> Vector.Vector Integer
    goDeserialise = deserialise
    integerDataSmall = Vector.replicate (100 :: Int) (10 :: Integer)
    integerDataLarge = Vector.replicate (100 :: Int) ((2 :: Integer)^(200 :: Integer))
    integerDataSerialisedSmall = serialise integerDataSmall
    integerDataSerialisedLarge = serialise integerDataLarge

