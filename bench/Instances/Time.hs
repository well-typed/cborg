{-# LANGUAGE ScopedTypeVariables #-}

module Instances.Time
  ( benchmarks -- :: [Benchmark]
  ) where

import           Criterion.Main
import           Data.Time.Clock (UTCTime(..))
import           Data.Time.Calendar (Day(..))
import           Data.Binary.Serialise.CBOR
import           Control.DeepSeq (force)
import qualified Data.ByteString.Lazy as BSL

benchmarks :: [Benchmark]
benchmarks =
  [ bench "serialise UTCTime" (whnf (BSL.length . serialise) timestamps)
  , bench "deserialise UTCTime" (nf (deserialise :: BSL.ByteString -> [UTCTime]) serialisedTimestamps)
  ]
  where
    timestamps = force (replicate 100 (UTCTime (ModifiedJulianDay 0) 0))
    serialisedTimestamps = force (serialise timestamps)
