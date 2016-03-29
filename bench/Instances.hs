{-# LANGUAGE ScopedTypeVariables #-}

module Instances
  ( benchmarks -- :: [Benchmark]
  ) where

import           Data.Proxy
import           Criterion.Main
import qualified Data.Vector.Generic as VG
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector as V
import qualified Data.ByteString.Lazy as BSL
import           Data.Binary.Serialise.CBOR
import           Control.DeepSeq (force)

benchmarks :: [Benchmark]
benchmarks =
  [ bgroup "unboxed"
      [ vector unboxed (42 :: Int) 500
      , vector unboxed (42 :: Int) 5000
      , vector unboxed (42 :: Int) 50000
      , vector unboxed (42 :: Int) 500000
      ]
  , bgroup "boxed"
      [ vector boxed (42 :: Int) 500
      , vector boxed (42 :: Int) 5000
      , vector boxed (42 :: Int) 50000
      , vector boxed (42 :: Int) 500000
      ]
  ]
  where
    unboxed = Proxy :: Proxy VU.Vector
    boxed   = Proxy :: Proxy V.Vector

vector :: forall v a. (Serialise (v a), VG.Vector v a, Num a)
       => Proxy v -> a -> Int -> Benchmark
vector Proxy x len = bgroup ("n="++show len)
    [ bench "encode" $ benchEncode (asVector $ VG.replicate len x)
    , bench "decode" $ whnf (VG.sum . asVector . deserialise) (force $ serialise $ asVector $ VG.replicate len x)
    ]
  where asVector v = v :: v a

benchEncode :: Serialise a => a -> Benchmarkable
benchEncode = whnf (BSL.length . serialise)
{-# INLINE benchEncode #-}
