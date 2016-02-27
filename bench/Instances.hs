{-# OPTIONS_GHC -fno-cse -fno-ignore-asserts #-}
module Instances
  ( benchmarks -- :: IO ()
  ) where

benchmarks :: IO ()
benchmarks = fail "Not Invented Here (yet)" -- issue #27
