{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE CPP                 #-}
{-# LANGUAGE MagicHash           #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE UnboxedTuples       #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Module      : Codec.CBOR.ByteArray.Internal
-- Copyright   : (c) Ben Gamari 2017-2018
-- License     : BSD3-style (see LICENSE.txt)
--
-- Maintainer  : duncan@community.haskell.org
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Various bytearray utilities
--
module Codec.CBOR.ByteArray.Internal
  ( foldrByteArray
  , copyToAddr
  , isTrue#
  , sameByteArray
  , mkByteArray
  , isByteArrayPinned
  , touch
  ) where

import Control.Monad.ST
import Control.Monad
import GHC.IO (IO(..))
import GHC.Exts
import GHC.Word

import qualified Data.Primitive.ByteArray as Prim

foldrByteArray :: (Word8 -> a -> a) -> a
               -> Int             -- ^ offset
               -> Int             -- ^ length
               -> Prim.ByteArray  -- ^ array
               -> a
foldrByteArray f z off0 len ba = go off0
  where
    go !off
      | off == len = z
      | otherwise  =
        let x = Prim.indexByteArray ba off
        in f x (go (off+1))

copyToAddr :: Prim.ByteArray -> Int -> Ptr a -> Int -> IO ()
copyToAddr (Prim.ByteArray ba) (I# off) (Ptr addr) (I# len) =
    IO (\s -> case copyByteArrayToAddr# ba off addr len s of
                s' -> (# s', () #))

#if __GLASGOW_HASKELL__ < 706
isTrue# :: Bool -> Bool
isTrue# = id
#endif

sameByteArray :: Prim.ByteArray -> Prim.ByteArray -> Bool
sameByteArray (Prim.ByteArray ba1#) (Prim.ByteArray ba2#) =
    case reallyUnsafePtrEquality# (unsafeCoerce# ba1# :: ()) (unsafeCoerce# ba2# :: ()) of
      r -> isTrue# r

-- | @mkByteArray n xs@ forms a 'Prim.ByteArray' with contents @xs@. Note that
-- @n@ must be the precise length of @xs@.
mkByteArray :: Int -> [Word8] -> Prim.ByteArray
mkByteArray n xs = runST $ do
    arr <- Prim.newByteArray n
    zipWithM_ (Prim.writeByteArray arr) [0..n-1] (take n $ xs ++ repeat 0)
    Prim.unsafeFreezeByteArray arr

-- | A conservative estimate of pinned-ness.
isByteArrayPinned :: Prim.ByteArray -> Bool
isByteArrayPinned (Prim.ByteArray ba) =
#if __GLASGOW_HASKELL__ > 800
    case isByteArrayPinned# ba of
      0# -> False
      _  -> True
#else
    False
#endif

touch :: a -> IO ()
touch x = IO $ \s -> case touch# x s of s' -> (# s', () #)
