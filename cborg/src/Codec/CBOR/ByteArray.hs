{-# LANGUAGE CPP                 #-}
{-# LANGUAGE MagicHash           #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Module      : Codec.CBOR.ByteArray
-- Copyright   : (c) Duncan Coutts 2015-2017
-- License     : BSD3-style (see LICENSE.txt)
--
-- Maintainer  : duncan@community.haskell.org
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- A ByteArray with more instances than 'Data.Primitive.ByteArray.ByteArray'.
-- Some day when these instances are reliably available from @primitive@ we can
-- likely replace this with 'Data.Primitive.ByteArray.ByteArray'.
--
module Codec.CBOR.ByteArray
  ( ByteArray(..)
  , sizeofByteArray
  , fromByteString
  , toBuilder
  ) where

import GHC.Exts
import GHC.Word
import Data.Primitive.Types
import qualified Data.Primitive.ByteArray as BA

import qualified Data.ByteString as BS
import qualified Data.ByteString.Short as BSS
import qualified Data.ByteString.Short.Internal as BSS
import qualified Data.ByteString.Builder as BSB

newtype ByteArray = BA {unBA :: BA.ByteArray}

indexByteArray :: Prim a => ByteArray -> Int -> a
{-# INLINE indexByteArray #-}
indexByteArray (BA arr) = BA.indexByteArray arr

sizeofByteArray :: ByteArray -> Int
{-# INLINE sizeofByteArray #-}
sizeofByteArray (BA arr) = BA.sizeofByteArray arr

fromByteString :: BS.ByteString -> ByteArray
fromByteString bs =
    case BSS.toShort bs of
      BSS.SBS ba -> BA (BA.ByteArray ba)

toBuilder :: ByteArray -> BSB.Builder
toBuilder (BA (BA.ByteArray ba)) = BSB.shortByteString $ BSS.SBS ba



#if __GLASGOW_HASKELL__ < 706
isTrue# :: Bool -> Bool
isTrue# = id
#endif

instance Show ByteArray where
    showsPrec _ ba =
        showString "ByteArray [" . go 0
      where
        go i
          | i < sizeofByteArray ba = comma . shows (indexByteArray ba i :: Word8) . go (i+1)
          | otherwise              = showChar ']'
          where
            comma | i == 0    = id
                  | otherwise = showString ", "

instance Eq ByteArray where
  ba1@(BA (BA.ByteArray ba1#)) == ba2@(BA (BA.ByteArray ba2#)) =
      case reallyUnsafePtrEquality# (unsafeCoerce# ba1# :: ()) (unsafeCoerce# ba2# :: ()) of
        r | isTrue# r -> True
        _ | sizeofByteArray ba1 /= sizeofByteArray ba2 -> False
        _ -> let (!) :: ByteArray -> Int -> Word8
                 (!) = indexByteArray
                 go i
                   | i == sizeofByteArray ba1 = True
                   | (ba1 ! i) == (ba2 ! i)   = go (i+1)
                   | otherwise                = False
             in go 0

instance Ord ByteArray where
  ba1@(BA (BA.ByteArray ba1#)) `compare` ba2@(BA (BA.ByteArray ba2#)) =
      case reallyUnsafePtrEquality# (unsafeCoerce# ba1# :: ()) (unsafeCoerce# ba2# :: ()) of
        r | isTrue# r -> EQ
        _ | n1 /= n2 -> n1 `compare` n2
        _ -> let (!) :: ByteArray -> Int -> Word8
                 (!) = indexByteArray
                 go i
                   | i == n1                = EQ
                   | (ba1 ! i) <  (ba2 ! i) = LT
                   | (ba1 ! i) == (ba2 ! i) = go (i+1)
                   | otherwise              = GT
             in go 0
    where
      n1 = sizeofByteArray ba1
      n2 = sizeofByteArray ba2
