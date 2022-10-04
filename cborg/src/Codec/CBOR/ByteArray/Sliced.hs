{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE MagicHash           #-}
{-# LANGUAGE CPP                 #-}
{-# LANGUAGE TypeFamilies        #-}

-- |
-- Module      : Codec.CBOR.ByteArray.Sliced
-- Copyright   : (c) Ben Gamari 2017-2018
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
module Codec.CBOR.ByteArray.Sliced
  ( SlicedByteArray(..)
    -- * Conversions
  , sizeofSlicedByteArray
  , fromShortByteString
  , fromByteString
  , fromByteArray
  , toByteString
  , toBuilder
  ) where

import GHC.Exts
import Data.Char (chr, ord)
import Data.Word
import Foreign.Ptr
import Control.Monad.ST
import System.IO.Unsafe

import qualified Data.Primitive.ByteArray as Prim
#if !MIN_VERSION_primitive(0,7,0)
import           Data.Primitive.Types (Addr(..))
#endif
import qualified Data.ByteString as BS
import qualified Data.ByteString.Unsafe as BS
import qualified Data.ByteString.Short as BSS
import qualified Data.ByteString.Short.Internal as BSS
import qualified Data.ByteString.Builder as BSB
import qualified Data.ByteString.Builder.Internal as BSB

import Codec.CBOR.ByteArray.Internal

data SlicedByteArray = SBA {unSBA :: !Prim.ByteArray, offset :: !Int, length :: !Int}

fromShortByteString :: BSS.ShortByteString -> SlicedByteArray
fromShortByteString (BSS.SBS ba) = fromByteArray (Prim.ByteArray ba)

fromByteString :: BS.ByteString -> SlicedByteArray
fromByteString = fromShortByteString . BSS.toShort

fromByteArray :: Prim.ByteArray -> SlicedByteArray
fromByteArray ba = SBA ba 0 (Prim.sizeofByteArray ba)

sizeofSlicedByteArray :: SlicedByteArray -> Int
sizeofSlicedByteArray (SBA _ _ len) = len

-- | Note that this may require a copy.
toByteString :: SlicedByteArray -> BS.ByteString
toByteString sba =
    unsafePerformIO
    $ BS.unsafePackCStringFinalizer ptr (sizeofSlicedByteArray sba) (touch pinned)
  where
    pinned = toPinned sba
#if MIN_VERSION_primitive(0,7,0)
    !(Ptr addr#) = Prim.byteArrayContents pinned
#else
    !(Addr addr#) = Prim.byteArrayContents pinned
#endif
    ptr = Ptr addr#

toPinned :: SlicedByteArray -> Prim.ByteArray
toPinned (SBA ba off len)
  | isByteArrayPinned ba = ba
  | otherwise = runST $ do
        ba' <- Prim.newPinnedByteArray len
        Prim.copyByteArray ba' 0 ba off len
        Prim.unsafeFreezeByteArray ba'

toBuilder :: SlicedByteArray -> BSB.Builder
toBuilder = \(SBA ba off len) -> BSB.builder (go ba off (len + off))
  where
    go ba !ip !ipe !k (BSB.BufferRange op ope)
      | inpRemaining <= outRemaining = do
          copyToAddr ba ip op inpRemaining
          let !br' = BSB.BufferRange (op `plusPtr` inpRemaining) ope
          k br'
      | otherwise = do
          copyToAddr ba ip op outRemaining
          let !ip' = ip + outRemaining
          return $ BSB.bufferFull 1 ope (go ba ip' ipe k)
      where
        outRemaining = ope `minusPtr` op
        inpRemaining = ipe - ip

instance IsString SlicedByteArray where
  fromString = fromList . map checkedOrd
    where
      checkedOrd c
        | c > '\xff' = error "IsString(Codec.CBOR.ByteArray.Sliced): Non-ASCII character"
        | otherwise  = fromIntegral $ ord c

instance IsList SlicedByteArray where
  type Item SlicedByteArray = Word8
  fromList xs = fromListN (Prelude.length xs) xs
  -- Note that we make no attempt to behave sensibly if @n /= length xs@.
  -- The class definition allows this.
  fromListN n xs =
      let arr = mkByteArray n xs
      in SBA arr 0 n
  toList (SBA arr off len) =
      foldrByteArray (:) [] off len arr

instance Show SlicedByteArray where
  showsPrec _ = shows . map (chr . fromIntegral) . toList

instance Eq SlicedByteArray where
  SBA arr1 off1 len1 == SBA arr2 off2 len2
    | len1 /= len2
    = False

    | sameByteArray arr1 arr2
    , off1 == off2
    , len1 == len2
    = True

    | otherwise
    = let (!) :: Prim.ByteArray -> Int -> Word8
          (!) = Prim.indexByteArray
          go i1 i2
            | i1 == len1 && i2 == len2   = True
            | i1 == len1 || i2 == len2   = False
            | (arr1 ! i1) == (arr2 ! i2) = go (i1+1) (i2+1)
            | otherwise                  = False
      in go off1 off2

instance Ord SlicedByteArray where
  SBA arr1 off1 len1 `compare` SBA arr2 off2 len2
    | sameByteArray arr1 arr2
    , off1 == off2
    , len1 == len2
    = EQ

    | otherwise
    = let (!) :: Prim.ByteArray -> Int -> Word8
          (!) = Prim.indexByteArray
          go i1 i2
            | i1 == len1 && i2 == len2 = EQ
            | i1 == len1 || i2 == len2 = len1 `compare` len2
            | EQ <- o                  = go (i1+1) (i2+1)
            | otherwise                = o
            where o = (arr1 ! i1) `compare` (arr2 ! i2)
      in go off1 off2
