{-# LANGUAGE CPP                 #-}
{-# LANGUAGE MagicHash           #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Module      : Codec.CBOR.ByteArray
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
module Codec.CBOR.ByteArray
  ( -- * Simple byte arrays
    ByteArray(..)
  , sizeofByteArray
  , fromByteString
  , toBuilder
  , toSliced
  ) where

import Data.Char (ord)
import Data.Word
import GHC.Exts (IsList(..), IsString(..))

import qualified Data.Primitive.ByteArray as Prim
import qualified Data.ByteString as BS
import qualified Data.ByteString.Short as BSS
import qualified Data.ByteString.Short.Internal as BSS
import qualified Data.ByteString.Builder as BSB

import qualified Codec.CBOR.ByteArray.Sliced as Sliced
import           Codec.CBOR.ByteArray.Internal

newtype ByteArray = BA {unBA :: Prim.ByteArray}

sizeofByteArray :: ByteArray -> Int
{-# INLINE sizeofByteArray #-}
sizeofByteArray (BA ba) = Prim.sizeofByteArray ba

fromByteString :: BS.ByteString -> ByteArray
fromByteString bs =
    case BSS.toShort bs of
      BSS.SBS ba -> BA (Prim.ByteArray ba)

toBuilder :: ByteArray -> BSB.Builder
toBuilder = Sliced.toBuilder . toSliced

toSliced :: ByteArray -> Sliced.SlicedByteArray
toSliced ba@(BA arr) = Sliced.SBA arr 0 (sizeofByteArray ba)

instance Show ByteArray where
  showsPrec _ = shows . toSliced

instance Eq ByteArray where
  ba1 == ba2 = toSliced ba1 == toSliced ba2

instance Ord ByteArray where
  ba1 `compare` ba2 = toSliced ba1 `compare` toSliced ba2

instance IsString ByteArray where
  fromString = fromList . map checkedOrd
    where
      checkedOrd c
        | c > '\xff' = error "IsString(Codec.CBOR.ByteArray): Non-ASCII character"
        | otherwise  = fromIntegral $ ord c

instance IsList ByteArray where
  type Item ByteArray = Word8
  fromList xs = fromListN (Prelude.length xs) xs
  fromListN n xs =
      let arr = mkByteArray n xs
      in BA arr
  toList ba@(BA arr) =
      foldrByteArray (:) [] 0 (sizeofByteArray ba) arr
