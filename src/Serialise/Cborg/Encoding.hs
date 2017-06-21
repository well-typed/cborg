{-# LANGUAGE CPP #-}

-- |
-- Module      : Serialise.Cborg.Encoding
-- Copyright   : (c) Duncan Coutts 2015-2017
-- License     : BSD3-style (see LICENSE.txt)
--
-- Maintainer  : duncan@community.haskell.org
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- High level API for encoding values, for later serialization into
-- CBOR binary format, using a @'Monoid'@ based interface.
--
module Serialise.Cborg.Encoding
  ( -- * Encoding implementation
    Encoding(..)             -- :: *
  , Tokens(..)               -- :: *

    -- * @'Encoding'@ API for serialisation
  , encodeWord               -- :: Word -> Encoding
  , encodeWord8              -- :: Word8 -> Encoding
  , encodeWord16             -- :: Word16 -> Encoding
  , encodeWord32             -- :: Word32 -> Encoding
  , encodeWord64             -- :: Word64 -> Encoding
  , encodeInt                -- :: Int -> Encoding
  , encodeInt8               -- :: Int8 -> Encoding
  , encodeInt16              -- :: Int16 -> Encoding
  , encodeInt32              -- :: Int32 -> Encoding
  , encodeInt64              -- :: Int64 -> Encoding
  , encodeInteger            -- :: Integer -> Encoding
  , encodeBytes              -- :: B.ByteString -> Encoding
  , encodeBytesIndef         -- :: Encoding
  , encodeString             -- :: T.Text -> Encoding
  , encodeStringIndef        -- :: Encoding
  , encodeListLen            -- :: Word -> Encoding
  , encodeListLenIndef       -- :: Encoding
  , encodeMapLen             -- :: Word -> Encoding
  , encodeMapLenIndef        -- :: Encoding
  , encodeBreak              -- :: Encoding
  , encodeTag                -- :: Word -> Encoding
  , encodeTag64              -- :: Word64 -> Encoding
  , encodeBool               -- :: Bool -> Encoding
  , encodeUndef              -- :: Encoding
  , encodeNull               -- :: Encoding
  , encodeSimple             -- :: Word8 -> Encoding
  , encodeFloat16            -- :: Float -> Encoding
  , encodeFloat              -- :: Float -> Encoding
  , encodeDouble             -- :: Double -> Encoding
  ) where

#include "cbor.h"

import           Data.Int
import           Data.Word
#if !MIN_VERSION_base(4,8,0)
import           Data.Monoid
#endif

import qualified Data.ByteString as B
import qualified Data.Text       as T

import           Prelude         hiding (encodeFloat)

import {-# SOURCE #-} qualified Serialise.Cborg.FlatTerm as FlatTerm

-- | An intermediate form used during serialisation, specified as a
-- @'Monoid'@. It supports efficient concatenation, and is equivalent
-- to a specialised @'Data.Monoid.Endo' 'Tokens'@ type.
--
-- It is used for the stage in serialisation where we flatten out the
-- Haskell data structure but it is independent of any specific
-- external binary or text format.
--
-- Traditionally, to build any arbitrary @'Encoding'@ value, you specify
-- larger structures from smaller ones and append the small ones together
-- using @'Data.Monoid.mconcat'@.
--
-- @since 0.2.0.0
newtype Encoding = Encoding (Tokens -> Tokens)

instance Show Encoding where
  show = show . FlatTerm.toFlatTerm

-- | A flattened representation of a term, which is independent
-- of any underlying binary representation, but which we later
-- serialise into CBOR format.
--
-- @since 0.2.0.0
data Tokens =

    -- Positive and negative integers (type 0,1)
      TkWord     {-# UNPACK #-} !Word         Tokens
    | TkWord64   {-# UNPACK #-} !Word64       Tokens
      -- convenience for either positive or negative
    | TkInt      {-# UNPACK #-} !Int          Tokens
    | TkInt64    {-# UNPACK #-} !Int64        Tokens

    -- Bytes and string (type 2,3)
    | TkBytes    {-# UNPACK #-} !B.ByteString Tokens
    | TkBytesBegin                            Tokens
    | TkString   {-# UNPACK #-} !T.Text       Tokens
    | TkStringBegin                           Tokens

    -- Structures (type 4,5)
    | TkListLen  {-# UNPACK #-} !Word         Tokens
    | TkListBegin                             Tokens
    | TkMapLen   {-# UNPACK #-} !Word         Tokens
    | TkMapBegin                              Tokens

    -- Tagged values (type 6)
    | TkTag      {-# UNPACK #-} !Word         Tokens
    | TkTag64    {-# UNPACK #-} !Word64       Tokens
    | TkInteger                 !Integer      Tokens

    -- Simple and floats (type 7)
    | TkNull                                  Tokens
    | TkUndef                                 Tokens
    | TkBool                    !Bool         Tokens
    | TkSimple   {-# UNPACK #-} !Word8        Tokens
    | TkFloat16  {-# UNPACK #-} !Float        Tokens
    | TkFloat32  {-# UNPACK #-} !Float        Tokens
    | TkFloat64  {-# UNPACK #-} !Double       Tokens
    | TkBreak                                 Tokens

    | TkEnd
    deriving (Show,Eq)

-- | @since 0.2.0.0
instance Monoid Encoding where
  mempty = Encoding (\ts -> ts)
  {-# INLINE mempty #-}

  Encoding b1 `mappend` Encoding b2 = Encoding (\ts -> b1 (b2 ts))
  {-# INLINE mappend #-}

  mconcat = foldr mappend mempty
  {-# INLINE mconcat #-}

-- | Encode a @'Word'@ in a flattened format.
--
-- @since 0.2.0.0
encodeWord :: Word -> Encoding
encodeWord = Encoding . TkWord

-- | Encode a @'Word8'@ in a flattened format.
--
-- @since 0.2.0.0
encodeWord8 :: Word8 -> Encoding
encodeWord8 = Encoding . TkWord . fromIntegral

-- | Encode a @'Word16'@ in a flattened format.
--
-- @since 0.2.0.0
encodeWord16 :: Word16 -> Encoding
encodeWord16 = Encoding . TkWord . fromIntegral

-- | Encode a @'Word32'@ in a flattened format.
--
-- @since 0.2.0.0
encodeWord32 :: Word32 -> Encoding
encodeWord32 = Encoding . TkWord . fromIntegral

-- | Encode a @'Word64'@ in a flattened format.
--
-- @since 0.2.0.0
encodeWord64 :: Word64 -> Encoding
encodeWord64 = Encoding . TkWord64

-- | Encode an @'Int'@ in a flattened format.
--
-- @since 0.2.0.0
encodeInt :: Int -> Encoding
encodeInt = Encoding . TkInt

-- | Encode an @'Int8'@ in a flattened format.
--
-- @since 0.2.0.0
encodeInt8 :: Int8 -> Encoding
encodeInt8 = Encoding . TkInt . fromIntegral

-- | Encode an @'Int16'@ in a flattened format.
--
-- @since 0.2.0.0
encodeInt16 :: Int16 -> Encoding
encodeInt16 = Encoding . TkInt . fromIntegral

-- | Encode an @'Int32'@ in a flattened format.
--
-- @since 0.2.0.0
encodeInt32 :: Int32 -> Encoding
encodeInt32 = Encoding . TkInt . fromIntegral

-- | Encode an @'Int64' in a flattened format.
--
-- @since 0.2.0.0
encodeInt64 :: Int64 -> Encoding
encodeInt64 = Encoding . TkInt64

-- | Encode an arbitrarily large @'Integer' in a
-- flattened format.
--
-- @since 0.2.0.0
encodeInteger :: Integer -> Encoding
encodeInteger n = Encoding (TkInteger n)

-- | Encode an arbitrary strict @'B.ByteString'@ in
-- a flattened format.
--
-- @since 0.2.0.0
encodeBytes :: B.ByteString -> Encoding
encodeBytes = Encoding . TkBytes

-- | Encode a token specifying the beginning of a string of bytes of
-- indefinite length. In reality, this specifies a stream of many
-- occurrences of `encodeBytes`, each specifying a single chunk of the
-- overall string. After all the bytes desired have been encoded, you
-- should follow it with a break token (see @'encodeBreak'@).
--
-- @since 0.2.0.0
encodeBytesIndef :: Encoding
encodeBytesIndef = Encoding TkBytesBegin

-- | Encode a @'T.Text'@ in a flattened format.
--
-- @since 0.2.0.0
encodeString :: T.Text -> Encoding
encodeString = Encoding . TkString

-- | Encode the beginning of an indefinite string.
--
-- @since 0.2.0.0
encodeStringIndef :: Encoding
encodeStringIndef = Encoding TkStringBegin

-- | Encode the length of a list, used to indicate that the following
-- tokens represent the list values.
--
-- @since 0.2.0.0
encodeListLen :: Word -> Encoding
encodeListLen = Encoding . TkListLen

-- | Encode a token specifying that this is the beginning of an
-- indefinite list of unknown size. Tokens representing the list are
-- expected afterwords, followed by a break token (see
-- @'encodeBreak'@) when the list has ended.
--
-- @since 0.2.0.0
encodeListLenIndef :: Encoding
encodeListLenIndef = Encoding TkListBegin

-- | Encode the length of a Map, used to indicate that
-- the following tokens represent the map values.
--
-- @since 0.2.0.0
encodeMapLen :: Word -> Encoding
encodeMapLen = Encoding . TkMapLen

-- | Encode a token specifying that this is the beginning of an
-- indefinite map of unknown size. Tokens representing the map are
-- expected afterwords, followed by a break token (see
-- @'encodeBreak'@) when the map has ended.
--
-- @since 0.2.0.0
encodeMapLenIndef :: Encoding
encodeMapLenIndef = Encoding TkMapBegin

-- | Encode a \'break\', used to specify the end of indefinite
-- length objects like maps or lists.
--
-- @since 0.2.0.0
encodeBreak :: Encoding
encodeBreak = Encoding TkBreak

-- | Encode an arbitrary @'Word'@ tag.
--
-- @since 0.2.0.0
encodeTag :: Word -> Encoding
encodeTag = Encoding . TkTag

-- | Encode an arbitrary 64-bit @'Word64'@ tag.
--
-- @since 0.2.0.0
encodeTag64 :: Word64 -> Encoding
encodeTag64 = Encoding . TkTag64

-- | Encode a @'Bool'@.
--
-- @since 0.2.0.0
encodeBool :: Bool -> Encoding
encodeBool b = Encoding (TkBool b)

-- | Encode an @Undef@ value.
--
-- @since 0.2.0.0
encodeUndef :: Encoding
encodeUndef = Encoding TkUndef

-- | Encode a @Null@ value.
--
-- @since 0.2.0.0
encodeNull :: Encoding
encodeNull = Encoding TkNull

-- | Encode a \'simple\' CBOR token that can be represented with an
-- 8-bit word. You probably don't ever need this.
--
-- @since 0.2.0.0
encodeSimple :: Word8 -> Encoding
encodeSimple = Encoding . TkSimple

-- | Encode a small 16-bit @'Float'@ in a flattened format.
--
-- @since 0.2.0.0
encodeFloat16 :: Float -> Encoding
encodeFloat16 = Encoding . TkFloat16

-- | Encode a full precision @'Float'@ in a flattened format.
--
-- @since 0.2.0.0
encodeFloat :: Float -> Encoding
encodeFloat = Encoding . TkFloat32

-- | Encode a @'Double'@ in a flattened format.
--
-- @since 0.2.0.0
encodeDouble :: Double -> Encoding
encodeDouble = Encoding . TkFloat64
