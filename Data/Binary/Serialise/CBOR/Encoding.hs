{-# LANGUAGE CPP #-}

-- |
-- Module      : Data.Binary.Serialise.CBOR.Encoding
-- Copyright   : (c) Duncan Coutts 2015
-- License     : BSD3-style (see LICENSE.txt)
--
-- Maintainer  : duncan@community.haskell.org
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Lorem ipsum...
--
module Data.Binary.Serialise.CBOR.Encoding
  ( -- * Encoding implementation
    Encoding(..)
  , Tokens(..)

    -- * @'Encoder'@ API for serialisation
  , encodeWord
  , encodeWord64
  , encodeInt
  , encodeInt64
  , encodeInteger
  , encodeBytes
  , encodeBytesIndef
  , encodeString
  , encodeStringIndef
  , encodeListLen
  , encodeListLenIndef
  , encodeMapLen
  , encodeMapLenIndef
  , encodeBreak
  , encodeTag
  , encodeTag64
  , encodeBool
  , encodeUndef
  , encodeNull
  , encodeSimple
  , encodeFloat16
  , encodeFloat
  , encodeDouble
  ) where

import           Data.Int
import           Data.Word
#if __GLASGOW_HASKELL__ < 710
import           Data.Monoid
#endif

import qualified Data.ByteString as B
import qualified Data.Text       as T

import           Prelude         hiding (encodeFloat)

-- | An intermediate form used during serialisation, specified as a
-- @'Monoid'@. It supports efficient concatenation, and is equivalent
-- to a specialised @'Data.Monoid.Endo' 'Tokens'@ type.
--
-- It is used for the stage in serialisation where we flatten out the
-- Haskell data structure but it is independent of any specific
-- external binary or text format.
--
newtype Encoding = Encoding (Tokens -> Tokens)

-- | A flattened representation of a term
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

instance Monoid Encoding where
  mempty = Encoding (\ts -> ts)
  {-# INLINE mempty #-}

  Encoding b1 `mappend` Encoding b2 = Encoding (\ts -> b1 (b2 ts))
  {-# INLINE mappend #-}

  mconcat = foldr mappend mempty
  {-# INLINE mconcat #-}

encodeWord :: Word -> Encoding
encodeWord = Encoding . TkWord

encodeWord64 :: Word64 -> Encoding
encodeWord64 = Encoding . TkWord64

encodeInt :: Int -> Encoding
encodeInt = Encoding . TkInt

encodeInt64 :: Int64 -> Encoding
encodeInt64 = Encoding . TkInt64

encodeInteger :: Integer -> Encoding
encodeInteger n = Encoding (TkInteger n)

encodeBytes :: B.ByteString -> Encoding
encodeBytes = Encoding . TkBytes

encodeBytesIndef :: Encoding
encodeBytesIndef = Encoding TkBytesBegin

encodeString :: T.Text -> Encoding
encodeString = Encoding . TkString

encodeStringIndef :: Encoding
encodeStringIndef = Encoding TkStringBegin

encodeListLen :: Word -> Encoding
encodeListLen = Encoding . TkListLen

encodeListLenIndef :: Encoding
encodeListLenIndef = Encoding TkListBegin

encodeMapLen :: Word -> Encoding
encodeMapLen = Encoding . TkMapLen

encodeMapLenIndef :: Encoding
encodeMapLenIndef = Encoding TkMapBegin

encodeBreak :: Encoding
encodeBreak = Encoding TkBreak

encodeTag :: Word -> Encoding
encodeTag = Encoding . TkTag

encodeTag64 :: Word64 -> Encoding
encodeTag64 = Encoding . TkTag64

encodeBool :: Bool -> Encoding
encodeBool b = Encoding (TkBool b)

encodeUndef :: Encoding
encodeUndef = Encoding TkUndef

encodeNull :: Encoding
encodeNull = Encoding TkNull

encodeSimple :: Word8 -> Encoding
encodeSimple = Encoding . TkSimple

encodeFloat16 :: Float -> Encoding
encodeFloat16 = Encoding . TkFloat16

encodeFloat :: Float -> Encoding
encodeFloat = Encoding . TkFloat32

encodeDouble :: Double -> Encoding
encodeDouble = Encoding . TkFloat64
