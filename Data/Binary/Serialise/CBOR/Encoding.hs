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
-- API for encoding values in a high level format, for later serialization
-- into CBOR binary values.
--
module Data.Binary.Serialise.CBOR.Encoding
  ( -- * Encoding implementation
    Encoding(..)             -- :: *
  , Tokens(..)               -- :: *

    -- * @'Encoding'@ API for serialisation
  , encodeWord               -- :: Word -> Encoding
  , encodeWord64             -- :: Word64 -> Encoding
  , encodeInt                -- :: Int -> Encoding
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
-- Traditionally, to build any arbitrary @'Encoding'@ value, you specify
-- larger structures from smaller ones and append the small ones together
-- using @'Data.Monoid.mconcat'@.
newtype Encoding = Encoding (Tokens -> Tokens)

-- | A flattened representation of a term, which is independent
-- of any underlying binary representation, but which we later
-- serialize into CBOR format.
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

instance Monoid Encoding where
  mempty = Encoding (\ts -> ts)
  {-# INLINE mempty #-}

  Encoding b1 `mappend` Encoding b2 = Encoding (\ts -> b1 (b2 ts))
  {-# INLINE mappend #-}

  mconcat = foldr mappend mempty
  {-# INLINE mconcat #-}

-- | Encode a @'Word'@ in a flattened format.
encodeWord :: Word -> Encoding
encodeWord = Encoding . TkWord

-- | Encode a @'Word64'@ in a flattened format.
encodeWord64 :: Word64 -> Encoding
encodeWord64 = Encoding . TkWord64

-- | Encode an @'Int'@ in a flattened format.
encodeInt :: Int -> Encoding
encodeInt = Encoding . TkInt

-- | Encode an @'Int64' in a flattened format.
encodeInt64 :: Int64 -> Encoding
encodeInt64 = Encoding . TkInt64

-- | Encode an arbitrarily large @'Integer' in a
-- flattened format.
encodeInteger :: Integer -> Encoding
encodeInteger n = Encoding (TkInteger n)

-- | Encode an arbitrary strict @'B.ByteString'@ in
-- a flattened format.
encodeBytes :: B.ByteString -> Encoding
encodeBytes = Encoding . TkBytes

-- | Encode 
encodeBytesIndef :: Encoding
encodeBytesIndef = Encoding TkBytesBegin

-- | Encode a @'T.Text'@ in a flattened format.
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

-- | Encode an arbitrary 64-bit @'Word64'@ tag in a flattened format.
encodeTag64 :: Word64 -> Encoding
encodeTag64 = Encoding . TkTag64

-- | Encode a @'Bool'@ in a flattened format.
encodeBool :: Bool -> Encoding
encodeBool b = Encoding (TkBool b)

encodeUndef :: Encoding
encodeUndef = Encoding TkUndef

encodeNull :: Encoding
encodeNull = Encoding TkNull

encodeSimple :: Word8 -> Encoding
encodeSimple = Encoding . TkSimple

-- | Encode a small 16-bit @'Float'@ in a flattened format.
encodeFloat16 :: Float -> Encoding
encodeFloat16 = Encoding . TkFloat16

-- | Encode a full precision @'Float'@ in a flattened format.
encodeFloat :: Float -> Encoding
encodeFloat = Encoding . TkFloat32

-- | Encode a @'Double'@ in a flattened format.
encodeDouble :: Double -> Encoding
encodeDouble = Encoding . TkFloat64
