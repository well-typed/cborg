module Serialise.Cborg.Encoding.Types where

import           Data.Int
import           Data.Monoid
import           Data.Word
import           Prelude

import qualified Data.ByteString as B
import qualified Data.Text       as T

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
