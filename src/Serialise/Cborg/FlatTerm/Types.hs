module Serialise.Cborg.FlatTerm.Types where

import           Data.Word
import           Data.Text (Text)
import           Data.ByteString (ByteString)

-- | A "flat" representation of an @'Enc.Encoding'@ value,
-- useful for round-tripping and writing tests.
--
-- @since 0.2.0.0
type FlatTerm = [TermToken]

-- | A concrete encoding of @'Enc.Encoding'@ values, one
-- which mirrors the original @'Enc.Encoding'@ type closely.
--
-- @since 0.2.0.0
data TermToken
    = TkInt      {-# UNPACK #-} !Int
    | TkInteger                 !Integer
    | TkBytes    {-# UNPACK #-} !ByteString
    | TkBytesBegin
    | TkString   {-# UNPACK #-} !Text
    | TkStringBegin
    | TkListLen  {-# UNPACK #-} !Word
    | TkListBegin
    | TkMapLen   {-# UNPACK #-} !Word
    | TkMapBegin
    | TkBreak
    | TkTag      {-# UNPACK #-} !Word64
    | TkBool                    !Bool
    | TkNull
    | TkSimple   {-# UNPACK #-} !Word8
    | TkFloat16  {-# UNPACK #-} !Float
    | TkFloat32  {-# UNPACK #-} !Float
    | TkFloat64  {-# UNPACK #-} !Double
    deriving (Eq, Ord, Show)
