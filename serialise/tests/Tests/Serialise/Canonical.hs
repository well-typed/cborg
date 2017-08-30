{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Tests.Serialise.Canonical where

import Data.Int
import Data.Typeable
import Data.Word
import Test.QuickCheck

import Codec.CBOR.Decoding
import Codec.CBOR.Encoding as E
import Codec.Serialise.Class

newtype Canonical a = Canonical { fromCanonical :: a }
  deriving (Arbitrary, Typeable, Eq, Show)

instance Serialise (Canonical Word) where
  encode = encodeWord . fromCanonical
  decode = Canonical <$> decodeWordCanonical

instance Serialise (Canonical Word8) where
  encode = encodeWord8 . fromCanonical
  decode = Canonical <$> decodeWord8Canonical

instance Serialise (Canonical Word16) where
  encode = encodeWord16 . fromCanonical
  decode = Canonical <$> decodeWord16Canonical

instance Serialise (Canonical Word32) where
  encode = encodeWord32 . fromCanonical
  decode = Canonical <$> decodeWord32Canonical

instance Serialise (Canonical Word64) where
  encode = encodeWord64 . fromCanonical
  decode = Canonical <$> decodeWord64Canonical

instance Serialise (Canonical Int) where
  encode = encodeInt . fromCanonical
  decode = Canonical <$> decodeIntCanonical

instance Serialise (Canonical Int8) where
  encode = encodeInt8 . fromCanonical
  decode = Canonical <$> decodeInt8Canonical

instance Serialise (Canonical Int16) where
  encode = encodeInt16 . fromCanonical
  decode = Canonical <$> decodeInt16Canonical

instance Serialise (Canonical Int32) where
  encode = encodeInt32 . fromCanonical
  decode = Canonical <$> decodeInt32Canonical

instance Serialise (Canonical Int64) where
  encode = encodeInt64 . fromCanonical
  decode = Canonical <$> decodeInt64Canonical

instance Serialise (Canonical Float) where
    encode = E.encodeFloat . fromCanonical
    decode = Canonical <$> decodeFloatCanonical

instance Serialise (Canonical Double) where
    encode = encodeDouble . fromCanonical
    decode = Canonical <$> decodeDoubleCanonical
