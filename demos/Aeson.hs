-- Simple module for encoding and decoding Aeson values that you can copy.

{-# LANGUAGE CPP          #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}  -- instance Serialise Value
module Aeson where

import           Data.Aeson                          ( Value(..) )
import           Data.Scientific
import qualified Data.Vector                         as V

import           Serialise.Cborg.Encoding
import           Serialise.Cborg.Decoding

import           Serialise.Cborg.Class

--------------------------------------------------------------------------------
-- Encoder from JSON values to CBOR values

instance Serialise Value where
  encode = encodeValue
  decode = decodeValue

encodeValue :: Value -> Encoding
encodeValue (Object vs) = encode vs
encodeValue (Array  vs) = encode vs

encodeValue (String s)  = encode s
encodeValue (Number n)  = case floatingOrInteger n of
                            Left  d -> encode (d::Double)
                            Right i -> encode (i::Integer)
encodeValue (Bool   b)  = encode b
encodeValue  Null       = encodeNull

--------------------------------------------------------------------------------
-- Decoder from CBOR values to JSON values

decodeValue :: Decoder s Value
decodeValue = do
    tkty <- peekTokenType
    case tkty of
      TypeUInt    -> decodeNumberIntegral
      TypeUInt64  -> decodeNumberIntegral
      TypeNInt    -> decodeNumberIntegral
      TypeNInt64  -> decodeNumberIntegral
      TypeInteger -> decodeNumberIntegral
      TypeFloat64 -> decodeNumberFloating

      TypeString       -> String <$> decode
      TypeListLen      -> Array  <$> decode
      TypeListLenIndef -> decodeIndefList
      TypeMapLen       -> Object <$> decode

      TypeBool    -> Bool   <$> decodeBool
      TypeNull    -> Null   <$  decodeNull
      _           -> fail $ "unexpected CBOR token type for a JSON value: "
                         ++ show tkty

decodeIndefList :: Decoder s Value
decodeIndefList = Array . V.fromList <$> decode

decodeNumberIntegral :: Decoder s Value
decodeNumberIntegral = Number . fromInteger <$> decode

decodeNumberFloating :: Decoder s Value
decodeNumberFloating = Number . fromFloatDigits <$> (decode :: Decoder s Double)
