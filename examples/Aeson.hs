{-# LANGUAGE BangPatterns #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Data.Binary.Serialise.CBOR.Aeson () where

import Data.Aeson
import Data.Scientific
import qualified Data.Vector as Vec
import qualified Data.HashMap.Lazy as HashMap

import Data.Binary.Serialise.CBOR.Encoding
import Data.Binary.Serialise.CBOR.Decoding
import Data.Binary.Serialise.CBOR

import Data.Monoid
import Control.Applicative


instance Serialise Value where
  encode = encodeValue
  decode = decodeValue

encodeValue :: Value -> Encoding

encodeValue (Object vs) = encodeMapLen (fromIntegral $ HashMap.size vs)
                       <> mconcat [ encodeString k <> encodeValue v
                                  | (k,v) <- HashMap.toList vs ]
encodeValue (Array  vs) = encodeListLen (fromIntegral $ Vec.length vs)
                       <> mconcat [ encodeValue v | v <- Vec.toList vs ]

encodeValue (String s)  = encodeString s
encodeValue (Number n)  = case floatingOrInteger n of
                            Left  d -> encodeDouble d
                            Right i -> encodeInteger i
encodeValue (Bool   b)  = encodeBool b
encodeValue  Null       = encodeNull

decodeValue :: Decoder Value
decodeValue = do
    tkty <- peekTokenType
    case tkty of
      TypeUInt    -> decodeNumberIntegral
      TypeUInt64  -> decodeNumberIntegral
      TypeNInt    -> decodeNumberIntegral
      TypeNInt64  -> decodeNumberIntegral
      TypeInteger -> decodeNumberIntegral
      TypeFloat64 -> decodeNumberFloating

      TypeString  -> String <$> decodeString
      TypeListLen -> decodeListLen >>= decodeArray
      TypeMapLen  -> decodeMapLen  >>= decodeObject

      TypeBool    -> Bool   <$> decodeBool
      TypeNull    -> Null   <$  decodeNull
      _           -> fail $ "unexpected CBOR token type for a JSON value: "
                         ++ show tkty

decodeNumberIntegral :: Decoder Value
decodeNumberIntegral = Number . fromInteger <$> decodeInteger

decodeNumberFloating :: Decoder Value
decodeNumberFloating = Number . fromFloatDigits <$> decodeDouble

decodeArray :: Int -> Decoder Value
decodeArray n0 = do
    vs <- go n0 []
    return $! Array (Vec.fromListN n0 (reverse vs))
  where
    go 0 acc = return acc
    go n acc = do t <- decodeValue
                  go (n-1) (t : acc)

decodeObject :: Int -> Decoder Value
decodeObject n0 = do
    kvs <- go n0 []
    return $! Object (HashMap.fromList kvs)
  where
    go 0 acc = return acc
    go n acc = do k <- decodeString
                  v <- decodeValue
                  go (n-1) ((k, v) : acc)

