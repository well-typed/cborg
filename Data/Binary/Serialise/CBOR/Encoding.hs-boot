module Data.Binary.Serialise.CBOR.Encoding where

newtype Encoding = Encoding (Tokens -> Tokens)

data Tokens