module Codec.CBOR.Encoding where

newtype Encoding = Encoding (Tokens -> Tokens)

data Tokens