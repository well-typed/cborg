module Serialise.Cborg.Encoding where

newtype Encoding = Encoding (Tokens -> Tokens)

data Tokens