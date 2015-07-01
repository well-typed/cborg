{-# OPTIONS_GHC -fno-warn-orphans -fsimpl-tick-factor=500 #-}
module Tree.PkgCereal (serialise, deserialise) where

import Tree.Types
import Data.Serialize as Cereal
import Data.ByteString.Lazy as BS

serialise :: Tree -> BS.ByteString
serialise = Cereal.encodeLazy

deserialise :: BS.ByteString -> Tree
deserialise = (\(Right x) -> x) . Cereal.decodeLazy

instance Serialize Tree

