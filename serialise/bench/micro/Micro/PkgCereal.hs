{-# OPTIONS_GHC -fno-warn-orphans -fsimpl-tick-factor=500 #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
module Micro.PkgCereal (serialise, deserialise) where

import Micro.Types
import Data.Serialize as Cereal
import Data.ByteString.Lazy as BS

serialise :: Tree -> BS.ByteString
serialise = Cereal.encodeLazy

deserialise :: BS.ByteString -> Tree
deserialise = (\(Right x) -> x) . Cereal.decodeLazy

instance Serialize Tree

