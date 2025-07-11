{-# OPTIONS_GHC -fno-warn-orphans -fsimpl-tick-factor=500 #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
module Micro.PkgStore (serialise, deserialise) where

import Micro.Types
import Data.Store as Store
import Data.ByteString as BS

serialise :: Tree -> BS.ByteString
serialise pkgs = Store.encode pkgs

deserialise :: BS.ByteString -> Tree
deserialise = (\(Right x) -> x) . Store.decode

instance Store Tree

