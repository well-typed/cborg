{-# OPTIONS_GHC -fno-warn-orphans -fsimpl-tick-factor=500 #-}
module Tree.PkgBinary (serialise, deserialise) where

import Tree.Types
import Data.Binary as Binary
import Data.ByteString.Lazy as BS

serialise :: Tree -> BS.ByteString
serialise = Binary.encode

deserialise :: BS.ByteString -> Tree
deserialise = Binary.decode

instance Binary Tree

