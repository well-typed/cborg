{-# LANGUAGE TemplateHaskell, StandaloneDeriving, GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Micro.PkgMsgpack where

import Micro.Types
import qualified Data.MessagePack as MsgPack
import Data.MessagePack (deriveObject)
import Data.ByteString.Lazy as BS


serialise :: Tree -> BS.ByteString
serialise = MsgPack.pack

deserialise :: BS.ByteString -> Tree
deserialise = MsgPack.unpack

deriveObject False ''Tree

