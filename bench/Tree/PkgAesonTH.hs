{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Tree.PkgAesonTH where

import Tree.Types
import Data.Aeson as Aeson
import Data.Aeson.TH as Aeson
import Data.ByteString.Lazy as BS
import Data.Maybe

serialise :: Tree -> BS.ByteString
serialise = Aeson.encode

deserialise :: BS.ByteString -> Tree
deserialise = fromJust . Aeson.decode'

deriveJSON defaultOptions ''Tree

