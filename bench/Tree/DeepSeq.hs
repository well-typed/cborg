{-# OPTIONS_GHC -fno-warn-orphans #-}
module Tree.DeepSeq where

import Tree.Types

import Control.DeepSeq

instance NFData Tree where
  rnf Leaf = ()
  rnf (Fork a b) = rnf a `seq` rnf b `seq` ()

