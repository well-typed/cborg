{-# LANGUAGE DeriveGeneric #-}
module Tree.Types (Tree(..)) where

import GHC.Generics

data Tree = Leaf | Fork Tree Tree
    deriving (Eq, Ord, Generic)

