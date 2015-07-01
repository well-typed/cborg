{-# LANGUAGE BangPatterns #-}
module Tree.Load (mkBigTree, mkBigTrees) where

import Tree.Types
import Tree.ReadShow ()


mkBigTrees :: Int -> Int -> [Tree]
mkBigTrees n depth =
   let !tree = mkBigTree depth
    in replicate n tree

mkBigTree :: Int -> Tree
mkBigTree 0     = Leaf
mkBigTree depth =
  let !subtree = mkBigTree (depth-1)
   in Fork subtree subtree

