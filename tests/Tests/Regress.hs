{-# LANGUAGE CPP                  #-}
{-# LANGUAGE ConstraintKinds      #-}
{-# LANGUAGE ScopedTypeVariables  #-}
module Tests.Regress
  ( testTree -- :: TestTree
  ) where

import           Test.Tasty

import qualified Tests.Regress.Issue13  as Issue13

--------------------------------------------------------------------------------
-- Tests and properties

testTree :: TestTree
testTree = testGroup "Regression tests"
  [ Issue13.testTree
  ]
