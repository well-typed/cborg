module Tests.Regress
  ( testTree -- :: TestTree
  ) where

import           Test.Tasty

import qualified Tests.Regress.Issue160 as Issue160
import qualified Tests.Regress.Issue162 as Issue162
import qualified Tests.Regress.FlatTerm as FlatTerm

--------------------------------------------------------------------------------
-- Tests and properties

testTree :: TestTree
testTree = testGroup "Regression tests"
  [ FlatTerm.testTree
  , Issue160.testTree
  , Issue162.testTree
  ]
