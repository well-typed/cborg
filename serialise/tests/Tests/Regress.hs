module Tests.Regress
  ( testTree -- :: TestTree
  ) where

import           Test.Tasty

import qualified Tests.Regress.Issue13  as Issue13
import qualified Tests.Regress.Issue67  as Issue67
import qualified Tests.Regress.Issue80  as Issue80
import qualified Tests.Regress.Issue106 as Issue106
import qualified Tests.Regress.Issue135 as Issue135
import qualified Tests.Regress.FlatTerm as FlatTerm

--------------------------------------------------------------------------------
-- Tests and properties

testTree :: TestTree
testTree = testGroup "Regression tests"
  [ FlatTerm.testTree
  , Issue13.testTree
  , Issue67.testTree
  , Issue80.testTree
  , Issue106.testTree
  , Issue135.testTree
  ]
