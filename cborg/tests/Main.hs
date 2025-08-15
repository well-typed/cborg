module Main (main) where

import           Test.Tasty (TestTree, defaultMain, testGroup)

import qualified Tests.Reference  as Reference
import qualified Tests.UnitTests  as UnitTests
import qualified Tests.Properties as Properties
import qualified Tests.Boundary   as Boundary
import qualified Tests.ByteOffset as ByteOffset
import qualified Tests.Canonical  as Canonical
import qualified Tests.Regress    as Regress
import qualified Tests.UTF8       as UTF8
import qualified Tests.PreEncoded as PreEncoded
import qualified Tests.GetInputSpan as GetInputSpan

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests =
  testGroup "CBOR"
    [ Reference.testTree
    , UnitTests.testTree
    , Properties.testTree
    , ByteOffset.testTree
    , GetInputSpan.testTree
    , Boundary.testTree
    , Canonical.testTree
    , Regress.testTree
    , UTF8.testTree
    , PreEncoded.testTree
    ]
