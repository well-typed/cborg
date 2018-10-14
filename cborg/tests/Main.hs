module Main (main) where

import           Test.Tasty (TestTree, defaultMain, testGroup)

import qualified Tests.Reference  as Reference
import qualified Tests.CBOR       as CBOR
import qualified Tests.Boundary   as Boundary
import qualified Tests.ByteOffset as ByteOffset
import qualified Tests.Regress    as Regress
import qualified Tests.UTF8       as UTF8

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests =
  testGroup "CBOR"
    [ Reference.testTree
    , CBOR.testTree
    , ByteOffset.testTree
    , Boundary.testTree
    , Regress.testTree
    , UTF8.testTree
    ]
