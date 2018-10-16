module Main
  ( main -- :: IO ()
  ) where
import           Test.Tasty (defaultMain, testGroup)

import qualified Tests.CBOR      as CBOR
import qualified Tests.Boundary  as Boundary
import qualified Tests.ByteOffset as ByteOffset
import qualified Tests.Regress   as Regress
import qualified Tests.Reference as Reference
import qualified Tests.UTF8      as UTF8

main :: IO ()
main =
  Reference.loadTestCases >>= \tcs ->
  defaultMain $
  testGroup "CBOR tests"
    [ CBOR.testTree tcs
    , Reference.testTree tcs
    , ByteOffset.testTree
    , Boundary.testTree
    , Regress.testTree
    , UTF8.testTree
    ]
