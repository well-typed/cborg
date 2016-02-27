module Main
  ( main -- :: IO ()
  ) where
import           Test.Tasty (defaultMain, testGroup)

import qualified Tests.CBOR        as CBORTests
import qualified Tests.Safe        as SafeTests
import qualified Tests.Serialise   as SerialiseTests
import qualified Tests.Reference   as ReferenceTests

main :: IO ()
main = ReferenceTests.loadTestCases >>= \tcs -> defaultMain $
  testGroup "CBOR tests"
    [ ReferenceTests.testTree tcs
    , CBORTests.testTree tcs
    , SafeTests.testTree
    , SerialiseTests.testTree
    ]
