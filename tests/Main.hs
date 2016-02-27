module Main
  ( main -- :: IO ()
  ) where
import           Test.Tasty (defaultMain, testGroup)

import qualified Tests.CBOR        as CBORTests
import qualified Tests.Regress     as RegressTests
import qualified Tests.Reference   as ReferenceTests
import qualified Tests.Serialise   as SerialiseTests

main :: IO ()
main = ReferenceTests.loadTestCases >>= \tcs -> defaultMain $
  testGroup "CBOR tests"
    [ CBORTests.testTree tcs
    , ReferenceTests.testTree tcs
    , SerialiseTests.testTree
    , RegressTests.testTree
    ]
