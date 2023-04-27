{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import           Codec.CBOR.JSON
import           Codec.CBOR.Read
import           Data.Aeson (Value (String))
import qualified Data.ByteString.Base16 as HEX
import           Data.ByteString.Lazy (fromStrict)
import           Test.Tasty (TestTree, defaultMain, testGroup)
import           Test.Tasty.HUnit (testCase, (@=?))


main :: IO ()
main = do
  defaultMain tests


tests :: TestTree
tests =
  testGroup "CBOR-JSON"
    [ testGroup "unit tests"
        [ testCase "decode variable ByteString as Base62Url String" $
            Right ("", String "MDEy") @=? deserialiseFromBytes (decodeValue True)
                                              (fromStrict . either error id $ HEX.decode "5803303132")
        ]
    ]
