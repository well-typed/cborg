{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import           Codec.CBOR.Encoding
import           Codec.CBOR.JSON
import           Codec.CBOR.Read
import           Codec.CBOR.Write
import           Data.Aeson (toJSON, Value (String))
import qualified Data.ByteString.Base16 as HEX
import           Data.ByteString.Lazy (fromStrict)
import           Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Text as T
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
        , testCase "decode Object encoded as indefinite length Map" $
            Right ("", toJSON testMap) @=? deserialiseFromBytes (decodeValue True)
                                              (toLazyByteString $ encodeMapIndef testMap)
        ]
    ]

testMap :: Map T.Text Value
testMap = Map.fromList $ [ (T.pack $ show n, toJSON (n :: Int)) | n <- [0..10]]

encodeMapIndef :: Map T.Text Value -> Encoding
encodeMapIndef vs =
     encodeMapLenIndef
  <> Map.foldrWithKey (\k v r -> encodeString k <> encodeValue v <> r) mempty vs
  <> encodeBreak
