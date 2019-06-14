{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}


module Main
  ( main -- :: IO ()
  ) where

import           Criterion.Main (bgroup, defaultMain, bench, nf)
import           Data.ByteString.Lazy (ByteString, fromStrict)
import           Data.FileEmbed (embedFile)
import           Data.Aeson (Value, decode, encode)
import           Codec.CBOR.JSON (encodeValue, decodeValue)
import           Codec.CBOR.Read (deserialiseFromBytes)
import           Codec.CBOR.Write (toLazyByteString)
-- import           Paths_app

largeJSONBytes :: ByteString
largeJSONBytes = fromStrict $(embedFile "bench/availcap.czml")

largeJSON :: Value
Just largeJSON = decode largeJSONBytes

largeCBORBytes :: ByteString
largeCBORBytes = toLazyByteString $ encodeValue largeJSON

main :: IO ()
main = defaultMain
    [ bgroup "Decode"
        [ bench "JSON" (nf (decode @Value) largeJSONBytes)
        , bench "CBOR" (nf (deserialiseFromBytes (decodeValue False)) largeCBORBytes)
        , bench "CBOR lenient" (nf (deserialiseFromBytes (decodeValue True)) largeCBORBytes)
        ]
    , bgroup "Encode"
        [ bench "JSON" (nf encode largeJSON)
        , bench "CBOR" (nf (toLazyByteString . encodeValue) largeJSON)
        ]
    ]