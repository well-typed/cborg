module Main (main) where

import qualified Data.ByteString.Lazy   as LBS
import qualified Codec.Compression.GZip as GZip
import           Control.Monad (unless)
import           System.Directory (doesFileExist)
import           System.Process   (callProcess)

import           Codec.CBOR.JSON (encodeValue, decodeValue)
import           Codec.CBOR.Read (deserialiseFromBytes)
import           Codec.CBOR.Write (toLazyByteString, toStrictByteString)

import           Criterion.Main
                   ( defaultMain, Benchmark, bgroup, bench, env, nf, whnf )
import qualified Data.Aeson as Aeson (Value, decode, encode)


dataFile :: FilePath
dataFile = "bench/data.json.gz"

dataFileURL :: FilePath
dataFileURL = "https://github.com/well-typed/cborg/raw/master/cborg-json/bench/data.json.gz"

fetchLargeJSON :: IO ()
fetchLargeJSON = do
    exists <- doesFileExist dataFile
    unless exists $ do
      putStrLn $ "Fetching bechmark data file from " ++ dataFileURL
      callProcess "curl" [dataFileURL, "--output", dataFile]

loadLargeJSONBytes :: IO LBS.ByteString
loadLargeJSONBytes = do
    fetchLargeJSON
    GZip.decompress <$> LBS.readFile dataFile

loadLargeJSON :: IO Aeson.Value
loadLargeJSON = do
    bytes <- loadLargeJSONBytes
    let Just json = Aeson.decode bytes
    return json

loadLargeCBORBytes :: IO LBS.ByteString
loadLargeCBORBytes =
    toLazyByteString . encodeValue <$> loadLargeJSON

main :: IO ()
main = defaultMain benchmarks

benchmarks :: [Benchmark]
benchmarks =
    [ bgroup "Decode"
        [ env loadLargeJSONBytes $ \largeJSONBytes ->
          bench "JSON" (whnf (Aeson.decode :: LBS.ByteString -> Maybe Aeson.Value) largeJSONBytes)

        , env loadLargeCBORBytes $ \largeCBORBytes ->
          bench "CBOR" (whnf (deserialiseFromBytes (decodeValue False)) largeCBORBytes)

        , env loadLargeCBORBytes $ \largeCBORBytes ->
          bench "CBOR lenient" (whnf (deserialiseFromBytes (decodeValue True)) largeCBORBytes)
        ]
    , env loadLargeJSON $ \largeJSON ->
      bgroup "Encode"
        [ bench "JSON" (nf Aeson.encode largeJSON)
        , bench "CBOR" (nf (toLazyByteString . encodeValue) largeJSON)
        , bench "CBOR strict" (whnf (toStrictByteString . encodeValue) largeJSON)
        ]
    ]
