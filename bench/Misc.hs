{-# OPTIONS_GHC -fno-cse -fno-ignore-asserts #-}
module Misc
  ( benchmarks -- :: IO ()
  ) where

import           Macro.DeepSeq ()
import qualified Macro.Load as Load

import qualified Macro.PkgBinary as PkgBinary
import qualified Macro.PkgCereal as PkgCereal
import qualified Macro.CBOR as CBOR

import qualified Data.ByteString.Lazy   as BS
import qualified Codec.Compression.GZip as GZip
import           Control.Exception
import           System.Environment
import           Data.Time

benchmarks :: IO ()
benchmarks = do
  args <- getArgs
  case args of
    ["prep-decode-binary"] -> do
      Right pkgs_ <- fmap (Load.readPkgIndex . GZip.decompress)
                          (BS.readFile "bench/00-index.tar.gz")
      let pkgs = take 20000 pkgs_
      BS.writeFile "bench/binary.bin" (PkgBinary.serialise pkgs)

    ["prep-decode-cereal"] -> do
      Right pkgs_ <- fmap (Load.readPkgIndex . GZip.decompress)
                          (BS.readFile "bench/00-index.tar.gz")
      let pkgs = take 20000 pkgs_
      BS.writeFile "bench/cereal.bin" (PkgCereal.serialise pkgs)

    ["prep-decode-cbor"] -> do
      Right pkgs_ <- fmap (Load.readPkgIndex . GZip.decompress)
                          (BS.readFile "bench/00-index.tar.gz")
      let pkgs = take 20000 pkgs_
      BS.writeFile "bench/cbor.bin" (CBOR.serialise pkgs)

    ["prep-decode-cbor-small"] -> do
      Right pkgs_ <- fmap (Load.readPkgIndex . GZip.decompress)
                          (BS.readFile "bench/00-index.tar.gz")
      let pkgs = take 1000 pkgs_
      BS.writeFile "bench/cbor-small.bin" (CBOR.serialise pkgs)

    ["perf-decode-binary"] -> do
      pkgdata <- BS.readFile "bench/binary.bin"
      time $ evaluate (BS.length pkgdata)
      time $ evaluate (length $ PkgBinary.deserialise pkgdata)

    ["perf-decode-cereal"] -> do
      pkgdata <- BS.readFile "bench/cereal.bin"
      time $ evaluate (BS.length pkgdata)
      time $ evaluate (length $ PkgCereal.deserialise pkgdata)

    ["perf-decode-cbor"] -> do
      pkgdata <- BS.readFile "bench/cbor.bin"
      time $ evaluate (BS.length pkgdata)
      time $ evaluate (length $ CBOR.deserialise pkgdata)

    ["perf-decode-binary-noaccum"] -> do
      pkgdata <- BS.readFile "bench/binary.bin"
      time $ evaluate (BS.length pkgdata)
      time $ evaluate (PkgBinary.deserialiseNull pkgdata)

    ["perf-decode-cereal-noaccum"] -> do
      pkgdata <- BS.readFile "bench/cereal.bin"
      time $ evaluate (BS.length pkgdata)
      time $ evaluate (PkgCereal.deserialiseNull pkgdata)

    ["perf-decode-cbor-noaccum"] -> do
      pkgdata <- BS.readFile "bench/cbor.bin"
      time $ evaluate (BS.length pkgdata)
      time $ evaluate (CBOR.deserialiseNull pkgdata)

    ["perf-decode-cbor-noaccum-small"] -> do
      pkgdata <- BS.readFile "bench/cbor-small.bin"
      time $ evaluate (BS.length pkgdata)
      time $ evaluate (CBOR.deserialiseNull pkgdata)

    [] -> fail "no arguments provided"
    _  -> fail "invalid arguments"

time :: Show a => IO a -> IO ()
time action = do
  t   <- getCurrentTime
  x   <- action
  t'  <- getCurrentTime
  putStrLn $ show (diffUTCTime t' t) ++ ":\t" ++ show x
