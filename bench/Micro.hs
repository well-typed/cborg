{-# LANGUAGE CPP          #-}
{-# LANGUAGE BangPatterns #-}
module Micro
  ( benchmarks -- :: [Benchmark]
  ) where

import           Criterion.Main
import           Control.DeepSeq
import qualified Data.ByteString.Lazy   as BS

import           Foreign

import           Data.Binary.Serialise.CBOR.ByteOrder

import           Macro.DeepSeq ()
import qualified Micro.MemSize
import           Micro.DeepSeq ()
import qualified Micro.Load      as Micro.Load
import qualified Micro.Types     as Micro.Types ()

import qualified Micro.ReadShow  as Micro.ReadShow
import qualified Micro.PkgBinary as Micro.PkgBinary
import qualified Micro.PkgCereal as Micro.PkgCereal
import qualified Micro.PkgAesonGeneric as Micro.PkgAesonGeneric
import qualified Micro.PkgAesonTH as Micro.PkgAesonTH
--import qualified Macro.PkgMsgpack as PkgMsgpack
import qualified Micro.CBOR as Micro.CBOR

benchmarks :: [Benchmark]
benchmarks =
  [ bgroup "reference"
      [ bench "deepseq" (whnf rnf tstdata)
      , bench "memSize" (whnf (flip Micro.MemSize.memSize 0) tstdata)
      ]
  , bgroup "encoding" $ deepseq tstdata
      [ bench "binary"        (whnf perfEncodeBinary       tstdata)
      , bench "cereal"        (whnf perfEncodeCereal       tstdata)
      , bench "aeson generic" (whnf perfEncodeAesonGeneric tstdata)
      , bench "aeson TH"      (whnf perfEncodeAesonTH      tstdata)
      , bench "read/show"     (whnf perfEncodeReadShow     tstdata)
--    , bench "msgpack lib"   (whnf perfEncodeMsgpack      tstdata)
--    , bench "new msgpack"   (whnf perfEncodeNewMsgPack   tstdata)
      , bench "cbor"          (whnf perfEncodeCBOR         tstdata)
      ]
  , bgroup "decoding" $ deepseq (tstdataB, tstdataC, tstdataA, tstdataS,
                                  tstdataR)
      [ bench "binary"        (whnf perfDecodeBinary       tstdataB)
      , bench "cereal"        (whnf perfDecodeCereal       tstdataC)
      , bench "aeson generic" (whnf perfDecodeAesonGeneric tstdataA)
      , bench "aeson TH"      (whnf perfDecodeAesonTH      tstdataA)
      , bench "read/show"     (whnf perfDecodeReadShow     tstdataS)
--    , bench "msgpack lib"   (whnf perfDecodeMsgpack      tstdataM)
--    , bench "new msgpack"   (whnf perfDecodeNewMsgPack   tstdataN)
      , bench "cbor"          (whnf perfDecodeCBOR         tstdataR)
      ]
  , bgroup "decoding + deepseq" $ deepseq (tstdataB, tstdataC, tstdataA,
                                           tstdataS, tstdataR)
      [ bench "binary"        (nf perfDecodeBinary       tstdataB)
      , bench "cereal"        (nf perfDecodeCereal       tstdataC)
      , bench "aeson generic" (nf perfDecodeAesonGeneric tstdataA)
      , bench "aeson TH"      (nf perfDecodeAesonTH      tstdataA)
      , bench "read/show"     (nf perfDecodeReadShow     tstdataS)
--    , bench "msgpack lib"   (nf perfDecodeMsgpack      tstdataM)
--    , bench "new msgpack"   (nf perfDecodeNewMsgPack   tstdataN)
      , bench "cbor"          (nf perfDecodeCBOR         tstdataR)
      ]
  , env lowlevelPtrEnv $ \ptr ->
    bgroup "lowlevel"
      [ bench "grabWord16"    (nf grabWord16 ptr)
      , bench "grabWord32"    (nf grabWord32 ptr)
      , bench "grabWord64"    (nf grabWord64 ptr)
      ]
  ]
  where
    -- Input data
    tstdata = Micro.Load.mkBigTree 16 -- tree of size 2^16
    tstdataB = Micro.PkgBinary.serialise tstdata
    tstdataC = Micro.PkgCereal.serialise tstdata
    tstdataA = Micro.PkgAesonTH.serialise tstdata
    tstdataS = Micro.ReadShow.serialise tstdata
--  tstdataM = PkgMsgpack.serialise tstdata
--  tstdataN = Micro.NewMsgpack.serialise tstdata
    tstdataR = Micro.CBOR.serialise tstdata

    -- Encoding tests
    perfEncodeBinary       = BS.length . Micro.PkgBinary.serialise
    perfEncodeCereal       = BS.length . Micro.PkgCereal.serialise
    perfEncodeAesonGeneric = BS.length . Micro.PkgAesonGeneric.serialise
    perfEncodeAesonTH      = BS.length . Micro.PkgAesonTH.serialise
    perfEncodeReadShow     = BS.length . Micro.ReadShow.serialise
--  perfEncodeMsgpack      = BS.length . Micro.PkgMsgpack.serialise
--  perfEncodeNewMsgPack   = BS.length . Micro.NewMsgpack.serialise
    perfEncodeCBOR         = BS.length . Micro.CBOR.serialise

    -- Decoding tests
    perfDecodeBinary       = Micro.PkgBinary.deserialise
    perfDecodeCereal       = Micro.PkgCereal.deserialise
    perfDecodeAesonGeneric = Micro.PkgAesonGeneric.deserialise
    perfDecodeAesonTH      = Micro.PkgAesonTH.deserialise
    perfDecodeReadShow     = Micro.ReadShow.deserialise
--  perfDecodeMsgpack      = PkgMsgpack.deserialise
--  perfDecodeNewMsgPack   = Micro.NewMsgpack.deserialise
    perfDecodeCBOR         = Micro.CBOR.deserialise

    -- | Allocate an 8-byte pointer, write a 64-bit word into
    -- it, and return a @'Ptr' ()@ to be used by the low-level routines.
    lowlevelPtrEnv :: IO (Ptr ())
    lowlevelPtrEnv = do
      ptr <- mallocBytes 8
      poke ptr (0xDEADBEEFCAFEBABE :: Word64)
      return (castPtr ptr)

--------------------------------------------------------------------------------

-- An NFData instance for Ptr is in deepseq HEAD/1.4.2, but it's not released.
#if !MIN_VERSION_deepseq(1,4,2)
instance NFData (Ptr a) where rnf !_ = ()
#endif
