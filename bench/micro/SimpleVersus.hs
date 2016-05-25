{-# LANGUAGE BangPatterns #-}
module SimpleVersus
  ( benchmarks -- :: [Benchmark]
  ) where

import           Control.DeepSeq
import           Criterion.Main
import qualified Data.Binary                      as Binary
import qualified Data.Binary.Serialise.CBOR       as CBOR
import qualified Data.ByteString.Lazy             as ByteString
import qualified Data.Serialize                   as Cereal
import           Data.Vector.Serialize            ()
import qualified Data.Vector.Unboxed              as Unboxed

benchmarks :: [Benchmark]
benchmarks =
  [ bgroup "unboxed-vector"
      [ bgroup "serialise"
          [ bench "binary" $ nf binarySerialise vector
          , bench "cbor" $ nf cborSerialise vector
          , bench "cereal" $ nf cerealSerialise vector
          ]
      , bgroup "deserialise"
          [ bench "binary" $ nf binaryDeserialise binaryVector
          , bench "cbor" $ nf cborDeserialise cborVector
          , bench "cereal" $ nf cerealDeserialise cerealVector
          ]
      ]
  ]
  where
    cborVector, cerealVector, binaryVector :: ByteString.ByteString
    !cborVector = force $ cborSerialise vector
    !cerealVector = force $ cerealSerialise vector
    !binaryVector = force $ binarySerialise vector
    !vector = Unboxed.fromList list
    list :: [Int]
    list = [1.. 1024 * 1024]

binarySerialise, cborSerialise, cerealSerialise
  :: Unboxed.Vector Int -> ByteString.ByteString
binarySerialise = Binary.encode
cborSerialise   = CBOR.serialise
cerealSerialise = Cereal.encodeLazy

binaryDeserialise, cborDeserialise, cerealDeserialise
  :: ByteString.ByteString -> Unboxed.Vector Int
binaryDeserialise = Binary.decode
cerealDeserialise = (\(Right x) -> x) . Cereal.decodeLazy
cborDeserialise   = CBOR.deserialise
