{-# LANGUAGE BangPatterns #-}
module SimpleVersus
  ( benchmarks -- :: [Benchmark]
  ) where

import           Control.DeepSeq
import           Criterion.Main
import qualified Data.Binary                      as Binary
import qualified Data.Binary.Get                  as Binary
import           Data.Binary.Serialise.CBOR.Class
import           Data.Binary.Serialise.CBOR.Read
import           Data.Binary.Serialise.CBOR.Write
import qualified Data.ByteString                  as ByteStringStrict
import qualified Data.ByteString.Lazy             as ByteString
import qualified Data.ByteString.Lazy.Internal    as ByteString
import qualified Data.Serialize                   as Cereal
import qualified Data.Store                       as Store
import           Data.Vector.Serialize            ()
import qualified Data.Vector.Unboxed              as Unboxed

benchmarks :: [Benchmark]
benchmarks =
  [ bgroup "unboxed-vector"
      [ bgroup "serialise"
          [ bench "binary" $ nf binarySerialise vector
          , bench "cbor" $ nf cborSerialise vector
          , bench "cereal" $ nf cerealSerialise vector
          , bench "store" $ nf storeSerialise vector
          ]
      , bgroup "deserialise"
          [ bench "binary" $ nf binaryDeserialise binaryVector
          , bench "cbor" $ nf cborDeserialise cborVector
          , bench "cereal" $ nf cerealDeserialise cerealVector
          , bench "store" $ nf storeDeserialise storeVector
          ]
      ]
  ]
  where
    cborVector, cerealVector, binaryVector :: ByteString.ByteString
    !cborVector = force $ cborSerialise vector
    !cerealVector = force $ cerealSerialise vector
    !binaryVector = force $ binarySerialise vector
    storeVector :: ByteStringStrict.ByteString
    !storeVector = force $ storeSerialise vector
    !vector = Unboxed.fromList list
    list :: [Int]
    list = [1.. 1024 * 1024]

binarySerialise, cborSerialise, cerealSerialise
  :: Unboxed.Vector Int -> ByteString.ByteString
binarySerialise = Binary.encode
cborSerialise = toLazyByteString . encode
cerealSerialise = Cereal.encodeLazy
storeSerialise :: Unboxed.Vector Int -> ByteStringStrict.ByteString
storeSerialise = Store.encode

binaryDeserialise, cborDeserialise, cerealDeserialise
  :: ByteString.ByteString -> Unboxed.Vector Int
binaryDeserialise = Binary.decode
cerealDeserialise = (\(Right x) -> x) . Cereal.decodeLazy
cborDeserialise bs = feedAll (deserialiseIncremental decode) bs
  where
    feedAll (Binary.Done _ _ x)    _ = x
    feedAll (Binary.Partial k) lbs   = case lbs of
      ByteString.Chunk bs' lbs' -> feedAll (k (Just bs')) lbs'
      ByteString.Empty          -> feedAll (k Nothing) ByteString.empty
    feedAll (Binary.Fail _ pos msg) _ =
      error ("Data.Binary.Get.runGet at position " ++ show pos ++ ": " ++ msg)

storeDeserialise :: ByteStringStrict.ByteString -> Unboxed.Vector Int
storeDeserialise = (\(Right x) -> x) . Store.decode
