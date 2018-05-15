module Tests.Regress.Issue162 ( testTree ) where

import           Control.Monad (void)
import           Data.Word
import           Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as LBS
import           Codec.CBOR.Decoding
import           Codec.CBOR.Read
import           Test.Tasty
import           Test.Tasty.HUnit

-- This example demonstrates a bug in cborg canonical decoding.
-- The bytes used here were drawn from a real application.

-- Decodes bigBytes x assuming x is a canonical integer encoding.
bigBytesDecoder :: Decoder s ()
bigBytesDecoder = ()
  <$ decodeListLenCanonical
  <* decodeMapLenCanonical
  <* decodeListLenCanonical
  <* decodeListLenCanonical
  <* decodeWord16Canonical
  <* decodeWord16Canonical
  <* decodeWord8Canonical
  <* decodeMapLenCanonical
  <* decodeMapLenCanonical
  <* decodeMapLenCanonical
  <* decodeListLenCanonical
  <* decodeWord8Canonical
  <* decodeListLenCanonical
  <* decodeListLenCanonical
  <* decodeMapLenCanonical
  <* decodeWord8Canonical
  <* decodeListLenCanonical
  <* decodeIntegerCanonical
  <* decodeWord32Canonical
  <* decodeWord8Canonical
  <* decodeListLenCanonical
  <* decodeIntegerCanonical
  <* decodeWord32Canonical

-- Encoding of 592033 :: BigInteger
-- 0xc2 means bignum, 0x43 means a 3-byte sequence
bigBytes :: [Word8] -> ByteString
bigBytes someEncodedInteger = LBS.pack $
    [ -- list of length 7
      0x87
      -- empty map
    , 0xa0
      -- Just
    , 0x81
      -- list of length 3, all items are 0
    , 0x83
    , 0x00, 0x00, 0x00
      -- empty maps
    , 0xa0
    , 0xa0
    , 0xa0
      -- singleton list (encoded Just)
    , 0x81
    , 0x00
    , 0x80
      -- singleton list (encoded Just)
    , 0x81
    , 0xa2
      -- key 0
    , 0x00
      -- value 0: a pair of numbers.
    , 0x82
    , 0x1a, 0x00, 0x04, 0xec, 0xf9
    , 0x1a, 0x1a, 0xeb, 0x97, 0x7a
      -- key 1
    , 0x01
      -- value 1: a pair of numbers.
    , 0x82 
    ] ++ someEncodedInteger ++
    [ 0x1a, 0x05, 0xee, 0x4d, 0x20
    ]

nonCanonicalInteger :: [Word8]
nonCanonicalInteger = [0xc2, 0x43, 0x09, 0x08, 0xa1]

shouldFailSimple :: Either DeserialiseFailure (LBS.ByteString, ())
shouldFailSimple = deserialiseFromBytes (void decodeIntegerCanonical) (LBS.pack nonCanonicalInteger)

shouldFailComposite :: Either DeserialiseFailure (LBS.ByteString, ())
shouldFailComposite = deserialiseFromBytes bigBytesDecoder (bigBytes nonCanonicalInteger)

testTree :: TestTree
testTree =
    testGroup "Issue 162 - canonical decoding"
        [ testCase "simple" (Left (DeserialiseFailure 0 "non-canonical integer") @=? shouldFailSimple)
        , testCase "composite" (Left (DeserialiseFailure 34 "non-canonical integer") @=? shouldFailComposite)
        ]
