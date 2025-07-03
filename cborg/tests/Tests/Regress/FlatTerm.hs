module Tests.Regress.FlatTerm
  ( testTree -- :: TestTree
  ) where

import           Data.Int

import           Test.Tasty
import           Test.Tasty.HUnit

import           Codec.CBOR.Encoding
import           Codec.CBOR.Decoding
import           Codec.CBOR.FlatTerm

--------------------------------------------------------------------------------
-- Tests and properties

-- | Test an edge case in the FlatTerm implementation: when encoding a word
-- larger than @'maxBound' :: 'Int'@, we store it as an 'Integer', and
-- need to remember to handle this case when we decode.
largeWordTest :: Either String Word
largeWordTest = fromFlatTerm decodeWord $ toFlatTerm (encodeWord largeWord)

largeWord :: Word
largeWord = fromIntegral (maxBound :: Int) + 1

-- | Test an edge case in the FlatTerm implementation: when encoding an
-- Int64 that is less than @'minBound' :: 'Int'@, make sure we use an
-- 'Integer' to store the result, because sticking it into an 'Int'
-- will result in overflow otherwise.
smallInt64Test :: Either String Int64
smallInt64Test = fromFlatTerm decodeInt64 $ toFlatTerm (encodeInt64 smallInt64)

smallInt64 :: Int64
smallInt64 = fromIntegral (minBound :: Int) - 1

--------------------------------------------------------------------------------
-- TestTree API

testTree :: TestTree
testTree = testGroup "FlatTerm regressions"
  [ testCase "Decoding of large-ish words" (Right largeWord  @=? largeWordTest)
  , testCase "Encoding of Int64s on 32bit" (Right smallInt64 @=? smallInt64Test)
  ]
