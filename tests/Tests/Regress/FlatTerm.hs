{-# LANGUAGE CPP                  #-}
{-# LANGUAGE ConstraintKinds      #-}
{-# LANGUAGE ScopedTypeVariables  #-}
module Tests.Regress.FlatTerm
  ( testTree -- :: TestTree
  ) where

import           Test.Tasty
import           Test.Tasty.HUnit

import           Data.Binary.Serialise.CBOR.Encoding
import           Data.Binary.Serialise.CBOR.Decoding
import           Data.Binary.Serialise.CBOR.FlatTerm

--------------------------------------------------------------------------------
-- Tests and properties

testTree :: TestTree
testTree = testGroup "FlatTerm regressions"
  [ testCase "Decoding of large-ish words" (Right largeWord @=? largeWordTest)
  ]

-- | Test an edge case in the FlatTerm implementation: when encoding a word
-- larger than @'maxBound' :: 'Int'@, we store it as an @'Integer'@, and
-- need to remember to handle this case when we decode.
largeWordTest :: Either String Word
largeWordTest = fromFlatTerm decodeWord $ toFlatTerm (encodeWord largeWord)

largeWord :: Word
largeWord = fromIntegral (maxBound :: Int) + 1
