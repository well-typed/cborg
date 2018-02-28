module Tests.UTF8
  ( testTree -- :: TestTree
  ) where

import           Control.DeepSeq
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import           Data.Monoid
import qualified Data.Text.Encoding as T

import           Codec.CBOR.Decoding
import           Codec.CBOR.Read
import           Tests.Util

import           Test.Tasty
import           Test.Tasty.QuickCheck

-- | Wrapper for ByteString with Arbitrary instance that might produce a valid
-- UTF-8 encoding of a string.
newtype MaybeText = MaybeText BS.ByteString
  deriving Show
instance Arbitrary MaybeText where
  arbitrary = MaybeText . BS.pack <$> arbitrary

-- | Test that decoding of both valid and invalid CBOR strings produces output
-- without exceptions hidden within.
utf8DecodingTest :: MaybeText -> Property
utf8DecodingTest (MaybeText bs) = case T.decodeUtf8' bs of
  Right _ -> collect "valid utf8"   $     (and splitsOk)
  Left  _ -> collect "invalid utf8" $ not (or splitsOk)
  where
    -- We test 2-splits to check all decoder paths.
    splitsOk = [ok $ deserialiseFromBytes decodeString v | v <- splits2 s]
      where
        ok (Right v) = deepseq v True
        ok (Left  v) = deepseq v False

    s = mkLengthPrefix True (Length . fromIntegral $ BS.length bs)
     <> BSL.fromStrict bs

----------------------------------------

testTree :: TestTree
testTree = localOption (QuickCheckTests 1000) . testGroup "UTF8" $
  [testProperty
     "Decoding of UTF8 encoded Text works and properly handles decoding failures" utf8DecodingTest
  ]
