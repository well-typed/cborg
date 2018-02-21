module Tests.Regress.Issue160 ( testTree ) where

import           Codec.CBOR.Decoding
import           Codec.CBOR.Read
import           Control.DeepSeq
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import           Data.Text (Text)
import           Test.Tasty
import           Test.Tasty.HUnit

testTree :: TestTree
testTree = testGroup "Issue 160 - decoder checks"
  [ testCase "decodeString fails on non-utf8 bytes instead of crashing" $ do
      let bs = BSL.fromStrict $ BS.pack [0x61, 128]
      case deserialiseFromBytes decodeString bs of
        Left err        -> deepseq err               $ pure ()
        Right (rest, t) -> deepseq (rest, t :: Text) $ pure ()
  , testCase "decodeListLen doesn't produce negative lengths using a Word64" $ do
      let bs = BSL.fromStrict $
               BS.pack [0x9b, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff]
      case deserialiseFromBytes decodeListLen bs of
          Left err        -> deepseq err  $ pure ()
          Right (rest, t) -> deepseq rest $ assertBool "Length is not negative" (t >= 0)
  , testCase "decodeMapLen doesn't produce negative lengths using a Word64" $ do
      let bs = BSL.fromStrict $
               BS.pack [0xbb, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff]
      case deserialiseFromBytes decodeMapLen bs of
          Left err        -> deepseq err  $ pure ()
          Right (rest, t) -> deepseq rest $ assertBool "Length is not negative" (t >= 0)
  , testCase "decodeBytes doesn't create bytestrings that cause segfaults or worse" $ do
      let bs = BSL.fromStrict $ BS.pack $
               [0x5b, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff] ++
               replicate 100 0x00
      case deserialiseFromBytes decodeBytes bs of
          Left err        -> deepseq err  $ pure ()
          Right (rest, t) -> deepseq rest $
            assertBool "Length is not negative" (BS.length t >= 0)
  ]
