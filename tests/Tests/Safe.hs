{-# LANGUAGE CPP #-}
module Tests.Safe
  ( prop_chunkByte
  , prop_longData
  ) where

#if !MIN_VERSION_base(4,8,0)
import           Control.Applicative                 ((<$>), (<*>))
#endif
import           Data.Binary.Serialise.CBOR
import           Data.Binary.Serialise.CBOR.Decoding (decodeListLen, decodeWord)
import           Data.Binary.Serialise.CBOR.Encoding (encodeListLen, encodeWord)
import qualified Data.ByteString                     as BS
import qualified Data.ByteString.Lazy                as BL
import           Data.Monoid                         ((<>))
import qualified Data.Text                           as T
import           Test.QuickCheck

newtype MyText = MyText T.Text deriving (Show, Eq)

instance Arbitrary MyText where
  arbitrary = MyText <$> (T.pack <$> arbitrary)

instance Serialise MyText where
  encode (MyText t) = encode t
  decode = MyText <$> decode

data Value
    = VNum Integer
  | VTerms [MyText]
  deriving (Show, Eq)

instance Serialise Value where
    encode (VNum num) = encodeListLen 2 <> encodeWord 0 <> encode num
    encode (VTerms tset) = encodeListLen 2 <> encodeWord 8 <> encodeList tset
    decode = do
      marker <- (,) <$> decodeListLen <*> decodeWord
      case marker of
        (2, 0) -> VNum <$> decode
        (2, 8) -> VTerms <$> decodeList
        _ -> fail "Incorrect CBOR value"

instance Arbitrary Value where
  arbitrary = oneof [
          VNum <$> arbitrary
        , VTerms <$> arbitrary
        ]

prop_chunkByte :: [Value] -> Bool
prop_chunkByte v = (deserialise . tokenize . serialise) v == v
  where
    tokenize = BL.fromChunks . map (\a -> BS.pack [a]) . BS.unpack . BS.concat . BL.toChunks

prop_longData :: [Value] -> Bool
prop_longData v = deserialise (serialise v) == v
