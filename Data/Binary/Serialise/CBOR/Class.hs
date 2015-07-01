module Data.Binary.Serialise.CBOR.Class (
    -- * The Serialise class
    Serialise(..),
    -- ** Instance helpers
    encodeShortList
  ) where

import Data.Binary.Serialise.CBOR.Encoding
import Data.Binary.Serialise.CBOR.Decoding

import Data.Monoid
import Control.Applicative

import Data.Word
import Data.Int
import qualified Data.Text       as Text
import qualified Data.ByteString as BS

--TODO: lots more instances
--import qualified Data.Text.Lazy  as Text.Lazy
--import qualified Data.ByteString as BS.Lazy
--import qualified Data.Map  as Map
--import qualified Data.Set  as Set
--import qualified Data.Sequence as Sequence
--import qualified Data.Array as Array
--import qualified Data.Array.Unboxed as UArray

import Prelude hiding (encodeFloat, decodeFloat)


class Serialise a where
    encode  :: a -> Encoding
    decode  :: Decoder a

    -- Mainly here to support the Char/String instance.
    encodeList :: [a] -> Encoding
    encodeList = defaultEncodeList

    decodeList :: Decoder [a]
    decodeList = defaultDecodeList

------------------------
-- Special list business
--

instance Serialise a => Serialise [a] where
    encode = encodeList
    decode = decodeList

defaultEncodeList :: Serialise a => [a] -> Encoding
defaultEncodeList [] = encodeListLen 0
defaultEncodeList xs = encodeListLenIndef
                    <> foldr (\x r -> encode x <> r) encodeBreak xs

defaultDecodeList :: Serialise a => Decoder [a]
defaultDecodeList = do
    mn <- decodeListLenOrIndef
    case mn of
      Nothing -> decodeSequenceLenIndef (flip (:)) [] reverse   decode
      Just n  -> decodeSequenceLenN     (flip (:)) [] reverse n decode


encodeShortList :: Serialise a => [a] -> Encoding
encodeShortList xs = encodeListLen (fromIntegral $ length xs)
                  <> foldr (\x r -> encode x <> r) mempty xs


------------------------
-- Primitive instances
--

instance Serialise () where
    encode = const encodeNull
    decode = decodeNull

instance Serialise Bool where
    encode = encodeBool
    decode = decodeBool

instance Serialise Int where
    encode = encodeInt
    decode = decodeInt

instance Serialise Int64 where
    encode = encodeInt64
    decode = decodeInt64

instance Serialise Word where
    encode = encodeWord
    decode = decodeWord

instance Serialise Word64 where
    encode = encodeWord64
    decode = decodeWord64

instance Serialise Integer where
    encode = encodeInteger
    decode = decodeInteger

instance Serialise Float where
    encode = encodeFloat
    decode = decodeFloat

instance Serialise Double where
    encode = encodeDouble
    decode = decodeDouble

instance Serialise Char where
    encode c = encodeString (Text.singleton c)
    decode = do t <- decodeString
                if Text.length t == 1
                  then return $! Text.head t
                  else fail "expected a single char, found a string"

    -- For [Char]/String we have a special encoding
    encodeList cs = encodeString (Text.pack cs)
    decodeList    = do txt <- decodeString
                       return (Text.unpack txt) -- unpack lazily

instance Serialise Text.Text where
    encode = encodeString
    decode = decodeString

instance Serialise BS.ByteString where
    encode = encodeBytes
    decode = decodeBytes


------------------------
-- Structure instances
--

instance (Serialise a, Serialise b) => Serialise (a,b) where
    encode (a,b) = encodeListLen 2
                <> encode a
                <> encode b
    decode = do decodeListLenOf 2
                (,) <$> decode <*> decode

instance (Serialise a, Serialise b, Serialise c) => Serialise (a,b,c) where
    encode (a,b,c) = encodeListLen 3
                  <> encode a
                  <> encode b
                  <> encode c

    decode = do decodeListLenOf 3
                (,,) <$> decode <*> decode <*> decode

instance Serialise a => Serialise (Maybe a) where
    encode Nothing  = encodeListLen 0
    encode (Just x) = encodeListLen 1 <> encode x

    decode = do n <- decodeListLen
                case n of
                  0 -> pure Nothing
                  1 -> Just <$> decode
                  _ -> fail "unknown tag"

instance (Serialise a, Serialise b) => Serialise (Either a b) where
    encode (Left  x) = encodeListLen 2 <> encodeWord 0 <> encode x
    encode (Right x) = encodeListLen 2 <> encodeWord 1 <> encode x

    decode = do decodeListLenOf 2
                t <- decodeWord
                case t of
                  0 -> Left  <$> decode
                  1 -> Right <$> decode
                  _ -> fail "unknown tag"

