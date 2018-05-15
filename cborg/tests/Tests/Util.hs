{-# LANGUAGE CPP                 #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Tests.Util
  ( splits2
  , splits3
  , arbitraryWithBounds
  , Length(..)
  , mkLengthPrefix
  ) where

import           Data.Bits
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import           Data.Word

import           Test.Tasty.QuickCheck

#if !MIN_VERSION_base(4,8,0)
import           Control.Applicative
#endif

-- | Generate all 2-splits of a serialised CBOR value.
splits2 :: BSL.ByteString -> [BSL.ByteString]
splits2 bs = zipWith (\a b -> BSL.fromChunks [a,b]) (BS.inits sbs) (BS.tails sbs)
  where
    sbs = BSL.toStrict bs

-- | Generate all 3-splits of a serialised CBOR value.
splits3 :: BSL.ByteString -> [BSL.ByteString]
splits3 bs =
    [ BSL.fromChunks [a,b,c]
    | (a,x) <- zip (BS.inits sbs) (BS.tails sbs)
    , (b,c) <- zip (BS.inits x)   (BS.tails x) ]
  where
    sbs = BSL.toStrict bs

----------------------------------------

-- | Generate values of type 'a' embedded within (usually larger) type 'r' with
-- upped probabilities of getting neighbourhood of bounds of 'a'.
arbitraryWithBounds
  :: forall a r. (Bounded a, Integral a, Num r, Arbitrary r)
  => a -> Gen r
arbitraryWithBounds _ = frequency
  [ (70, arbitrary)
  -- Boundaries
  , (5, pure $ fromIntegral (minBound     :: a))
  , (5, pure $ fromIntegral (maxBound     :: a))
  -- Near boundaries, in range
  , (5, pure $ fromIntegral (minBound + 1 :: a))
  , (5, pure $ fromIntegral (maxBound - 1 :: a))
  -- Near boundaries, out of range (assuming there is no overflow). It overflows
  -- if a ~ r, but it's fine as then we just get a value within range.
  , (5, pure $ fromIntegral (minBound :: a) - 1)
  , (5, pure $ fromIntegral (maxBound :: a) + 1)
  ]

----------------------------------------

-- | Wrapper for list/map length.
newtype Length = Length { unLength :: Word }

instance Show Length where
  showsPrec p = showsPrec p . unLength

instance Arbitrary Length where
  arbitrary = Length <$> arbitraryWithBounds (undefined::Int)

-- | Generate CBOR prefix of non-empty string/bytes containing its length.
mkLengthPrefix :: Bool -> Length -> BSL.ByteString
mkLengthPrefix string (Length w)
  | w <= 23         = BSL.pack $ [64 + stringBit + fromIntegral w]
  | w <= 0xff       = BSL.pack $ [88 + stringBit] ++ f 1 w []
  | w <= 0xffff     = BSL.pack $ [89 + stringBit] ++ f 2 w []
  | w <= 0xffffffff = BSL.pack $ [90 + stringBit] ++ f 4 w []
  | otherwise       = BSL.pack $ [91 + stringBit] ++ f 8 w []
  where
    stringBit :: Word8
    stringBit = if string then 32 else 0

    f :: Int -> Word -> [Word8] -> [Word8]
    f 0 _ acc = acc
    f k n acc = f (k - 1) (n `shiftR` 8) (fromIntegral n : acc)
