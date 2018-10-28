{-# LANGUAGE CPP, BangPatterns, MagicHash, UnboxedTuples, RankNTypes, ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Tests.Reference.Generators (
    -- * Integer with a large range
    LargeInteger(..)

    -- * Floats with special values
  , FloatSpecials(..)

    -- * Floating types to bit representation conversion
  , halfToWord
  , floatToWord
  , doubleToWord
  , wordToHalf
  , wordToFloat
  , wordToDouble
  ) where

import           Data.Word
import           Numeric.Half as Half

import           Foreign
import           System.IO.Unsafe

import           Test.QuickCheck

#if !MIN_VERSION_base(4,8,0)
import           Control.Applicative
#endif

-- | QuickCheck generator for large integers
--
newtype LargeInteger = LargeInteger { getLargeInteger :: Integer }
  deriving (Show, Eq)

instance Arbitrary LargeInteger where
  arbitrary =
    sized $ \n ->
      oneof $ take (1 + n `div` 10)
        [ LargeInteger .          fromIntegral <$> (arbitrary :: Gen Int8)
        , LargeInteger .          fromIntegral <$> choose (minBound, maxBound :: Int64)
        , LargeInteger . bigger . fromIntegral <$> choose (minBound, maxBound :: Int64)
        ]
    where
      bigger n = n * abs n

----------------------------------------
-- Float <-> Integral conversions
--

wordToHalf :: Word16 -> Half
wordToHalf = Half.Half . fromIntegral

wordToFloat :: Word32 -> Float
wordToFloat = toFloat

wordToDouble :: Word64 -> Double
wordToDouble = toFloat

toFloat :: (Storable word, Storable float) => word -> float
toFloat w =
    unsafeDupablePerformIO $ alloca $ \buf -> do
      poke (castPtr buf) w
      peek buf

halfToWord :: Half -> Word16
halfToWord (Half.Half w) = fromIntegral w

floatToWord :: Float -> Word32
floatToWord = fromFloat

doubleToWord :: Double -> Word64
doubleToWord = fromFloat

fromFloat :: (Storable word, Storable float) => float -> word
fromFloat float =
    unsafeDupablePerformIO $ alloca $ \buf -> do
            poke (castPtr buf) float
            peek buf


---------------------------------------------------
-- Generators for float types with special values
--

instance Arbitrary Half where
  arbitrary = Half.Half . fromIntegral <$> (arbitrary :: Gen Word16)

newtype FloatSpecials n = FloatSpecials { getFloatSpecials :: n }
  deriving (Show, Eq)

instance (Arbitrary n, RealFloat n) => Arbitrary (FloatSpecials n) where
  arbitrary =
    frequency
      [ (7, FloatSpecials <$> arbitrary)
      , (1, pure (FloatSpecials (1/0)) )  -- +Infinity
      , (1, pure (FloatSpecials (0/0)) )  --  NaN
      , (1, pure (FloatSpecials (-1/0)) ) -- -Infinity
      ]
