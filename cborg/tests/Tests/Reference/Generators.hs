{-# LANGUAGE CPP                        #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Tests.Reference.Generators (
    -- * Integer with a large range
    LargeInteger(..)

    -- * Floats with NaNs
  , FloatNaN(..)
  , canonicaliseNaN

    -- * Floats with special values
  , HalfSpecials(..)
  , FloatSpecials(..)
  , DoubleSpecials(..)

    -- * Floating types to bit representation conversion
  , halfToWord
  , floatToWord
  , doubleToWord
  , wordToHalf
  , wordToFloat
  , wordToDouble
  ) where

import           Data.Word
import           Numeric (showHex)
import           Numeric.Half as Half
import           GHC.Float (float2Double)
import           Data.Proxy

import           Foreign
import           System.IO.Unsafe
import           System.Random (Random)

import           Test.QuickCheck

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
-- Floats with NaNs
--

class RealFloat n => FloatNaN n where
  canonicalNaN :: n

canonicaliseNaN :: FloatNaN n => n -> n
canonicaliseNaN n | isNaN n   = canonicalNaN
                  | otherwise = n

instance FloatNaN Half where
  canonicalNaN = Half 0x7e00

instance FloatNaN Float where
  canonicalNaN = Half.fromHalf canonicalNaN

instance FloatNaN Double where
  canonicalNaN = float2Double canonicalNaN


---------------------------------------------------
-- Generators for float types with special values
--

instance Arbitrary Half where
  arbitrary = getHalfSpecials <$> arbitrary
  shrink    = shrinkRealFrac

newtype HalfSpecials   = HalfSpecials   { getHalfSpecials   :: Half }
  deriving (Ord, Num, Fractional, RealFrac, Real, Floating, RealFloat, FloatNaN)

newtype FloatSpecials  = FloatSpecials  { getFloatSpecials  :: Float }
  deriving (Ord, Num, Fractional, RealFrac, Real, Floating, RealFloat, FloatNaN)

newtype DoubleSpecials = DoubleSpecials { getDoubleSpecials :: Double }
  deriving (Ord, Num, Fractional, RealFrac, Real, Floating, RealFloat, FloatNaN)

instance Eq HalfSpecials where
  HalfSpecials a == HalfSpecials b = halfToWord a == halfToWord b

instance Eq FloatSpecials where
  FloatSpecials a == FloatSpecials b = floatToWord a == floatToWord b

instance Eq DoubleSpecials where
  DoubleSpecials a == DoubleSpecials b = doubleToWord a == doubleToWord b

instance Show HalfSpecials where
  showsPrec p (HalfSpecials n)
    | isNaN n   = showString "NaN{-0x" . showHex (halfToWord n) . showString "-}"
    | otherwise = showsPrec p n

instance Show FloatSpecials where
  showsPrec p (FloatSpecials n)
    | isNaN n   = showString "NaN{-0x" . showHex (floatToWord n) . showString "-}"
    | otherwise = showsPrec p n

instance Show DoubleSpecials where
  showsPrec p (DoubleSpecials n)
    | isNaN n   = showString "NaN{-0x" . showHex (doubleToWord n) . showString "-}"
    | otherwise = showsPrec p n

instance Arbitrary HalfSpecials where
  arbitrary = HalfSpecials <$> frequency [ (2, arbitraryFloating)
                                         , (1, arbitraryFloatSpecials) ]
  shrink (HalfSpecials n) = [ HalfSpecials n' | n' <- shrinkRealFrac n ]

instance Arbitrary FloatSpecials where
  arbitrary = FloatSpecials <$> frequency [ (2, arbitraryFloating)
                                          , (1, arbitraryFloatSpecials) ]
  shrink (FloatSpecials n) = [ FloatSpecials n' | n' <- shrinkRealFrac n ]

instance Arbitrary DoubleSpecials where
  arbitrary = DoubleSpecials <$> frequency [ (2, arbitraryFloating)
                                           , (1, arbitraryFloatSpecials) ]
  shrink (DoubleSpecials n) = [ DoubleSpecials n' | n' <- shrinkRealFrac n ]


-- | Generate a float from a uniformly random bit pattern
--
arbitraryFloating :: forall n. RealFloatIEEE n => Gen n
arbitraryFloating = wordToFloating <$> arbitraryBoundedIntegral

-- | Generate float special values, see 'IeeeSpecials',
--
-- In particular we generate more than a single NaN bit pattern so that we can
-- test non-canonical representations. The other special values have a single
-- bit pattern.
--
arbitraryFloatSpecials :: forall n. (RealFloatIEEE n, Random (FloatWord n))
                       => Gen n
arbitraryFloatSpecials =
    frequency
      [ (1, pure (wordToFloating positiveInfinity))
      , (1, pure (wordToFloating negativeInfinity))
      , (1, pure (wordToFloating negativeZero))
      , (3, wordToFloating <$> choose nanRange)
      ]
  where
    IeeeSpecials {..} = floatIeeeSpecials (Proxy :: Proxy n)

-- | Special values for IEEE float types, including negative 0,
-- positive and negative infinity and a range of NaN values.
--
data IeeeSpecials n = IeeeSpecials {
       positiveInfinity :: n,
       negativeInfinity :: n,
       negativeZero     :: n,
       nanRange         :: (n, n)
     }
  deriving (Eq, Functor, Show)

-- | The 'IeeeSpecials' values for 'RealFloatIEEE' types (i.e. 'Half', 'Float'
-- and 'Double').
--
-- To make sense of the bit-twiddling here, see
--
-- <https://en.wikipedia.org/wiki/Single-precision_floating-point_format>
-- <https://en.wikipedia.org/wiki/Half-precision_floating-point_format>
-- <https://en.wikipedia.org/wiki/Double-precision_floating-point_format>
--
floatIeeeSpecials :: RealFloatIEEE n => Proxy n -> IeeeSpecials (FloatWord n)
floatIeeeSpecials p =
    IeeeSpecials {..}
  where
    positiveInfinity = (setBit 0 (exponentBits p)    - 1)
                       `shiftL` significandBits p
    negativeInfinity = (setBit 0 (exponentBits p +1) - 1)
                       `shiftL` significandBits p
    negativeZero     =  setBit 0 (exponentBits p + significandBits p)
    nanRange         = (positiveInfinity, negativeZero - 1)

class (RealFloat n, Integral (FloatWord n), Show (FloatWord n),
       Bounded (FloatWord n), Bits (FloatWord n))
   => RealFloatIEEE n where
  exponentBits    :: Proxy n -> Int
  significandBits :: Proxy n -> Int

  type FloatWord n :: *
  wordToFloating  :: FloatWord n -> n
--floatingToWord  :: n -> FloatWord n

instance RealFloatIEEE Half where
  exponentBits    _ = 5
  significandBits _ = 10

  type FloatWord Half = Word16
  wordToFloating      = wordToHalf
--floatingToWord      = halfToWord

instance RealFloatIEEE Float where
  exponentBits    _ = 8
  significandBits _ = 23

  type FloatWord Float = Word32
  wordToFloating       = wordToFloat
--floatingToWord       = floatToWord

instance RealFloatIEEE Double where
  exponentBits    _ = 11
  significandBits _ = 52

  type FloatWord Double = Word64
  wordToFloating        = wordToDouble
--floatingToWord        = doubleToWord

