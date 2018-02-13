{-# LANGUAGE CPP                        #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# OPTIONS_GHC -Werror #-}
module Tests.Boundary
  ( testTree -- :: TestTree
  ) where

import           Data.Either
import           Data.Int
import           Data.Typeable
import           Data.Word

import           Codec.CBOR.Decoding
import           Codec.CBOR.Read
import           Codec.CBOR.Write
import           Codec.Serialise.Class

import           Test.Tasty
import           Test.Tasty.QuickCheck

#if !MIN_VERSION_base(4,8,0)
import           Control.Applicative
#endif

-- | Wrapper for bounded, integral type 'a' that potentially contains values
-- outside of range of 'a'.
newtype B a = B { unB :: BRep a }

type family BRep a where
  BRep Word   = Word64
  BRep Word8  = Word64
  BRep Word16 = Word64
  BRep Word32 = Word64
  BRep Word64 = Word64
  BRep Int    = Int64
  BRep Int8   = Int64
  BRep Int16  = Int64
  BRep Int32  = Int64
  BRep Int64  = Int64

instance Show (BRep a) => Show (B a) where
  showsPrec p = showsPrec p . unB

instance (Arbitrary (BRep a), Num (BRep a), Bounded a, Integral a
         ) => Arbitrary (B a) where
  arbitrary = B <$> frequency
    [ (490, arbitrary)
    -- Boundaries
    , (85, pure $ fromIntegral (minBound     :: a))
    , (85, pure $ fromIntegral (maxBound     :: a))
    -- Near boundaries, in range
    , (85, pure $ fromIntegral (minBound + 1 :: a))
    , (85, pure $ fromIntegral (maxBound - 1 :: a))
    -- Near boundaries, out of range (note: might overflow)
    , (85, pure $ fromIntegral (minBound :: a) - 1)
    , (85, pure $ fromIntegral (maxBound :: a) + 1)
    ]

-- | Check if deserialisation of values of type 'a' deals properly with the ones
-- that are out of range, i.e. fails to decode them.
boundaryTest
  :: forall a rep. (Bounded a, Integral a, rep ~ BRep a,
                    Ord rep, Num rep, Serialise rep)
  => (forall s. Decoder s a)
  -> B a
  -> Bool
boundaryTest dec a = if outsideRange
                     then isLeft  a'
                     else isRight a'
  where
    a' = deserialiseFromBytes dec . toLazyByteString . encode $ unB a

    outsideRange = unB a < fromIntegral (minBound :: a)
                || unB a > fromIntegral (maxBound :: a)

mkBoundaryTest
  :: forall a rep. (Bounded a, Integral a, Typeable a, rep ~ BRep a,
                    Arbitrary rep, Ord rep, Show rep, Num rep, Serialise rep)
  => (forall s. Decoder s a)
  -> (forall s. Decoder s a)
  -> [TestTree]
mkBoundaryTest dec decCan =
  [ testProperty (show a)                   $ boundaryTest dec
  , testProperty (show a ++ " (canonical)") $ boundaryTest decCan
  ]
  where
    a = typeOf (undefined::a)

testTree :: TestTree
testTree = testGroup "Boundary checks" $ concat
  [ mkBoundaryTest decodeWord   decodeWordCanonical
  , mkBoundaryTest decodeWord8  decodeWord8Canonical
  , mkBoundaryTest decodeWord16 decodeWord16Canonical
  , mkBoundaryTest decodeWord32 decodeWord32Canonical
  , mkBoundaryTest decodeWord64 decodeWord64Canonical
  , mkBoundaryTest decodeInt    decodeIntCanonical
  , mkBoundaryTest decodeInt8   decodeInt8Canonical
  , mkBoundaryTest decodeInt16  decodeInt16Canonical
  , mkBoundaryTest decodeInt32  decodeInt32Canonical
  , mkBoundaryTest decodeInt64  decodeInt64Canonical
  ]
