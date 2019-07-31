{-# LANGUAGE CPP                        #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiWayIf                 #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}
module Tests.Boundary
  ( testTree -- :: TestTree
  ) where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import           Data.Either
import           Data.Int
import           Data.Word
import qualified Data.Text as T

import           Codec.CBOR.Decoding
import           Codec.CBOR.Encoding
import           Codec.CBOR.Read
import           Codec.CBOR.Write
import           Tests.Util

import           Test.Tasty
import           Test.Tasty.QuickCheck

#if !MIN_VERSION_base(4,8,0)
import           Control.Applicative
#endif

-- | CBOR can represent 64 bit negative and positive integers, hence we need
-- wrapper for Integer to represent the whole range.
newtype Int65 = Int65 Integer
  deriving (Eq, Ord, Enum, Num, Integral, Real, Show)

instance Bounded Int65 where
  maxBound = Int65 (2^(64 :: Int) - 1)
  minBound = Int65 (-2^(64 :: Int))

instance Arbitrary Int65 where
  arbitrary = arbitraryBoundedIntegral

encodeInt65 :: Int65 -> Encoding
encodeInt65 (Int65 n) = encodeInteger n


-- | Wrapper for bounded, integral type 'a' that potentially contains values
-- outside of range of 'a'.
newtype B a = B { unB :: BRep a }

type family BRep a where
  BRep Word   = Word64
  BRep Word8  = Word64
  BRep Word16 = Word64
  BRep Word32 = Word64
  BRep Word64 = Word64
  BRep Int    = Int65
  BRep Int8   = Int64
  BRep Int16  = Int64
  BRep Int32  = Int64
  BRep Int64  = Int65

instance Show (BRep a) => Show (B a) where
  showsPrec p = showsPrec p . unB

instance (Arbitrary (BRep a), Num (BRep a), Bounded a, Integral a
         ) => Arbitrary (B a) where
  arbitrary = B <$> arbitraryWithBounds (undefined::a)

-- | Check if deserialisation of values of type 'a' deals properly with the ones
-- out of range, i.e. fails to decode them.
boundaryTest
  :: forall a rep. (Bounded a, Integral a, Show a, rep ~ BRep a,
                    Ord rep, Num rep)
  => (rep -> Encoding)          -- ^ encode
  -> (forall s. Decoder s a)  -- ^ decode
  -> B a
  -> Property
boundaryTest enc dec a = if outsideRange
                           then collect "outside" $ isLeft  a'
                           else collect "inside"  $ isRight a'
  where
    a' = deserialiseFromBytes dec . toLazyByteString . enc $ unB a

    -- Note that this is always true for a ~ rep.
    outsideRange = unB a < fromIntegral (minBound :: a)
                || unB a > fromIntegral (maxBound :: a)

mkBoundaryTest
  :: forall a rep. (Bounded a, Integral a, Show a, rep ~ BRep a,
                    Arbitrary rep, Ord rep, Show rep, Num rep)
  => String
  -> (rep -> Encoding)
  -> (forall s. Decoder s a)
  -> (forall s. Decoder s a)
  -> [TestTree]
mkBoundaryTest aName enc dec decCan =
  [ testProperty aName                     $ boundaryTest enc dec
  , testProperty (aName ++ " (canonical)") $ boundaryTest enc decCan
  ]

----------------------------------------

-- | Check if deserialisation of map/list length deals properly with the ones
-- out of range, i.e. fails to decode them.
lenBoundaryTest
  :: (Word -> Encoding)
  -> (forall s. Decoder s Word)
  -> Length
  -> Property
lenBoundaryTest enc dec a = if outsideRange
                            then collect "outside" $ isLeft  a'
                            else collect "inside"  $ isRight a'
  where
    a' = deserialiseFromBytes dec . toLazyByteString . enc $ unLength a

    outsideRange = fromIntegral (unLength a) < (0::Int)

mkLenBoundaryTest
  :: String
  -> (Word -> Encoding)
  -> (forall s. Decoder s Word)
  -> (forall s. Decoder s Word)
  -> [TestTree]
mkLenBoundaryTest aName enc dec decCan =
  [ testProperty aName                     $ lenBoundaryTest enc dec
  , testProperty (aName ++ " (canonical)") $ lenBoundaryTest enc decCan
  ]

----------------------------------------

data StringLengthPrefix = StringLP Word BSL.ByteString
  deriving Show
instance Arbitrary StringLengthPrefix where
  arbitrary = (\l -> StringLP (unLength l) (mkLengthPrefix True l)) <$> arbitrary

data BytesLengthPrefix = BytesLP Word BSL.ByteString
  deriving Show
instance Arbitrary BytesLengthPrefix where
  arbitrary = (\l -> BytesLP (unLength l) (mkLengthPrefix False l)) <$> arbitrary

-- | Test that positive length prefixes of string/bytes are parsed successfully,
-- whereas negative are not.
stringBytesBoundaryTest :: [TestTree]
stringBytesBoundaryTest =
  [ testProperty "String" $ \(StringLP w bs) ->
      case deserialiseFromBytes decodeString bs of
        Right (_rest, string)           -> w == 0 && T.length string == 0
        Left (DeserialiseFailure _ msg) -> if fromIntegral w < (0::Int)
                                           then msg == "expected string"
                                           else msg == "end of input"
  , testProperty "Bytes" $ \(BytesLP w bs) ->
      case deserialiseFromBytes decodeBytes bs of
        Right (_rest, bytes)            -> w == 0 && BS.length bytes == 0
        Left (DeserialiseFailure _ msg) -> if fromIntegral w < (0::Int)
                                           then msg == "expected bytes"
                                           else msg == "end of input"
  ]

----------------------------------------

testTree :: TestTree
testTree = localOption (QuickCheckTests 1000) . testGroup "Boundary checks" $ concat
  [ mkBoundaryTest "Word"    encodeWord64  decodeWord    decodeWordCanonical
  , mkBoundaryTest "Word8"   encodeWord64  decodeWord8   decodeWord8Canonical
  , mkBoundaryTest "Word16"  encodeWord64  decodeWord16  decodeWord16Canonical
  , mkBoundaryTest "Word32"  encodeWord64  decodeWord32  decodeWord32Canonical
  , mkBoundaryTest "Word64"  encodeWord64  decodeWord64  decodeWord64Canonical
  , mkBoundaryTest "Int"     encodeInt65   decodeInt     decodeIntCanonical
  , mkBoundaryTest "Int8"    encodeInt64   decodeInt8    decodeInt8Canonical
  , mkBoundaryTest "Int16"   encodeInt64   decodeInt16   decodeInt16Canonical
  , mkBoundaryTest "Int32"   encodeInt64   decodeInt32   decodeInt32Canonical
  , mkBoundaryTest "Int64"   encodeInt65   decodeInt64   decodeInt64Canonical

  , mkLenBoundaryTest "ListLen" encodeListLen decodeListLen decodeListLenCanonical
  , mkLenBoundaryTest "MapLen"  encodeMapLen  decodeMapLen  decodeMapLenCanonical

  , stringBytesBoundaryTest
  ]
