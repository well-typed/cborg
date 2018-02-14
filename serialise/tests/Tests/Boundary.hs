{-# LANGUAGE CPP                        #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE MultiWayIf                 #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}
module Tests.Boundary
  ( testTree -- :: TestTree
  ) where

import           Data.Bits
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
import           Codec.Serialise.Class

import           Test.Tasty
import           Test.Tasty.QuickCheck

#if !MIN_VERSION_base(4,8,0)
import           Control.Applicative
#endif

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
  -- Near boundaries, out of range (note: might overflow)
  , (5, pure $ fromIntegral (minBound :: a) - 1)
  , (5, pure $ fromIntegral (maxBound :: a) + 1)
  ]

----------------------------------------

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
  arbitrary = B <$> arbitraryWithBounds (undefined::a)

-- | Check if deserialisation of values of type 'a' deals properly with the ones
-- out of range, i.e. fails to decode them.
boundaryTest
  :: forall a rep. (Bounded a, Integral a, Show a, rep ~ BRep a,
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
  :: forall a rep. (Bounded a, Integral a, Show a, rep ~ BRep a,
                    Arbitrary rep, Ord rep, Show rep, Num rep, Serialise rep)
  => String
  -> (forall s. Decoder s a)
  -> (forall s. Decoder s a)
  -> [TestTree]
mkBoundaryTest aName dec decCan =
  [ testProperty aName                     $ boundaryTest dec
  , testProperty (aName ++ " (canonical)") $ boundaryTest decCan
  ]

----------------------------------------

-- | Wrapper for list/map length.
newtype Length = Length { unLength :: Word }

instance Show Length where
  showsPrec p = showsPrec p . unLength

instance Arbitrary Length where
  arbitrary = Length <$> arbitraryWithBounds (undefined::Int)

-- | Check if deserialisation of map/list length deals properly with the ones
-- out of range, i.e. fails to decode them.
lenBoundaryTest
  :: (Word -> Encoding)
  -> (forall s. Decoder s Int)
  -> Length
  -> Bool
lenBoundaryTest enc dec a = if outsideRange
                            then isLeft  a'
                            else isRight a'
  where
    a' = deserialiseFromBytes dec . toLazyByteString . enc $ unLength a

    outsideRange = fromIntegral (unLength a) < (0::Int)

mkLenBoundaryTest
  :: String
  -> (Word -> Encoding)
  -> (forall s. Decoder s Int)
  -> (forall s. Decoder s Int)
  -> [TestTree]
mkLenBoundaryTest aName enc dec decCan =
  [ testProperty aName                     $ lenBoundaryTest enc dec
  , testProperty (aName ++ " (canonical)") $ lenBoundaryTest enc decCan
  ]

----------------------------------------

-- | Generate random CBOR prefix of non-empty string/bytes containing its
-- length.
arbitraryLengthPrefix :: Bool -> Gen (Word, BSL.ByteString)
arbitraryLengthPrefix string = do
  Length w <- arbitrary
  if | w <= 23         -> pure (w, BSL.pack $ [64 + stringBit + fromIntegral w])
     | w <= 0xff       -> pure (w, BSL.pack $ [88 + stringBit] ++ f 1 w [])
     | w <= 0xffff     -> pure (w, BSL.pack $ [89 + stringBit] ++ f 2 w [])
     | w <= 0xffffffff -> pure (w, BSL.pack $ [90 + stringBit] ++ f 4 w [])
     | otherwise       -> pure (w, BSL.pack $ [91 + stringBit] ++ f 8 w [])
  where
    stringBit :: Word8
    stringBit = if string then 32 else 0

    f :: Int -> Word -> [Word8] -> [Word8]
    f 0 _ acc = acc
    f k w acc = f (k - 1) (w `shiftR` 8) (fromIntegral w : acc)

data StringLengthPrefix = StringLP Word BSL.ByteString
  deriving Show
instance Arbitrary StringLengthPrefix where
  arbitrary = uncurry StringLP <$> arbitraryLengthPrefix True

data BytesLengthPrefix = BytesLP Word BSL.ByteString
  deriving Show
instance Arbitrary BytesLengthPrefix where
  arbitrary = uncurry BytesLP <$> arbitraryLengthPrefix False

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

testTree :: TestTree
testTree = localOption (QuickCheckTests 1000) . testGroup "Boundary checks" $ concat
  [ mkBoundaryTest "Word"      decodeWord      decodeWordCanonical
  , mkBoundaryTest "Word8"     decodeWord8     decodeWord8Canonical
  , mkBoundaryTest "Word16"    decodeWord16    decodeWord16Canonical
  , mkBoundaryTest "Word32"    decodeWord32    decodeWord32Canonical
  , mkBoundaryTest "Word64"    decodeWord64    decodeWord64Canonical
  , mkBoundaryTest "Int"       decodeInt       decodeIntCanonical
  , mkBoundaryTest "Int8"      decodeInt8      decodeInt8Canonical
  , mkBoundaryTest "Int16"     decodeInt16     decodeInt16Canonical
  , mkBoundaryTest "Int32"     decodeInt32     decodeInt32Canonical
  , mkBoundaryTest "Int64"     decodeInt64     decodeInt64Canonical

  , mkLenBoundaryTest "ListLen" encodeListLen decodeListLen decodeListLenCanonical
  , mkLenBoundaryTest "MapLen"  encodeMapLen  decodeMapLen  decodeMapLenCanonical

  , stringBytesBoundaryTest
  ]
