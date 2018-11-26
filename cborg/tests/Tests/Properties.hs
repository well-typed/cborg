{-# LANGUAGE CPP                  #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE DefaultSignatures    #-}
{-# LANGUAGE AllowAmbiguousTypes  #-}
{-# LANGUAGE TypeApplications     #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Tests.Properties (
    testTree

    -- * Token type class and derived functions
  , Token(..)
  , serialiseRef
  , serialiseImp
  , deserialiseRef
  , deserialiseImp

    -- * Various test token types
  , TokInt
  , TokInt8
  , TokInt16
  , TokInt32
  , TokInt64
  , TokInteger
  , TokWord
  , TokWord8
  , TokWord16
  , TokWord32
  , TokWord64
  , TokHalf
  , TokFloat
  , TokDouble
  , TokTag
  , TokTag64
  , Ref.Simple
  , Ref.Term
  ) where

import           Prelude hiding (decodeFloat, encodeFloat)

import qualified Data.ByteString.Lazy as LBS
import           Data.Word
import           Data.Int
import           Data.Bits (complement)
import qualified Numeric.Half as Half
import           Data.Function (on)

import           Codec.CBOR.Term
import           Codec.CBOR.Read
import           Codec.CBOR.Write
import           Codec.CBOR.Decoding
import           Codec.CBOR.Encoding

import           Test.Tasty (TestTree, testGroup, localOption)
import           Test.Tasty.QuickCheck (testProperty, QuickCheckMaxSize(..))
import           Test.QuickCheck
import           System.Random (Random)

import qualified Tests.Reference.Implementation as Ref
import           Tests.Reference.Implementation (UInt(..))
import           Tests.Reference.Generators
import           Tests.Term
                   ( fromRefTerm, toRefTerm, eqTerm, canonicaliseTerm )
import           Tests.Util

#if !MIN_VERSION_base(4,8,0)
import           Control.Applicative
#endif


-- | The CBOR implementation and its reference implementation satisfy all the
-- properties implied in the following commuting diagram.
--
-- The properties in this module exercise various paths throguh this diagram,
-- and do so for various different types.
--
-- >        canon          id
-- >  Ref──────────▶Ref───────────▶Ref
-- >    │            ▲ ╲  (ref)  ╱ │
-- >    │            │  ╲enc dec╱  │
-- >    │            │   ╲     ╱   │
-- >    │from      to│    ▶Enc▶    │from
-- >    │            │   ╱     ╲   │
-- >    │            │  ╱enc dec╲  │
-- >    ▼            │ ╱  (imp)  ╲ ▼
-- >  Imp──────────▶Imp───────────▶Imp
-- >         id           canon
--
-- Key
--
--  * Imp:  Implementation token type
--  * Ref:  Reference token type
--  * Enc:  Encoding (ie bytes)
--  * canon: canonicaliseRef or canonicaliseImp
--  * enc:   encodeRef       or encodeImp
--  * dec:   decodeRef       or decodeImp
--
-- We capture these types and arrows with a type class and an associated type.
--
class (Eq t, Show t) => Token t where
  type Imp t :: *

  encodeImp :: Imp t -> Encoding
  encodeRef :: Ref.Encoder t

  decodeImp :: forall s. Decoder s (Imp t)
  decodeRef :: Ref.Decoder t

  canonicaliseImp :: Imp t -> Imp t
  canonicaliseRef ::     t ->     t

  eqImp   :: Imp t -> Imp t -> Bool

  toRef   :: Imp t -> t
  fromRef :: t -> Imp t

  -- defaults
  canonicaliseImp = id
  canonicaliseRef = toRef . fromRef

  default eqImp :: Eq (Imp t) => Imp t -> Imp t -> Bool
  eqImp = (==)


-- A few derived utils

serialiseRef :: forall t. Token t => t -> LBS.ByteString
serialiseRef = LBS.pack . encodeRef

serialiseImp :: forall t. Token t => Imp t -> LBS.ByteString
serialiseImp = toLazyByteString . encodeImp @t

deserialiseRef :: forall t. Token t => LBS.ByteString -> t
deserialiseRef bytes =
  case Ref.runDecoder (decodeRef @t) (LBS.unpack bytes) of
    Just (x, trailing)
      | null trailing -> x
      | otherwise     -> error "deserialiseRef: trailing bytes"
    Nothing           -> error "deserialiseRef: decode failure"

deserialiseImp :: forall t. Token t => LBS.ByteString -> Imp t
deserialiseImp bytes =
    case deserialiseFromBytes (decodeImp @ t) bytes of
      Right (trailing, x)
        | LBS.null trailing -> x
        | otherwise         -> error "deserialiseImp: trailing data"
      Left _failure         -> error "deserialiseImp: decode failure"


--------------------------------------------------------------------------------
-- Properties
--


-- | The property corresponding to the following part of the commuting diagram.
--
-- >        canon
-- >  Ref──────────▶Ref . . . . . ▷.
-- >    │            ▲ .         . .
-- >    │            │  .       .  .
-- >    │            │   .     .   .
-- >    │from      to│    ▷   ▷    .
-- >    │            │   .     .   .
-- >    │            │  .       .  .
-- >    ▼            │ .         . ▽
-- >  Imp──────────▶Imp . . . . . ▷.
-- >         id
--
-- > to . id . from = canon_ref
--
prop_fromRefToRef :: Token t => t -> Bool
prop_fromRefToRef x =

    (toRef . fromRef) x == canonicaliseRef x


-- | The property corresponding to the following part of the commuting diagram.
--
-- >                       id
-- >    . . . . . .▷Ref───────────▶Ref
-- >    .            ▲ .         . │
-- >    .            │  .       .  │
-- >    .            │   .     .   │
-- >    .          to│    ▷   ▷    │from
-- >    .            │   .     .   │
-- >    .            │  .       .  │
-- >    ▽            │ .         . ▼
-- >    . . . . . .▶Imp───────────▶Imp
-- >                      canon
--
-- > from . id . to = canon_imp
--
prop_toRefFromRef :: forall t. Token t => Imp t -> Bool
prop_toRefFromRef x =

    (fromRef . toRef @t) x  `eq`  canonicaliseImp @t x

  where
    eq = eqImp @t


-- | The property corresponding to the following part of the commuting diagram.
--
-- This is a round trip property, with the reference implementation of the
-- encoder and decoder.
--
-- >                       id
-- >    . . . . . .▷Ref───────────▶Ref
-- >    .            △ ╲         ╱ .
-- >    .            .  ╲enc dec╱  .
-- >    .            .   ╲     ╱   .
-- >    .            .    ▶Enc▶    .
-- >    .            .   .     .   .
-- >    .            .  .       .  .
-- >    ▽            . .         . ▽
-- >    . . . . . . ▷.. . . . . . ▷.
--
-- > dec_ref . enc_ref = id
--
prop_encodeRefdecodeRef :: forall t. Token t => t -> Bool
prop_encodeRefdecodeRef x =

    (deserialiseRef . serialiseRef) x  ==  x


-- | The property corresponding to the following part of the commuting diagram.
--
-- This is a round trip property, with the production implementation of the
-- encoder and decoder.
--
-- >    . . . . . . ▷. . . . . . .▷.
-- >    .            △ .         . .
-- >    .            .  .       .  .
-- >    .            .   .     .   .
-- >    .            .    ▶Enc▶    .
-- >    .            .   ╱     ╲   .
-- >    .            .  ╱enc dec╲  .
-- >    ▽            . ╱         ╲ ▽
-- >    . . . . . .▷Imp───────────▶Imp
-- >                      canon
--
-- > dec_imp . enc_imp = canon_imp
--
prop_encodeImpdecodeImp :: forall t. Token t => Imp t -> Bool
prop_encodeImpdecodeImp x =

    (deserialiseImp @t . serialiseImp @t) x  `eq`  canonicaliseImp @t x

  where
    eq = eqImp @t


-- | This is the same property as 'prop_encodeImpdecodeImp' but the encoded
-- data is split into two chunks provided as input into the decoder. All
-- possible 2-chunk splits are tried. This checks that the decoder gives the
-- same result irrespective of the chunk boundaries.
--
prop_encodeImpdecodeImp_splits2 :: forall t. Token t => Imp t -> Bool
prop_encodeImpdecodeImp_splits2 x =
    and [ deserialiseImp @t enc'  `eq`  x'
        | let enc = serialiseImp    @t x
              x'  = canonicaliseImp @t x
        , enc' <- splits2 enc ]
  where
    eq = eqImp @t


-- | This is the same idea as 'prop_encodeImpdecodeImp_splits2' but with all
-- possible 3-chunk splits of the input data. This test is of course more
-- expensive and so the size of the input must be limited.
--
prop_encodeImpdecodeImp_splits3 :: forall t. Token t => Imp t -> Bool
prop_encodeImpdecodeImp_splits3 x =
    and [ deserialiseImp @t enc'  `eq`  x'
        | let enc = serialiseImp    @t x
              x'  = canonicaliseImp @t x
        , enc' <- splits3 enc ]
  where
    eq = eqImp @t


-- | The property corresponding to the following part of the commuting diagram.
--
-- This checks that the reference and real implementation produce the same
-- encoded bytes. It starts from a value in the reference implementation.
--
-- >        canon
-- >  Ref──────────▶Ref . . . . . ▷.
-- >    │            △ ╲         . .
-- >    │            .  ╲enc    .  .
-- >    │            .   ╲     .   .
-- >    │from        .    ▶Enc▷    .
-- >    │            .   ╱     .   .
-- >    │            .  ╱enc    .  .
-- >    ▼            . ╱         . ▽
-- >  Imp──────────▶Imp . . . . . ▷.
-- >         id
--
-- > enc_imp . id . from = enc_ref . canon_ref
--
prop_encodeRefencodeImp1 :: forall t. Token t => t -> Bool
prop_encodeRefencodeImp1 x =

    (serialiseImp @t . fromRef) x  ==  (serialiseRef . canonicaliseRef) x


-- | The property corresponding to the following part of the commuting diagram.
--
-- This checks that the reference and real implementation produce the same
-- encoded bytes.  It starts from a value in the real implementation.
--
-- >    . . . . . .▷Ref . . . . . ▷.
-- >    .            ▲ ╲         . .
-- >    .            │  ╲enc    .  .
-- >    .            │   ╲     .   .
-- >    .          to│    ▶Enc▷    .
-- >    .            │   ╱     .   .
-- >    .            │  ╱enc    .  .
-- >    ▽            │ ╱         . ▽
-- >    . . . . . .▷Imp . . . . . ▷.
--
-- > enc_ref . id . to = enc_imp
--
prop_encodeRefencodeImp2 :: forall t. Token t => Imp t -> Bool
prop_encodeRefencodeImp2 x =

    (serialiseRef . toRef @t) x == serialiseImp @t x


-- | The property corresponding to the following part of the commuting diagram.
--
-- This checks that starting from the same encoding, the reference and real
-- implementation deserialise to equivalent values.
--
-- >    . . . . . .▷Ref . . . . . ▶Ref
-- >    .            △ ╲         ╱ │
-- >    .            .  ╲enc dec╱  │
-- >    .            .   ╲     ╱   │
-- >    .            .    ▶Enc▶    │from
-- >    .            .   .     ╲   │
-- >    .            .  .    dec╲  │
-- >    ▽            . .         ╲ ▼
-- >    . . . . . . ▷.. . . . . . ▶Imp
--
-- > dec_imp . enc_ref = from . dec_ref . enc_ref
--
prop_decodeRefdecodeImp :: forall t. Token t => t -> Bool
prop_decodeRefdecodeImp x =

    deserialiseImp @t enc  `eq`  (fromRef . deserialiseRef @t) enc

  where
    enc = serialiseRef x
    eq  = eqImp @t


--------------------------------------------------------------------------------
-- Token class instances for unsigned types
--

newtype TokWord8 = TokWord8 { unTokWord8 :: UInt }
  deriving (Eq, Show)

instance Token TokWord8 where
    type Imp TokWord8 = Word8

    fromRef = fromIntegral . Ref.fromUInt . unTokWord8
    toRef   = TokWord8 . Ref.toUInt . fromIntegral

    encodeImp = encodeWord8
    decodeImp = decodeWord8

    encodeRef (TokWord8 n) = Ref.encodeToken (Ref.MT0_UnsignedInt n)
    decodeRef = do Ref.MT0_UnsignedInt n <- Ref.decodeToken
                   return (TokWord8 n)

instance Arbitrary TokWord8 where
    arbitrary = TokWord8 <$> oneof arbitraryUInt_Word8

arbitraryUInt_Word8 :: [Gen UInt]
arbitraryUInt_Word8  = [ UIntSmall <$> arbitrarySmall
                       , UInt8     <$> arbitrarySmall
                       , UInt8     <$> arbitraryUInt8
                       ]


newtype TokWord16 = TokWord16 { unTokWord16 :: UInt }
  deriving (Eq, Show)

instance Token TokWord16 where
    type Imp TokWord16 = Word16

    fromRef = fromIntegral . Ref.fromUInt . unTokWord16
    toRef   = TokWord16 . Ref.toUInt . fromIntegral

    encodeImp = encodeWord16
    decodeImp = decodeWord16

    encodeRef (TokWord16 n) = Ref.encodeToken (Ref.MT0_UnsignedInt n)
    decodeRef = do Ref.MT0_UnsignedInt n <- Ref.decodeToken
                   return (TokWord16 n)

instance Arbitrary TokWord16 where
      arbitrary = TokWord16 <$> oneof arbitraryUInt_Word16

arbitraryUInt_Word16 :: [Gen UInt]
arbitraryUInt_Word16 = arbitraryUInt_Word8
                    ++ [ UInt16 <$> arbitrarySmall
                       , UInt16 <$> arbitraryUInt8
                       , UInt16 <$> arbitraryUInt16
                       ]


newtype TokWord32 = TokWord32 { unTokWord32 :: UInt }
  deriving (Eq, Show)

instance Token TokWord32 where
    type Imp TokWord32 = Word32

    fromRef = fromIntegral . Ref.fromUInt . unTokWord32
    toRef   = TokWord32 . Ref.toUInt . fromIntegral

    encodeImp = encodeWord32
    decodeImp = decodeWord32

    encodeRef (TokWord32 n) = Ref.encodeToken (Ref.MT0_UnsignedInt n)
    decodeRef = do Ref.MT0_UnsignedInt n <- Ref.decodeToken
                   return (TokWord32 n)

instance Arbitrary TokWord32 where
    arbitrary = TokWord32 <$> oneof arbitraryUInt_Word32

arbitraryUInt_Word32 :: [Gen UInt]
arbitraryUInt_Word32 = arbitraryUInt_Word16
                    ++ [ UInt32 <$> arbitrarySmall
                       , UInt32 <$> arbitraryUInt8
                       , UInt32 <$> arbitraryUInt16
                       , UInt32 <$> arbitraryUInt32
                       ]

newtype TokWord64 = TokWord64 { unTokWord64 :: UInt }
  deriving (Eq, Show)

instance Token TokWord64 where
    type Imp TokWord64 = Word64

    fromRef = fromIntegral . Ref.fromUInt . unTokWord64
    toRef   = TokWord64 . Ref.toUInt . fromIntegral

    encodeImp = encodeWord64
    decodeImp = decodeWord64

    encodeRef (TokWord64 n) = Ref.encodeToken (Ref.MT0_UnsignedInt n)
    decodeRef = do Ref.MT0_UnsignedInt n <- Ref.decodeToken
                   return (TokWord64 n)

instance Arbitrary TokWord64 where
    arbitrary = TokWord64 <$> oneof arbitraryUInt_Word64

arbitraryUInt_Word64 :: [Gen UInt]
arbitraryUInt_Word64 = arbitraryUInt_Word32
                    ++ [ UInt64 <$> arbitrarySmall
                       , UInt64 <$> arbitraryUInt8
                       , UInt64 <$> arbitraryUInt16
                       , UInt64 <$> arbitraryUInt32
                       , UInt64 <$> arbitraryUInt64
                       ]


newtype TokWord = TokWord { unTokWord :: UInt }
  deriving (Eq, Show)

instance Arbitrary TokWord where
    arbitrary = TokWord <$> oneof arbitraryUInt_Word

arbitraryUInt_Word :: [Gen UInt]
arbitraryUInt_Word   = arbitraryUInt_Word32
                    ++ [ UInt64 <$> arbitrarySmall
                       , UInt64 <$> arbitraryUInt8
                       , UInt64 <$> arbitraryUInt16
                       , UInt64 <$> arbitraryUInt32
#if defined(ARCH_64bit)
                       , UInt64 <$> arbitraryUInt64
#endif
                       ]

instance Token TokWord where
    type Imp TokWord = Word

    fromRef = fromIntegral . Ref.fromUInt . unTokWord
    toRef   = TokWord . Ref.toUInt . fromIntegral

    encodeImp = encodeWord
    decodeImp = decodeWord

    encodeRef (TokWord n) = Ref.encodeToken (Ref.MT0_UnsignedInt n)
    decodeRef = do Ref.MT0_UnsignedInt n <- Ref.decodeToken
                   return (TokWord n)


--------------------------------------------------------------------------------
-- Token class instances for signed types
--


data TokInt8 = TokInt8 Bool UInt
  deriving (Eq, Show)

instance Token TokInt8 where
    type Imp TokInt8 = Int8

    fromRef (TokInt8 True  n) =              (fromIntegral . Ref.fromUInt) n
    fromRef (TokInt8 False n) = (complement . fromIntegral . Ref.fromUInt) n
    toRef n | n >= 0    = TokInt8 True  ((Ref.toUInt . fromIntegral) n)
            | otherwise = TokInt8 False ((Ref.toUInt . fromIntegral . complement) n)

    encodeImp = encodeInt8
    decodeImp = decodeInt8

    encodeRef (TokInt8 True  n) = Ref.encodeToken (Ref.MT0_UnsignedInt n)
    encodeRef (TokInt8 False n) = Ref.encodeToken (Ref.MT1_NegativeInt n)

    decodeRef = do tok <- Ref.decodeToken
                   case tok of
                     Ref.MT0_UnsignedInt n -> return (TokInt8 True  n)
                     Ref.MT1_NegativeInt n -> return (TokInt8 False n)
                     _                     -> fail "decodeRef (TokInt)"

instance Arbitrary TokInt8 where
    arbitrary = TokInt8 <$> arbitrary <*> oneof arbitraryUInt_Int8
    shrink (TokInt8 sign n) = [ TokInt8 sign' n'
                              | (sign', n') <- shrink (sign, n) ]

arbitraryUInt_Int8 :: [Gen UInt]
arbitraryUInt_Int8   = [ UIntSmall <$> arbitrarySmall
                       , UInt8     <$> arbitrarySmall
                       , UInt8     <$> arbitraryUInt7
                       ]


data TokInt16 = TokInt16 Bool UInt
  deriving (Eq, Show)

instance Token TokInt16 where
    type Imp TokInt16 = Int16

    fromRef (TokInt16 True  n) =              (fromIntegral . Ref.fromUInt) n
    fromRef (TokInt16 False n) = (complement . fromIntegral . Ref.fromUInt) n
    toRef n | n >= 0    = TokInt16 True  ((Ref.toUInt . fromIntegral) n)
            | otherwise = TokInt16 False ((Ref.toUInt . fromIntegral . complement) n)

    encodeImp = encodeInt16
    decodeImp = decodeInt16

    encodeRef (TokInt16 True  n) = Ref.encodeToken (Ref.MT0_UnsignedInt n)
    encodeRef (TokInt16 False n) = Ref.encodeToken (Ref.MT1_NegativeInt n)

    decodeRef = do tok <- Ref.decodeToken
                   case tok of
                     Ref.MT0_UnsignedInt n -> return (TokInt16 True  n)
                     Ref.MT1_NegativeInt n -> return (TokInt16 False n)
                     _                     -> fail "decodeRef (TokInt16)"


instance Arbitrary TokInt16 where
    arbitrary = TokInt16 <$> arbitrary <*> oneof arbitraryUInt_Int16

arbitraryUInt_Int16 :: [Gen UInt]
arbitraryUInt_Int16  = arbitraryUInt_Int8
                    ++ [ UInt16 <$> arbitrarySmall
                       , UInt16 <$> arbitraryUInt7
                       , UInt16 <$> arbitraryUInt15
                       ]


data TokInt32 = TokInt32 Bool UInt
  deriving (Eq, Show)

instance Token TokInt32 where
    type Imp TokInt32 = Int32

    fromRef (TokInt32 True  n) =              (fromIntegral . Ref.fromUInt) n
    fromRef (TokInt32 False n) = (complement . fromIntegral . Ref.fromUInt) n
    toRef n | n >= 0    = TokInt32 True  ((Ref.toUInt . fromIntegral) n)
            | otherwise = TokInt32 False ((Ref.toUInt . fromIntegral . complement) n)

    encodeImp = encodeInt32
    decodeImp = decodeInt32

    encodeRef (TokInt32 True  n) = Ref.encodeToken (Ref.MT0_UnsignedInt n)
    encodeRef (TokInt32 False n) = Ref.encodeToken (Ref.MT1_NegativeInt n)

    decodeRef = do tok <- Ref.decodeToken
                   case tok of
                     Ref.MT0_UnsignedInt n -> return (TokInt32 True  n)
                     Ref.MT1_NegativeInt n -> return (TokInt32 False n)
                     _                     -> fail "decodeRef (TokInt32)"

instance Arbitrary TokInt32 where
    arbitrary = TokInt32 <$> arbitrary <*> oneof arbitraryUInt_Int32

arbitraryUInt_Int32 :: [Gen UInt]
arbitraryUInt_Int32  = arbitraryUInt_Int16
                    ++ [ UInt32 <$> arbitrarySmall
                       , UInt32 <$> arbitraryUInt7
                       , UInt32 <$> arbitraryUInt15
                       , UInt32 <$> arbitraryUInt31
                       ]


data TokInt64 = TokInt64 Bool UInt
  deriving (Eq, Show)

instance Token TokInt64 where
    type Imp TokInt64 = Int64

    fromRef (TokInt64 True  n) =              (fromIntegral . Ref.fromUInt) n
    fromRef (TokInt64 False n) = (complement . fromIntegral . Ref.fromUInt) n
    toRef n | n >= 0    = TokInt64 True  ((Ref.toUInt . fromIntegral) n)
            | otherwise = TokInt64 False ((Ref.toUInt . fromIntegral . complement) n)

    encodeImp = encodeInt64
    decodeImp = decodeInt64

    encodeRef (TokInt64 True  n) = Ref.encodeToken (Ref.MT0_UnsignedInt n)
    encodeRef (TokInt64 False n) = Ref.encodeToken (Ref.MT1_NegativeInt n)

    decodeRef = do tok <- Ref.decodeToken
                   case tok of
                     Ref.MT0_UnsignedInt n -> return (TokInt64 True  n)
                     Ref.MT1_NegativeInt n -> return (TokInt64 False n)
                     _                     -> fail "decodeRef (TokInt64)"

instance Arbitrary TokInt64 where
    arbitrary = TokInt64 <$> arbitrary <*> oneof arbitraryUInt_Int64

arbitraryUInt_Int64 :: [Gen UInt]
arbitraryUInt_Int64  = arbitraryUInt_Int32
                    ++ [ UInt64 <$> arbitrarySmall
                       , UInt64 <$> arbitraryUInt7
                       , UInt64 <$> arbitraryUInt15
                       , UInt64 <$> arbitraryUInt31
                       , UInt64 <$> arbitraryUInt63
                       ]


data TokInt = TokInt Bool UInt
  deriving (Eq, Show)

instance Token TokInt where
    type Imp TokInt = Int

    fromRef (TokInt True  n) =              (fromIntegral . Ref.fromUInt) n
    fromRef (TokInt False n) = (complement . fromIntegral . Ref.fromUInt) n
    toRef n | n >= 0    = TokInt True  ((Ref.toUInt . fromIntegral) n)
            | otherwise = TokInt False ((Ref.toUInt . fromIntegral . complement) n)

    encodeImp = encodeInt
    decodeImp = decodeInt

    encodeRef (TokInt True  n) = Ref.encodeToken (Ref.MT0_UnsignedInt n)
    encodeRef (TokInt False n) = Ref.encodeToken (Ref.MT1_NegativeInt n)

    decodeRef = do tok <- Ref.decodeToken
                   case tok of
                     Ref.MT0_UnsignedInt n -> return (TokInt True  n)
                     Ref.MT1_NegativeInt n -> return (TokInt False n)
                     _                     -> fail "decodeRef (TokInt)"

instance Arbitrary TokInt where
    arbitrary = TokInt <$> arbitrary <*> oneof arbitraryUInt_Int

arbitraryUInt_Int :: [Gen UInt]
arbitraryUInt_Int    = arbitraryUInt_Int32
                    ++ [ UInt64 <$> arbitrarySmall
                       , UInt64 <$> arbitraryUInt7
                       , UInt64 <$> arbitraryUInt15
                       , UInt64 <$> arbitraryUInt31
#if defined(ARCH_64bit)
                       , UInt64 <$> arbitraryUInt63
#endif
                       ]


data TokInteger = TokIntegerUInt UInt
                | TokIntegerNInt UInt
                | TokIntegerBig  LargeInteger
  deriving (Eq, Show)

instance Arbitrary TokInteger where
  arbitrary = oneof [ TokIntegerUInt <$> arbitrary
                    , TokIntegerNInt <$> arbitrary
                    , TokIntegerBig  <$> arbitrary
                    ]


instance Token TokInteger where
    type Imp TokInteger = Integer

    fromRef (TokIntegerUInt n) =              (fromIntegral . Ref.fromUInt) n
    fromRef (TokIntegerNInt n) = (complement . fromIntegral . Ref.fromUInt) n
    fromRef (TokIntegerBig  n) = getLargeInteger n

    toRef n | n >= 0 && n <= fromIntegral (maxBound :: Word64)
            = TokIntegerUInt ((Ref.toUInt . fromIntegral) n)

            | n < 0  && complement n <= fromIntegral (maxBound :: Word64)
            = TokIntegerNInt ((Ref.toUInt . fromIntegral . complement) n)

            | otherwise = TokIntegerBig (LargeInteger n)

    encodeImp = encodeInteger
    decodeImp = decodeInteger

    encodeRef (TokIntegerUInt n) = Ref.encodeToken (Ref.MT0_UnsignedInt n)
    encodeRef (TokIntegerNInt n) = Ref.encodeToken (Ref.MT1_NegativeInt n)
    encodeRef (TokIntegerBig  n) = Ref.encodeTerm (Ref.TBigInt (getLargeInteger n))

    decodeRef = do
      tok <- Ref.decodeToken
      case tok of
        Ref.MT0_UnsignedInt n -> return (TokIntegerUInt  n)
        Ref.MT1_NegativeInt n -> return (TokIntegerNInt n)
        Ref.MT6_Tag       tag -> do Ref.TBigInt n <- Ref.decodeTagged tag
                                    return (TokIntegerBig (LargeInteger n))
        _                     -> fail "decodeRef (TokInteger)"


--------------------------------------------------------------------------------
-- Arbitrary helpers for integer types
--

arbitrarySmall,
  arbitraryUInt8, arbitraryUInt16, arbitraryUInt32, arbitraryUInt64,
  arbitraryUInt7, arbitraryUInt15, arbitraryUInt31, arbitraryUInt63
  :: (Num n, Random n) => Gen n

arbitrarySmall  = chooseZeroToBound (23       :: Word)
arbitraryUInt8  = chooseZeroToBound (maxBound :: Word8)
arbitraryUInt16 = chooseZeroToBound (maxBound :: Word16)
arbitraryUInt32 = chooseZeroToBound (maxBound :: Word32)
arbitraryUInt64 = chooseZeroToBound (maxBound :: Word64)

arbitraryUInt7  = chooseZeroToBound (maxBound :: Int8)
arbitraryUInt15 = chooseZeroToBound (maxBound :: Int16)
arbitraryUInt31 = chooseZeroToBound (maxBound :: Int32)
arbitraryUInt63 = chooseZeroToBound (maxBound :: Int64)

chooseZeroToBound :: (Num a, Random a, Integral a1) => a1 -> Gen a
chooseZeroToBound bound =
    frequency [ (9, choose (0, bound'))
              , (1, pure bound') ]
  where
    bound' = fromIntegral bound


--------------------------------------------------------------------------------
-- Token class instances for floating point types
--

data TokHalf = TokHalf HalfSpecials
  deriving (Eq, Show)

instance Arbitrary TokHalf where
  arbitrary = TokHalf <$> arbitrary

instance Token TokHalf where
    type Imp TokHalf = Float

    eqImp = (==) `on` floatToWord

    fromRef (TokHalf (HalfSpecials n)) = Half.fromHalf n
    toRef = TokHalf
          . canonicaliseNaN
          . HalfSpecials
          . Half.toHalf

    canonicaliseImp = Half.fromHalf
                    . canonicaliseNaN
                    . Half.toHalf
    canonicaliseRef (TokHalf n) = TokHalf (canonicaliseNaN n)

    encodeImp = encodeFloat16
    decodeImp = decodeFloat

    encodeRef (TokHalf n) = Ref.encodeToken (Ref.MT7_Float16 n)
    decodeRef = do Ref.MT7_Float16 n <- Ref.decodeToken
                   return (TokHalf n)

data TokFloat = TokFloat FloatSpecials
              | TokFloatNan
  deriving (Eq, Show)

instance Arbitrary TokFloat where
  arbitrary = frequency [(19, TokFloat <$> arbitrary), (1, pure TokFloatNan)]

instance Token TokFloat where
    type Imp TokFloat = Float

    eqImp = (==) `on` floatToWord

    fromRef (TokFloat n) = getFloatSpecials n
    fromRef TokFloatNan  = canonicalNaN
    toRef n
      | isNaN n   = TokFloatNan
      | otherwise = TokFloat (FloatSpecials n)

    canonicaliseImp = canonicaliseNaN

    canonicaliseRef TokFloatNan = TokFloatNan
    canonicaliseRef (TokFloat (FloatSpecials n))
      | isNaN n   = TokFloatNan
      | otherwise = TokFloat (FloatSpecials n)

    encodeImp = encodeFloat
    decodeImp = decodeFloat

    encodeRef (TokFloat n) = Ref.encodeToken (Ref.MT7_Float32 n)
    encodeRef TokFloatNan  = Ref.encodeToken (Ref.MT7_Float16 canonicalNaN)

    decodeRef = do tok <- Ref.decodeToken
                   case tok of
                     Ref.MT7_Float16 n | isNaN n
                                       -> return TokFloatNan
                     Ref.MT7_Float32 n -> return (TokFloat n)
                     _                 -> fail "decodeRef (TokFloat)"


data TokDouble = TokDouble DoubleSpecials
               | TokDoubleNan
  deriving (Eq, Show)

instance Arbitrary TokDouble where
  arbitrary = frequency [(19, TokDouble <$> arbitrary), (1, pure TokDoubleNan)]

instance Token TokDouble where
    type Imp TokDouble = Double

    eqImp = (==) `on` doubleToWord

    fromRef (TokDouble n) = getDoubleSpecials n
    fromRef TokDoubleNan  = canonicalNaN
    toRef n
      | isNaN n   = TokDoubleNan
      | otherwise = TokDouble (DoubleSpecials n)

    canonicaliseImp = canonicaliseNaN
    canonicaliseRef TokDoubleNan = TokDoubleNan
    canonicaliseRef (TokDouble (DoubleSpecials n))
      | isNaN n   = TokDoubleNan
      | otherwise = TokDouble (DoubleSpecials n)

    encodeImp = encodeDouble
    decodeImp = decodeDouble

    encodeRef (TokDouble n) = Ref.encodeToken (Ref.MT7_Float64 n)
    encodeRef TokDoubleNan  = Ref.encodeToken (Ref.MT7_Float16 canonicalNaN)

    decodeRef = do tok <- Ref.decodeToken
                   case tok of
                     Ref.MT7_Float16 n | isNaN n
                                       -> return TokDoubleNan
                     Ref.MT7_Float64 n -> return (TokDouble n)
                     _                 -> fail "decodeRef (TokDouble)"


--------------------------------------------------------------------------------
-- Miscelaneous token class instances
--

data TokTag = TokTag { unTokTag :: UInt }
  deriving (Eq, Show)

instance Arbitrary TokTag where
    arbitrary = TokTag <$> oneof arbitraryUInt_Word

instance Token TokTag where
    type Imp TokTag = Word

    fromRef = fromIntegral . Ref.fromUInt . unTokTag
    toRef   = TokTag . Ref.toUInt . fromIntegral

    encodeImp = encodeTag
    decodeImp = decodeTag

    encodeRef (TokTag n) = Ref.encodeToken (Ref.MT6_Tag n)
    decodeRef            = do Ref.MT6_Tag n <- Ref.decodeToken
                              return (TokTag n)


data TokTag64 = TokTag64 { unTokTag64 :: UInt }
  deriving (Eq, Show)

instance Arbitrary TokTag64 where
    arbitrary = TokTag64 <$> oneof arbitraryUInt_Word64

instance Token TokTag64 where
    type Imp TokTag64 = Word64

    fromRef = fromIntegral . Ref.fromUInt . unTokTag64
    toRef   = TokTag64 . Ref.toUInt . fromIntegral

    encodeImp = encodeTag64
    decodeImp = decodeTag64

    encodeRef (TokTag64 n) = Ref.encodeToken (Ref.MT6_Tag n)
    decodeRef              = do Ref.MT6_Tag n <- Ref.decodeToken
                                return (TokTag64 n)



instance Token Ref.Simple where
    type Imp Ref.Simple = Word8

    fromRef = Ref.fromSimple
    toRef   = Ref.toSimple

    encodeImp = encodeSimple
    decodeImp = decodeSimple

    encodeRef n = Ref.encodeToken (Ref.MT7_Simple n)
    decodeRef   = do Ref.MT7_Simple n <- Ref.decodeToken
                     return n


--------------------------------------------------------------------------------
-- Token class instances for Term type
--

instance Token Ref.Term where
    type Imp Ref.Term = Term

    eqImp = eqTerm

    fromRef = fromRefTerm
    toRef   = toRefTerm

    canonicaliseImp = canonicaliseTerm
    canonicaliseRef = Ref.canonicaliseTerm

    encodeImp = encodeTerm
    decodeImp = decodeTerm

    encodeRef = Ref.encodeTerm
    decodeRef = Ref.decodeTerm


--------------------------------------------------------------------------------
-- TestTree API

testTree :: TestTree
testTree =
  testGroup "properties"
  [ testGroup "to . id . from = canon_ref"
    [ testProperty "Word8"   (prop_fromRefToRef @ TokWord8)
    , testProperty "Word16"  (prop_fromRefToRef @ TokWord16)
    , testProperty "Word32"  (prop_fromRefToRef @ TokWord32)
    , testProperty "Word64"  (prop_fromRefToRef @ TokWord64)
    , testProperty "Word"    (prop_fromRefToRef @ TokWord)
--  , testProperty "NegWord" (prop_fromRefToRef @ TokNegWord)
    , testProperty "Int8"    (prop_fromRefToRef @ TokInt8)
    , testProperty "Int16"   (prop_fromRefToRef @ TokInt16)
    , testProperty "Int32"   (prop_fromRefToRef @ TokInt32)
    , testProperty "Int64"   (prop_fromRefToRef @ TokInt64)
    , testProperty "Int"     (prop_fromRefToRef @ TokInt)
    , testProperty "Integer" (prop_fromRefToRef @ TokInteger)
    , testProperty "Half"    (prop_fromRefToRef @ TokHalf)
    , testProperty "Float"   (prop_fromRefToRef @ TokFloat)
    , testProperty "Double"  (prop_fromRefToRef @ TokDouble)
    , testProperty "Tag"     (prop_fromRefToRef @ TokTag)
    , testProperty "Tag64"   (prop_fromRefToRef @ TokTag64)
    , testProperty "Simple"  (prop_fromRefToRef @ Ref.Simple)
    , testProperty "Term"    (prop_fromRefToRef @ Ref.Term)
    ]

  , testGroup "from . id . to = canon_imp"
    [ testProperty "Word8"   (prop_toRefFromRef @ TokWord8)
    , testProperty "Word16"  (prop_toRefFromRef @ TokWord16)
    , testProperty "Word32"  (prop_toRefFromRef @ TokWord32)
    , testProperty "Word64"  (prop_toRefFromRef @ TokWord64)
    , testProperty "Word"    (prop_toRefFromRef @ TokWord)
--  , testProperty "NegWord" (prop_toRefFromRef @ TokNegWord)
    , testProperty "Int8"    (prop_toRefFromRef @ TokInt8)
    , testProperty "Int16"   (prop_toRefFromRef @ TokInt16)
    , testProperty "Int32"   (prop_toRefFromRef @ TokInt32)
    , testProperty "Int64"   (prop_toRefFromRef @ TokInt64)
    , testProperty "Int"     (prop_toRefFromRef @ TokInt)
    , testProperty "Integer" (prop_toRefFromRef @ TokInteger)
    , testProperty "Half"    (prop_toRefFromRef @ TokHalf)
    , testProperty "Float"   (prop_toRefFromRef @ TokFloat)
    , testProperty "Double"  (prop_toRefFromRef @ TokDouble)
    , testProperty "Tag"     (prop_toRefFromRef @ TokTag)
    , testProperty "Tag64"   (prop_toRefFromRef @ TokTag64)
    , testProperty "Simple"  (prop_toRefFromRef @ Ref.Simple)
    , testProperty "Term"    (prop_toRefFromRef @ Ref.Term)
    ]

  , testGroup "dec_ref . enc_ref = id"
    [ testProperty "Word8"   (prop_encodeRefdecodeRef @ TokWord8)
    , testProperty "Word16"  (prop_encodeRefdecodeRef @ TokWord16)
    , testProperty "Word32"  (prop_encodeRefdecodeRef @ TokWord32)
    , testProperty "Word64"  (prop_encodeRefdecodeRef @ TokWord64)
    , testProperty "Word"    (prop_encodeRefdecodeRef @ TokWord)
--  , testProperty "NegWord" (prop_encodeRefdecodeRef @ TokNegWord)
    , testProperty "Int8"    (prop_encodeRefdecodeRef @ TokInt8)
    , testProperty "Int16"   (prop_encodeRefdecodeRef @ TokInt16)
    , testProperty "Int32"   (prop_encodeRefdecodeRef @ TokInt32)
    , testProperty "Int64"   (prop_encodeRefdecodeRef @ TokInt64)
    , testProperty "Int"     (prop_encodeRefdecodeRef @ TokInt)
    , testProperty "Integer" (prop_encodeRefdecodeRef @ TokInteger)
    , testProperty "Half"    (prop_encodeRefdecodeRef @ TokHalf)
    , testProperty "Float"   (prop_encodeRefdecodeRef @ TokFloat)
    , testProperty "Double"  (prop_encodeRefdecodeRef @ TokDouble)
    , testProperty "Tag"     (prop_encodeRefdecodeRef @ TokTag)
    , testProperty "Tag64"   (prop_encodeRefdecodeRef @ TokTag64)
    , testProperty "Simple"  (prop_encodeRefdecodeRef @ Ref.Simple)
    , testProperty "Term"    (prop_encodeRefdecodeRef @ Ref.Term)
    ]

  , testGroup "dec_imp . enc_imp = canon_imp"
    [ testProperty "Word8"   (prop_encodeImpdecodeImp @ TokWord8)
    , testProperty "Word16"  (prop_encodeImpdecodeImp @ TokWord16)
    , testProperty "Word32"  (prop_encodeImpdecodeImp @ TokWord32)
    , testProperty "Word64"  (prop_encodeImpdecodeImp @ TokWord64)
    , testProperty "Word"    (prop_encodeImpdecodeImp @ TokWord)
--  , testProperty "NegWord" (prop_encodeImpdecodeImp @ TokNegWord)
    , testProperty "Int8"    (prop_encodeImpdecodeImp @ TokInt8)
    , testProperty "Int16"   (prop_encodeImpdecodeImp @ TokInt16)
    , testProperty "Int32"   (prop_encodeImpdecodeImp @ TokInt32)
    , testProperty "Int64"   (prop_encodeImpdecodeImp @ TokInt64)
    , testProperty "Int"     (prop_encodeImpdecodeImp @ TokInt)
    , testProperty "Integer" (prop_encodeImpdecodeImp @ TokInteger)
    , testProperty "Half"    (prop_encodeImpdecodeImp @ TokHalf)
    , testProperty "Float"   (prop_encodeImpdecodeImp @ TokFloat)
    , testProperty "Double"  (prop_encodeImpdecodeImp @ TokDouble)
    , testProperty "Tag"     (prop_encodeImpdecodeImp @ TokTag)
    , testProperty "Tag64"   (prop_encodeImpdecodeImp @ TokTag64)
    , testProperty "Simple"  (prop_encodeImpdecodeImp @ Ref.Simple)
    , testProperty "Term"    (prop_encodeImpdecodeImp @ Ref.Term)
    ]

  , testGroup "dec_imp . enc_imp = canon_imp (all 2-splits)"
    [ testProperty "Word8"   (prop_encodeImpdecodeImp_splits2 @ TokWord8)
    , testProperty "Word16"  (prop_encodeImpdecodeImp_splits2 @ TokWord16)
    , testProperty "Word32"  (prop_encodeImpdecodeImp_splits2 @ TokWord32)
    , testProperty "Word64"  (prop_encodeImpdecodeImp_splits2 @ TokWord64)
    , testProperty "Word"    (prop_encodeImpdecodeImp_splits2 @ TokWord)
--  , testProperty "NegWord" (prop_encodeImpdecodeImp_splits2 @ TokNegWord)
    , testProperty "Int8"    (prop_encodeImpdecodeImp_splits2 @ TokInt8)
    , testProperty "Int16"   (prop_encodeImpdecodeImp_splits2 @ TokInt16)
    , testProperty "Int32"   (prop_encodeImpdecodeImp_splits2 @ TokInt32)
    , testProperty "Int64"   (prop_encodeImpdecodeImp_splits2 @ TokInt64)
    , testProperty "Int"     (prop_encodeImpdecodeImp_splits2 @ TokInt)
    , testProperty "Integer" (prop_encodeImpdecodeImp_splits2 @ TokInteger)
    , testProperty "Half"    (prop_encodeImpdecodeImp_splits2 @ TokHalf)
    , testProperty "Float"   (prop_encodeImpdecodeImp_splits2 @ TokFloat)
    , testProperty "Double"  (prop_encodeImpdecodeImp_splits2 @ TokDouble)
    , testProperty "Tag"     (prop_encodeImpdecodeImp_splits2 @ TokTag)
    , testProperty "Tag64"   (prop_encodeImpdecodeImp_splits2 @ TokTag64)
    , testProperty "Simple"  (prop_encodeImpdecodeImp_splits2 @ Ref.Simple)
    , localOption (QuickCheckMaxSize 100) $
      testProperty "Term"    (prop_encodeImpdecodeImp_splits2 @ Ref.Term)
    ]

  , testGroup "dec_imp . enc_imp = canon_imp (all 3-splits)"
    [ testProperty "Word8"   (prop_encodeImpdecodeImp_splits3 @ TokWord8)
    , testProperty "Word16"  (prop_encodeImpdecodeImp_splits3 @ TokWord16)
    , testProperty "Word32"  (prop_encodeImpdecodeImp_splits3 @ TokWord32)
    , testProperty "Word64"  (prop_encodeImpdecodeImp_splits3 @ TokWord64)
    , testProperty "Word"    (prop_encodeImpdecodeImp_splits3 @ TokWord)
--  , testProperty "NegWord" (prop_encodeImpdecodeImp_splits3 @ TokNegWord)
    , testProperty "Int8"    (prop_encodeImpdecodeImp_splits3 @ TokInt8)
    , testProperty "Int16"   (prop_encodeImpdecodeImp_splits3 @ TokInt16)
    , testProperty "Int32"   (prop_encodeImpdecodeImp_splits3 @ TokInt32)
    , testProperty "Int64"   (prop_encodeImpdecodeImp_splits3 @ TokInt64)
    , testProperty "Int"     (prop_encodeImpdecodeImp_splits3 @ TokInt)
    , testProperty "Integer" (prop_encodeImpdecodeImp_splits3 @ TokInteger)
    , testProperty "Half"    (prop_encodeImpdecodeImp_splits3 @ TokHalf)
    , testProperty "Float"   (prop_encodeImpdecodeImp_splits3 @ TokFloat)
    , testProperty "Double"  (prop_encodeImpdecodeImp_splits3 @ TokDouble)
    , testProperty "Tag"     (prop_encodeImpdecodeImp_splits3 @ TokTag)
    , testProperty "Tag64"   (prop_encodeImpdecodeImp_splits3 @ TokTag64)
    , testProperty "Simple"  (prop_encodeImpdecodeImp_splits3 @ Ref.Simple)
    , localOption (QuickCheckMaxSize 25) $
      testProperty "Term"    (prop_encodeImpdecodeImp_splits3 @ Ref.Term)
    ]

  , testGroup "enc_imp . from = enc_ref . canon_ref"
    [ testProperty "Word8"   (prop_encodeRefencodeImp1 @ TokWord8)
    , testProperty "Word16"  (prop_encodeRefencodeImp1 @ TokWord16)
    , testProperty "Word32"  (prop_encodeRefencodeImp1 @ TokWord32)
    , testProperty "Word64"  (prop_encodeRefencodeImp1 @ TokWord64)
    , testProperty "Word"    (prop_encodeRefencodeImp1 @ TokWord)
--  , testProperty "NegWord" (prop_encodeRefencodeImp1 @ TokNegWord)
    , testProperty "Int8"    (prop_encodeRefencodeImp1 @ TokInt8)
    , testProperty "Int16"   (prop_encodeRefencodeImp1 @ TokInt16)
    , testProperty "Int32"   (prop_encodeRefencodeImp1 @ TokInt32)
    , testProperty "Int64"   (prop_encodeRefencodeImp1 @ TokInt64)
    , testProperty "Int"     (prop_encodeRefencodeImp1 @ TokInt)
    , testProperty "Integer" (prop_encodeRefencodeImp1 @ TokInteger)
    , testProperty "Half"    (prop_encodeRefencodeImp1 @ TokHalf)
    , testProperty "Float"   (prop_encodeRefencodeImp1 @ TokFloat)
    , testProperty "Double"  (prop_encodeRefencodeImp1 @ TokDouble)
    , testProperty "Tag"     (prop_encodeRefencodeImp1 @ TokTag)
    , testProperty "Tag64"   (prop_encodeRefencodeImp1 @ TokTag64)
    , testProperty "Simple"  (prop_encodeRefencodeImp1 @ Ref.Simple)
    , testProperty "Term"    (prop_encodeRefencodeImp1 @ Ref.Term)
    ]

  , testGroup "enc_ref . to = enc_imp"
    [ testProperty "Word8"   (prop_encodeRefencodeImp2 @ TokWord8)
    , testProperty "Word16"  (prop_encodeRefencodeImp2 @ TokWord16)
    , testProperty "Word32"  (prop_encodeRefencodeImp2 @ TokWord32)
    , testProperty "Word64"  (prop_encodeRefencodeImp2 @ TokWord64)
    , testProperty "Word"    (prop_encodeRefencodeImp2 @ TokWord)
--  , testProperty "NegWord" (prop_encodeRefencodeImp2 @ TokNegWord)
    , testProperty "Int8"    (prop_encodeRefencodeImp2 @ TokInt8)
    , testProperty "Int16"   (prop_encodeRefencodeImp2 @ TokInt16)
    , testProperty "Int32"   (prop_encodeRefencodeImp2 @ TokInt32)
    , testProperty "Int64"   (prop_encodeRefencodeImp2 @ TokInt64)
    , testProperty "Int"     (prop_encodeRefencodeImp2 @ TokInt)
    , testProperty "Integer" (prop_encodeRefencodeImp2 @ TokInteger)
    , testProperty "Half"    (prop_encodeRefencodeImp2 @ TokHalf)
    , testProperty "Float"   (prop_encodeRefencodeImp2 @ TokFloat)
    , testProperty "Double"  (prop_encodeRefencodeImp2 @ TokDouble)
    , testProperty "Tag"     (prop_encodeRefencodeImp2 @ TokTag)
    , testProperty "Tag64"   (prop_encodeRefencodeImp2 @ TokTag64)
    , testProperty "Simple"  (prop_encodeRefencodeImp2 @ Ref.Simple)
    , testProperty "Term"    (prop_encodeRefencodeImp2 @ Ref.Term)
    ]

  , testGroup "dec_imp . enc_ref = from . dec_ref . enc_ref"
    [ testProperty "Word8"   (prop_decodeRefdecodeImp @ TokWord8)
    , testProperty "Word16"  (prop_decodeRefdecodeImp @ TokWord16)
    , testProperty "Word32"  (prop_decodeRefdecodeImp @ TokWord32)
    , testProperty "Word64"  (prop_decodeRefdecodeImp @ TokWord64)
    , testProperty "Word"    (prop_decodeRefdecodeImp @ TokWord)
--  , testProperty "NegWord" (prop_decodeRefdecodeImp @ TokNegWord)
    , testProperty "Int8"    (prop_decodeRefdecodeImp @ TokInt8)
    , testProperty "Int16"   (prop_decodeRefdecodeImp @ TokInt16)
    , testProperty "Int32"   (prop_decodeRefdecodeImp @ TokInt32)
    , testProperty "Int64"   (prop_decodeRefdecodeImp @ TokInt64)
    , testProperty "Int"     (prop_decodeRefdecodeImp @ TokInt)
    , testProperty "Integer" (prop_decodeRefdecodeImp @ TokInteger)
    , testProperty "Half"    (prop_decodeRefdecodeImp @ TokHalf)
    , testProperty "Float"   (prop_decodeRefdecodeImp @ TokFloat)
    , testProperty "Double"  (prop_decodeRefdecodeImp @ TokDouble)
    , testProperty "Tag"     (prop_decodeRefdecodeImp @ TokTag)
    , testProperty "Tag64"   (prop_decodeRefdecodeImp @ TokTag64)
    , testProperty "Simple"  (prop_decodeRefdecodeImp @ Ref.Simple)
    , testProperty "Term"    (prop_decodeRefdecodeImp @ Ref.Term)
    ]
  ]
