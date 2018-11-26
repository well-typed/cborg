{-# LANGUAGE CPP                  #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE AllowAmbiguousTypes  #-}
{-# LANGUAGE TypeApplications     #-}

module Tests.Canonical (testTree) where

import           Prelude hiding (decodeFloat, encodeFloat)

#if !MIN_VERSION_base(4,8,0)
import           Control.Applicative
#endif

import qualified Data.ByteString.Lazy as LBS

import           Codec.CBOR.Read (deserialiseFromBytes)
import           Codec.CBOR.Decoding

import           Test.Tasty (TestTree, testGroup)
import           Test.Tasty.QuickCheck (testProperty)
import           Test.QuickCheck

import           Tests.Properties hiding (testTree)


-- | This is a version of 'prop_decodeRefdecodeImp' but where we restrict the
-- encoded input to non-canonical forms. These forms are covered by the
-- original property. This property just ensures that we have good coverage of
-- this case.
--
prop_decodeRefdecodeImp_NonCanonical :: forall t. Token t => t -> Property
prop_decodeRefdecodeImp_NonCanonical x =
    let enc  = serialiseRef x
        y    = deserialiseRef @t enc
        y'   = deserialiseImp @t enc
        enc' = serialiseImp @t y'
        isCanonical = enc == enc'
     in not isCanonical ==>
        -- This property holds without this pre-condition, as demonstrated by
        -- prop_decodeRefdecodeImp, but using it ensures we get good coverage
        -- of the non-canonical cases
        y' `eq` fromRef y

  where
    eq = eqImp @t


-- | Check that the special checked canonical form decoder primitives work.
--
-- We decode with the normal and canonical decoder, and check that they agree
-- in the canonical cases, and that the canonical decoder rejects the
-- non-canonical cases.
--
-- We have a QC coverage check to make sure we are covering enough of both
-- canonical and non-canonical cases.
--
prop_decodeCanonical :: forall t. Token t
                     => (forall s. Decoder s (Imp t))
                     -> t -> Property
prop_decodeCanonical decodeCanonical x =
    classify isCanonical "canonical" $
    case deserialiseFromBytes decodeCanonical enc of
      Left  _failure       -> not isCanonical
      Right (trailing, y') ->     isCanonical
                               && eqImp @t y y'
                               && LBS.null trailing
  where
    enc = serialiseRef x
    y   = deserialiseImp @t enc
    -- It is canonical if it re-encodes to the same bytes we decoded
    isCanonical = serialiseImp @t y == enc



prop_decodeCanonical_Word :: TokWord -> Property
prop_decodeCanonical_Word = prop_decodeCanonical decodeWordCanonical

prop_decodeCanonical_Word8 :: TokWord8 -> Property
prop_decodeCanonical_Word8 = prop_decodeCanonical decodeWord8Canonical

prop_decodeCanonical_Word16 :: TokWord16 -> Property
prop_decodeCanonical_Word16 = prop_decodeCanonical decodeWord16Canonical

prop_decodeCanonical_Word32 :: TokWord32 -> Property
prop_decodeCanonical_Word32 = prop_decodeCanonical decodeWord32Canonical

prop_decodeCanonical_Word64 :: TokWord64 -> Property
prop_decodeCanonical_Word64 = prop_decodeCanonical decodeWord64Canonical

--prop_decodeCanonical_NegWord :: TokNegWord -> Property
--prop_decodeCanonical_NegWord = prop_decodeCanonical decodeNegWordCanonical

prop_decodeCanonical_Int :: TokInt -> Property
prop_decodeCanonical_Int = prop_decodeCanonical decodeIntCanonical

prop_decodeCanonical_Int8 :: TokInt8 -> Property
prop_decodeCanonical_Int8 = prop_decodeCanonical decodeInt8Canonical

prop_decodeCanonical_Int16 :: TokInt16 -> Property
prop_decodeCanonical_Int16 = prop_decodeCanonical decodeInt16Canonical

prop_decodeCanonical_Int32 :: TokInt32 -> Property
prop_decodeCanonical_Int32 = prop_decodeCanonical decodeInt32Canonical

prop_decodeCanonical_Int64 :: TokInt64 -> Property
prop_decodeCanonical_Int64 = prop_decodeCanonical decodeInt64Canonical

prop_decodeCanonical_Integer :: TokInteger -> Property
prop_decodeCanonical_Integer = prop_decodeCanonical decodeIntegerCanonical

prop_decodeCanonical_Half :: TokHalf -> Property
prop_decodeCanonical_Half = prop_decodeCanonical decodeFloat16Canonical

prop_decodeCanonical_Float :: TokFloat -> Property
prop_decodeCanonical_Float = prop_decodeCanonical decodeFloatCanonical

prop_decodeCanonical_Double :: TokDouble -> Property
prop_decodeCanonical_Double = prop_decodeCanonical decodeDoubleCanonical

prop_decodeCanonical_Tag :: TokTag -> Property
prop_decodeCanonical_Tag = prop_decodeCanonical decodeTagCanonical

prop_decodeCanonical_Tag64 :: TokTag64 -> Property
prop_decodeCanonical_Tag64 = prop_decodeCanonical decodeTag64Canonical

prop_decodeCanonical_Simple :: Simple -> Property
prop_decodeCanonical_Simple = prop_decodeCanonical decodeSimpleCanonical


{-
  , decodeNegWordCanonical   -- :: Decoder s Word
  , decodeNegWord64Canonical -- :: Decoder s Word64
  , decodeBytesCanonical -- :: Decoder s ByteString
  , decodeByteArrayCanonical -- :: Decoder s ByteArray
  , decodeStringCanonical -- :: Decoder s Text
  , decodeUtf8ByteArrayCanonical -- :: Decoder s ByteArray
  , decodeListLenCanonical -- :: Decoder s Int
  , decodeMapLenCanonical -- :: Decoder s Int
-}

--------------------------------------------------------------------------------
-- TestTree API

testTree :: TestTree
testTree =
  testGroup "properties"
  [ testGroup "decode non-canonical encoding"
    [ testProperty "Word8"   (prop_decodeRefdecodeImp_NonCanonical @ TokWord8)
    , testProperty "Word16"  (prop_decodeRefdecodeImp_NonCanonical @ TokWord16)
    , testProperty "Word32"  (prop_decodeRefdecodeImp_NonCanonical @ TokWord32)
    , testProperty "Word64"  (prop_decodeRefdecodeImp_NonCanonical @ TokWord64)
    , testProperty "Word"    (prop_decodeRefdecodeImp_NonCanonical @ TokWord)
--  , testProperty "NegWord" (prop_decodeRefdecodeImp_NonCanonical @ TokNegWord)
    , testProperty "Int8"    (prop_decodeRefdecodeImp_NonCanonical @ TokInt8)
    , testProperty "Int16"   (prop_decodeRefdecodeImp_NonCanonical @ TokInt16)
    , testProperty "Int32"   (prop_decodeRefdecodeImp_NonCanonical @ TokInt32)
    , testProperty "Int64"   (prop_decodeRefdecodeImp_NonCanonical @ TokInt64)
    , testProperty "Int"     (prop_decodeRefdecodeImp_NonCanonical @ TokInt)
    , testProperty "Integer" (prop_decodeRefdecodeImp_NonCanonical @ TokInteger)
    , testProperty "Half"    (prop_decodeRefdecodeImp_NonCanonical @ TokHalf)
    , testProperty "Float"   (prop_decodeRefdecodeImp_NonCanonical @ TokFloat)
    , testProperty "Double"  (prop_decodeRefdecodeImp_NonCanonical @ TokDouble)
    , testProperty "Tag"     (prop_decodeRefdecodeImp_NonCanonical @ TokTag)
    , testProperty "Tag64"   (prop_decodeRefdecodeImp_NonCanonical @ TokTag64)
    , testProperty "Simple"  (prop_decodeRefdecodeImp_NonCanonical @ Simple)
    , testProperty "Term"    (prop_decodeRefdecodeImp_NonCanonical @ Term)
    ]

  , testGroup "canonical decoding"
    [ testProperty "Word"     prop_decodeCanonical_Word
    , testProperty "Word8"    prop_decodeCanonical_Word8
    , testProperty "Word16"   prop_decodeCanonical_Word16
    , testProperty "Word32"   prop_decodeCanonical_Word32
    , testProperty "Word64"   prop_decodeCanonical_Word64
    , testProperty "Int"      prop_decodeCanonical_Int
    , testProperty "Int8"     prop_decodeCanonical_Int8
    , testProperty "Int16"    prop_decodeCanonical_Int16
    , testProperty "Int32"    prop_decodeCanonical_Int32
    , testProperty "Int64"    prop_decodeCanonical_Int64
    , testProperty "Integer"  prop_decodeCanonical_Integer
    , testProperty "Half"     prop_decodeCanonical_Half
    , testProperty "Float"    prop_decodeCanonical_Float
    , testProperty "Double"   prop_decodeCanonical_Double
    , testProperty "Tag"      prop_decodeCanonical_Tag
    , testProperty "Tag64"    prop_decodeCanonical_Tag64
    , testProperty "Simple"   prop_decodeCanonical_Simple
    ]
  ]

