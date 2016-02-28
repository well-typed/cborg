{-# LANGUAGE CPP                  #-}
{-# LANGUAGE ConstraintKinds      #-}
{-# LANGUAGE ScopedTypeVariables  #-}
module Tests.Serialise
  ( testTree -- :: TestTree
  ) where

import           Data.Int
import           Data.Time
import           Data.Word
import           GHC.Float (float2Double)
import           Data.Version

import           Data.Typeable
#if !MIN_VERSION_base(4,8,0)
import           Control.Applicative
#endif

import           Test.QuickCheck
import           Test.Tasty
import           Test.Tasty.QuickCheck
import           Test.QuickCheck.Instances ()

import qualified Data.ByteString as BS

import           Data.Binary.Serialise.CBOR
import           Data.Binary.Serialise.CBOR.Decoding
import           Data.Binary.Serialise.CBOR.FlatTerm

import qualified Data.HashMap.Strict        as HashMap
import qualified Data.HashSet               as HashSet
import qualified Data.IntMap                as IntMap
import qualified Data.IntSet                as IntSet
import qualified Data.Map                   as Map
import qualified Data.Sequence              as Sequence
import qualified Data.Set                   as Set
import qualified Data.Vector                as Vector
--------------------------------------------------------------------------------
-- Tests and properties

-- | Simple proxy type, used as a witness.
data T a = T

-- | Ensure that serializing and deserializing a term results in the original
-- term.
prop_serialiseId :: (Serialise a, Eq a, Show a) => T a -> a -> Bool
prop_serialiseId _ a = a == (deserialise . serialise) a

-- | Ensure that serializing and deserializing a term (into @'FlatTerm'@ form)
-- results in the original term.
prop_flatTermId :: forall a. (Serialise a, Eq a, Show a) => T a -> a -> Bool
prop_flatTermId _ a = Right a == (fromFlat . toFlat) a
  where
    toFlat   = toFlatTerm . encode
    fromFlat = fromFlatTerm (decode :: Decoder a)

-- | Ensure that serializing a term into a @'FlatTerm'@ always gives us a
-- valid @'FlatTerm'@ back.
prop_validFlatTerm :: (Serialise a, Eq a, Show a) => T a -> a -> Bool
prop_validFlatTerm _ = validFlatTerm . toFlatTerm . encode

--------------------------------------------------------------------------------
-- Corner case or specific properties to test

-- | Ensure that when we encode a Float but decode as a Double, we get the same
-- value.
prop_encodeFloatToDouble :: Float -> Bool
prop_encodeFloatToDouble x = Right dbl == fromFlatTerm dec ft
  where
    dbl = float2Double x

    dec = decode :: Decoder Double
    ft  = toFlatTerm (encode x)

--------------------------------------------------------------------------------
-- Extra orphan instances

instance Arbitrary Version where
  arbitrary = Version <$> listOf1 (choose (1, 15)) <*> pure []

--------------------------------------------------------------------------------
-- TestTree API

testTree :: TestTree
testTree = testGroup "Serialise class"
  [ testGroup "Corner cases"
      [ testProperty "decode float to double"      prop_encodeFloatToDouble
      ]
  , testGroup "Simple instance invariants"
      [ mkTest (T :: T ())
      , mkTest (T :: T Bool)
      , mkTest (T :: T Int)
      , mkTest (T :: T Int64)
      , mkTest (T :: T Word)
      , mkTest (T :: T Word64)
      , mkTest (T :: T Integer)
      , mkTest (T :: T Float)
      , mkTest (T :: T Double)
      , mkTest (T :: T Char)
      , mkTest (T :: T (Int, Char))
      , mkTest (T :: T (Int, Char, Bool))
      , mkTest (T :: T (Maybe Int))
      , mkTest (T :: T (Either String Int))
      , mkTest (T :: T String)
      , mkTest (T :: T BS.ByteString)
      , mkTest (T :: T [Int])
      , mkTest (T :: T UTCTime)
      , mkTest (T :: T Version)
      , mkTest (T :: T (Map.Map Int String))
      , mkTest (T :: T (Sequence.Seq Int))
      , mkTest (T :: T (Set.Set Int))
      , mkTest (T :: T IntSet.IntSet)
      , mkTest (T :: T (IntMap.IntMap String))
      , mkTest (T :: T (HashMap.HashMap Int String))
      , mkTest (T :: T (HashSet.HashSet Int))
      , mkTest (T :: T (Vector.Vector Int))
      ]
  ]

--------------------------------------------------------------------------------
-- Extra machinery

-- A simple alias to make the following properties more convenient to write
type BasicType a = (Arbitrary a, Typeable a, Serialise a, Eq a, Show a)

mkTest :: forall a. BasicType a => T a -> TestTree
mkTest t = testGroup ("type: " ++ show (typeOf (undefined :: a)))
  [ testProperty "cbor roundtrip"      (prop_serialiseId   t)
  , testProperty "flat roundtrip"      (prop_flatTermId    t)
  , testProperty "flat term is valid"  (prop_validFlatTerm t)
  ]
