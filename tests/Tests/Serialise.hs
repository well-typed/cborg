{-# LANGUAGE ConstraintKinds      #-}
{-# LANGUAGE ScopedTypeVariables  #-}
module Tests.Serialise
  ( testTree -- :: TestTree
  ) where

import           Data.Int
import           Data.Time
import           Data.Word
import           Data.Version
import           Data.Typeable
#if !MIN_VERSION_base(4,8,0)
import           Control.Applicative
#endif

import           Test.Tasty
import           Test.QuickCheck
import           Test.Tasty.QuickCheck
import           Test.QuickCheck.Instances ()

import qualified Data.ByteString as BS

import           Data.Binary.Serialise.CBOR
import           Data.Binary.Serialise.CBOR.Decoding
import           Data.Binary.Serialise.CBOR.FlatTerm

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
-- Extra orphan instances

instance Arbitrary Version where
  arbitrary = Version <$> listOf1 (choose (1, 15)) <*> pure []

--------------------------------------------------------------------------------
-- TestTree API

testTree :: TestTree
testTree = testGroup "Serialise class"
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
