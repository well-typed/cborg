{-# LANGUAGE CPP                 #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Tests.FlatTerm
  ( testTree -- :: TestTree
  ) where

#if __GLASGOW_HASKELL__ < 710
import           Data.Word
#endif
import           Data.Typeable

import           Test.Tasty
import           Test.QuickCheck
import           Test.Tasty.QuickCheck

import           Data.Binary.Serialise.CBOR
import           Data.Binary.Serialise.CBOR.Decoding
import           Data.Binary.Serialise.CBOR.FlatTerm

--------------------------------------------------------------------------------
-- Tests and properties

data T a = T

prop_validFlatTerm :: (Serialise a, Eq a, Show a) => T a -> a -> Bool
prop_validFlatTerm _ = validFlatTerm . toFlatTerm . encode

prop_flatTermId :: forall a. (Serialise a, Eq a, Show a) => T a -> a -> Bool
prop_flatTermId _ a = Right a == (fromFlat . toFlat) a
  where
    toFlat   = toFlatTerm . encode
    fromFlat = fromFlatTerm (decode :: Decoder a)

--------------------------------------------------------------------------------
-- TestTree API

testTree :: TestTree
testTree =
  testGroup "FlatTerm implementation"
    [ testGroup "Valid term construction"
        [ validTerm (T :: T ())
        , validTerm (T :: T Bool)
        , validTerm (T :: T Int)
        , validTerm (T :: T Word)
        , validTerm (T :: T Integer)
        , validTerm (T :: T (Maybe Int))
        , validTerm (T :: T (Either String Int))
        , validTerm (T :: T String)
        , validTerm (T :: T [Int])
        ]

    , testGroup "Full round trip"
        [ roundTrip (T :: T ())
        , roundTrip (T :: T Bool)
        , roundTrip (T :: T Int)
        , roundTrip (T :: T Word)
        , roundTrip (T :: T Integer)
        , roundTrip (T :: T (Maybe Int))
        , roundTrip (T :: T (Either String Int))
        , roundTrip (T :: T String)
        , roundTrip (T :: T [Int])
        ]
    ]

validTerm
    :: forall a. (Arbitrary a, Typeable a, Serialise a, Eq a, Show a)
    => T a -> TestTree
validTerm = basicProp prop_validFlatTerm

roundTrip
    :: forall a. (Arbitrary a, Typeable a, Serialise a, Eq a, Show a)
    => T a -> TestTree
roundTrip = basicProp prop_flatTermId

basicProp
    :: forall a prop.
      ( Arbitrary a
      , Typeable a
      , Serialise a
      , Eq a
      , Show a
      , Testable prop)
    => (T a -> prop) -> T a -> TestTree
basicProp k t = testProperty name (k t)
  where
    name = show (typeOf (undefined :: a))
