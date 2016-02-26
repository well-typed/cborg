{-# LANGUAGE CPP                 #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Tests.Serialise
  ( testTree -- :: TestTree
  ) where

#if !MIN_VERSION_base(4,8,0)
import           Data.Word
#endif
import           Data.Typeable

import           Test.Tasty
import           Test.QuickCheck
import           Test.Tasty.QuickCheck

import           Data.Binary.Serialise.CBOR

--------------------------------------------------------------------------------
-- Tests and properties

data T a = T

prop_serialiseRoundTrip :: (Serialise a, Eq a, Show a) => T a -> a -> Bool
prop_serialiseRoundTrip _ a = a == (deserialise . serialise) a

--------------------------------------------------------------------------------
-- TestTree API

testTree :: TestTree
testTree =
  testGroup "Serialise class"
    [ testGroup "Full round trip"
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

roundTrip
    :: forall a. (Arbitrary a, Typeable a, Serialise a, Eq a, Show a)
    => T a -> TestTree
roundTrip t = testProperty name prop
  where
    name = show (typeOf (undefined :: a))
    prop = prop_serialiseRoundTrip t
