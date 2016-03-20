{-# LANGUAGE CPP                  #-}
{-# LANGUAGE ConstraintKinds     #-}
{-# LANGUAGE DeriveDataTypeable  #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Tests.Serialise
  ( testTree   -- :: TestTree
  , testFormat -- :: TestTree
  ) where

import           Control.Applicative

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
import           Test.Tasty.HUnit
import           GHC.Generics  (Generic)
import qualified Data.ByteString as BS

import           Data.Binary.Serialise.CBOR
import           Data.Binary.Serialise.CBOR.Encoding
import           Data.Binary.Serialise.CBOR.Decoding
import           Data.Binary.Serialise.CBOR.FlatTerm (toFlatTerm,fromFlatTerm,validFlatTerm)

import qualified Data.HashMap.Strict        as HashMap
import qualified Data.HashSet               as HashSet
import qualified Data.IntMap                as IntMap
import qualified Data.IntSet                as IntSet
import qualified Data.Map                   as Map
import qualified Data.Sequence              as Sequence
import qualified Data.Set                   as Set
import qualified Data.Vector                as Vector
import qualified Data.Vector.Unboxed        as Vector.Unboxed


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

format :: (Typeable a, Show a, Serialise a) => a -> Tokens -> TestTree
format a toks
  = testCase (show (typeOf a) ++ ": " ++ show a)
  $ toks @=? toks'
  where
    Encoding f = encode a
    toks'      = f TkEnd

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
      , mkTest (T :: T Int8)
      , mkTest (T :: T Int16)
      , mkTest (T :: T Int32)
      , mkTest (T :: T Int64)
      , mkTest (T :: T Word)
      , mkTest (T :: T Word8)
      , mkTest (T :: T Word16)
      , mkTest (T :: T Word32)
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
      , mkTest (T :: T (Vector.Unboxed.Vector (Int,Bool)))
      -- generics:
      , mkTest (T :: T Unit)
      , mkTest (T :: T P1)
      , mkTest (T :: T P2)
      , mkTest (T :: T P3)
      , mkTest (T :: T C4)
      , mkTest (T :: T (List Int))
      ]
  ]

testFormat :: TestTree
testFormat = testGroup "Encoding format"
  [ format
      Unit
      (TkListLen 1 $ TkWord 0 $ TkEnd)
  , format
      (P1 12)
      (TkListLen 2 $ TkWord 0 $ TkInt 12 $ TkEnd)
  , format
      (P2 12 0.5)
      (TkListLen 3 $ TkWord 0 $ TkInt 12 $ TkFloat32 0.5 $ TkEnd)
  , format
      (P3 12 0.5 "asdf")
      (TkListLen 4 $ TkWord 0 $ TkInt 12 $ TkFloat32 0.5 $ TkString "asdf" $ TkEnd)
  , format
      (C1 12)
      (TkListLen 2 $ TkWord 0 $ TkInt 12 $ TkEnd)
  , format
      (C2 12 0.5)
      (TkListLen 3 $ TkWord 1 $ TkInt 12 $ TkFloat32 0.5 $ TkEnd)
  , format
      (C3 12 0.5 "asdf")
      (TkListLen 4 $ TkWord 2 $ TkInt 12 $ TkFloat32 0.5 $ TkString "asdf" $ TkEnd)
  , format
       C4
      (TkListLen 1 $ TkWord 3 $ TkEnd)
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


--------------------------------------------------------------------------------
-- Generic data types

data Unit = Unit
          deriving (Show,Eq,Typeable,Generic)
data P1 = P1 Int
          deriving (Show,Eq,Typeable,Generic)
data P2 = P2 Int Float
          deriving (Show,Eq,Typeable,Generic)
data P3 = P3 Int Float String
          deriving (Show,Eq,Typeable,Generic)

data C4 = C1 Int
        | C2 Int Float
        | C3 Int Float String
        | C4
        deriving (Show,Eq,Typeable,Generic)

data List a = Cons a (List a)
            | Nil
            deriving (Show,Eq,Typeable,Generic)

instance Serialise Unit
instance Serialise P1
instance Serialise P2
instance Serialise P3
instance Serialise C4
instance Serialise a => Serialise (List a)


instance Arbitrary Unit where
    arbitrary = pure Unit

instance Arbitrary P1 where
    arbitrary = P1 <$> arbitrary
instance Arbitrary P2 where
    arbitrary = P2 <$> arbitrary <*> arbitrary
instance Arbitrary P3 where
    arbitrary = P3 <$> arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary C4 where
    arbitrary = oneof [ C1 <$> arbitrary
                      , C2 <$> arbitrary <*> arbitrary
                      , C3 <$> arbitrary <*> arbitrary <*> arbitrary
                      , pure C4
                      ]

instance Arbitrary a => Arbitrary (List a) where
    arbitrary = cnv <$> arbitrary
      where
        cnv :: [a] -> List a
        cnv = foldr Cons Nil
