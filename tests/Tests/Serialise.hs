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

import           Data.Complex
import           Data.Int
import           Data.Monoid as Monoid
import           Data.Ord
import           Data.Ratio
import           Data.Time
import           Data.Word
import           GHC.Float (float2Double)
import           Data.Version

import           Data.Typeable
#if !MIN_VERSION_base(4,8,0)
import           Control.Applicative
#endif
import           Foreign.C.Types

import           Test.QuickCheck
import           Test.Tasty
import           Test.Tasty.QuickCheck
import           Test.QuickCheck.Instances ()
import           Test.Tasty.HUnit
import           GHC.Generics  (Generic)
import qualified Data.ByteString as BS
import           System.Exit (ExitCode(..))

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
      , mkTest (T :: T CChar)
      , mkTest (T :: T CSChar)
      , mkTest (T :: T CUChar)
      , mkTest (T :: T CShort)
      , mkTest (T :: T CUShort)
      , mkTest (T :: T CInt)
      , mkTest (T :: T CUInt)
      , mkTest (T :: T CLong)
      , mkTest (T :: T CULong)
      , mkTest (T :: T CPtrdiff)
      , mkTest (T :: T CSize)
      , mkTest (T :: T CWchar)
      , mkTest (T :: T CSigAtomic)
      , mkTest (T :: T CLLong)
      , mkTest (T :: T CULLong)
      , mkTest (T :: T CIntPtr)
      , mkTest (T :: T CUIntPtr)
      , mkTest (T :: T CIntMax)
      , mkTest (T :: T CUIntMax)
      , mkTest (T :: T CClock)
      , mkTest (T :: T CTime)
      , mkTest (T :: T CUSeconds)
      , mkTest (T :: T CSUSeconds)
      , mkTest (T :: T CFloat)
      , mkTest (T :: T CDouble)
      , mkTest (T :: T (Int, Char))
      , mkTest (T :: T (Int, Char, Bool))
      , mkTest (T :: T (Int, Char, Bool, String))
      , mkTest (T :: T (Int, Char, Bool, String, ()))
      , mkTest (T :: T (Int, Char, Bool, String, (), Maybe Char))
      , mkTest (T :: T (Int, Char, Bool, String, (), Maybe Char, Maybe ()))
      , mkTest (T :: T (Maybe Int))
      , mkTest (T :: T (Either String Int))
      , mkTest (T :: T String)
      , mkTest (T :: T BS.ByteString)
      , mkTest (T :: T [Int])
      , mkTest (T :: T UTCTime)
      , mkTest (T :: T Version)
      , mkTest (T :: T ExitCode)
      , mkTest (T :: T (Ratio Integer))
      , mkTest (T :: T (Complex Double))
      , mkTest (T :: T (Const Int ()))
      , mkTest (T :: T (ZipList Int))
      , mkTest (T :: T (ZipList Char))
      , mkTest (T :: T Ordering)
      , mkTest (T :: T (Down Int64))
      , mkTest (T :: T (Dual (Maybe (Sum Int))))
      , mkTest (T :: T All)
      , mkTest (T :: T Any)
      , mkTest (T :: T (Sum Int))
      , mkTest (T :: T (Product Int))
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


--------------------------------------------------------------------------------
-- QC Orphans
--
-- A _LOT_ of orphans instances for QuickCheck. Some are already in
-- git HEAD and some are still waiting as pull request
--
-- [https://github.com/nick8325/quickcheck/pull/90]

instance ( Arbitrary a, Arbitrary b, Arbitrary c, Arbitrary d, Arbitrary e
         , Arbitrary f
         )
      => Arbitrary (a,b,c,d,e,f)
 where
  arbitrary = return (,,,,,)
          <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
          <*> arbitrary <*> arbitrary

  shrink (u, v, w, x, y, z) =
    [ (u', v', w', x', y', z')
    | (u', (v', (w', (x', (y', z'))))) <- shrink (u, (v, (w, (x, (y, z))))) ]

instance ( Arbitrary a, Arbitrary b, Arbitrary c, Arbitrary d, Arbitrary e
         , Arbitrary f, Arbitrary g
         )
      => Arbitrary (a,b,c,d,e,f,g)
 where
  arbitrary = return (,,,,,,)
          <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
          <*> arbitrary <*> arbitrary <*> arbitrary
instance Arbitrary CChar where
  arbitrary = CChar <$> arbitrary
  shrink (CChar x) = CChar <$> shrink x

instance Arbitrary CSChar where
  arbitrary = CSChar <$> arbitrary
  shrink (CSChar x) = CSChar <$> shrink x

instance Arbitrary CUChar where
  arbitrary = CUChar <$> arbitrary
  shrink (CUChar x) = CUChar <$> shrink x

instance Arbitrary CShort where
  arbitrary = CShort <$> arbitrary
  shrink (CShort x) = CShort <$> shrink x

instance Arbitrary CUShort where
  arbitrary = CUShort <$> arbitrary
  shrink (CUShort x) = CUShort <$> shrink x

instance Arbitrary CInt where
  arbitrary = CInt <$> arbitrary
  shrink (CInt x) = CInt <$> shrink x

instance Arbitrary CUInt where
  arbitrary = CUInt <$> arbitrary
  shrink (CUInt x) = CUInt <$> shrink x

instance Arbitrary CLong where
  arbitrary = CLong <$> arbitrary
  shrink (CLong x) = CLong <$> shrink x

instance Arbitrary CULong where
  arbitrary = CULong <$> arbitrary
  shrink (CULong x) = CULong <$> shrink x

instance Arbitrary CPtrdiff where
  arbitrary = CPtrdiff <$> arbitrary
  shrink (CPtrdiff x) = CPtrdiff <$> shrink x

instance Arbitrary CSize where
  arbitrary = CSize <$> arbitrary
  shrink (CSize x) = CSize <$> shrink x

instance Arbitrary CWchar where
  arbitrary = CWchar <$> arbitrary
  shrink (CWchar x) = CWchar <$> shrink x

instance Arbitrary CSigAtomic where
  arbitrary = CSigAtomic <$> arbitrary
  shrink (CSigAtomic x) = CSigAtomic <$> shrink x

instance Arbitrary CLLong where
  arbitrary = CLLong <$> arbitrary
  shrink (CLLong x) = CLLong <$> shrink x

instance Arbitrary CULLong where
  arbitrary = CULLong <$> arbitrary
  shrink (CULLong x) = CULLong <$> shrink x

instance Arbitrary CIntPtr where
  arbitrary = CIntPtr <$> arbitrary
  shrink (CIntPtr x) = CIntPtr <$> shrink x

instance Arbitrary CUIntPtr where
  arbitrary = CUIntPtr <$> arbitrary
  shrink (CUIntPtr x) = CUIntPtr <$> shrink x

instance Arbitrary CIntMax where
  arbitrary = CIntMax <$> arbitrary
  shrink (CIntMax x) = CIntMax <$> shrink x

instance Arbitrary CUIntMax where
  arbitrary = CUIntMax <$> arbitrary
  shrink (CUIntMax x) = CUIntMax <$> shrink x

instance Arbitrary CClock where
  arbitrary = CClock <$> arbitrary
  shrink (CClock x) = CClock <$> shrink x

instance Arbitrary CTime where
  arbitrary = CTime <$> arbitrary
  shrink (CTime x) = CTime <$> shrink x

instance Arbitrary CUSeconds where
  arbitrary = CUSeconds <$> arbitrary
  shrink (CUSeconds x) = CUSeconds <$> shrink x

instance Arbitrary CSUSeconds where
  arbitrary = CSUSeconds <$> arbitrary
  shrink (CSUSeconds x) = CSUSeconds <$> shrink x

instance Arbitrary CFloat where
  arbitrary = CFloat <$> arbitrary
  shrink (CFloat x) = CFloat <$> shrink x

instance Arbitrary CDouble where
  arbitrary = CDouble <$> arbitrary
  shrink (CDouble x) = CDouble <$> shrink x

instance Arbitrary a => Arbitrary (Monoid.Dual a) where
  arbitrary = fmap Monoid.Dual arbitrary
  shrink = map Monoid.Dual . shrink . Monoid.getDual

instance (Arbitrary a, CoArbitrary a) => Arbitrary (Monoid.Endo a) where
  arbitrary = fmap Monoid.Endo arbitrary
  shrink = map Monoid.Endo . shrink . Monoid.appEndo

instance Arbitrary Monoid.All where
  arbitrary = fmap Monoid.All arbitrary
  shrink = map Monoid.All . shrink . Monoid.getAll

instance Arbitrary Monoid.Any where
  arbitrary = fmap Monoid.Any arbitrary
  shrink = map Monoid.Any . shrink . Monoid.getAny

instance Arbitrary a => Arbitrary (Monoid.Sum a) where
  arbitrary = fmap Monoid.Sum arbitrary
  shrink = map Monoid.Sum . shrink . Monoid.getSum

instance Arbitrary a => Arbitrary (Monoid.Product a) where
  arbitrary = fmap Monoid.Product  arbitrary
  shrink = map Monoid.Product  . shrink . Monoid.getProduct

instance Arbitrary a => Arbitrary (Monoid.First a) where
  arbitrary = fmap Monoid.First arbitrary
  shrink = map Monoid.First . shrink . Monoid.getFirst

instance Arbitrary a => Arbitrary (Monoid.Last a) where
  arbitrary = fmap Monoid.Last arbitrary
  shrink = map Monoid.Last . shrink . Monoid.getLast

instance Arbitrary a => Arbitrary (Down a) where
  arbitrary = fmap Down arbitrary
  shrink = map Down . shrink . (\(Down a) -> a)

instance Arbitrary a => Arbitrary (ZipList a) where
  arbitrary = fmap ZipList arbitrary
  shrink = map ZipList . shrink . getZipList

instance Arbitrary a => Arbitrary (Const a b) where
  arbitrary = fmap Const arbitrary
  shrink = map Const . shrink . getConst

instance Arbitrary ExitCode where
  arbitrary = frequency [(1, return ExitSuccess), (3, fmap ExitFailure arbitrary)]

  shrink (ExitFailure x) = ExitSuccess : [ ExitFailure x' | x' <- shrink x ]
  shrink _        = []
