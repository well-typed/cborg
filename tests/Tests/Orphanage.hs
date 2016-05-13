{-# LANGUAGE CPP                #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveDataTypeable #-}
module Tests.Orphanage where

#if MIN_VERSION_base(4,8,0)
import           Data.Functor.Identity
#endif
import           Data.Typeable

import           Control.Applicative

import           Data.Ord
import           Data.Monoid as Monoid
import           Foreign.C.Types
import           System.Exit (ExitCode(..))

import           Test.QuickCheck.Gen
import           Test.QuickCheck.Arbitrary

--------------------------------------------------------------------------------
-- QuickCheck Orphans

-- A _LOT_ of orphans instances for QuickCheck. Some are already in
-- git HEAD and some are still waiting as pull request
--
-- [https://github.com/nick8325/quickcheck/pull/90]

-- Tuples

instance ( Arbitrary a
         , Arbitrary b
         , Arbitrary c
         , Arbitrary d
         , Arbitrary e
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

instance ( Arbitrary a
         , Arbitrary b
         , Arbitrary c
         , Arbitrary d
         , Arbitrary e
         , Arbitrary f
         , Arbitrary g
         )
      => Arbitrary (a,b,c,d,e,f,g)
 where
  arbitrary = return (,,,,,,)
          <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
          <*> arbitrary <*> arbitrary <*> arbitrary

-- Foreign C types

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

-- Miscellaneous types from base

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

#if MIN_VERSION_base(4,8,0)
instance Arbitrary (f a) => Arbitrary (Alt f a) where
  arbitrary = fmap Alt arbitrary
  shrink (Alt a) = map Alt $ shrink a

instance Arbitrary a => Arbitrary (Identity a) where
  arbitrary = fmap Identity arbitrary
  shrink (Identity a) = map Identity $ shrink a
#endif

#if !MIN_VERSION_base(4,8,0)
deriving instance Typeable Const
deriving instance Typeable ZipList
deriving instance Typeable Down
deriving instance Typeable Sum
deriving instance Typeable All
deriving instance Typeable Any
deriving instance Typeable Product
deriving instance Typeable Dual

deriving instance Show a => Show (Const a b)
deriving instance Eq a   => Eq   (Const a b)
#endif

#if MIN_VERSION_base(4,7,0)
instance Arbitrary (Proxy a) where
  arbitrary = return Proxy
#endif
