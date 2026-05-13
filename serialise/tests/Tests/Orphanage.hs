{-# LANGUAGE CPP                #-}
{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications   #-}
module Tests.Orphanage where

import           Data.Proxy
import qualified Type.Reflection as Refl

import           GHC.Fingerprint.Type
#if !MIN_VERSION_QuickCheck(2,17,0)
import           Data.Ord (Down (..))
#endif

import           Test.QuickCheck.Arbitrary

--------------------------------------------------------------------------------
-- QuickCheck Orphans

-- A _LOT_ of orphans instances for QuickCheck. Some are already in
-- git HEAD and some are still waiting as pull request
--
-- [https://github.com/nick8325/quickcheck/pull/90]

#if !MIN_VERSION_QuickCheck(2,17,0)
instance Arbitrary a => Arbitrary (Down a) where
  arbitrary = fmap Down arbitrary
  shrink = map Down . shrink . (\(Down a) -> a)
#endif

instance Arbitrary Fingerprint where
  arbitrary = Fingerprint <$> arbitrary <*> arbitrary

data Kind a = Type a

instance Arbitrary Refl.SomeTypeRep where
  arbitrary = return (Refl.someTypeRep $ Proxy @([Either (Maybe Int) (Proxy ('Type String))]))
