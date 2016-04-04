{-# LANGUAGE CPP #-}
module Tests.Orphanage where

#if !MIN_VERSION_base(4,8,0)
import           Control.Applicative
#endif

import           Test.QuickCheck.Arbitrary

--------------------------------------------------------------------------------
-- QuickCheck Orphans


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
