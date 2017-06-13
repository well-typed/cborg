{-# LANGUAGE DeriveGeneric #-}

module Tests.Deriving (testTree) where

import           GHC.Generics

import qualified Data.ByteString.Lazy       as LBS
import qualified Data.Binary.Serialise.CBOR as CBOR
import           System.FilePath

import           Test.Tasty
import           Test.Tasty.HUnit

-- | A simple case exercising many of the cases implemented by the generic
-- deriving mechinery
data ARecord = ARecord String Int ARecord
             | ANull
             deriving (Generic, Eq, Show)
instance CBOR.Serialise ARecord

aRecord :: ARecord
aRecord = ARecord "hello" 42 (ARecord "world" 52 ANull)

testAgainstFile :: (Eq a, Show a, CBOR.Serialise a) => String -> a -> TestTree
testAgainstFile name x =
    testGroup name
      [ testCase "serialise" $ do
            expected <- LBS.readFile path
            let actual = CBOR.serialise x
            expected @=? actual
      , testCase "deserialise" $ do
            actual <- CBOR.deserialise <$> LBS.readFile path
            x @=? actual
      ]
  where path = "tests" </> "test-vectors" </> "deriving" </> name

testTree :: TestTree
testTree =
    testGroup "Stability of derived instances"
      [ testAgainstFile "aRecord" aRecord
      ]
