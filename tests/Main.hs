{-# LANGUAGE CPP, ScopedTypeVariables #-}
module Main where

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck
import Data.Typeable
import Data.Binary.Serialise.CBOR (Serialise)
import ReferenceTests (TestCase, specTestVector)
import qualified ReferenceTests as Ref
import ReferenceImpl
import CBORTests
import SafeTests

#if __GLASGOW_HASKELL__ < 710
import Data.Word
#endif

main :: IO ()
main = do
    tcs <- Ref.loadTestCases
    defaultMain (testTree tcs)
  where
    testTree tcs =
        testGroup "CBOR tests"
          [ referenceImplTests tcs
          , cborImplTests tcs
          , safeTests
          ]

referenceImplTests :: [TestCase] -> TestTree
referenceImplTests testCases =
  testGroup "Reference implementation"
    [ testCase "external test vector" $
        mapM_ Ref.externalTestCase testCases

    , testCase "internal test vector" $ do
        sequence_  [ do Ref.expectedDiagnosticNotation d e
                        Ref.encodedRoundtrip d e
                   | (d,e) <- specTestVector ]

    , testGroup "properties"
        [ testProperty "encoding/decoding initial byte"    prop_InitialByte
        , testProperty "encoding/decoding additional info" prop_AdditionalInfo
        , testProperty "encoding/decoding token header"    prop_TokenHeader
        , testProperty "encoding/decoding token header 2"  prop_TokenHeader2
        , testProperty "encoding/decoding tokens"          prop_Token
        , --localOption (QuickCheckTests  1000) $
          localOption (QuickCheckMaxSize 150) $
          testProperty "encoding/decoding terms"           prop_Term
        ]

    , testGroup "internal properties"
        [ testProperty "Integer to/from bytes" prop_integerToFromBytes
        , testProperty "Word16 to/from network byte order" prop_word16ToFromNet
        , testProperty "Word32 to/from network byte order" prop_word32ToFromNet
        , testProperty "Word64 to/from network byte order" prop_word64ToFromNet
        , testProperty "Numeric.Half to/from Float"        prop_halfToFromFloat
        ]
    ]

cborImplTests :: [TestCase] -> TestTree
cborImplTests testCases =
  testGroup "Main implementation"
    [ testCase "external test vector" $
        mapM_ externalTestCase testCases

    , testCase "internal test vector" $ do
        sequence_  [ do expectedDiagnosticNotation d e
                        encodedRoundtrip d e
                   | (d,e) <- specTestVector ]

    , --localOption (QuickCheckTests  5000) $
      localOption (QuickCheckMaxSize 150) $
      testGroup "properties"
        [ testProperty "from/to reference terms"        prop_fromToRefTerm
        , testProperty "to/from reference terms"        prop_toFromRefTerm
        , testProperty "rountrip de/encoding terms"     prop_encodeDecodeTermRoundtrip
          --TODO: need to fix the generation of terms to give better size
          -- distribution some get far too big for the splits properties.
        , localOption (QuickCheckMaxSize 30) $
          testProperty "decoding with all 2-chunks"     prop_encodeDecodeTermRoundtrip_splits2
        , localOption (QuickCheckMaxSize 20) $
          testProperty "decoding with all 3-chunks"     prop_encodeDecodeTermRoundtrip_splits3
        , testProperty "encode term matches ref impl 1" prop_encodeTermMatchesRefImpl
        , testProperty "encode term matches ref impl 2" prop_encodeTermMatchesRefImpl2
        , testProperty "decoding term matches ref impl" prop_decodeTermMatchesRefImpl
        ]
    , testGroup "Serialise class"
        [ cborSerializeRoundTrip (T :: T ())
        , cborSerializeRoundTrip (T :: T Bool)
        , cborSerializeRoundTrip (T :: T Int)
        , cborSerializeRoundTrip (T :: T Word)
        , cborSerializeRoundTrip (T :: T Integer)
        , cborSerializeRoundTrip (T :: T (Maybe Int))
        , cborSerializeRoundTrip (T :: T (Either String Int))
        , cborSerializeRoundTrip (T :: T String)
        , cborSerializeRoundTrip (T :: T [Int])
        ]
    ]

safeTests :: TestTree
safeTests =
  testGroup "Tests for incorrect lazy access"
    [ testProperty "from/to 1-byte chunks"        prop_chunkByte
    , testProperty "from/to long data"            prop_longData
    ]

cborSerializeRoundTrip
    :: forall a. (Arbitrary a, Typeable a, Serialise a, Eq a, Show a)
    => T a -> TestTree
cborSerializeRoundTrip t =
  testProperty
  (show $ typeOf (undefined :: a))
  (prop_serialiseRoundTrip t)
