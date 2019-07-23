module Tests.PreEncoded (
    testTree
  ) where

import           Data.Monoid (Monoid(mconcat))

import           Codec.CBOR.Term     (Term, encodeTerm)
import           Codec.CBOR.FlatTerm (FlatTerm, toFlatTerm, TermToken(..))
import           Codec.CBOR.Write    (toStrictByteString, toLazyByteString)
import           Codec.CBOR.Encoding (Encoding, encodePreEncoded)

import           Test.Tasty (TestTree, testGroup)
import           Test.Tasty.QuickCheck (testProperty)

import           Tests.Term () -- instance Arbitrary Term
import           Tests.Reference.Generators
                   (canonicalNaN, halfToWord, floatToWord, doubleToWord)


-- | Use 'encodePreEncoded' but with a serialised term as the bytes.
--
encodePreEncoded' :: Term -> Encoding
encodePreEncoded' = encodePreEncoded . toStrictByteString . encodeTerm


prop_preEncodedTerm_sameBytes :: Term -> Bool
prop_preEncodedTerm_sameBytes t =
    sameBytes
      (encodeTerm t)
      (encodePreEncoded' t)


prop_preEncodedTerm_sameTokens :: Term -> Bool
prop_preEncodedTerm_sameTokens t =
    sameTokens
      (encodeTerm t)
      (encodePreEncoded' t)


prop_preEncodedTerms_sameBytes :: [(Term, Bool)] -> Bool
prop_preEncodedTerms_sameBytes ts  =
    sameBytes
      (mconcat [ encodeTerm t | (t, _) <- ts ])
      (mconcat [ if pre then encodePreEncoded' t
                        else encodeTerm t
               | (t, pre) <- ts ])

prop_preEncodedTerms_sameTokens :: [(Term, Bool)] -> Bool
prop_preEncodedTerms_sameTokens ts  =
    sameTokens
      (mconcat [ encodeTerm t | (t, _) <- ts ])
      (mconcat [ if pre then encodePreEncoded' t
                        else encodeTerm t
               | (t, pre) <- ts ])


sameBytes :: Encoding -> Encoding -> Bool
sameBytes e1 e2 = toLazyByteString e1 == toLazyByteString e2

sameTokens :: Encoding -> Encoding -> Bool
sameTokens e1 e2 = canonicaliseFlatTerm (toFlatTerm e1)
      `eqFlatTerm` canonicaliseFlatTerm (toFlatTerm e2)

canonicaliseFlatTerm :: FlatTerm -> FlatTerm
canonicaliseFlatTerm = map canonicaliseTermToken

canonicaliseTermToken :: TermToken -> TermToken
canonicaliseTermToken (TkFloat16 f) | isNaN f = TkFloat16 canonicalNaN
canonicaliseTermToken (TkFloat32 f) | isNaN f = TkFloat16 canonicalNaN
canonicaliseTermToken (TkFloat64 f) | isNaN f = TkFloat16 canonicalNaN
canonicaliseTermToken x = x

eqFlatTerm :: FlatTerm -> FlatTerm -> Bool
eqFlatTerm x y = and (zipWith eqTermToken x y)

-- NaNs strike again!
eqTermToken :: TermToken -> TermToken -> Bool
eqTermToken (TkFloat16 x) (TkFloat16 y) = halfToWord   x == halfToWord   y
eqTermToken (TkFloat32 x) (TkFloat32 y) = floatToWord  x == floatToWord  y
eqTermToken (TkFloat64 x) (TkFloat64 y) = doubleToWord x == doubleToWord y
eqTermToken x y = x == y


--------------------------------------------------------------------------------
-- TestTree API

testTree :: TestTree
testTree =
  testGroup "pre-encoded"
  [ testProperty "single term, same bytes"   prop_preEncodedTerm_sameBytes
  , testProperty "single term, same tokens"  prop_preEncodedTerm_sameTokens
  , testProperty "list terms, same bytes"    prop_preEncodedTerms_sameBytes
  , testProperty "list terms, same tokens"   prop_preEncodedTerms_sameTokens
  ]

