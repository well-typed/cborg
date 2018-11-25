{-# LANGUAGE CPP               #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Tests.Properties (
    testTree
  ) where

import           Codec.CBOR.Term

import           Test.Tasty (TestTree, testGroup, localOption)
import           Test.Tasty.QuickCheck (testProperty, QuickCheckMaxSize(..))
import           Test.QuickCheck

import qualified Tests.Reference.Implementation as RefImpl
import           Tests.Term
                   ( fromRefTerm, toRefTerm, eqTerm
                   , prop_toFromRefTerm, prop_fromToRefTerm
                   , serialise, deserialise
                   , canonicaliseTermNaNs, canonicaliseTermIntegers )
import           Tests.Util

#if !MIN_VERSION_base(4,8,0)
import           Control.Applicative
#endif


--------------------------------------------------------------------------------
-- Properties
--

prop_encodeDecodeTermRoundtrip :: Term -> Bool
prop_encodeDecodeTermRoundtrip term =
             (deserialise . serialise) term
    `eqTerm` (canonicaliseTermNaNs . canonicaliseTermIntegers) term

prop_encodeDecodeTermRoundtrip_splits2 :: Term -> Bool
prop_encodeDecodeTermRoundtrip_splits2 term =
    and [          deserialise thedata'
          `eqTerm` (canonicaliseTermNaNs . canonicaliseTermIntegers) term
        | let thedata = serialise term
        , thedata' <- splits2 thedata ]

prop_encodeDecodeTermRoundtrip_splits3 :: Term -> Bool
prop_encodeDecodeTermRoundtrip_splits3 term =
    and [          deserialise thedata'
          `eqTerm` (canonicaliseTermNaNs . canonicaliseTermIntegers) term
        | let thedata = serialise term
        , thedata' <- splits3 thedata ]

prop_encodeTermMatchesRefImpl :: RefImpl.Term -> Bool
prop_encodeTermMatchesRefImpl term =
    let encoded  = serialise (fromRefTerm term)
        encoded' = RefImpl.serialise (RefImpl.canonicaliseTerm term)
     in encoded == encoded'

prop_encodeTermMatchesRefImpl2 :: Term -> Bool
prop_encodeTermMatchesRefImpl2 term =
    let encoded  = serialise term
        encoded' = RefImpl.serialise (toRefTerm term)
     in encoded == encoded'

prop_decodeTermMatchesRefImpl :: RefImpl.Term -> Bool
prop_decodeTermMatchesRefImpl term0 =
    let encoded = RefImpl.serialise term0
        term    = RefImpl.deserialise encoded
        term'   = deserialise encoded
     in term' `eqTerm` fromRefTerm term

prop_decodeTermNonCanonical :: RefImpl.Term -> Property
prop_decodeTermNonCanonical term0 =
    let encoded = RefImpl.serialise term0
        term    = RefImpl.deserialise encoded
        term'   = deserialise encoded
        encoded'= serialise term'
        isCanonical = encoded == encoded'
     in not isCanonical ==>
        -- This property holds without this pre-condition, as demonstrated by
        -- prop_decodeTermMatchesRefImpl, but using it ensures we get good
        -- coverage of the non-canonical cases
        term' `eqTerm` fromRefTerm term


--------------------------------------------------------------------------------
-- TestTree API

testTree :: TestTree
testTree =
  testGroup "properties"
    [ testProperty "from/to reference terms"        prop_fromToRefTerm
    , testProperty "to/from reference terms"        prop_toFromRefTerm
    , testProperty "rountrip de/encoding terms"     prop_encodeDecodeTermRoundtrip
      -- TODO FIXME: need to fix the generation of terms to give
      -- better size distribution some get far too big for the
      -- splits properties.
    , localOption (QuickCheckMaxSize 30) $
      testProperty "decoding with all 2-chunks"     prop_encodeDecodeTermRoundtrip_splits2
    , localOption (QuickCheckMaxSize 20) $
      testProperty "decoding with all 3-chunks"     prop_encodeDecodeTermRoundtrip_splits3
    , testProperty "encode term matches ref impl 1" prop_encodeTermMatchesRefImpl
    , testProperty "encode term matches ref impl 2" prop_encodeTermMatchesRefImpl2
    , testProperty "decoding term matches ref impl" prop_decodeTermMatchesRefImpl
    , testProperty "non-canonical encoding"         prop_decodeTermNonCanonical
    ]
