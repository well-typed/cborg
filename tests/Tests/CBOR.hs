{-# LANGUAGE CPP               #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Tests.CBOR
  ( testTree -- :: TestTree
  ) where

import qualified Data.ByteString      as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text            as T
import qualified Data.Text.Lazy       as LT
import           Data.Word
import qualified Numeric.Half as Half

import           Serialise.Cborg.Term
import           Serialise.Cborg.Read
import           Serialise.Cborg.Write

import           Test.Tasty
import           Test.Tasty.HUnit
import           Test.Tasty.QuickCheck

import qualified Tests.Reference.Implementation  as RefImpl
import qualified Tests.Reference as TestVector
import           Tests.Reference (TestCase(..))

#if !MIN_VERSION_base(4,8,0)
import           Control.Applicative
#endif
import           Control.Exception (throw)


externalTestCase :: TestCase -> Assertion
externalTestCase TestCase { encoded, decoded = Left expectedJson } = do
  let term       = deserialise encoded
      actualJson = TestVector.termToJson (toRefTerm term)
      reencoded  = serialise term

  expectedJson `TestVector.equalJson` actualJson
  encoded @=? reencoded

externalTestCase TestCase { encoded, decoded = Right expectedDiagnostic } = do
  let term             = deserialise encoded
      actualDiagnostic = RefImpl.diagnosticNotation (toRefTerm term)
      reencoded        = serialise term

  expectedDiagnostic @=? actualDiagnostic
  encoded @=? reencoded

expectedDiagnosticNotation :: String -> [Word8] -> Assertion
expectedDiagnosticNotation expectedDiagnostic encoded = do
  let term             = deserialise (LBS.pack encoded)
      actualDiagnostic = RefImpl.diagnosticNotation (toRefTerm term)

  expectedDiagnostic @=? actualDiagnostic


-- | The reference implementation satisfies the roundtrip property for most
-- examples (all the ones from Appendix A). It does not satisfy the roundtrip
-- property in general however, non-canonical over-long int encodings for
-- example.
--
--
encodedRoundtrip :: String -> [Word8] -> Assertion
encodedRoundtrip expectedDiagnostic encoded = do
  let term       = deserialise (LBS.pack encoded)
      reencoded  = LBS.unpack (serialise term)

  assertEqual ("for CBOR: " ++ expectedDiagnostic) encoded reencoded

prop_encodeDecodeTermRoundtrip :: Term -> Bool
prop_encodeDecodeTermRoundtrip term =
    (deserialise . serialise) term `eqTerm` term

prop_encodeDecodeTermRoundtrip_splits2 :: Term -> Bool
prop_encodeDecodeTermRoundtrip_splits2 term =
    and [ deserialise thedata' `eqTerm` term
        | let thedata = serialise term
        , thedata' <- splits2 thedata ]

prop_encodeDecodeTermRoundtrip_splits3 :: Term -> Bool
prop_encodeDecodeTermRoundtrip_splits3 term =
    and [ deserialise thedata' `eqTerm` term
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
    let encoded = RefImpl.serialise (RefImpl.canonicaliseTerm term0)
        term    = RefImpl.deserialise encoded
        term'   = deserialise encoded
     in term' `eqTerm` fromRefTerm term

------------------------------------------------------------------------------

splits2 :: LBS.ByteString -> [LBS.ByteString]
splits2 bs = zipWith (\a b -> LBS.fromChunks [a,b]) (BS.inits sbs) (BS.tails sbs)
  where
    sbs = LBS.toStrict bs

splits3 :: LBS.ByteString -> [LBS.ByteString]
splits3 bs =
    [ LBS.fromChunks [a,b,c]
    | (a,x) <- zip (BS.inits sbs) (BS.tails sbs)
    , (b,c) <- zip (BS.inits x)   (BS.tails x) ]
  where
    sbs = LBS.toStrict bs


serialise :: Term -> LBS.ByteString
serialise = toLazyByteString . encodeTerm

deserialise :: LBS.ByteString -> Term
deserialise = either throw id . deserialiseFromBytes decodeTerm

------------------------------------------------------------------------------

toRefTerm :: Term -> RefImpl.Term
toRefTerm (TInt      n)
            | n >= 0    = RefImpl.TUInt (RefImpl.toUInt (fromIntegral n))
            | otherwise = RefImpl.TNInt (RefImpl.toUInt (fromIntegral (-1 - n)))
toRefTerm (TInteger  n) -- = RefImpl.TBigInt n
            | n >= 0 && n <= fromIntegral (maxBound :: Word64)
                        = RefImpl.TUInt (RefImpl.toUInt (fromIntegral n))
            | n <  0 && n >= -1 - fromIntegral (maxBound :: Word64)
                        = RefImpl.TNInt (RefImpl.toUInt (fromIntegral (-1 - n)))
            | otherwise = RefImpl.TBigInt n
toRefTerm (TBytes   bs) = RefImpl.TBytes   (BS.unpack bs)
toRefTerm (TBytesI  bs) = RefImpl.TBytess  (map BS.unpack (LBS.toChunks bs))
toRefTerm (TString  st) = RefImpl.TString  (T.unpack st)
toRefTerm (TStringI st) = RefImpl.TStrings (map T.unpack (LT.toChunks st))
toRefTerm (TList    ts) = RefImpl.TArray   (map toRefTerm ts)
toRefTerm (TListI   ts) = RefImpl.TArrayI  (map toRefTerm ts)
toRefTerm (TMap     ts) = RefImpl.TMap  [ (toRefTerm x, toRefTerm y)
                                        | (x,y) <- ts ]
toRefTerm (TMapI    ts) = RefImpl.TMapI [ (toRefTerm x, toRefTerm y)
                                        | (x,y) <- ts ]
toRefTerm (TTagged w t) = RefImpl.TTagged (RefImpl.toUInt (fromIntegral w))
                                          (toRefTerm t)
toRefTerm (TBool False) = RefImpl.TFalse
toRefTerm (TBool True)  = RefImpl.TTrue
toRefTerm  TNull        = RefImpl.TNull
toRefTerm (TSimple  23) = RefImpl.TUndef
toRefTerm (TSimple   w) = RefImpl.TSimple (fromIntegral w)
toRefTerm (THalf     f) = RefImpl.TFloat16 (Half.toHalf f)
toRefTerm (TFloat    f) = RefImpl.TFloat32 f
toRefTerm (TDouble   f) = RefImpl.TFloat64 f


fromRefTerm :: RefImpl.Term -> Term
fromRefTerm (RefImpl.TUInt u)
  | n <= fromIntegral (maxBound :: Int) = TInt     (fromIntegral n)
  | otherwise                           = TInteger (fromIntegral n)
  where n = RefImpl.fromUInt u

fromRefTerm (RefImpl.TNInt u)
  | n <= fromIntegral (maxBound :: Int) = TInt     (-1 - fromIntegral n)
  | otherwise                           = TInteger (-1 - fromIntegral n)
  where n = RefImpl.fromUInt u

fromRefTerm (RefImpl.TBigInt   n) = TInteger n
fromRefTerm (RefImpl.TBytes   bs) = TBytes (BS.pack bs)
fromRefTerm (RefImpl.TBytess  bs) = TBytesI  (LBS.fromChunks (map BS.pack bs))
fromRefTerm (RefImpl.TString  st) = TString  (T.pack st)
fromRefTerm (RefImpl.TStrings st) = TStringI (LT.fromChunks (map T.pack st))

fromRefTerm (RefImpl.TArray   ts) = TList  (map fromRefTerm ts)
fromRefTerm (RefImpl.TArrayI  ts) = TListI (map fromRefTerm ts)
fromRefTerm (RefImpl.TMap     ts) = TMap  [ (fromRefTerm x, fromRefTerm y)
                                          | (x,y) <- ts ]
fromRefTerm (RefImpl.TMapI    ts) = TMapI [ (fromRefTerm x, fromRefTerm y)
                                          | (x,y) <- ts ]
fromRefTerm (RefImpl.TTagged w t) = TTagged (RefImpl.fromUInt w)
                                            (fromRefTerm t)
fromRefTerm (RefImpl.TFalse)     = TBool False
fromRefTerm (RefImpl.TTrue)      = TBool True
fromRefTerm  RefImpl.TNull       = TNull
fromRefTerm  RefImpl.TUndef      = TSimple 23
fromRefTerm (RefImpl.TSimple  w) = TSimple w
fromRefTerm (RefImpl.TFloat16 f) = THalf (Half.fromHalf f)
fromRefTerm (RefImpl.TFloat32 f) = TFloat f
fromRefTerm (RefImpl.TFloat64 f) = TDouble f

-- NaNs are so annoying...
eqTerm :: Term -> Term -> Bool
eqTerm (TInt    n)   (TInteger n')   = fromIntegral n == n'
eqTerm (TList   ts)  (TList   ts')   = and (zipWith eqTerm ts ts')
eqTerm (TListI  ts)  (TListI  ts')   = and (zipWith eqTerm ts ts')
eqTerm (TMap    ts)  (TMap    ts')   = and (zipWith eqTermPair ts ts')
eqTerm (TMapI   ts)  (TMapI   ts')   = and (zipWith eqTermPair ts ts')
eqTerm (TTagged w t) (TTagged w' t') = w == w' && eqTerm t t'
eqTerm (THalf   f)   (THalf   f') | isNaN f && isNaN f' = True
eqTerm (TFloat  f)   (TFloat  f') | isNaN f && isNaN f' = True
eqTerm (TDouble f)   (TDouble f') | isNaN f && isNaN f' = True
eqTerm a b = a == b

eqTermPair :: (Term, Term) -> (Term, Term) -> Bool
eqTermPair (a,b) (a',b') = eqTerm a a' && eqTerm b b'


prop_fromToRefTerm :: RefImpl.Term -> Bool
prop_fromToRefTerm term = toRefTerm (fromRefTerm term)
         `RefImpl.eqTerm` RefImpl.canonicaliseTerm term

prop_toFromRefTerm :: Term -> Bool
prop_toFromRefTerm term = fromRefTerm (toRefTerm term) `eqTerm` term

instance Arbitrary Term where
  arbitrary = fromRefTerm <$> arbitrary

  shrink (TInt     n)   = [ TInt     n'   | n' <- shrink n ]
  shrink (TInteger n)   = [ TInteger n'   | n' <- shrink n ]

  shrink (TBytes  ws)   = [ TBytes (BS.pack ws') | ws' <- shrink (BS.unpack ws) ]
  shrink (TBytesI wss)  = [ TBytesI (LBS.fromChunks (map BS.pack wss'))
                          | wss' <- shrink (map BS.unpack (LBS.toChunks wss)) ]
  shrink (TString  cs)  = [ TString (T.pack cs') | cs' <- shrink (T.unpack cs) ]
  shrink (TStringI css) = [ TStringI (LT.fromChunks (map T.pack css'))
                          | css' <- shrink (map T.unpack (LT.toChunks css)) ]

  shrink (TList  xs@[x]) = x : [ TList  xs' | xs' <- shrink xs ]
  shrink (TList  xs)     =     [ TList  xs' | xs' <- shrink xs ]
  shrink (TListI xs@[x]) = x : [ TListI xs' | xs' <- shrink xs ]
  shrink (TListI xs)     =     [ TListI xs' | xs' <- shrink xs ]

  shrink (TMap  xys@[(x,y)]) = x : y : [ TMap  xys' | xys' <- shrink xys ]
  shrink (TMap  xys)         =         [ TMap  xys' | xys' <- shrink xys ]
  shrink (TMapI xys@[(x,y)]) = x : y : [ TMapI xys' | xys' <- shrink xys ]
  shrink (TMapI xys)         =         [ TMapI xys' | xys' <- shrink xys ]

  shrink (TTagged w t) = [ TTagged w' t' | (w', t') <- shrink (w, t)
                         , not (RefImpl.reservedTag (fromIntegral w')) ]

  shrink (TBool _) = []
  shrink TNull  = []

  shrink (TSimple w) = [ TSimple w' | w' <- shrink w
                       , not (RefImpl.reservedSimple (fromIntegral w)) ]
  shrink (THalf  _f) = []
  shrink (TFloat  f) = [ TFloat  f' | f' <- shrink f ]
  shrink (TDouble f) = [ TDouble f' | f' <- shrink f ]

--------------------------------------------------------------------------------
-- TestTree API

testTree :: [TestCase] -> TestTree
testTree testCases =
  testGroup "Main implementation"
    [ testCase "external test vector" $
        mapM_ externalTestCase testCases

    , testCase "internal test vector" $ do
        sequence_  [ do expectedDiagnosticNotation d e
                        encodedRoundtrip d e
                   | (d,e) <- TestVector.specTestVector ]

    , --localOption (QuickCheckTests  5000) $
      localOption (QuickCheckMaxSize 150) $
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
        ]
    ]
