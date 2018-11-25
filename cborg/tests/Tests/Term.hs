{-# LANGUAGE CPP               #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Tests.Term (
    Term
  , serialise
  , deserialise
  , toRefTerm
  , fromRefTerm
  , eqTerm
  , canonicaliseTermNaNs
  , canonicaliseTermIntegers
  , prop_fromToRefTerm
  , prop_toFromRefTerm
  ) where

import qualified Data.ByteString      as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text            as T
import qualified Data.Text.Lazy       as LT
import           Data.Word
import qualified Numeric.Half as Half

import           Codec.CBOR.Term
import           Codec.CBOR.Read
import           Codec.CBOR.Write

import           Test.QuickCheck

import qualified Tests.Reference.Implementation as Ref
import           Tests.Reference.Generators
                   ( floatToWord, doubleToWord, canonicalNaN )

#if !MIN_VERSION_base(4,8,0)
import           Control.Applicative
#endif
import           Control.Exception (throw)


------------------------------------------------------------------------------

serialise :: Term -> LBS.ByteString
serialise = toLazyByteString . encodeTerm

deserialise :: LBS.ByteString -> Term
deserialise = either throw snd . deserialiseFromBytes decodeTerm

------------------------------------------------------------------------------

toRefTerm :: Term -> Ref.Term
toRefTerm (TInt      n)
            | n >= 0    = Ref.TUInt (Ref.toUInt (fromIntegral n))
            | otherwise = Ref.TNInt (Ref.toUInt (fromIntegral (-1 - n)))
toRefTerm (TInteger  n) -- = Ref.TBigInt n
            | n >= 0 && n <= fromIntegral (maxBound :: Word64)
                        = Ref.TUInt (Ref.toUInt (fromIntegral n))
            | n <  0 && n >= -1 - fromIntegral (maxBound :: Word64)
                        = Ref.TNInt (Ref.toUInt (fromIntegral (-1 - n)))
            | otherwise = Ref.TBigInt n
toRefTerm (TBytes   bs) = Ref.TBytes   (BS.unpack bs)
toRefTerm (TBytesI  bs) = Ref.TBytess  (map BS.unpack (LBS.toChunks bs))
toRefTerm (TString  st) = Ref.TString  (T.unpack st)
toRefTerm (TStringI st) = Ref.TStrings (map T.unpack (LT.toChunks st))
toRefTerm (TList    ts) = Ref.TArray   (map toRefTerm ts)
toRefTerm (TListI   ts) = Ref.TArrayI  (map toRefTerm ts)
toRefTerm (TMap     ts) = Ref.TMap  [ (toRefTerm x, toRefTerm y)
                                        | (x,y) <- ts ]
toRefTerm (TMapI    ts) = Ref.TMapI [ (toRefTerm x, toRefTerm y)
                                        | (x,y) <- ts ]
toRefTerm (TTagged w t) = Ref.TTagged (Ref.toUInt (fromIntegral w))
                                          (toRefTerm t)
toRefTerm (TBool False) = Ref.TFalse
toRefTerm (TBool True)  = Ref.TTrue
toRefTerm  TNull        = Ref.TNull
toRefTerm (TSimple  23) = Ref.TUndef
toRefTerm (TSimple   w) = Ref.TSimple (fromIntegral w)
toRefTerm (THalf     f) = if isNaN f
                          then Ref.TFloat16 canonicalNaN
                          else Ref.TFloat16 (Half.toHalf f)
toRefTerm (TFloat    f) = if isNaN f
                          then Ref.TFloat16 canonicalNaN
                          else Ref.TFloat32 f
toRefTerm (TDouble   f) = if isNaN f
                          then Ref.TFloat16 canonicalNaN
                          else Ref.TFloat64 f


fromRefTerm :: Ref.Term -> Term
fromRefTerm (Ref.TUInt u)
  | n <= fromIntegral (maxBound :: Int) = TInt     (fromIntegral n)
  | otherwise                           = TInteger (fromIntegral n)
  where n = Ref.fromUInt u

fromRefTerm (Ref.TNInt u)
  | n <= fromIntegral (maxBound :: Int) = TInt     (-1 - fromIntegral n)
  | otherwise                           = TInteger (-1 - fromIntegral n)
  where n = Ref.fromUInt u

fromRefTerm (Ref.TBigInt   n) = TInteger n
fromRefTerm (Ref.TBytes   bs) = TBytes (BS.pack bs)
fromRefTerm (Ref.TBytess  bs) = TBytesI  (LBS.fromChunks (map BS.pack bs))
fromRefTerm (Ref.TString  st) = TString  (T.pack st)
fromRefTerm (Ref.TStrings st) = TStringI (LT.fromChunks (map T.pack st))

fromRefTerm (Ref.TArray   ts) = TList  (map fromRefTerm ts)
fromRefTerm (Ref.TArrayI  ts) = TListI (map fromRefTerm ts)
fromRefTerm (Ref.TMap     ts) = TMap  [ (fromRefTerm x, fromRefTerm y)
                                      | (x,y) <- ts ]
fromRefTerm (Ref.TMapI    ts) = TMapI [ (fromRefTerm x, fromRefTerm y)
                                      | (x,y) <- ts ]
fromRefTerm (Ref.TTagged w t) = TTagged (Ref.fromUInt w)
                                        (fromRefTerm t)
fromRefTerm (Ref.TFalse)     = TBool False
fromRefTerm (Ref.TTrue)      = TBool True
fromRefTerm  Ref.TNull       = TNull
fromRefTerm  Ref.TUndef      = TSimple 23
fromRefTerm (Ref.TSimple  w) = TSimple w
fromRefTerm (Ref.TFloat16 f) = THalf (Half.fromHalf f)
fromRefTerm (Ref.TFloat32 f) = TFloat f
fromRefTerm (Ref.TFloat64 f) = TDouble f

-- | Compare terms for equality.
--
-- It does exact bit for bit equality of floats. This means we can compare
-- NaNs, and different NaNs do not compare equal. If you need equality
-- modulo different NaNs then use 'canonicaliseTermNaNs'.
--
-- If you need equality modulo different representations of 'TInt' vs 'TInteger'
-- then use 'canonicaliseTermIntegers'.
--
eqTerm :: Term -> Term -> Bool
eqTerm (TList   ts)  (TList   ts')   = and (zipWith eqTerm ts ts')
eqTerm (TListI  ts)  (TListI  ts')   = and (zipWith eqTerm ts ts')
eqTerm (TMap    ts)  (TMap    ts')   = and (zipWith eqTermPair ts ts')
eqTerm (TMapI   ts)  (TMapI   ts')   = and (zipWith eqTermPair ts ts')
eqTerm (TTagged w t) (TTagged w' t') = w == w' && eqTerm t t'
eqTerm (THalf   f)   (THalf   f')    = floatToWord  f == floatToWord  f'
eqTerm (TFloat  f)   (TFloat  f')    = floatToWord  f == floatToWord  f'
eqTerm (TDouble f)   (TDouble f')    = doubleToWord f == doubleToWord f'
eqTerm a b = a == b

eqTermPair :: (Term, Term) -> (Term, Term) -> Bool
eqTermPair (a,b) (a',b') = eqTerm a a' && eqTerm b b'

-- | Both 'toRefTerm' and the encoding \/ decoding round trip canonicalises
-- NaNs. So tests involving these often need this in combination with
-- comparing for exact equality using 'eqTerm'.
--
canonicaliseTermNaNs :: Term -> Term
canonicaliseTermNaNs (THalf   f) | isNaN f = canonicalTermNaN
canonicaliseTermNaNs (TFloat  f) | isNaN f = canonicalTermNaN
canonicaliseTermNaNs (TDouble f) | isNaN f = canonicalTermNaN
canonicaliseTermNaNs (TList  ts)     = TList  (map canonicaliseTermNaNs ts)
canonicaliseTermNaNs (TListI ts)     = TListI (map canonicaliseTermNaNs ts)
canonicaliseTermNaNs (TMap   ts)     = TMap   (map canonicaliseTermNaNsPair ts)
canonicaliseTermNaNs (TMapI  ts)     = TMapI  (map canonicaliseTermNaNsPair ts)
canonicaliseTermNaNs (TTagged tag t) = TTagged tag (canonicaliseTermNaNs t)
canonicaliseTermNaNs t = t

canonicalTermNaN :: Term
canonicalTermNaN = THalf canonicalNaN

canonicaliseTermNaNsPair :: (Term, Term) -> (Term, Term)
canonicaliseTermNaNsPair (a,b) = (canonicaliseTermNaNs a, canonicaliseTermNaNs b)

canonicaliseTermIntegers :: Term -> Term
canonicaliseTermIntegers (TInteger n)    | n <= fromIntegral (maxBound :: Int)
                                         , n >= fromIntegral (minBound :: Int)
                                         = TInt (fromIntegral n)
canonicaliseTermIntegers (TList  ts)     = TList  (map canonicaliseTermIntegers ts)
canonicaliseTermIntegers (TListI ts)     = TListI (map canonicaliseTermIntegers ts)
canonicaliseTermIntegers (TMap   ts)     = TMap   (map canonicaliseTermIntegersPair ts)
canonicaliseTermIntegers (TMapI  ts)     = TMapI  (map canonicaliseTermIntegersPair ts)
canonicaliseTermIntegers (TTagged tag t) = TTagged tag (canonicaliseTermIntegers t)
canonicaliseTermIntegers t = t

canonicaliseTermIntegersPair :: (Term, Term) -> (Term, Term)
canonicaliseTermIntegersPair (a,b) =
    (canonicaliseTermIntegers a, canonicaliseTermIntegers b)


prop_fromToRefTerm :: Ref.Term -> Bool
prop_fromToRefTerm term = toRefTerm (fromRefTerm term)
         `Ref.eqTerm` Ref.canonicaliseTerm term

prop_toFromRefTerm :: Term -> Bool
prop_toFromRefTerm term = fromRefTerm (toRefTerm term)
                 `eqTerm` (canonicaliseTermNaNs . canonicaliseTermIntegers) term

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

  shrink (TTagged w t) = t : [ TTagged w' t' | (w', t') <- shrink (w, t)
                             , not (Ref.reservedTag (fromIntegral w')) ]

  shrink (TBool _) = []
  shrink TNull  = []

  shrink (TSimple w) = [ TSimple w' | w' <- shrink w
                       , not (Ref.reservedSimple (fromIntegral w)) ]
  shrink (THalf  _f) = []
  shrink (TFloat  f) = [ TFloat  f' | f' <- shrink f ]
  shrink (TDouble f) = [ TDouble f' | f' <- shrink f ]

