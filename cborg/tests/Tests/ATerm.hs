{-# LANGUAGE DeriveFunctor, BangPatterns #-}

module Tests.ATerm (
    ATerm (..),
    TermF (..),
    subterms,
    prop_ATerm_isomorphic,
    eqATerm,
    eqATermProp,
    convertTermToATerm,
    convertATermToTerm,
) where

import           Data.Word
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS

import           Codec.CBOR.Term (Term)
import qualified Codec.CBOR.Term as Term (Term(..))

import           Test.QuickCheck hiding (subterms)

import           Tests.Term (eqTermProp)
import           Tests.Reference.Generators (floatToWord, doubleToWord)

-- | Like a 'Term', but with an annotation on top level terms and every
-- subterm. This is used for tests of 'peekByteOffset' where we annotate a
-- decoded term with the byte range it covers.
--
data ATerm annotation = ATerm (TermF (ATerm annotation)) annotation
  deriving (Show, Eq, Functor)

-- | Term one-level functor.
--
data TermF t = TInt     Int
             | TInteger Integer
             | TBytes   BS.ByteString
             | TBytesI  LBS.ByteString
             | TString  T.Text
             | TStringI LT.Text
             | TList    [t]
             | TListI   [t]
             | TMap     [(t, t)]
             | TMapI    [(t, t)]
             | TTagged  Word64 t
             | TBool    Bool
             | TNull
             | TSimple  Word8
             | THalf    Float
             | TFloat   Float
             | TDouble  Double
  deriving (Show, Eq, Functor)

------------------------------------------------------------------------------

subterms :: ATerm a -> [ATerm a]
subterms at@(ATerm t0 _) = at : subtermsF t0
  where
    subtermsF :: TermF (ATerm a) -> [ATerm a]
    subtermsF (TList  ts)  = concatMap subterms ts
    subtermsF (TListI ts)  = concatMap subterms ts
    subtermsF (TMap   ts)  = [ t' | (x, y) <- ts
                                 , t' <- subterms x
                                      ++ subterms y ]
    subtermsF (TMapI  ts)  = [ t' | (x, y) <- ts
                                 , t' <- subterms x
                                      ++ subterms y ]
    subtermsF (TTagged _ t') = subterms t'

    subtermsF TInt    {} = []
    subtermsF TInteger{} = []
    subtermsF TBytes  {} = []
    subtermsF TBytesI {} = []
    subtermsF TString {} = []
    subtermsF TStringI{} = []
    subtermsF TBool   {} = []
    subtermsF TNull   {} = []
    subtermsF TSimple {} = []
    subtermsF THalf   {} = []
    subtermsF TFloat  {} = []
    subtermsF TDouble {} = []

--------------------------------------------------------------------------------
-- Properties of the framework
--

-- | Basic property to check that 'ATerm' is isomorphic to the 'Term'.
--
prop_ATerm_isomorphic :: Term -> Property
prop_ATerm_isomorphic t =
    t `eqTermProp` (convertATermToTerm . convertTermToATerm) t

--------------------------------------------------------------------------------
-- Converting between terms and annotated terms


convertTermToATerm :: Term -> ATerm ()
convertTermToATerm t = ATerm (convertTermToTermF t) ()

convertTermToTermF :: Term -> TermF (ATerm ())
convertTermToTermF (Term.TList   ts)  = TList  (map convertTermToATerm ts)
convertTermToTermF (Term.TListI  ts)  = TListI (map convertTermToATerm ts)
convertTermToTermF (Term.TMap    ts)  = TMap  [ ( convertTermToATerm x
                                                , convertTermToATerm y )
                                              | (x, y) <- ts ]
convertTermToTermF (Term.TMapI   ts)  = TMapI [ ( convertTermToATerm x
                                                , convertTermToATerm y )
                                              | (x, y) <- ts ]
convertTermToTermF (Term.TTagged x t) = TTagged x (convertTermToATerm t)

convertTermToTermF (Term.TInt     x)  = TInt   x
convertTermToTermF (Term.TInteger x)  = TInteger x
convertTermToTermF (Term.TBytes   x)  = TBytes  x
convertTermToTermF (Term.TBytesI  x)  = TBytesI x
convertTermToTermF (Term.TString  x)  = TString  x
convertTermToTermF (Term.TStringI x)  = TStringI x
convertTermToTermF (Term.TBool    x)  = TBool x
convertTermToTermF  Term.TNull        = TNull
convertTermToTermF (Term.TSimple  x)  = TSimple x
convertTermToTermF (Term.THalf    x)  = THalf x
convertTermToTermF (Term.TFloat   x)  = TFloat x
convertTermToTermF (Term.TDouble  x)  = TDouble x

convertATermToTerm :: ATerm a -> Term
convertATermToTerm (ATerm t _ann) = convertTermFToTerm t

convertTermFToTerm :: TermF (ATerm a) -> Term
convertTermFToTerm (TList   ts)  = Term.TList  (map convertATermToTerm ts)
convertTermFToTerm (TListI  ts)  = Term.TListI (map convertATermToTerm ts)
convertTermFToTerm (TMap    ts)  = Term.TMap  [ ( convertATermToTerm x
                                                , convertATermToTerm y )
                                              | (x, y) <- ts ]
convertTermFToTerm (TMapI   ts)  = Term.TMapI [ ( convertATermToTerm x
                                                , convertATermToTerm y )
                                              | (x, y) <- ts ]
convertTermFToTerm (TTagged x t) = Term.TTagged  x (convertATermToTerm t)
convertTermFToTerm (TInt     x)  = Term.TInt     x
convertTermFToTerm (TInteger x)  = Term.TInteger x
convertTermFToTerm (TBytes   x)  = Term.TBytes   x
convertTermFToTerm (TBytesI  x)  = Term.TBytesI  x
convertTermFToTerm (TString  x)  = Term.TString  x
convertTermFToTerm (TStringI x)  = Term.TStringI x
convertTermFToTerm (TBool    x)  = Term.TBool    x
convertTermFToTerm  TNull        = Term.TNull
convertTermFToTerm (TSimple  x)  = Term.TSimple  x
convertTermFToTerm (THalf    x)  = Term.THalf    x
convertTermFToTerm (TFloat   x)  = Term.TFloat   x
convertTermFToTerm (TDouble  x)  = Term.TDouble  x

eqATermProp :: (Eq a, Show a) => ATerm a -> ATerm a -> Property
eqATermProp x y =
    counterexample (show x ++ " =?= " ++ show y) $
    eqATerm x y

-- NaNs are so annoying...
eqATerm :: Eq a => ATerm a -> ATerm a -> Bool
eqATerm (ATerm t1 ann1) (ATerm t2 ann2) =
    ann1 == ann2 && eqATermF t1 t2

eqATermF :: Eq a => TermF (ATerm a) -> TermF (ATerm a) -> Bool
eqATermF (TList   ts)  (TList   ts')   = and (zipWith eqATerm ts ts')
eqATermF (TListI  ts)  (TListI  ts')   = and (zipWith eqATerm ts ts')
eqATermF (TMap    ts)  (TMap    ts')   = and (zipWith eqATermPair ts ts')
eqATermF (TMapI   ts)  (TMapI   ts')   = and (zipWith eqATermPair ts ts')
eqATermF (TTagged w t) (TTagged w' t') = w == w' && eqATerm t t'
eqATermF (THalf   f)   (THalf   f')    = floatToWord  f == floatToWord  f'
eqATermF (TFloat  f)   (TFloat  f')    = floatToWord  f == floatToWord  f'
eqATermF (TDouble f)   (TDouble f')    = doubleToWord f == doubleToWord f'
eqATermF a b = a == b

eqATermPair :: (Eq a, Eq b)
              => (ATerm a, ATerm b)
              -> (ATerm a, ATerm b)
              -> Bool
eqATermPair (a,b) (a',b') = eqATerm a a' && eqATerm b b'
