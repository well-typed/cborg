{-# LANGUAGE DeriveFunctor, BangPatterns #-}

module Tests.ByteOffset (testTree) where

import           Data.Word
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import           Control.Exception (throw)
import           Control.Applicative

import           Codec.CBOR.Decoding
import           Codec.CBOR.Read (deserialiseFromBytes)
import           Codec.CBOR.Write (toLazyByteString)
import           Codec.CBOR.Term (Term, encodeTerm, decodeTerm)
import qualified Codec.CBOR.Term as Term (Term(..))

import           Test.Tasty (TestTree, testGroup, localOption)
import           Test.Tasty.QuickCheck (testProperty, QuickCheckMaxSize(..))
import           Test.QuickCheck hiding (subterms)

import qualified Tests.Reference.Implementation as RefImpl
import           Tests.CBOR (eqTerm)
import           Tests.Util

import Prelude hiding (encodeFloat, decodeFloat)


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


testTree :: TestTree
testTree =
  testGroup "peekByteOffset"
    [ testGroup "ATerm framework"
        [ testProperty "isomorphic 1" prop_ATerm_isomorphic
        , testProperty "isomorphic 2" prop_ATerm_isomorphic2
        , testProperty "isomorphic 3" prop_ATerm_isomorphic3
        ]
    , testProperty "bytes deserialise" prop_peekByteOffset_deserialise
    , testProperty "bytes reserialise" prop_peekByteOffset_reserialise
    , testProperty "non-canonical encoding" prop_peekByteOffset_noncanonical
    , localOption (QuickCheckMaxSize 30) $
      testProperty "same offsets with all 2-splits" prop_peekByteOffset_splits2
    , localOption (QuickCheckMaxSize 20) $
      testProperty "same offsets with all 3-splits" prop_peekByteOffset_splits3
    ]


--------------------------------------------------------------------------------
-- Properties of the framework
--

-- | Basic property to check that 'ATerm' is isomorphic to the 'Term'.
--
prop_ATerm_isomorphic :: Term -> Bool
prop_ATerm_isomorphic t =
    t `eqTerm` (convertATermToTerm . convertTermToATerm) t
    -- eqTerm is (==) modulo NaNs and overlapping Int vs Integer

-- | Variation on 'prop_ATerm_isomorphic', checking that serialising as a
-- 'Term', deserialising as an 'ATerm' and converting back gives an equivalent
-- term.
--
prop_ATerm_isomorphic2 :: Term -> Bool
prop_ATerm_isomorphic2 t =
    t `eqTerm` (convertATermToTerm . deserialiseATerm . serialiseTerm) t
    -- eqTerm is (==) modulo NaNs and overlapping Int vs Integer

-- | Variation on 'prop_ATerm_isomorphic2', but where we check the terms are
-- equivalent as 'ATerm's.
--
prop_ATerm_isomorphic3 :: Term -> Bool
prop_ATerm_isomorphic3 t =
            convertTermToATerm t
  `eqATerm` (fmap (const ()) . deserialiseATerm . serialiseTerm) t


--------------------------------------------------------------------------------
-- Properties of peekByteOffset
--

-- | A key consistency property for terms annotated with their bytes:
-- taking those bytes and deserialising them gives the corresponding term
--
prop_ATerm_deserialise :: ATerm ByteSpan -> Bool
prop_ATerm_deserialise t@(ATerm _ bs) =
    deserialiseTerm bs `eqTerm` convertATermToTerm t

-- | For the case of canonical encodings it is also true for terms annotated
-- with their bytes: taking the term and serialising it gives the bytes.
--
-- Note this is /only/ expected to hold for canonical encodings. See
-- 'prop_peekByteOffset_noncanonical' for a demonstration of this not holding
-- for non-canonical encodings.
--
prop_ATerm_reserialise :: ATerm ByteSpan -> Bool
prop_ATerm_reserialise t@(ATerm _ bs) =
    serialiseTerm (convertATermToTerm t) == bs

-- | For an 'ATerm' annotated with its bytes (obtained by decoding a term),
-- 'prop_ATerm_deserialise' should be true for the whole term and all subterms.
--
prop_peekByteOffset_deserialise :: Term -> Bool
prop_peekByteOffset_deserialise t =
    all prop_ATerm_deserialise (subterms t')
  where
    t' = deserialiseATerm (serialiseTerm t)

-- | For an 'ATerm' annotated with its bytes (obtained by decoding a canonical
-- term), 'prop_ATerm_serialise' should be true for the whole term and all
-- subterms.
--
prop_peekByteOffset_reserialise :: Term -> Bool
prop_peekByteOffset_reserialise t =
    all prop_ATerm_reserialise (subterms t')
  where
    t' = deserialiseATerm (serialiseTerm t)

-- | For an 'ATerm' annotated with its bytes obtained by decoding a
-- /non-canonical/ term, 'prop_ATerm_serialise' should not always hold.
--
-- This is in some sense the essence of why we want 'peekByteOffset' in the
-- first place: to get the bytes corresponding to a term we have to get the
-- original input bytes since we cannot rely on re-serialising to recover the
-- bytes (at least not without relying on and checking for canonical encodings).
--
prop_peekByteOffset_noncanonical :: RefImpl.Term -> Property
prop_peekByteOffset_noncanonical t =
    not (RefImpl.isCanonicalTerm t) ==>
    not (prop_ATerm_reserialise t')
  where
    t' = deserialiseATerm (RefImpl.serialise t)

-- | The offsets we get when decoding a term should be the same irrespective of
-- block boundaries in the input data stream. This checks the property for all
-- possible 2-chunk splits of the input data.
--
prop_peekByteOffset_splits2 :: Term -> Bool
prop_peekByteOffset_splits2 t =
    and [ deserialiseATermOffsets lbs' `eqATerm` t'
        | lbs' <- splits2 lbs ]
  where
    lbs = serialiseTerm t
    t'  = deserialiseATermOffsets lbs

-- | The offsets we get when decoding a term should be the same irrespective of
-- block boundaries in the input data stream. This checks the property for all
-- possible 3-chunk splits of the input data.
--
prop_peekByteOffset_splits3 :: Term -> Bool
prop_peekByteOffset_splits3 t =
    and [ deserialiseATermOffsets lbs' `eqATerm` t'
        | lbs' <- splits3 lbs ]
  where
    lbs = serialiseTerm t
    t'  = deserialiseATermOffsets lbs


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


------------------------------------------------------------------------------

serialiseTerm :: Term -> LBS.ByteString
serialiseTerm = toLazyByteString . encodeTerm

deserialiseTerm :: LBS.ByteString -> Term
deserialiseTerm = either throw snd . deserialiseFromBytes decodeTerm


--------------------------------------------------------------------------------
-- Decoding a term, annotated with its underlying bytes
--

type Offsets  = (ByteOffset, ByteOffset)
type ByteSpan = LBS.ByteString

deserialiseATermOffsets :: LBS.ByteString -> ATerm Offsets
deserialiseATermOffsets = either throw snd . deserialiseFromBytes decodeATerm

deserialiseATerm :: LBS.ByteString -> ATerm ByteSpan
deserialiseATerm lbs = atermOffsetsToBytes lbs (deserialiseATermOffsets lbs)

atermOffsetsToBytes :: LBS.ByteString -> ATerm Offsets -> ATerm ByteSpan
atermOffsetsToBytes original =
    fmap (`slice` original)
  where
    slice :: (ByteOffset, ByteOffset) -> LBS.ByteString -> LBS.ByteString
    slice (n,m) = LBS.take (m-n) . LBS.drop n


decodeATerm :: Decoder s (ATerm Offsets)
decodeATerm = do
    start <- peekByteOffset
    t     <- decodeTermFATerm
    end   <- peekByteOffset
    return (ATerm t (start, end))

decodeTermFATerm :: Decoder s (TermF (ATerm Offsets))
decodeTermFATerm = do
    tkty <- peekTokenType
    case tkty of
      TypeUInt   -> do w <- decodeWord
                       return $! fromWord w
                    where
                      fromWord :: Word -> TermF (ATerm Offsets)
                      fromWord w
                        | w <= fromIntegral (maxBound :: Int)
                                    = TInt     (fromIntegral w)
                        | otherwise = TInteger (fromIntegral w)

      TypeUInt64 -> do w <- decodeWord64
                       return $! fromWord64 w
                    where
                      fromWord64 w
                        | w <= fromIntegral (maxBound :: Int)
                                    = TInt     (fromIntegral w)
                        | otherwise = TInteger (fromIntegral w)

      TypeNInt   -> do w <- decodeNegWord
                       return $! fromNegWord w
                    where
                      fromNegWord w
                        | w <= fromIntegral (maxBound :: Int)
                                    = TInt     (-1 - fromIntegral w)
                        | otherwise = TInteger (-1 - fromIntegral w)

      TypeNInt64 -> do w <- decodeNegWord64
                       return $! fromNegWord64 w
                    where
                      fromNegWord64 w
                        | w <= fromIntegral (maxBound :: Int)
                                    = TInt     (-1 - fromIntegral w)
                        | otherwise = TInteger (-1 - fromIntegral w)

      TypeInteger -> do !x <- decodeInteger
                        return (TInteger x)
      TypeFloat16 -> do !x <- decodeFloat
                        return (THalf x)
      TypeFloat32 -> do !x <- decodeFloat
                        return (TFloat x)
      TypeFloat64 -> do !x <- decodeDouble
                        return (TDouble x)

      TypeBytes        -> do !x <- decodeBytes
                             return (TBytes x)
      TypeBytesIndef   -> decodeBytesIndef >> decodeBytesIndefLen []
      TypeString       -> do !x <- decodeString
                             return (TString x)
      TypeStringIndef  -> decodeStringIndef >> decodeStringIndefLen []

      TypeListLen      -> decodeListLen      >>= flip decodeListN []
      TypeListLen64    -> decodeListLen      >>= flip decodeListN []
      TypeListLenIndef -> decodeListLenIndef >>  decodeListIndefLen []
      TypeMapLen       -> decodeMapLen       >>= flip decodeMapN []
      TypeMapLen64     -> decodeMapLen       >>= flip decodeMapN []
      TypeMapLenIndef  -> decodeMapLenIndef  >>  decodeMapIndefLen []
      TypeTag          -> do !x <- decodeTag64
                             !y <- decodeATerm
                             return (TTagged x y)
      TypeTag64        -> do !x <- decodeTag64
                             !y <- decodeATerm
                             return (TTagged x y)

      TypeBool    -> do !x <- decodeBool
                        return (TBool x)
      TypeNull    -> TNull   <$  decodeNull
      TypeSimple  -> do !x <- decodeSimple
                        return (TSimple x)
      TypeBreak   -> fail "unexpected break"
      TypeInvalid -> fail "invalid token encoding"


decodeBytesIndefLen :: [BS.ByteString] -> Decoder s (TermF (ATerm Offsets))
decodeBytesIndefLen acc = do
    stop <- decodeBreakOr
    if stop then return $! TBytesI (LBS.fromChunks (reverse acc))
            else do !bs <- decodeBytes
                    decodeBytesIndefLen (bs : acc)


decodeStringIndefLen :: [T.Text] -> Decoder s (TermF (ATerm Offsets))
decodeStringIndefLen acc = do
    stop <- decodeBreakOr
    if stop then return $! TStringI (LT.fromChunks (reverse acc))
            else do !str <- decodeString
                    decodeStringIndefLen (str : acc)


decodeListN :: Int -> [ATerm Offsets] -> Decoder s (TermF (ATerm Offsets))
decodeListN !n acc =
    case n of
      0 -> return $! TList (reverse acc)
      _ -> do !t <- decodeATerm
              decodeListN (n-1) (t : acc)


decodeListIndefLen :: [ATerm Offsets] -> Decoder s (TermF (ATerm Offsets))
decodeListIndefLen acc = do
    stop <- decodeBreakOr
    if stop then return $! TListI (reverse acc)
            else do !tm <- decodeATerm
                    decodeListIndefLen (tm : acc)


decodeMapN :: Int -> [(ATerm Offsets, ATerm Offsets)] -> Decoder s (TermF (ATerm Offsets))
decodeMapN !n acc =
    case n of
      0 -> return $! TMap (reverse acc)
      _ -> do !tm   <- decodeATerm
              !tm'  <- decodeATerm
              decodeMapN (n-1) ((tm, tm') : acc)


decodeMapIndefLen :: [(ATerm Offsets, ATerm Offsets)] -> Decoder s (TermF (ATerm Offsets))
decodeMapIndefLen acc = do
    stop <- decodeBreakOr
    if stop then return $! TMapI (reverse acc)
            else do !tm  <- decodeATerm
                    !tm' <- decodeATerm
                    decodeMapIndefLen ((tm, tm') : acc)


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


-- NaNs are so annoying...
eqATerm :: Eq a => ATerm a -> ATerm a -> Bool
eqATerm (ATerm t1 ann1) (ATerm t2 ann2) =
    ann1 == ann2 && eqATermF t1 t2

eqATermF :: Eq a => TermF (ATerm a) -> TermF (ATerm a) -> Bool
eqATermF (TInt    n)   (TInteger n')   = fromIntegral n == n'
eqATermF (TInteger n)  (TInt     n')   = n == fromIntegral n'
eqATermF (TList   ts)  (TList   ts')   = and (zipWith eqATerm ts ts')
eqATermF (TListI  ts)  (TListI  ts')   = and (zipWith eqATerm ts ts')
eqATermF (TMap    ts)  (TMap    ts')   = and (zipWith eqATermPair ts ts')
eqATermF (TMapI   ts)  (TMapI   ts')   = and (zipWith eqATermPair ts ts')
eqATermF (TTagged w t) (TTagged w' t') = w == w' && eqATerm t t'
eqATermF (THalf   f)   (THalf   f') | isNaN f && isNaN f' = True
eqATermF (TFloat  f)   (TFloat  f') | isNaN f && isNaN f' = True
eqATermF (TDouble f)   (TDouble f') | isNaN f && isNaN f' = True
eqATermF (THalf   f)   (TFloat  f') | isNaN f && isNaN f' = True
eqATermF (THalf   f)   (TDouble f') | isNaN f && isNaN f' = True
eqATermF (TFloat  f)   (THalf   f') | isNaN f && isNaN f' = True
eqATermF (TDouble f)   (THalf   f') | isNaN f && isNaN f' = True
eqATermF a b = a == b

eqATermPair :: (Eq a, Eq b)
              => (ATerm a, ATerm b)
              -> (ATerm a, ATerm b)
              -> Bool
eqATermPair (a,b) (a',b') = eqATerm a a' && eqATerm b b'

