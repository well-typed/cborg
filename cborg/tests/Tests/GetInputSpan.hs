{-# LANGUAGE DeriveFunctor, BangPatterns #-}

module Tests.GetInputSpan (testTree) where

import           Data.Word
import           Data.Either (isLeft)
import           Data.String (fromString)
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import           Control.Exception (throw)
import           Control.Applicative

import           Codec.CBOR.Decoding
import           Codec.CBOR.Read (deserialiseFromBytes)
import           Codec.CBOR.Term (Term)
import qualified Codec.CBOR.Term as Term (Term(..))

import           Test.Tasty (TestTree, testGroup, localOption)
import           Test.Tasty.QuickCheck (testProperty, QuickCheckMaxSize(..))
import           Test.QuickCheck hiding (subterms)

import qualified Tests.Reference.Implementation as RefImpl
import           Tests.Term (eqTermProp, canonicaliseTerm, serialiseTerm, deserialiseTerm)
import           Tests.Util
import           Tests.ATerm

import Prelude hiding (encodeFloat, decodeFloat)

testTree :: TestTree
testTree =
  testGroup "getInputSpan"
    [ testGroup "ATerm framework"
        [ testProperty "isomorphic 1" prop_ATerm_isomorphic
        , testProperty "isomorphic 2" prop_ATerm_isomorphic2
        , testProperty "isomorphic 3" prop_ATerm_isomorphic3
        ]
    , testGroup "examples"
        -- basic smoke tests
        [ testProperty "ex01" $ prop_peekByteOffset_deserialise $ Term.TFloat 1.0 -- simplest test
        , testProperty "ex02" $ prop_peekByteOffset_deserialise $ Term.TList [Term.TSimple 0] -- tests non-zero offsets
        , testProperty "ex03" $ prop_peekByteOffset_splits2 $ Term.TFloat 1.0 -- tests multiple chunks
        , testProperty "ex04" $ prop_peekByteOffset_splits2 $ Term.TList [Term.TStringI $ fromString $ replicate 100 'x'] -- tests getTokenVarLen
        , testProperty "ex05" $ prop_peekByteOffset_splits3 $ Term.TString $ fromString "abcdef"
        , testProperty "ex06" $ prop_peekByteOffset_splits2 $ Term.TMapI [(Term.TListI [],Term.TString (fromString "ab")  )]
        , testProperty "ex07" $ prop_peekByteOffset_splits3 $ Term.TMapI [(Term.TString (fromString "xyz"),Term.TStringI (fromString ""))]
        ]
    , testProperty "empty-deserialise" empty_deserialise
    , testProperty "empty-deserialise-fail" empty_deserialise_fail
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

-- | Variation on 'prop_ATerm_isomorphic', checking that serialising as a
-- 'Term', deserialising as an 'ATerm' and converting back gives an equivalent
-- term.
--
prop_ATerm_isomorphic2 :: Term -> Property
prop_ATerm_isomorphic2 t =
           canonicaliseTerm t
  `eqTermProp` (convertATermToTerm . deserialiseATerm . serialiseTerm) t

-- | Variation on 'prop_ATerm_isomorphic2', but where we check the terms are
-- equivalent as 'ATerm's.
--
prop_ATerm_isomorphic3 :: Term -> Property
prop_ATerm_isomorphic3 t =
            (convertTermToATerm . canonicaliseTerm) t
  `eqATermProp` (fmap (const ()) . deserialiseATerm . serialiseTerm) t


--------------------------------------------------------------------------------
-- Properties of getInputSpan
--
-- The comments talk about peekByteOffset, but it's the same for getInputSpan
--

-- | A smoke test that nothing weird happens with an empty span the input.
empty_deserialise :: Property
empty_deserialise = res === LBS.empty
  where
    -- unfortunately we need a non-empty input stream.
    -- the decoder will fail unconditionally, if there are commands,
    -- even ones which consume nothing.
    res = either throw snd (deserialiseFromBytes emptyDecoder (LBS.pack [0]))

empty_deserialise_fail :: Bool
empty_deserialise_fail = isLeft (deserialiseFromBytes emptyDecoder LBS.empty)

emptyDecoder :: Decoder s LBS.ByteString
emptyDecoder = do
    markInput
    getInputSpan

-- | A key consistency property for terms annotated with their bytes:
-- taking those bytes and deserialising them gives the corresponding term
--
prop_ATerm_deserialise :: ATerm ByteSpan -> Property
prop_ATerm_deserialise t@(ATerm _ bs) =
    deserialiseTerm bs `eqTermProp` convertATermToTerm t

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
prop_peekByteOffset_deserialise :: Term -> Property
prop_peekByteOffset_deserialise t =
    conjoin (map prop_ATerm_deserialise (subterms t'))
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
prop_peekByteOffset_splits2 :: Term -> Property
prop_peekByteOffset_splits2 t =
    conjoin [ counterexample (show (LBS.toChunks lbs')) $ deserialiseATerm lbs' `eqATermProp` t'
            | lbs' <- splits2 lbs ]
  where
    lbs = serialiseTerm t
    t'  = deserialiseATerm lbs

-- | The offsets we get when decoding a term should be the same irrespective of
-- block boundaries in the input data stream. This checks the property for all
-- possible 3-chunk splits of the input data.
--
prop_peekByteOffset_splits3 :: Term -> Property
prop_peekByteOffset_splits3 t =
    conjoin [ counterexample (show (LBS.toChunks lbs')) $ deserialiseATerm lbs' `eqATermProp` t'
            | lbs' <- splits3 lbs ]
  where
    lbs = serialiseTerm t
    t'  = deserialiseATerm lbs

--------------------------------------------------------------------------------
-- Decoding a term, annotated with its underlying bytes
--

type ByteSpan = LBS.ByteString

deserialiseATerm :: LBS.ByteString -> ATerm ByteSpan
deserialiseATerm = either throw snd . deserialiseFromBytes decodeATerm

decodeATerm :: Decoder s (ATerm ByteSpan)
decodeATerm = do
    markInput
    t     <- decodeTermFATerm
    lbs   <- getInputSpan
    unmarkInput
    return (ATerm t lbs)

decodeTermFATerm :: Decoder s (TermF (ATerm ByteSpan))
decodeTermFATerm = do
    tkty <- peekTokenType
    case tkty of
      TypeUInt   -> do w <- decodeWord
                       return $! fromWord w
                    where
                      fromWord :: Word -> TermF (ATerm ByteSpan)
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


decodeBytesIndefLen :: [BS.ByteString] -> Decoder s (TermF (ATerm ByteSpan))
decodeBytesIndefLen acc = do
    stop <- decodeBreakOr
    if stop then return $! TBytesI (LBS.fromChunks (reverse acc))
            else do !bs <- decodeBytes
                    decodeBytesIndefLen (bs : acc)


decodeStringIndefLen :: [T.Text] -> Decoder s (TermF (ATerm ByteSpan))
decodeStringIndefLen acc = do
    stop <- decodeBreakOr
    if stop then return $! TStringI (LT.fromChunks (reverse acc))
            else do !str <- decodeString
                    decodeStringIndefLen (str : acc)


decodeListN :: Int -> [ATerm ByteSpan] -> Decoder s (TermF (ATerm ByteSpan))
decodeListN !n acc =
    case n of
      0 -> return $! TList (reverse acc)
      _ -> do !t <- decodeATerm
              decodeListN (n-1) (t : acc)


decodeListIndefLen :: [ATerm ByteSpan] -> Decoder s (TermF (ATerm ByteSpan))
decodeListIndefLen acc = do
    stop <- decodeBreakOr
    if stop then return $! TListI (reverse acc)
            else do !tm <- decodeATerm
                    decodeListIndefLen (tm : acc)


decodeMapN :: Int -> [(ATerm ByteSpan, ATerm ByteSpan)] -> Decoder s (TermF (ATerm ByteSpan))
decodeMapN !n acc =
    case n of
      0 -> return $! TMap (reverse acc)
      _ -> do !tm   <- decodeATerm
              !tm'  <- decodeATerm
              decodeMapN (n-1) ((tm, tm') : acc)


decodeMapIndefLen :: [(ATerm ByteSpan, ATerm ByteSpan)] -> Decoder s (TermF (ATerm ByteSpan))
decodeMapIndefLen acc = do
    stop <- decodeBreakOr
    if stop then return $! TMapI (reverse acc)
            else do !tm  <- decodeATerm
                    !tm' <- decodeATerm
                    decodeMapIndefLen ((tm, tm') : acc)
