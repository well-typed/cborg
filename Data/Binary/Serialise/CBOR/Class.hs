{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE CPP                 #-}
{-# LANGUAGE DefaultSignatures   #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}

-- |
-- Module      : Data.Binary.Serialise.CBOR.Class
-- Copyright   : (c) Duncan Coutts 2015
-- License     : BSD3-style (see LICENSE.txt)
--
-- Maintainer  : duncan@community.haskell.org
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- The @'Serialise'@ class allows you to encode a given type into a
-- CBOR object, or decode a CBOR object into the user-specified type.
--
module Data.Binary.Serialise.CBOR.Class
       where
  -- ( -- * The Serialise class
  --   Serialise(..)

#include "cbor.h"

import           Control.Applicative
import           Control.Monad
import           Data.Hashable
import           Data.Int
import           Data.Monoid
import           Data.Version
import           Data.Word
import           Data.Complex
import           Data.Ratio
import           Data.Ord
import           Data.Functor.Identity
import qualified Data.Foldable                       as Foldable
import qualified Data.ByteString                     as BS
import qualified Data.Text                           as Text

-- TODO: more instances
--import qualified Data.Array                          as Array
--import qualified Data.Array.Unboxed                  as UArray
--import qualified Data.ByteString                     as BS.Lazy
import qualified Data.Map                            as Map
import qualified Data.Sequence                       as Sequence
import qualified Data.Set                            as Set
import qualified Data.IntSet                         as IntSet
import qualified Data.IntMap                         as IntMap
import qualified Data.HashSet                        as HashSet
import qualified Data.HashMap.Strict                 as HashMap
import qualified Data.Vector                         as Vector
import qualified Data.Vector.Unboxed                 as Vector.Unboxed
--import qualified Data.Text.Lazy                      as Text.Lazy
import           Foreign.C.Types

import           Data.Time                           (UTCTime (..))
#if MIN_VERSION_time(1,5,0)
import           Data.Time.Format                    (defaultTimeLocale,
                                                      formatTime, parseTimeM)
#else
import           Data.Time.Format                    (formatTime, parseTime)
import           System.Locale                       (defaultTimeLocale)
#endif
import           System.Exit                         (ExitCode(..))

import           Prelude hiding (decodeFloat, encodeFloat, foldr)
import qualified Prelude
import           GHC.Generics

import           Data.Binary.Serialise.CBOR.Decoding
import           Data.Binary.Serialise.CBOR.Encoding


--------------------------------------------------------------------------------
-- The Serialise class

-- | Types that are instances of the @'Serialise'@ class allow values
-- to be quickly encoded or decoded directly to a CBOR representation,
-- for object transmission or storage.
class Serialise a where
    {-# MINIMAL encode, decode #-}

    -- | Definition for encoding a given type into a binary
    -- representation, using the @'Encoding'@ @'Monoid'@.
    encode  :: a -> Encoding
    default encode :: (Generic a, GSerialize (Rep a)) => a -> Encoding
    encode = gencode . from

    -- | Definition of a given @'Decoder'@ for a type.
    decode  :: Decoder a
    default decode :: (Generic a, GSerialize (Rep a)) => Decoder a
    decode = to <$> gdecode

    -- | Utility to support specialised encoding for some list type -
    -- used for @'Char'@/@'String'@ instances in this package.
    encodeList :: [a] -> Encoding
    encodeList = defaultEncodeList

    -- | Utility to support specialised decoding for some list type -
    -- used for @'Char'@/@'String'@ instances in this package.
    decodeList :: Decoder [a]
    decodeList = defaultDecodeList

--------------------------------------------------------------------------------
-- Special list business

instance Serialise a => Serialise [a] where
    encode = encodeList
    decode = decodeList

defaultEncodeList :: Serialise a => [a] -> Encoding
defaultEncodeList [] = encodeListLen 0
defaultEncodeList xs = encodeListLenIndef
                    <> Prelude.foldr (\x r -> encode x <> r) encodeBreak xs

defaultDecodeList :: Serialise a => Decoder [a]
defaultDecodeList = do
    mn <- decodeListLenOrIndef
    case mn of
      Nothing -> decodeSequenceLenIndef (flip (:)) [] reverse   decode
      Just n  -> decodeSequenceLenN     (flip (:)) [] reverse n decode


--------------------------------------------------------------------------------
-- Primitive instances

instance Serialise () where
    encode = const encodeNull
    decode = decodeNull

instance Serialise Bool where
    encode = encodeBool
    decode = decodeBool

instance Serialise Int where
    encode = encodeInt
    decode = decodeInt

instance Serialise Int8 where
    encode = encodeInt8
    decode = decodeInt8

instance Serialise Int16 where
    encode = encodeInt16
    decode = decodeInt16

instance Serialise Int32 where
    encode = encodeInt32
    decode = decodeInt32

instance Serialise Int64 where
    encode = encodeInt64
    decode = decodeInt64

instance Serialise Word where
    encode = encodeWord
    decode = decodeWord

instance Serialise Word8 where
    encode = encodeWord8
    decode = decodeWord8

instance Serialise Word16 where
    encode = encodeWord16
    decode = decodeWord16

instance Serialise Word32 where
    encode = encodeWord32
    decode = decodeWord32

instance Serialise Word64 where
    encode = encodeWord64
    decode = decodeWord64

instance Serialise Integer where
    encode = encodeInteger
    decode = decodeInteger

instance Serialise Float where
    encode = encodeFloat
    decode = decodeFloat

instance Serialise Double where
    encode = encodeDouble
    decode = decodeDouble

instance Serialise Char where
    encode c = encodeString (Text.singleton c)
    decode = do t <- decodeString
                if Text.length t == 1
                  then return $! Text.head t
                  else fail "expected a single char, found a string"

    -- For [Char]/String we have a special encoding
    encodeList cs = encodeString (Text.pack cs)
    decodeList    = do txt <- decodeString
                       return (Text.unpack txt) -- unpack lazily

instance Serialise Text.Text where
    encode = encodeString
    decode = decodeString

instance Serialise BS.ByteString where
    encode = encodeBytes
    decode = decodeBytes

instance Serialise a => Serialise (Const a b) where
    encode (Const a) = encode a
    decode = Const <$> decode

instance Serialise a => Serialise (ZipList a) where
    encode (ZipList xs) = encode xs
    decode = ZipList <$> decode

instance (Serialise a, Integral a) => Serialise (Ratio a) where
    encode a = encodeListLen 2
            <> encode (numerator a)
            <> encode (denominator a)
    decode = do decodeListLenOf 2
                !a <- decode
                !b <- decode
                return $ a % b

instance Serialise a => Serialise (Complex a) where
    encode (r :+ i) = encodeListLen 2
                   <> encode r
                   <> encode i
    decode = do decodeListLenOf 2
                !r <- decode
                !i <- decode
                return $ r :+ i

instance Serialise Ordering where
    encode a = encodeListLen 1
            <> encodeWord (case a of LT -> 0
                                     EQ -> 1
                                     GT -> 2)
    decode = do
      decodeListLenOf 1
      t <- decodeWord
      case t of
        0 -> return LT
        1 -> return EQ
        2 -> return GT
        _ -> fail "unexpected tag"

instance Serialise a => Serialise (Down a) where
    encode (Down a) = encode a
    decode = Down <$> decode

instance Serialise a => Serialise (Dual a) where
    encode (Dual a) = encode a
    decode = Dual <$> decode

instance Serialise All where
    encode (All b) = encode b
    decode = All <$> decode

instance Serialise Any where
    encode (Any b) = encode b
    decode = Any <$> decode

instance Serialise a => Serialise (Sum a) where
    encode (Sum b) = encode b
    decode = Sum <$> decode

instance Serialise a => Serialise (Product a) where
    encode (Product b) = encode b
    decode = Product <$> decode

instance Serialise a => Serialise (First a) where
    encode (First b) = encode b
    decode = First <$> decode

instance Serialise a => Serialise (Last a) where
    encode (Last b) = encode b
    decode = Last <$> decode

instance Serialise (f a) => Serialise (Alt f a) where
    encode (Alt b) = encode b
    decode = Alt <$> decode

instance Serialise a => Serialise (Identity a) where
    encode (Identity b) = encode b
    decode = Identity <$> decode

instance Serialise ExitCode where
    encode ExitSuccess     = encodeListLen 1
                          <> encodeWord 0
    encode (ExitFailure i) = encodeListLen 2
                          <> encodeWord 1
                          <> encode i
    decode = do
      n <- decodeListLen
      case n of
        1 -> do t <- decodeWord
                case t of
                  0 -> return ExitSuccess
                  _ -> fail "unexpected tag"
        2 -> do t <- decodeWord
                case t of
                  1 -> return ()
                  _ -> fail "unexpected tag"
                !i <- decode
                return $ ExitFailure i
        _ -> fail "Bad list length"

instance Serialise CChar where
    encode (CChar x) = encode x
    decode = CChar <$> decode

instance Serialise CSChar where
    encode (CSChar x) = encode x
    decode = CSChar <$> decode

instance Serialise CUChar where
    encode (CUChar x) = encode x
    decode = CUChar <$> decode

instance Serialise CShort where
    encode (CShort x) = encode x
    decode = CShort <$> decode

instance Serialise CUShort where
    encode (CUShort x) = encode x
    decode = CUShort <$> decode

instance Serialise CInt where
    encode (CInt x) = encode x
    decode = CInt <$> decode

instance Serialise CUInt where
    encode (CUInt x) = encode x
    decode = CUInt <$> decode

instance Serialise CLong where
    encode (CLong x) = encode x
    decode = CLong <$> decode

instance Serialise CULong where
    encode (CULong x) = encode x
    decode = CULong <$> decode

instance Serialise CPtrdiff where
    encode (CPtrdiff x) = encode x
    decode = CPtrdiff <$> decode

instance Serialise CSize where
    encode (CSize x) = encode x
    decode = CSize <$> decode

instance Serialise CWchar where
    encode (CWchar x) = encode x
    decode = CWchar <$> decode

instance Serialise CSigAtomic where
    encode (CSigAtomic x) = encode x
    decode = CSigAtomic <$> decode

instance Serialise CLLong where
    encode (CLLong x) = encode x
    decode = CLLong <$> decode

instance Serialise CULLong where
    encode (CULLong x) = encode x
    decode = CULLong <$> decode

instance Serialise CIntPtr where
    encode (CIntPtr x) = encode x
    decode = CIntPtr <$> decode

instance Serialise CUIntPtr where
    encode (CUIntPtr x) = encode x
    decode = CUIntPtr <$> decode

instance Serialise CIntMax where
    encode (CIntMax x) = encode x
    decode = CIntMax <$> decode

instance Serialise CUIntMax where
    encode (CUIntMax x) = encode x
    decode = CUIntMax <$> decode

instance Serialise CClock where
    encode (CClock x) = encode x
    decode = CClock <$> decode

instance Serialise CTime where
    encode (CTime x) = encode x
    decode = CTime <$> decode

instance Serialise CUSeconds where
    encode (CUSeconds x) = encode x
    decode = CUSeconds <$> decode

instance Serialise CSUSeconds where
    encode (CSUSeconds x) = encode x
    decode = CSUSeconds <$> decode

instance Serialise CFloat where
    encode (CFloat x) = encode x
    decode = CFloat <$> decode

instance Serialise CDouble where
    encode (CDouble x) = encode x
    decode = CDouble <$> decode

--------------------------------------------------------------------------------
-- Structure instances

instance (Serialise a, Serialise b) => Serialise (a,b) where
    encode (a,b) = encodeListLen 2
                <> encode a
                <> encode b
    decode = do decodeListLenOf 2
                !x <- decode
                !y <- decode
                return (x, y)

instance (Serialise a, Serialise b, Serialise c) => Serialise (a,b,c) where
    encode (a,b,c) = encodeListLen 3
                  <> encode a
                  <> encode b
                  <> encode c

    decode = do decodeListLenOf 3
                !x <- decode
                !y <- decode
                !z <- decode
                return (x, y, z)

instance (Serialise a, Serialise b, Serialise c, Serialise d
         ) => Serialise (a,b,c,d) where
    encode (a,b,c,d) = encodeListLen 4
                    <> encode a
                    <> encode b
                    <> encode c
                    <> encode d

    decode = do decodeListLenOf 4
                !a <- decode
                !b <- decode
                !c <- decode
                !d <- decode
                return (a, b, c, d)

instance (Serialise a, Serialise b, Serialise c, Serialise d, Serialise e
         ) => Serialise (a,b,c,d,e) where
    encode (a,b,c,d,e) = encodeListLen 5
                      <> encode a
                      <> encode b
                      <> encode c
                      <> encode d
                      <> encode e

    decode = do decodeListLenOf 5
                !a <- decode
                !b <- decode
                !c <- decode
                !d <- decode
                !e <- decode
                return (a, b, c, d, e)

instance ( Serialise a, Serialise b, Serialise c, Serialise d, Serialise e
         , Serialise f
         ) => Serialise (a,b,c,d,e,f) where
    encode (a,b,c,d,e,f) = encodeListLen 6
                        <> encode a
                        <> encode b
                        <> encode c
                        <> encode d
                        <> encode e
                        <> encode f

    decode = do decodeListLenOf 6
                !a <- decode
                !b <- decode
                !c <- decode
                !d <- decode
                !e <- decode
                !f <- decode
                return (a, b, c, d, e, f)

instance ( Serialise a, Serialise b, Serialise c, Serialise d, Serialise e
         , Serialise f, Serialise g
         ) => Serialise (a,b,c,d,e,f,g) where
    encode (a,b,c,d,e,f,g) = encodeListLen 7
                          <> encode a
                          <> encode b
                          <> encode c
                          <> encode d
                          <> encode e
                          <> encode f
                          <> encode g

    decode = do decodeListLenOf 7
                !a <- decode
                !b <- decode
                !c <- decode
                !d <- decode
                !e <- decode
                !f <- decode
                !g <- decode
                return (a, b, c, d, e, f, g)

instance Serialise a => Serialise (Maybe a) where
    encode Nothing  = encodeListLen 0
    encode (Just x) = encodeListLen 1 <> encode x

    decode = do n <- decodeListLen
                case n of
                  0 -> return Nothing
                  1 -> do !x <- decode
                          return (Just x)
                  _ -> fail "unknown tag"

instance (Serialise a, Serialise b) => Serialise (Either a b) where
    encode (Left  x) = encodeListLen 2 <> encodeWord 0 <> encode x
    encode (Right x) = encodeListLen 2 <> encodeWord 1 <> encode x

    decode = do decodeListLenOf 2
                t <- decodeWord
                case t of
                  0 -> do !x <- decode
                          return (Left x)
                  1 -> do !x <- decode
                          return (Right x)
                  _ -> fail "unknown tag"


--------------------------------------------------------------------------------
-- Container instances

encodeContainerSkel :: (Word -> Encoding)
                    -> (container -> Int)
                    -> (accumFunc -> Encoding -> container -> Encoding)
                    -> accumFunc
                    -> container
                    -> Encoding
encodeContainerSkel encodeLen size foldr f  c =
    encodeLen (fromIntegral (size c)) <> foldr f mempty c
{-# INLINE encodeContainerSkel #-}

decodeContainerSkel :: Decoder Int
                    -> (Int -> [a] -> container)
                    -> Decoder a
                    -> Decoder container
decodeContainerSkel decodeLen fromList decodeItem = do
    n <- decodeLen
    fmap (fromList n) (replicateM n decodeItem)
{-# INLINE decodeContainerSkel #-}

instance (Serialise a) => Serialise (Sequence.Seq a) where
  encode = encodeContainerSkel
             encodeListLen
             Sequence.length
             Foldable.foldr
             (\a b -> encode a <> b)
  decode = decodeContainerSkel
             decodeListLen
             (\_len -> Sequence.fromList)
             decode

instance (Serialise a) => Serialise (Vector.Vector a) where
  encode = encodeContainerSkel
             encodeListLen
             Vector.length
             Vector.foldr
             (\a b -> encode a <> b)
  {-# INLINE encode #-}
  decode = decodeContainerSkel
             decodeListLen
             Vector.fromListN
             decode
  {-# INLINE decode #-}

--TODO: we really ought to be able to do better than going via lists,
-- especially for unboxed vectors
instance (Serialise a, Vector.Unboxed.Unbox a) =>
         Serialise (Vector.Unboxed.Vector a) where
  encode = encodeContainerSkel
             encodeListLen
             Vector.Unboxed.length
             Vector.Unboxed.foldr
             (\a b -> encode a <> b)
  {-# INLINE encode #-}
  decode = decodeContainerSkel
             decodeListLen
             Vector.Unboxed.fromListN
             decode
  {-# INLINE decode #-}


encodeSetSkel :: Serialise a
              => (s -> Int)
              -> ((a -> Encoding -> Encoding) -> Encoding -> s -> Encoding)
              -> s
              -> Encoding
encodeSetSkel size foldr =
    encodeContainerSkel encodeListLen size foldr (\a b -> encode a <> b)
{-# INLINE encodeSetSkel #-}

decodeSetSkel :: Serialise a
              => ([a] -> s) -> Decoder s
decodeSetSkel fromList =
  decodeContainerSkel decodeListLen (\_len -> fromList) decode
{-# INLINE decodeSetSkel #-}

instance (Ord a, Serialise a) => Serialise (Set.Set a) where
  encode = encodeSetSkel Set.size Set.foldr
  decode = decodeSetSkel Set.fromList

instance Serialise IntSet.IntSet where
  encode = encodeSetSkel IntSet.size IntSet.foldr
  decode = decodeSetSkel IntSet.fromList

instance (Serialise a, Hashable a, Eq a) => Serialise (HashSet.HashSet a) where
  encode = encodeSetSkel HashSet.size HashSet.foldr
  decode = decodeSetSkel HashSet.fromList

encodeMapSkel :: (Serialise k, Serialise v)
              => (m -> Int)
              -> ((k -> v -> Encoding -> Encoding) -> Encoding -> m -> Encoding)
              -> m
              -> Encoding
encodeMapSkel size foldrWithKey =
  encodeContainerSkel
    encodeMapLen
    size
    foldrWithKey
    (\k v b -> encode k <> encode v <> b)
{-# INLINE encodeMapSkel #-}

decodeMapSkel :: (Serialise k, Serialise v)
              => ([(k,v)] -> m)
              -> Decoder m
decodeMapSkel fromList =
  decodeContainerSkel
    decodeMapLen
    (\_len -> fromList)
    (do { !k <- decode; !v <- decode; return (k, v); })
{-# INLINE decodeMapSkel #-}

instance (Ord k, Serialise k, Serialise v) => Serialise (Map.Map k v) where
  encode = encodeMapSkel Map.size Map.foldrWithKey
  decode = decodeMapSkel Map.fromList

instance (Serialise a) => Serialise (IntMap.IntMap a) where
  encode = encodeMapSkel IntMap.size IntMap.foldrWithKey
  decode = decodeMapSkel IntMap.fromList

instance (Serialise k, Hashable k, Eq k, Serialise v) =>
  Serialise (HashMap.HashMap k v) where
  encode = encodeMapSkel HashMap.size HashMap.foldrWithKey
  decode = decodeMapSkel HashMap.fromList


--------------------------------------------------------------------------------
-- Misc base package instances

instance Serialise Version where
    encode (Version ns ts) = encodeListLen 3
                          <> encodeWord 0 <> encode ns <> encode ts
    decode = do
      len <- decodeListLen
      tag <- decodeWord
      case tag of
        0 | len == 3
          -> do !x <- decode
                !y <- decode
                return (Version x y)
        _ -> fail "unexpected tag"

--------------------------------------------------------------------------------
-- Time instances
--
-- CBOR has some special encodings for times/timestamps

instance Serialise UTCTime where
    encode d = encodeTag 0
            <> encode (formatUTCrfc3339 d)

    decode = do
      tag <- decodeTag
      case tag of
        0 -> do str <- decodeString
                case parseUTCrfc3339 (Text.unpack str) of
                  Just t  -> return $! forceUTCTime t
                  Nothing -> fail "Could not parse RFC3339 date"
        _ -> fail "Expected timestamp (tag 0 or 1)"


formatUTCrfc3339 :: UTCTime -> String
parseUTCrfc3339  :: String -> Maybe UTCTime

-- Format UTC as timezone 'Z' but on parsing accept 'Z' or any numeric offset
formatUTCrfc3339 = formatTime       defaultTimeLocale "%Y-%m-%dT%H:%M:%S%QZ"
#if MIN_VERSION_time(1,5,0)
parseUTCrfc3339  = parseTimeM False defaultTimeLocale "%Y-%m-%dT%H:%M:%S%Q%Z"
#else
parseUTCrfc3339  = parseTime        defaultTimeLocale "%Y-%m-%dT%H:%M:%S%Q%Z"
#endif

-- UTCTime has an unnecessarily lazy representation, and the parsing is lazy
forceUTCTime :: UTCTime -> UTCTime
forceUTCTime t@(UTCTime !_day !_daytime) = t


--------------------------------------------------------------------------------
-- Generic instances

-- | Serialise type class for generic representation of
class GSerialize f where
    gencode  :: f a -> Encoding
    gdecode  :: Decoder (f a)

data Proxy (f :: * -> *) = P

-- Data types without constructors are still serialized as null value
instance GSerialize V1 where
    gencode _ = encodeNull
    gdecode   = error "V1 don't have contructors" <$ decodeNull

-- Constructors without fields are serialized as null value
instance GSerialize U1 where
    gencode _ = encodeListLen 1 <> encodeWord 0
    gdecode   = do
      n <- decodeListLen
      when (n /= 1) $ fail "expect list of length 1"
      tag <- decodeWord
      when (tag /= 0) $ fail "unexpected tag. Expect 0"
      return U1

-- Metadata (constructor name, etc) is skipped
instance GSerialize a => GSerialize (M1 i c a) where
    gencode = gencode . unM1
    gdecode = M1 <$> gdecode

-- Constructor field (Could only appear in one-field & one-constructor
-- data types). In all other cases we go through GSerialize{Sum,Prod}
instance Serialise a => GSerialize (K1 i a) where
    gencode (K1 a) = encodeListLen 2
                  <> encodeWord 0
                  <> encode a
    gdecode = do
      n <- decodeListLen
      when (n /= 2) $
        fail "expect list of length 2"
      tag <- decodeWord
      when (tag /= 0) $
        fail "unexpected tag. Expects 0"
      K1 <$> decode

-- Products are serialized as N-tuples with 0 constructor tag
instance (GSerializeProd f, GSerializeProd g) => GSerialize (f :*: g) where
    gencode (f :*: g)
        = encodeListLen (nFields (P :: Proxy (f :*: g)) + 1)
       <> encodeWord 0
       <> encodeSeq f
       <> encodeSeq g
    gdecode = do
      let nF = nFields (P :: Proxy (f :*: g))
      n <- decodeListLen
      -- FIXME: signedness of list length
      when (fromIntegral n /= nF + 1) $
        fail $ "Wrong number of fields: expected="++show nF++" got="++show n
      tag <- decodeWord
      when (tag /= 0) $
        fail $ "unexpect tag (expect 0)"
      !f <- gdecodeSeq
      !g <- gdecodeSeq
      return $ f :*: g

-- Sum types are serialized as N-tuples and first element is
-- constructor tag
instance (GSerializeSum f, GSerializeSum g) => GSerialize (f :+: g) where
    gencode a = encodeListLen (numOfFields a + 1)
             <> encode (conNumber a)
             <> encodeSum a

    gdecode = do
        n <- decodeListLen
        -- FIXME: Again signedness
        when (n == 0) $
          fail "Empty list encountered for sum type"
        nCon  <- decodeWord
        trueN <- fieldsForCon (P :: Proxy (f :+: g)) nCon
        when (n-1 /= fromIntegral trueN ) $
          fail $ "Number of fields mismatch: expected="++show trueN++" got="++show n
        decodeSum nCon


-- | Serialization of product types
class GSerializeProd f where
    -- | Number of fields in product type
    nFields   :: Proxy f -> Word
    -- | Encode fields sequentially without writing header
    encodeSeq :: f a -> Encoding
    -- | Decode fields sequentially without reading header
    gdecodeSeq :: Decoder (f a)

instance (GSerializeProd f, GSerializeProd g) => GSerializeProd (f :*: g) where
    nFields _ = nFields (P :: Proxy f) + nFields (P :: Proxy g)
    encodeSeq (f :*: g) = encodeSeq f <> encodeSeq g
    gdecodeSeq = do !f <- gdecodeSeq
                    !g <- gdecodeSeq
                    return (f :*: g)

-- N.B. Could only be reached when one of constructors in sum type
--      don't have parameters
instance GSerializeProd U1 where
    nFields   _ = 0
    encodeSeq _ = mempty
    gdecodeSeq  = return U1

-- Ordinary field
instance (Serialise a) => GSerializeProd (K1 i a) where
    nFields    _     = 1
    encodeSeq (K1 f) = encode f
    gdecodeSeq       = K1 <$> decode

-- We skip metadata
instance (i ~ S, GSerializeProd f) => GSerializeProd (M1 i c f) where
    nFields     _     = 1
    encodeSeq  (M1 f) = encodeSeq f
    gdecodeSeq        = M1 <$> gdecodeSeq


-- | Serialization of sum types
class GSerializeSum f where
    -- | Number of constructor of given value
    conNumber   :: f a -> Word
    -- | Number of fields of given value
    numOfFields :: f a -> Word
    -- | Encode field
    encodeSum   :: f a  -> Encoding

    -- | Decode field
    decodeSum     :: Word -> Decoder (f a)
    -- | Number of constructors
    nConstructors :: Proxy f -> Word
    -- | Number of fields for given constructor number
    fieldsForCon  :: Proxy f -> Word -> Decoder Word


instance (GSerializeSum f, GSerializeSum g) => GSerializeSum (f :+: g) where
    conNumber x = case x of
      L1 f -> conNumber f
      R1 g -> conNumber g + nConstructors (P :: Proxy f)
    numOfFields x = case x of
      L1 f -> numOfFields f
      R1 g -> numOfFields g
    encodeSum x = case x of
      L1 f -> encodeSum f
      R1 g -> encodeSum g

    nConstructors _ = nConstructors (P :: Proxy f)
                    + nConstructors (P :: Proxy g)

    fieldsForCon _ n | n < nL    = fieldsForCon (P :: Proxy f) n
                     | otherwise = fieldsForCon (P :: Proxy g) (n - nL)
      where
        nL = nConstructors (P :: Proxy f)

    decodeSum nCon | nCon < nL = L1 <$> decodeSum nCon
                   | otherwise = R1 <$> decodeSum (nCon - nL)
      where
        nL = nConstructors (P :: Proxy f)


instance (i ~ C, GSerializeProd f) => GSerializeSum (M1 i c f) where
    conNumber    _     = 0
    numOfFields  _     = nFields (P :: Proxy f)
    encodeSum   (M1 f) = encodeSeq f

    nConstructors  _ = 1
    fieldsForCon _ 0 = return $ nFields (P :: Proxy f)
    fieldsForCon _ _ = fail "Bad constructor number"
    decodeSum      0 = M1 <$> gdecodeSeq
    decodeSum      _ = fail "bad constructor number"
