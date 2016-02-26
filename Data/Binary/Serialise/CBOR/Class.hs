{-# LANGUAGE CPP          #-}
{-# LANGUAGE BangPatterns #-}

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
  ( -- * The Serialise class
    Serialise(..)
  ) where

#include "cbor.h"
import           Control.Monad
import           Data.Hashable
import           Data.Int
import           Data.Monoid
import           Data.Version
import           Data.Word
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
--import qualified Data.Text.Lazy                      as Text.Lazy

import           Data.Time                           (UTCTime (..))
#if MIN_VERSION_time(1,5,0)
import           Data.Time.Format                    (defaultTimeLocale,
                                                      formatTime, parseTimeM)
#else
import           Data.Time.Format                    (formatTime, parseTime)
import           System.Locale                       (defaultTimeLocale)
#endif

import           Prelude                             hiding (decodeFloat,
                                                      encodeFloat)

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

    -- | Definition of a given @'Decoder'@ for a type.
    decode  :: Decoder a

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
                    <> foldr (\x r -> encode x <> r) encodeBreak xs

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

instance Serialise Int64 where
    encode = encodeInt64
    decode = decodeInt64

instance Serialise Word where
    encode = encodeWord
    decode = decodeWord

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



encodeContainerSkel :: (Word -> Encoding)
                    -> (container -> Int)
                    -> (accumFunc -> Encoding -> container -> Encoding)
                    -> accumFunc
                    -> container
                    -> Encoding
encodeContainerSkel encodeLen size foldl' f  c =
  encodeLen (fromIntegral (size c)) <> (foldl' f (mempty) c)

decodeContainerSkel :: Decoder Int
                    -> ([a] -> container)
                    -> container
                    -> Decoder a
                    -> Decoder container
decodeContainerSkel decodeLen fromList empty decodeItem = do
  n <- decodeLen
  case compare n 0 of
    EQ -> return empty
    LT -> fail "negative size"
    GT -> fmap fromList (replicateM n decodeItem)

instance (Serialise a) => Serialise (Sequence.Seq a) where
  encode = encodeContainerSkel
             encodeListLen
             Sequence.length
             Foldable.foldl'
             (\b a -> b <> encode a)
  decode = decodeContainerSkel
             decodeListLen
             Sequence.fromList
             Sequence.empty
             decode

encodeSetSkel :: Serialise a
              => (s -> Int)
              -> ((Encoding -> a -> Encoding) -> Encoding -> s -> Encoding)
              -> s
              -> Encoding
encodeSetSkel size foldl' =
  encodeContainerSkel encodeListLen size foldl' (\b a -> b <> encode a)

decodeSetSkel :: Serialise a
              => ([a] -> s) -> s -> Decoder s
decodeSetSkel fromList empty =
  decodeContainerSkel decodeListLen fromList empty decode

instance (Ord a, Serialise a) => Serialise (Set.Set a) where
  encode = encodeSetSkel Set.size Set.foldl'
  decode = decodeSetSkel Set.fromList Set.empty

instance Serialise IntSet.IntSet where
  encode = encodeSetSkel IntSet.size IntSet.foldl'
  decode = decodeSetSkel IntSet.fromList IntSet.empty

instance (Serialise a, Hashable a, Eq a) => Serialise (HashSet.HashSet a) where
  encode = encodeSetSkel HashSet.size HashSet.foldl'
  decode = decodeSetSkel HashSet.fromList HashSet.empty


encodeMapSkel :: (Serialise k, Serialise v)
              => (m -> Int)
              -> ((Encoding -> k -> v -> Encoding) -> Encoding -> m -> Encoding)
              -> m
              -> Encoding
encodeMapSkel size foldlWithKey' =
  encodeContainerSkel
    encodeMapLen
    size
    foldlWithKey'
    (\b k v -> b <> encode k <> encode v)

decodeMapSkel :: (Serialise k, Serialise v)
              => ([(k,v)] -> m)
              -> m
              -> Decoder m
decodeMapSkel fromList empty =
  decodeContainerSkel
    decodeMapLen
    fromList
    empty
    (do { !k <- decode; !v <- decode; return (k, v); })

instance (Ord k, Serialise k, Serialise v) => Serialise (Map.Map k v) where
  encode = encodeMapSkel Map.size Map.foldlWithKey'
  decode = decodeMapSkel Map.fromList Map.empty

instance (Serialise a) => Serialise (IntMap.IntMap a) where
  encode = encodeMapSkel IntMap.size IntMap.foldlWithKey'
  decode = decodeMapSkel IntMap.fromList IntMap.empty

instance (Serialise k, Hashable k, Eq k, Serialise v) =>
  Serialise (HashMap.HashMap k v) where
  encode = encodeMapSkel HashMap.size HashMap.foldlWithKey'
  decode = decodeMapSkel HashMap.fromList HashMap.empty

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
