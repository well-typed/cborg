{-# LANGUAGE CPP #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RankNTypes #-}

-- |
-- Module      : Data.Binary.Serialise.CBOR.Decoding
-- Copyright   : (c) Duncan Coutts 2015
-- License     : BSD3-style (see LICENSE.txt)
--
-- Maintainer  : duncan@community.haskell.org
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Lorem ipsum...
--
module Data.Binary.Serialise.CBOR.Decoding (

  -- * Decode primitive operations
  Decoder,
  DecodeAction(..),
  getDecodeAction,

  -- ** Read input tokens
  decodeWord,
  decodeWord64,
  decodeNegWord,
  decodeNegWord64,
  decodeInt,
  decodeInt64,
  decodeInteger,
  decodeFloat,
  decodeDouble,
  decodeBytes,
  decodeBytesIndef,
  decodeString,
  decodeStringIndef,
  decodeListLen,
  decodeListLenIndef,
  decodeMapLen,
  decodeMapLenIndef,
  decodeTag,
  decodeTag64,
  decodeBool,
  decodeNull,
  decodeSimple,

  -- ** Specialised Read input token operations
  decodeWordOf,
  decodeListLenOf,

  -- ** Branching operations
--  decodeBytesOrIndef,
--  decodeStringOrIndef,
  decodeListLenOrIndef,
  decodeMapLenOrIndef,
  decodeBreakOr,

  -- ** Inspecting the token type
  peekTokenType,
  TokenType(..),

  -- ** Special operations
--  ignoreTerms,
--  decodeTrace,

  -- * Sequence operations
  decodeSequenceLenIndef,
  decodeSequenceLenN,
  ) where


import           GHC.Exts
import           GHC.Word
import           GHC.Int
import           Data.Text (Text)
import           Data.ByteString (ByteString)
import           Control.Applicative

import           Prelude hiding (decodeFloat)

#include "MachDeps.h"

#if WORD_SIZE_IN_BITS == 64
#define ARCH_64bit
#elif WORD_SIZE_IN_BITS == 32
import           GHC.IntWord64 (wordToWord64#)
#else
#error expected WORD_SIZE_IN_BITS to be 32 or 64
#endif



data Decoder a = Decoder {
       runDecoder :: forall r. (a -> DecodeAction r) -> DecodeAction r
     }

data DecodeAction a
    = ConsumeWord    (Word# -> DecodeAction a)
    | ConsumeNegWord (Word# -> DecodeAction a)
    | ConsumeInt     (Int#  -> DecodeAction a)
    | ConsumeListLen (Int#  -> DecodeAction a)
    | ConsumeMapLen  (Int#  -> DecodeAction a)
    | ConsumeTag     (Word# -> DecodeAction a)

-- 64bit variants for 32bit machines
#ifndef ARCH_64bit
    | ConsumeWord64    (Word64# -> DecodeAction a)
    | ConsumeNegWord64 (Word64# -> DecodeAction a)
    | ConsumeInt64     (Int64#  -> DecodeAction a)
    | ConsumeListLen64 (Int64#  -> DecodeAction a)
    | ConsumeMapLen64  (Int64#  -> DecodeAction a)
    | ConsumeTag64     (Word64# -> DecodeAction a)
#endif

    | ConsumeInteger (Integer    -> DecodeAction a)
    | ConsumeFloat   (Float#     -> DecodeAction a)
    | ConsumeDouble  (Double#    -> DecodeAction a)
    | ConsumeBytes   (ByteString -> DecodeAction a)
    | ConsumeString  (Text       -> DecodeAction a)
    | ConsumeBool    (Bool       -> DecodeAction a)
    | ConsumeSimple  (Word#      -> DecodeAction a)

    | ConsumeBytesIndef   (DecodeAction a)
    | ConsumeStringIndef  (DecodeAction a)
    | ConsumeListLenIndef (DecodeAction a)
    | ConsumeMapLenIndef  (DecodeAction a)
    | ConsumeNull         (DecodeAction a)

    | ConsumeListLenOrIndef (Int# -> DecodeAction a)
    | ConsumeMapLenOrIndef  (Int# -> DecodeAction a)
    | ConsumeBreakOr        (Bool -> DecodeAction a)

    | PeekTokenType  (TokenType -> DecodeAction a)

    | Fail String
    | Done a

data TokenType = TypeUInt
               | TypeUInt64
               | TypeNInt
               | TypeNInt64
               | TypeInteger
               | TypeFloat16
               | TypeFloat32
               | TypeFloat64
               | TypeBytes
               | TypeBytesIndef
               | TypeString
               | TypeStringIndef
               | TypeListLen
               | TypeListLen64
               | TypeListLenIndef
               | TypeMapLen
               | TypeMapLen64
               | TypeMapLenIndef
               | TypeTag
               | TypeTag64
               | TypeBool
               | TypeNull
               | TypeUndef
               | TypeSimple
               | TypeBreak
               | TypeInvalid
  deriving (Eq, Ord, Enum, Bounded, Show)


instance Functor Decoder where
    {-# INLINE fmap #-}
    fmap f = \d -> Decoder $ \k -> runDecoder d (k . f)

instance Applicative Decoder where
    {-# INLINE pure #-}
    pure = \x -> Decoder $ \k -> k x

    {-# INLINE (<*>) #-}
    (<*>) = \df dx -> Decoder $ \k ->
                        runDecoder df (\f -> runDecoder dx (\x -> k (f x)))

instance Monad Decoder where
    return = pure

    {-# INLINE (>>=) #-}
    (>>=) = \dm f -> Decoder $ \k -> runDecoder dm (\m -> runDecoder (f m) k)

    {-# INLINE (>>) #-}
    (>>) = \dm dn -> Decoder $ \k -> runDecoder dm (\_ -> runDecoder dn k)

    fail msg = Decoder $ \_ -> Fail msg

getDecodeAction :: Decoder a -> DecodeAction a
getDecodeAction (Decoder k) = k (\x -> Done x)


---------------------------------------
-- Read input tokens of various types
--

{-# INLINE decodeWord #-}
decodeWord :: Decoder Word
decodeWord = Decoder (\k -> ConsumeWord (\w# -> k (W# w#)))

{-# INLINE decodeWord64 #-}
decodeWord64 :: Decoder Word64
decodeWord64 =
#ifdef ARCH_64bit
  Decoder (\k -> ConsumeWord (\w# -> k (W64# w#)))
#else
  Decoder (\k -> ConsumeWord64 (\w64# -> k (W64# w64#)))
#endif

{-# INLINE decodeNegWord #-}
decodeNegWord :: Decoder Word
decodeNegWord = Decoder (\k -> ConsumeNegWord (\w# -> k (W# w#)))

{-# INLINE decodeNegWord64 #-}
decodeNegWord64 :: Decoder Word64
decodeNegWord64 =
#ifdef ARCH_64bit
  Decoder (\k -> ConsumeNegWord (\w# -> k (W64# w#)))
#else
  Decoder (\k -> ConsumeNegWord64 (\w64# -> k (W64# w64#)))
#endif


{-# INLINE decodeInt #-}
decodeInt :: Decoder Int
decodeInt = Decoder (\k -> ConsumeInt (\n# -> k (I# n#)))

{-# INLINE decodeInt64 #-}
decodeInt64 :: Decoder Int64
decodeInt64 =
#ifdef ARCH_64bit
  Decoder (\k -> ConsumeInt (\n# -> k (I64# n#)))
#else
  Decoder (\k -> ConsumeInt64 (\n64# -> k (I64# n64#)))
#endif

{-# INLINE decodeInteger #-}
decodeInteger :: Decoder Integer
decodeInteger = Decoder (\k -> ConsumeInteger (\n -> k n))

{-# INLINE decodeFloat #-}
decodeFloat :: Decoder Float
decodeFloat = Decoder (\k -> ConsumeFloat (\f# -> k (F# f#)))

{-# INLINE decodeDouble #-}
decodeDouble :: Decoder Double
decodeDouble = Decoder (\k -> ConsumeDouble (\f# -> k (D# f#)))

{-# INLINE decodeBytes #-}
decodeBytes :: Decoder ByteString
decodeBytes = Decoder (\k -> ConsumeBytes (\bs -> k bs))

{-# INLINE decodeBytesIndef #-}
decodeBytesIndef :: Decoder ()
decodeBytesIndef = Decoder (\k -> ConsumeBytesIndef (k ()))

{-# INLINE decodeString #-}
decodeString :: Decoder Text
decodeString = Decoder (\k -> ConsumeString (\str -> k str))

{-# INLINE decodeStringIndef #-}
decodeStringIndef :: Decoder ()
decodeStringIndef = Decoder (\k -> ConsumeStringIndef (k ()))

{-# INLINE decodeListLen #-}
decodeListLen :: Decoder Int
decodeListLen = Decoder (\k -> ConsumeListLen (\n# -> k (I# n#)))

{-# INLINE decodeListLenIndef #-}
decodeListLenIndef :: Decoder ()
decodeListLenIndef = Decoder (\k -> ConsumeListLenIndef (k ()))

{-# INLINE decodeMapLen #-}
decodeMapLen :: Decoder Int
decodeMapLen = Decoder (\k -> ConsumeMapLen (\n# -> k (I# n#)))

{-# INLINE decodeMapLenIndef #-}
decodeMapLenIndef :: Decoder ()
decodeMapLenIndef = Decoder (\k -> ConsumeMapLenIndef (k ()))

{-# INLINE decodeTag #-}
decodeTag :: Decoder Word
decodeTag = Decoder (\k -> ConsumeTag (\w# -> k (W# w#)))

{-# INLINE decodeTag64 #-}
decodeTag64 :: Decoder Word64
decodeTag64 =
#ifdef ARCH_64bit
  Decoder (\k -> ConsumeTag (\w# -> k (W64# w#)))
#else
  Decoder (\k -> ConsumeTag64 (\w64# -> k (W64# w64#)))
#endif

{-# INLINE decodeBool #-}
decodeBool :: Decoder Bool
decodeBool = Decoder (\k -> ConsumeBool (\b -> k b))

{-# INLINE decodeNull #-}
decodeNull :: Decoder ()
decodeNull = Decoder (\k -> ConsumeNull (k ()))

{-# INLINE decodeSimple #-}
decodeSimple :: Decoder Word8
decodeSimple = Decoder (\k -> ConsumeSimple (\w# -> k (W8# w#)))


--------------------------------------------------------------
-- Specialised read operations: expect a token with a specific value
--

{-# INLINE decodeWordOf #-}
decodeWordOf :: Word -> Decoder ()
decodeWordOf n = do
  n' <- decodeWord
  if n == n' then return ()
             else fail $ "expected word " ++ show n

{-# INLINE decodeListLenOf #-}
decodeListLenOf :: Int -> Decoder ()
decodeListLenOf len = do
  len' <- decodeListLen
  if len == len' then return ()
                 else fail $ "expected list of length " ++ show len


--------------------------------------------------------------
-- Branching operations


{-# INLINE decodeListLenOrIndef #-}
decodeListLenOrIndef :: Decoder (Maybe Int)
decodeListLenOrIndef =
    Decoder (\k -> ConsumeListLenOrIndef (\n# ->
                     if I# n# >= 0
                       then k (Just (I# n#))
                       else k Nothing))

{-# INLINE decodeMapLenOrIndef #-}
decodeMapLenOrIndef :: Decoder (Maybe Int)
decodeMapLenOrIndef =
    Decoder (\k -> ConsumeMapLenOrIndef (\n# ->
                     if I# n# >= 0
                       then k (Just (I# n#))
                       else k Nothing))

{-# INLINE decodeBreakOr #-}
decodeBreakOr :: Decoder Bool
decodeBreakOr = Decoder (\k -> ConsumeBreakOr (\b -> k b))


--------------------------------------------------------------
-- Special operations


{-# INLINE peekTokenType #-}
peekTokenType :: Decoder TokenType
peekTokenType = Decoder (\k -> PeekTokenType (\tk -> k tk))

{-
expectExactly :: Word -> Decoder (Word :#: s) s
expectExactly n = expectExactly_ n done

expectAtLeast :: Word -> Decoder (Word :#: s) (Word :#: s)
expectAtLeast n = expectAtLeast_ n done

ignoreTrailingTerms :: Decoder (a :*: Word :#: s) (a :*: s)
ignoreTrailingTerms = IgnoreTerms done
-}

------------------------------------------------------------------------------
-- Special combinations for sequences
--


{-# INLINE decodeSequenceLenIndef #-}
decodeSequenceLenIndef :: (r -> a -> r)
                       -> r
                       -> (r -> r')
                       -> Decoder a
                       -> Decoder r'
decodeSequenceLenIndef f z g get =
    go z
  where
    go !acc = do
      stop <- decodeBreakOr
      if stop then return $! g acc
              else do x <- get; go (f acc x)

{-# INLINE decodeSequenceLenN #-}
decodeSequenceLenN :: (r -> a -> r)
                   -> r
                   -> (r -> r')
                   -> Int
                   -> Decoder a
                   -> Decoder r'
decodeSequenceLenN f z g c get =
    go z c
  where
    go !acc 0 = return $! g acc
    go !acc n = do x <- get; go (f acc x) (n-1)

