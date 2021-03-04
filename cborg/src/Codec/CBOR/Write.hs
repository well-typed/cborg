{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE CPP                 #-}
{-# LANGUAGE DeriveFunctor       #-}
{-# LANGUAGE MagicHash           #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE PatternSynonyms     #-}

#include "cbor.h"

#if defined(OPTIMIZE_GMP)
#if __GLASGOW_HASKELL__ >= 900
#define HAVE_GHC_BIGNUM 1
{-# LANGUAGE UnboxedSums         #-}
#endif
#endif

-- |
-- Module      : Codec.CBOR.Write
-- Copyright   : (c) Duncan Coutts 2015-2017
-- License     : BSD3-style (see LICENSE.txt)
--
-- Maintainer  : duncan@community.haskell.org
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Functions for writing out CBOR 'Encoding' values in a variety of forms.
--
module Codec.CBOR.Write
  ( toBuilder          -- :: Encoding -> B.Builder
  , toLazyByteString   -- :: Encoding -> L.ByteString
  , toStrictByteString -- :: Encoding -> S.ByteString
  ) where

import           Data.Bits
import           Data.Int

#if ! MIN_VERSION_base(4,11,0)
import           Data.Monoid
#endif

import           Data.Word
import           Foreign.Ptr

import qualified Data.ByteString                       as S
import qualified Data.ByteString.Builder               as B
import qualified Data.ByteString.Builder.Internal      as BI
import           Data.ByteString.Builder.Prim          (condB, (>$<), (>*<))
import qualified Data.ByteString.Builder.Prim          as P
import qualified Data.ByteString.Builder.Prim.Internal as PI
import qualified Data.ByteString.Lazy                  as L
import qualified Data.Text                             as T
import qualified Data.Text.Encoding                    as T

import           Control.Exception.Base                (assert)
import           GHC.Exts
import           GHC.IO                                (IO(IO))
#if defined(HAVE_GHC_BIGNUM)
import qualified GHC.Num.Integer
import qualified GHC.Num.BigNat                        as Gmp
import qualified GHC.Num.BigNat
import           GHC.Num.BigNat                        (BigNat)
#else
import qualified GHC.Integer.GMP.Internals             as Gmp
import           GHC.Integer.GMP.Internals             (BigNat)
#endif

#if __GLASGOW_HASKELL__ < 710
import           GHC.Word
#endif

import qualified Codec.CBOR.ByteArray.Sliced           as BAS
import           Codec.CBOR.Encoding
import           Codec.CBOR.Magic

--------------------------------------------------------------------------------

-- | Turn an 'Encoding' into a lazy 'L.ByteString' in CBOR binary
-- format.
--
-- @since 0.2.0.0
toLazyByteString :: Encoding     -- ^ The 'Encoding' of a CBOR value.
                 -> L.ByteString -- ^ The encoded CBOR value.
toLazyByteString = B.toLazyByteString . toBuilder

-- | Turn an 'Encoding' into a strict 'S.ByteString' in CBOR binary
-- format.
--
-- @since 0.2.0.0
toStrictByteString :: Encoding     -- ^ The 'Encoding' of a CBOR value.
                   -> S.ByteString -- ^ The encoded value.
toStrictByteString = L.toStrict . B.toLazyByteString . toBuilder

-- | Turn an 'Encoding' into a 'L.ByteString' 'B.Builder' in CBOR
-- binary format.
--
-- @since 0.2.0.0
toBuilder :: Encoding  -- ^ The 'Encoding' of a CBOR value.
          -> B.Builder -- ^ The encoded value as a 'B.Builder'.
toBuilder =
    \(Encoding vs0) -> BI.builder (buildStep (vs0 TkEnd))

buildStep :: Tokens
          -> (BI.BufferRange -> IO (BI.BuildSignal a))
          -> BI.BufferRange
          -> IO (BI.BuildSignal a)
buildStep vs1 k (BI.BufferRange op0 ope0) =
    go vs1 op0
  where
    go vs !op
      | op `plusPtr` bound <= ope0 = case vs of
          TkWord     x vs' -> PI.runB wordMP     x op >>= go vs'
          TkWord64   x vs' -> PI.runB word64MP   x op >>= go vs'

          TkInt      x vs' -> PI.runB intMP      x op >>= go vs'
          TkInt64    x vs' -> PI.runB int64MP    x op >>= go vs'

          TkBytes        x vs' -> BI.runBuilderWith
                                    (bytesMP  x) (buildStep vs' k)
                                    (BI.BufferRange op ope0)
          TkByteArray    x vs' -> BI.runBuilderWith
                                    (byteArrayMP x) (buildStep vs' k)
                                    (BI.BufferRange op ope0)

          TkUtf8ByteArray x vs' -> BI.runBuilderWith
                                     (utf8ByteArrayMP x) (buildStep vs' k)
                                     (BI.BufferRange op ope0)
          TkString        x vs' -> BI.runBuilderWith
                                     (stringMP x) (buildStep vs' k)
                                     (BI.BufferRange op ope0)

          TkBytesBegin vs' -> PI.runB bytesBeginMP  () op >>= go vs'
          TkStringBegin vs'-> PI.runB stringBeginMP () op >>= go vs'

          TkListLen  x vs' -> PI.runB arrayLenMP     x op >>= go vs'
          TkListBegin  vs' -> PI.runB arrayBeginMP  () op >>= go vs'

          TkMapLen   x vs' -> PI.runB mapLenMP       x op >>= go vs'
          TkMapBegin   vs' -> PI.runB mapBeginMP    () op >>= go vs'

          TkTag      x vs' -> PI.runB tagMP          x op >>= go vs'
          TkTag64    x vs' -> PI.runB tag64MP        x op >>= go vs'

#if defined(OPTIMIZE_GMP)
          -- This code is specialized for GMP implementation of Integer. By
          -- looking directly at the constructors we can avoid some checks.
          -- S# hold an Int, so we can just use intMP.
          TkInteger (SmallInt i) vs' ->
               PI.runB intMP (I# i) op >>= go vs'
          -- PosBigInt is guaranteed to be > 0.
          TkInteger integer@(PosBigInt bigNat) vs'
            | integer <= fromIntegral (maxBound :: Word64) ->
                PI.runB word64MP (fromIntegral integer) op >>= go vs'
            | otherwise ->
               let buffer = BI.BufferRange op ope0
               in BI.runBuilderWith
                    (bigNatMP bigNat) (buildStep vs' k) buffer
          -- Jn# is guaranteed to be < 0.
          TkInteger integer@(NegBigInt bigNat) vs'
            | integer >= -1 - fromIntegral (maxBound :: Word64) ->
                PI.runB negInt64MP (fromIntegral (-1 - integer)) op >>= go vs'
            | otherwise ->
                let buffer = BI.BufferRange op ope0
                in BI.runBuilderWith
                     (negBigNatMP bigNat) (buildStep vs' k) buffer
#else
          TkInteger  x vs'
            | x >= 0
            , x <= fromIntegral (maxBound :: Word64)
                            -> PI.runB word64MP (fromIntegral x) op >>= go vs'
            | x <  0
            , x >= -1 - fromIntegral (maxBound :: Word64)
                            -> PI.runB negInt64MP (fromIntegral (-1 - x)) op >>= go vs'
            | otherwise     -> BI.runBuilderWith
                                 (integerMP x) (buildStep vs' k)
                                 (BI.BufferRange op ope0)
#endif

          TkBool False vs' -> PI.runB falseMP   () op >>= go vs'
          TkBool True  vs' -> PI.runB trueMP    () op >>= go vs'
          TkNull       vs' -> PI.runB nullMP    () op >>= go vs'
          TkUndef      vs' -> PI.runB undefMP   () op >>= go vs'
          TkSimple   w vs' -> PI.runB simpleMP   w op >>= go vs'
          TkFloat16  f vs' -> PI.runB halfMP     f op >>= go vs'
          TkFloat32  f vs' -> PI.runB floatMP    f op >>= go vs'
          TkFloat64  f vs' -> PI.runB doubleMP   f op >>= go vs'
          TkBreak      vs' -> PI.runB breakMP   () op >>= go vs'

          TkEncoded  x vs' -> BI.runBuilderWith
                                (B.byteString x) (buildStep vs' k)
                                (BI.BufferRange op ope0)

          TkEnd            -> k (BI.BufferRange op ope0)

      | otherwise = return $ BI.bufferFull bound op (buildStep vs k)

    -- The maximum size in bytes of the fixed-size encodings
    bound :: Int
    bound = 9

header :: P.BoundedPrim Word8
header = P.liftFixedToBounded P.word8

constHeader :: Word8 -> P.BoundedPrim ()
constHeader h = P.liftFixedToBounded (const h >$< P.word8)

withHeader :: P.FixedPrim a -> P.BoundedPrim (Word8, a)
withHeader p = P.liftFixedToBounded (P.word8 >*< p)

withConstHeader :: Word8 -> P.FixedPrim a -> P.BoundedPrim a
withConstHeader h p = P.liftFixedToBounded ((,) h >$< (P.word8 >*< p))


{-
From RFC 7049:

   Major type 0:  an unsigned integer.  The 5-bit additional information
      is either the integer itself (for additional information values 0
      through 23) or the length of additional data.  Additional
      information 24 means the value is represented in an additional
      uint8_t, 25 means a uint16_t, 26 means a uint32_t, and 27 means a
      uint64_t.  For example, the integer 10 is denoted as the one byte
      0b000_01010 (major type 0, additional information 10).  The
      integer 500 would be 0b000_11001 (major type 0, additional
      information 25) followed by the two bytes 0x01f4, which is 500 in
      decimal.

-}

{-# INLINE wordMP #-}
wordMP :: P.BoundedPrim Word
wordMP =
    condB (<= 0x17)       (fromIntegral >$< header) $
    condB (<= 0xff)       (fromIntegral >$< withConstHeader 24 P.word8) $
    condB (<= 0xffff)     (fromIntegral >$< withConstHeader 25 P.word16BE) $
#if defined(ARCH_64bit)
    condB (<= 0xffffffff) (fromIntegral >$< withConstHeader 26 P.word32BE) $
                          (fromIntegral >$< withConstHeader 27 P.word64BE)
#else
                          (fromIntegral >$< withConstHeader 26 P.word32BE)
#endif

{-# INLINE word64MP #-}
word64MP :: P.BoundedPrim Word64
word64MP =
    condB (<= 0x17)       (fromIntegral >$< header) $
    condB (<= 0xff)       (fromIntegral >$< withConstHeader 24 P.word8) $
    condB (<= 0xffff)     (fromIntegral >$< withConstHeader 25 P.word16BE) $
    condB (<= 0xffffffff) (fromIntegral >$< withConstHeader 26 P.word32BE) $
                          (fromIntegral >$< withConstHeader 27 P.word64BE)

{-
From RFC 7049:

   Major type 1:  a negative integer.  The encoding follows the rules
      for unsigned integers (major type 0), except that the value is
      then -1 minus the encoded unsigned integer.  For example, the
      integer -500 would be 0b001_11001 (major type 1, additional
      information 25) followed by the two bytes 0x01f3, which is 499 in
      decimal.
-}

negInt64MP :: P.BoundedPrim Word64
negInt64MP =
    condB (<= 0x17)       (fromIntegral . (0x20 +) >$< header) $
    condB (<= 0xff)       (fromIntegral >$< withConstHeader 0x38 P.word8) $
    condB (<= 0xffff)     (fromIntegral >$< withConstHeader 0x39 P.word16BE) $
    condB (<= 0xffffffff) (fromIntegral >$< withConstHeader 0x3a P.word32BE) $
                          (fromIntegral >$< withConstHeader 0x3b P.word64BE)

{-
   Major types 0 and 1 are designed in such a way that they can be
   encoded in C from a signed integer without actually doing an if-then-
   else for positive/negative (Figure 2).  This uses the fact that
   (-1-n), the transformation for major type 1, is the same as ~n
   (bitwise complement) in C unsigned arithmetic; ~n can then be
   expressed as (-1)^n for the negative case, while 0^n leaves n
   unchanged for non-negative.  The sign of a number can be converted to
   -1 for negative and 0 for non-negative (0 or positive) by arithmetic-
   shifting the number by one bit less than the bit length of the number
   (for example, by 63 for 64-bit numbers).

   void encode_sint(int64_t n) {
     uint64t ui = n >> 63;    // extend sign to whole length
     mt = ui & 0x20;          // extract major type
     ui ^= n;                 // complement negatives
     if (ui < 24)
       *p++ = mt + ui;
     else if (ui < 256) {
       *p++ = mt + 24;
       *p++ = ui;
     } else
          ...

            Figure 2: Pseudocode for Encoding a Signed Integer
-}

{-# INLINE intMP #-}
intMP :: P.BoundedPrim Int
intMP =
    prep >$< (
      condB ((<= 0x17)       . snd) (encIntSmall >$< header) $
      condB ((<= 0xff)       . snd) (encInt8  >$< withHeader P.word8) $
      condB ((<= 0xffff)     . snd) (encInt16 >$< withHeader P.word16BE) $
#if defined(ARCH_64bit)
      condB ((<= 0xffffffff) . snd) (encInt32 >$< withHeader P.word32BE)
                                    (encInt64 >$< withHeader P.word64BE)
#else
                                    (encInt32 >$< withHeader P.word32BE)
#endif
    )
  where
    prep :: Int -> (Word8, Word)
    prep n = (mt, ui)
      where
        sign :: Word     -- extend sign to whole length
        sign = fromIntegral (n `unsafeShiftR` intBits)
#if MIN_VERSION_base(4,7,0)
        intBits = finiteBitSize (undefined :: Int) - 1
#else
        intBits = bitSize (undefined :: Int) - 1
#endif

        mt   :: Word8    -- select major type
        mt   = fromIntegral (sign .&. 0x20)

        ui   :: Word     -- complement negatives
        ui   = fromIntegral n `xor` sign

    encIntSmall :: (Word8, Word) -> Word8
    encIntSmall (mt, ui) =  mt + fromIntegral ui
    encInt8     (mt, ui) = (mt + 24, fromIntegral ui)
    encInt16    (mt, ui) = (mt + 25, fromIntegral ui)
    encInt32    (mt, ui) = (mt + 26, fromIntegral ui)
#if defined(ARCH_64bit)
    encInt64    (mt, ui) = (mt + 27, fromIntegral ui)
#endif


{-# INLINE int64MP #-}
int64MP :: P.BoundedPrim Int64
int64MP =
    prep >$< (
      condB ((<= 0x17)       . snd) (encIntSmall >$< header) $
      condB ((<= 0xff)       . snd) (encInt8  >$< withHeader P.word8) $
      condB ((<= 0xffff)     . snd) (encInt16 >$< withHeader P.word16BE) $
      condB ((<= 0xffffffff) . snd) (encInt32 >$< withHeader P.word32BE)
                                    (encInt64 >$< withHeader P.word64BE)
    )
  where
    prep :: Int64 -> (Word8, Word64)
    prep n = (mt, ui)
      where
        sign :: Word64   -- extend sign to whole length
        sign = fromIntegral (n `unsafeShiftR` intBits)
#if MIN_VERSION_base(4,7,0)
        intBits = finiteBitSize (undefined :: Int64) - 1
#else
        intBits = bitSize (undefined :: Int64) - 1
#endif

        mt   :: Word8    -- select major type
        mt   = fromIntegral (sign .&. 0x20)

        ui   :: Word64   -- complement negatives
        ui   = fromIntegral n `xor` sign

    encIntSmall (mt, ui) =  mt + fromIntegral ui
    encInt8     (mt, ui) = (mt + 24, fromIntegral ui)
    encInt16    (mt, ui) = (mt + 25, fromIntegral ui)
    encInt32    (mt, ui) = (mt + 26, fromIntegral ui)
    encInt64    (mt, ui) = (mt + 27, fromIntegral ui)

{-
   Major type 2:  a byte string.  The string's length in bytes is
      represented following the rules for positive integers (major type
      0).  For example, a byte string whose length is 5 would have an
      initial byte of 0b010_00101 (major type 2, additional information
      5 for the length), followed by 5 bytes of binary content.  A byte
      string whose length is 500 would have 3 initial bytes of
      0b010_11001 (major type 2, additional information 25 to indicate a
      two-byte length) followed by the two bytes 0x01f4 for a length of
      500, followed by 500 bytes of binary content.
-}

bytesMP :: S.ByteString -> B.Builder
bytesMP bs =
    P.primBounded bytesLenMP (fromIntegral $ S.length bs) <> B.byteString bs

bytesLenMP :: P.BoundedPrim Word
bytesLenMP =
    condB (<= 0x17)       (fromIntegral . (0x40 +) >$< header) $
    condB (<= 0xff)       (fromIntegral >$< withConstHeader 0x58 P.word8) $
    condB (<= 0xffff)     (fromIntegral >$< withConstHeader 0x59 P.word16BE) $
    condB (<= 0xffffffff) (fromIntegral >$< withConstHeader 0x5a P.word32BE) $
                          (fromIntegral >$< withConstHeader 0x5b P.word64BE)
byteArrayMP :: BAS.SlicedByteArray -> B.Builder
byteArrayMP ba =
    P.primBounded bytesLenMP n <> BAS.toBuilder ba
  where n = fromIntegral $ BAS.sizeofSlicedByteArray ba

bytesBeginMP :: P.BoundedPrim ()
bytesBeginMP = constHeader 0x5f

{-
   Major type 3:  a text string, specifically a string of Unicode
      characters that is encoded as UTF-8 [RFC3629].  The format of this
      type is identical to that of byte strings (major type 2), that is,
      as with major type 2, the length gives the number of bytes.  This
      type is provided for systems that need to interpret or display
      human-readable text, and allows the differentiation between
      unstructured bytes and text that has a specified repertoire and
      encoding.  In contrast to formats such as JSON, the Unicode
      characters in this type are never escaped.  Thus, a newline
      character (U+000A) is always represented in a string as the byte
      0x0a, and never as the bytes 0x5c6e (the characters "\" and "n")
      or as 0x5c7530303061 (the characters "\", "u", "0", "0", "0", and
      "a").
-}

stringMP :: T.Text -> B.Builder
stringMP t =
    P.primBounded stringLenMP (fromIntegral $ S.length bs) <> B.byteString bs
  where
    bs  = T.encodeUtf8 t

stringLenMP :: P.BoundedPrim Word
stringLenMP =
    condB (<= 0x17)       (fromIntegral . (0x60 +) >$< header) $
    condB (<= 0xff)       (fromIntegral >$< withConstHeader 0x78 P.word8) $
    condB (<= 0xffff)     (fromIntegral >$< withConstHeader 0x79 P.word16BE) $
    condB (<= 0xffffffff) (fromIntegral >$< withConstHeader 0x7a P.word32BE) $
                          (fromIntegral >$< withConstHeader 0x7b P.word64BE)

stringBeginMP :: P.BoundedPrim ()
stringBeginMP = constHeader 0x7f

utf8ByteArrayMP :: BAS.SlicedByteArray -> B.Builder
utf8ByteArrayMP t =
    P.primBounded stringLenMP n <> BAS.toBuilder t
  where
    n = fromIntegral $ BAS.sizeofSlicedByteArray t

{-
   Major type 4:  an array of data items.  Arrays are also called lists,
      sequences, or tuples.  The array's length follows the rules for
      byte strings (major type 2), except that the length denotes the
      number of data items, not the length in bytes that the array takes
      up.  Items in an array do not need to all be of the same type.
      For example, an array that contains 10 items of any type would
      have an initial byte of 0b100_01010 (major type of 4, additional
      information of 10 for the length) followed by the 10 remaining
      items.
-}

arrayLenMP :: P.BoundedPrim Word
arrayLenMP =
    condB (<= 0x17)       (fromIntegral . (0x80 +) >$< header) $
    condB (<= 0xff)       (fromIntegral >$< withConstHeader 0x98 P.word8) $
    condB (<= 0xffff)     (fromIntegral >$< withConstHeader 0x99 P.word16BE) $
    condB (<= 0xffffffff) (fromIntegral >$< withConstHeader 0x9a P.word32BE) $
                          (fromIntegral >$< withConstHeader 0x9b P.word64BE)

arrayBeginMP :: P.BoundedPrim ()
arrayBeginMP = constHeader 0x9f

{-
   Major type 5:  a map of pairs of data items.  Maps are also called
      tables, dictionaries, hashes, or objects (in JSON).  A map is
      comprised of pairs of data items, each pair consisting of a key
      that is immediately followed by a value.  The map's length follows
      the rules for byte strings (major type 2), except that the length
      denotes the number of pairs, not the length in bytes that the map
      takes up.  For example, a map that contains 9 pairs would have an
      initial byte of 0b101_01001 (major type of 5, additional
      information of 9 for the number of pairs) followed by the 18
      remaining items.  The first item is the first key, the second item
      is the first value, the third item is the second key, and so on.
      A map that has duplicate keys may be well-formed, but it is not
      valid, and thus it causes indeterminate decoding; see also
      Section 3.7.
-}

mapLenMP :: P.BoundedPrim Word
mapLenMP =
    condB (<= 0x17)       (fromIntegral . (0xa0 +) >$< header) $
    condB (<= 0xff)       (fromIntegral >$< withConstHeader 0xb8 P.word8) $
    condB (<= 0xffff)     (fromIntegral >$< withConstHeader 0xb9 P.word16BE) $
    condB (<= 0xffffffff) (fromIntegral >$< withConstHeader 0xba P.word32BE) $
                          (fromIntegral >$< withConstHeader 0xbb P.word64BE)

mapBeginMP :: P.BoundedPrim ()
mapBeginMP = constHeader 0xbf

{-
   Major type 6:  optional semantic tagging of other major types.

      In CBOR, a data item can optionally be preceded by a tag to give it
      additional semantics while retaining its structure.  The tag is major
      type 6, and represents an integer number as indicated by the tag's
      integer value; the (sole) data item is carried as content data.

      The initial bytes of the tag follow the rules for positive integers
      (major type 0).
-}

tagMP :: P.BoundedPrim Word
tagMP =
    condB (<= 0x17)       (fromIntegral . (0xc0 +) >$< header) $
    condB (<= 0xff)       (fromIntegral >$< withConstHeader 0xd8 P.word8) $
    condB (<= 0xffff)     (fromIntegral >$< withConstHeader 0xd9 P.word16BE) $
#if defined(ARCH_64bit)
    condB (<= 0xffffffff) (fromIntegral >$< withConstHeader 0xda P.word32BE) $
                          (fromIntegral >$< withConstHeader 0xdb P.word64BE)
#else
                          (fromIntegral >$< withConstHeader 0xda P.word32BE)
#endif

tag64MP :: P.BoundedPrim Word64
tag64MP =
    condB (<= 0x17)       (fromIntegral . (0xc0 +) >$< header) $
    condB (<= 0xff)       (fromIntegral >$< withConstHeader 0xd8 P.word8) $
    condB (<= 0xffff)     (fromIntegral >$< withConstHeader 0xd9 P.word16BE) $
    condB (<= 0xffffffff) (fromIntegral >$< withConstHeader 0xda P.word32BE) $
                          (fromIntegral >$< withConstHeader 0xdb P.word64BE)

{-
   Major type 7:  floating-point numbers and simple data types that need
      no content, as well as the "break" stop code.

      Major type 7 is for two types of data: floating-point numbers and
      "simple values" that do not need any content.  Each value of the
      5-bit additional information in the initial byte has its own separate
      meaning, as defined in Table 1.  Like the major types for integers,
      items of this major type do not carry content data; all the
      information is in the initial bytes.

    +-------------+--------------------------------------------------+
    | 5-Bit Value | Semantics                                        |
    +-------------+--------------------------------------------------+
    | 0..23       | Simple value (value 0..23)                       |
    |             |                                                  |
    | 24          | Simple value (value 32..255 in following byte)   |
    |             |                                                  |
    | 25          | IEEE 754 Half-Precision Float (16 bits follow)   |
    |             |                                                  |
    | 26          | IEEE 754 Single-Precision Float (32 bits follow) |
    |             |                                                  |
    | 27          | IEEE 754 Double-Precision Float (64 bits follow) |
    |             |                                                  |
    | 28-30       | (Unassigned)                                     |
    |             |                                                  |
    | 31          | "break" stop code for indefinite-length items    |
    +-------------+--------------------------------------------------+
-}

simpleMP :: P.BoundedPrim Word8
simpleMP =
    condB (<= 0x17) ((0xe0 +) >$< header) $
                    (withConstHeader 0xf8 P.word8)

falseMP :: P.BoundedPrim ()
falseMP = constHeader 0xf4

trueMP :: P.BoundedPrim ()
trueMP = constHeader 0xf5

nullMP :: P.BoundedPrim ()
nullMP = constHeader 0xf6

undefMP :: P.BoundedPrim ()
undefMP = constHeader 0xf7

-- Canonical encoding of a NaN as per RFC 7049, section 3.9.
canonicalNaN :: PI.BoundedPrim a
canonicalNaN = P.liftFixedToBounded $ const (0xf9, (0x7e, 0x00))
                                   >$< P.word8 >*< P.word8 >*< P.word8

halfMP :: P.BoundedPrim Float
halfMP = condB isNaN canonicalNaN
                     (floatToWord16 >$< withConstHeader 0xf9 P.word16BE)

floatMP :: P.BoundedPrim Float
floatMP = condB isNaN canonicalNaN
                      (withConstHeader 0xfa P.floatBE)

doubleMP :: P.BoundedPrim Double
doubleMP = condB isNaN canonicalNaN
                       (withConstHeader 0xfb P.doubleBE)

breakMP :: P.BoundedPrim ()
breakMP = constHeader 0xff

#if defined(OPTIMIZE_GMP)
-- ---------------------------------------- --
-- Implementation optimized for integer-gmp --
-- ---------------------------------------- --

-- Below is where we try to abstract over the differences between the legacy
-- integer-gmp interface and ghc-bignum, shipped in GHC >= 9.0.

-- | Write the limbs of a 'BigNat' to the given address in big-endian byte
-- ordering.
exportBigNatToAddr :: BigNat -> Addr# -> IO Word

#if defined(HAVE_GHC_BIGNUM)

pattern SmallInt  n = GHC.Num.Integer.IS n
pattern PosBigInt n = GHC.Num.Integer.IP n
pattern NegBigInt n = GHC.Num.Integer.IN n

bigNatSizeInBytes :: GHC.Num.BigNat.BigNat -> Word
bigNatSizeInBytes bigNat =
  Gmp.bigNatSizeInBase 256 (GHC.Num.BigNat.unBigNat bigNat)

bigNatMP :: GHC.Num.BigNat.BigNat# -> B.Builder
bigNatMP n = P.primBounded header 0xc2 <> bigNatToBuilder (GHC.Num.BigNat.BN# n)

negBigNatMP :: GHC.Num.BigNat.BigNat# -> B.Builder
negBigNatMP n =
  -- If value `n` is stored in CBOR, it is interpreted as -1 - n. Since BigNat
  -- already represents n (note: it's unsigned), we simply decrement it to get
  -- the correct encoding.
     P.primBounded header 0xc3
  <> bigNatToBuilder (subtractOneBigNat (GHC.Num.BigNat.BN# n))
  where
    subtractOneBigNat (GHC.Num.BigNat.BN# nat) =
      case GHC.Num.BigNat.bigNatSubWord# nat 1## of
        (#       | r #) -> GHC.Num.BigNat.BN# r
        (# (# #) | #)   -> error "subtractOneBigNat: impossible"

exportBigNatToAddr (GHC.Num.BigNat.BN# b) addr = IO $ \s ->
  -- The last parameter (`1#`) makes the export function use big endian encoding.
  case GHC.Num.BigNat.bigNatToAddr# b addr 1# s of
    (# s', w #) -> (# s', W# w #)
#else

pattern SmallInt  n = Gmp.S# n
pattern PosBigInt n = Gmp.Jp# n
pattern NegBigInt n = Gmp.Jn# n

bigNatSizeInBytes :: BigNat -> Word
bigNatSizeInBytes bigNat = W# (Gmp.sizeInBaseBigNat bigNat 256#)

bigNatMP :: BigNat -> B.Builder
bigNatMP n = P.primBounded header 0xc2 <> bigNatToBuilder n

negBigNatMP :: BigNat -> B.Builder
negBigNatMP n =
  -- If value `n` is stored in CBOR, it is interpreted as -1 - n. Since BigNat
  -- already represents n (note: it's unsigned), we simply decrement it to get
  -- the correct encoding.
     P.primBounded header 0xc3
  <> bigNatToBuilder (subtractOneBigNat n)
  where
    subtractOneBigNat n = Gmp.minusBigNatWord n (int2Word# 1#)

exportBigNatToAddr bigNat addr# =
  -- The last parameter (`1#`) makes the export function use big endian encoding.
  Gmp.exportBigNatToAddr bigNat addr# 1#
#endif

bigNatToBuilder :: BigNat -> B.Builder
bigNatToBuilder = bigNatBuilder
  where
    bigNatBuilder :: BigNat -> B.Builder
    bigNatBuilder bigNat =
        let sizeW = bigNatSizeInBytes bigNat
#if MIN_VERSION_bytestring(0,10,12)
            bounded = PI.boundedPrim (fromIntegral sizeW) (dumpBigNat sizeW)
#else
            bounded = PI.boudedPrim (fromIntegral sizeW) (dumpBigNat sizeW)
#endif
        in P.primBounded bytesLenMP sizeW <> P.primBounded bounded bigNat

    dumpBigNat :: Word -> BigNat -> Ptr a -> IO (Ptr a)
    dumpBigNat (W# sizeW#) bigNat ptr@(Ptr addr#) = do
        (W# written#) <- exportBigNatToAddr bigNat addr#
        let !newPtr = ptr `plusPtr` (I# (word2Int# written#))
            sanity = isTrue# (sizeW# `eqWord#` written#)
        return $ assert sanity newPtr

#else

-- ---------------------- --
-- Generic implementation --
-- ---------------------- --
integerMP :: Integer -> B.Builder
integerMP n
  | n >= 0    = P.primBounded header 0xc2 <> integerToBuilder n
  | otherwise = P.primBounded header 0xc3 <> integerToBuilder (-1 - n)

integerToBuilder :: Integer -> B.Builder
integerToBuilder n = bytesMP (integerToBytes n)

integerToBytes :: Integer -> S.ByteString
integerToBytes n0
  | n0 == 0   = S.pack [0]
  | otherwise = S.pack (reverse (go n0))
  where
    go n | n == 0    = []
         | otherwise = narrow n : go (n `shiftR` 8)

    narrow :: Integer -> Word8
    narrow = fromIntegral
#endif
