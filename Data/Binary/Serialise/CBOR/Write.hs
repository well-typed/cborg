{-# LANGUAGE CPP                 #-}
{-# LANGUAGE MagicHash           #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE UnboxedTuples       #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving  #-}
{-# LANGUAGE DeriveFunctor       #-}

-- |
-- Module      : Data.Binary.Serialise.CBOR.Write
-- Copyright   : (c) Duncan Coutts 2015
-- License     : BSD3-style (see LICENSE.txt)
--
-- Maintainer  : duncan@community.haskell.org
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Tools for writing out CBOR @'Encoding'@ values in
-- a variety of forms.
--
module Data.Binary.Serialise.CBOR.Write
  ( toBuilder          -- :: Encoding -> B.Builder
  , toLazyByteString   -- :: Encoding -> L.ByteString
  , toStrictByteString -- :: Encoding -> S.ByteString
  , prettyEnc          -- :: Encoding -> Either String String
  ) where

#include "cbor.h"

import           Data.Bits
import           Data.Int
import           Data.Monoid
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

import           Data.Binary.Serialise.CBOR.ByteOrder
import           Data.Binary.Serialise.CBOR.Encoding


import           Control.Monad (replicateM_)
import           Numeric
--------------------------------------------------------------------------------

-- | Turn an @'Encoding'@ into a lazy @'L.ByteString'@ in CBOR binary
-- format.
toLazyByteString :: Encoding     -- ^ The @'Encoding'@ of a CBOR value.
                 -> L.ByteString -- ^ The encoded CBOR value.
toLazyByteString = B.toLazyByteString . toBuilder

-- | Turn an @'Encoding'@ into a strict @'S.ByteString'@ in CBOR binary
-- format.
toStrictByteString :: Encoding     -- ^ The @'Encoding'@ of a CBOR value.
                   -> S.ByteString -- ^ The encoded value.
toStrictByteString = L.toStrict . B.toLazyByteString . toBuilder

-- | Turn an @'Encoding'@ into a @'L.ByteString'@ @'B.Builder'@ in CBOR
-- binary format.
toBuilder :: Encoding  -- ^ The @'Encoding'@ of a CBOR value.
          -> B.Builder -- ^ The encoded value as a @'B.Builder'@.
toBuilder =
    \(Encoding vs0) -> BI.builder (step (vs0 TkEnd))
  where
    step vs1 k (BI.BufferRange op0 ope0) =
        go vs1 op0
      where
        go vs !op
          | op `plusPtr` bound <= ope0 = case vs of
              TkWord     x vs' -> PI.runB wordMP     x op >>= go vs'
              TkWord64   x vs' -> PI.runB word64MP   x op >>= go vs'

              TkInt      x vs' -> PI.runB intMP      x op >>= go vs'
              TkInt64    x vs' -> PI.runB int64MP    x op >>= go vs'

              TkBytes    x vs' -> BI.runBuilderWith (bytesMP  x) (step vs' k) (BI.BufferRange op ope0)
              TkBytesBegin vs' -> PI.runB bytesBeginMP  () op >>= go vs'

              TkString   x vs' -> BI.runBuilderWith (stringMP x) (step vs' k) (BI.BufferRange op ope0)
              TkStringBegin vs'-> PI.runB stringBeginMP () op >>= go vs'

              TkListLen  x vs' -> PI.runB arrayLenMP x op >>= go vs'
              TkListBegin  vs' -> PI.runB arrayBeginMP  () op >>= go vs'

              TkMapLen   x vs' -> PI.runB mapLenMP   x op >>= go vs'
              TkMapBegin   vs' -> PI.runB mapBeginMP    () op >>= go vs'

              TkTag      x vs' -> PI.runB tagMP      x op >>= go vs'
              TkTag64    x vs' -> PI.runB tag64MP      x op >>= go vs'
              TkInteger  x vs'
                --TODO: for GMP can optimimise this by looking at the S#
                -- constructors to see if it fits in an Int, and if it's
                -- positive or negative.
                | x >= 0
                , x <= fromIntegral (maxBound :: Word64)
                                -> PI.runB word64MP (fromIntegral x) op >>= go vs'
                | x <  0
                , x >= -1 - fromIntegral (maxBound :: Word64)
                                -> PI.runB negInt64MP (fromIntegral (-1 - x)) op >>= go vs'
                | otherwise     -> BI.runBuilderWith (integerMP x) (step vs' k) (BI.BufferRange op ope0)

              TkBool False vs' -> PI.runB falseMP   () op >>= go vs'
              TkBool True  vs' -> PI.runB trueMP    () op >>= go vs'
              TkNull       vs' -> PI.runB nullMP    () op >>= go vs'
              TkUndef      vs' -> PI.runB undefMP   () op >>= go vs'
              TkSimple   w vs' -> PI.runB simpleMP   w op >>= go vs'
              TkFloat16  f vs' -> PI.runB halfMP     f op >>= go vs'
              TkFloat32  f vs' -> PI.runB floatMP    f op >>= go vs'
              TkFloat64  f vs' -> PI.runB doubleMP   f op >>= go vs'
              TkBreak      vs' -> PI.runB breakMP   () op >>= go vs'

              TkEnd            -> k (BI.BufferRange op ope0)

          | otherwise = return $ BI.bufferFull bound op (step vs k)

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

integerMP :: Integer -> B.Builder
integerMP n
  | n >= 0    = P.primBounded header 0xc2 <> bytesMP (integerToBytes n)
  | otherwise = P.primBounded header 0xc3 <> bytesMP (integerToBytes (-1 - n))

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

halfMP :: P.BoundedPrim Float
halfMP = floatToWord16 >$<
         withConstHeader 0xf9 P.word16BE

floatMP :: P.BoundedPrim Float
floatMP = withConstHeader 0xfa P.floatBE

doubleMP :: P.BoundedPrim Double
doubleMP = withConstHeader 0xfb P.doubleBE

breakMP :: P.BoundedPrim ()
breakMP = constHeader 0xff

--TODO: optimised implementation for GMP
integerToBytes :: Integer -> S.ByteString
integerToBytes n0
  | n0 == 0   = S.pack [0]
  | otherwise = S.pack (reverse (go n0))
  where
    go n | n == 0    = []
         | otherwise = narrow n : go (n `shiftR` 8)

    narrow :: Integer -> Word8
    narrow = fromIntegral


--------------------
-- pretty printing
newtype PP a = PP (Tokens -> Int -> (String -> String) -> Either String (Tokens,Int,String -> String,a))

-- | Pretty prints an Encoding in a format similar to the
--   format used on http://cbor.me/.
prettyEnc :: Encoding -> Either String String
prettyEnc e = case runPP pprint e of
  Left s -> Left s
  Right (TkEnd,_,ss,_) -> Right (ss "")
  Right (toks,_,_,_) -> Left $ "prettyEnc: Not all input was consumed (this is probably a problem with the pretty printing code). Tokens left: " ++ show toks

runPP :: PP a -> Encoding -> Either String (Tokens, Int, String -> String, a)
runPP (PP f) (Encoding enc) = f (enc TkEnd) 0 id

deriving instance Functor PP

instance Applicative PP where
  pure a  = PP (\toks ind ss -> Right (toks, ind, ss, a))
  (PP f) <*> (PP x) = PP $ \toks ind ss -> case f toks ind ss of
    Left s                     -> Left s
    Right (toks', ind',ss',f') -> case x toks' ind' ss' of
      Left s                          -> Left s
      Right (toks'', ind'', ss'', x') -> Right (toks'', ind'', ss'', f' x')

instance Monad PP where
  (PP f) >>= g = PP $ \toks ind ss -> case f toks ind ss of
    Left s -> Left s
    Right (toks', ind', ss', x) -> let PP g' = g x
      in g' toks' ind' ss'
  return = pure
  fail s = PP $ \_ _ _ -> Left s


indent :: PP ()
indent = PP (\toks ind ss -> Right (toks,ind,ss . (replicate ind ' ' ++),()))

nl :: PP ()
nl = PP (\toks ind ss -> Right (toks,ind,ss . ('\n':), ()))

inc :: Int -> PP ()
inc i = PP (\toks ind ss -> Right (toks,ind+i,ss,()))

dec :: Int -> PP ()
dec i = inc (-i)

getTerm :: PP Tokens
getTerm = PP $ \toks ind ss ->
  case unconsToken toks of
    Just (tk,rest) -> Right (rest,ind,ss,tk)
    Nothing -> Left "getTok: Unexpected end of input"

peekTerm :: PP Tokens
peekTerm = PP $ \toks ind ss ->
  case unconsToken toks of
    Just (tk,_) -> Right (toks,ind,ss,tk)
    Nothing -> Left "peekTerm: Unexpected end of input"

str :: String -> PP ()
str s = PP $ \toks ind ss -> Right (toks,ind,ss . (s++),())

shown :: Show a => a -> PP ()
shown x = PP $ \toks ind ss -> Right (toks,ind,ss . shows x,())

parens :: PP a -> PP a
parens pp = str "(" *> pp <* str ")"

indef :: PP ()
indef = do
  tk <- peekTerm
  case tk of
    TkBreak TkEnd -> dec 3 >> pprint
    _ -> pprint >> indef


pprint :: PP ()
pprint = do
  nl
  indent
  term <- getTerm
  hexRep term
  str "  "
  case term of
    TkInt      i  TkEnd -> ppTkInt i
    TkInteger  i  TkEnd -> ppTkInteger i
    TkBytes    bs TkEnd -> ppTkBytes bs
    TkBytesBegin  TkEnd -> ppTkBytesBegin
    TkString   t  TkEnd -> ppTkString t
    TkStringBegin TkEnd -> ppTkStringBegin
    TkListLen  w  TkEnd -> ppTkListLen w
    TkListBegin   TkEnd -> ppTkListBegin
    TkMapLen   w  TkEnd -> ppTkMapLen w
    TkMapBegin    TkEnd -> ppTkMapBegin
    TkBreak       TkEnd -> ppTkBreak
    TkTag      w  TkEnd -> ppTkTag w
    TkBool     b  TkEnd -> ppTkBool b
    TkNull        TkEnd -> ppTkNull
    TkSimple   w  TkEnd -> ppTkSimple w
    TkFloat16  f  TkEnd -> ppTkFloat16 f
    TkFloat32  f  TkEnd -> ppTkFloat32 f
    TkFloat64  f  TkEnd -> ppTkFloat64 f
    _ -> fail $ unwords ["pprint: Unexpected token:", show term]

ppTkInt        :: Int        -> PP ()
ppTkInt i = str "# int" >> parens (shown i)

ppTkInteger    :: Integer    -> PP ()
ppTkInteger i = str "# integer" >> parens (shown i)

ppTkBytes      :: S.ByteString -> PP ()
ppTkBytes bs = str "# bytes" >> parens (shown (S.length bs))

ppTkBytesBegin ::               PP ()
ppTkBytesBegin = str "# bytes(*)" >> inc 3 >> indef

ppTkString     :: T.Text       -> PP ()
ppTkString t = str "# text" >> parens (shown t)

ppTkStringBegin::               PP ()
ppTkStringBegin = str "# text(*)" >> inc 3 >> indef

ppTkListLen    :: Word       -> PP ()
ppTkListLen n = do
  str "# list"
  parens (shown n)
  inc 3
  replicateM_ (fromIntegral n) pprint
  dec 3

ppTkListBegin  ::               PP ()
ppTkListBegin = str "# list(*)" >> inc 3 >> indef

ppTkMapLen     :: Word       -> PP ()
ppTkMapLen w = do
  str "# map"
  parens (shown w)
  inc 3
  replicateM_ (fromIntegral w) (pprint >> pprint)
  dec 3

ppTkMapBegin   ::               PP ()
ppTkMapBegin = str "# map(*)" >> inc 3

ppTkBreak      ::               PP ()
ppTkBreak = str "# break"

ppTkTag        :: Word     -> PP ()
ppTkTag w = do
  str "# tag"
  parens (shown w)
  inc 3
  pprint
  dec 3

ppTkBool       :: Bool       -> PP ()
ppTkBool True = str "# bool" >> parens (str "true")
ppTkBool False = str "# bool" >> parens (str "false")

ppTkNull       ::               PP ()
ppTkNull = str "# null"

ppTkSimple     :: Word8      -> PP ()
ppTkSimple w = str "# simple" >> parens (shown w)

ppTkFloat16    :: Float      -> PP ()
ppTkFloat16 f = str "# float16" >> parens (shown f)

ppTkFloat32    :: Float      -> PP ()
ppTkFloat32 f = str "# float32" >> parens (shown f)

ppTkFloat64    :: Double     -> PP ()
ppTkFloat64 f = str "# float64" >> parens (shown f)

unconsToken :: Tokens -> Maybe (Tokens, Tokens)
unconsToken TkEnd = Nothing
unconsToken (TkWord w tks)      = Just (TkWord w      TkEnd,tks)
unconsToken (TkWord64 w tks)    = Just (TkWord64 w    TkEnd,tks)
unconsToken (TkInt i tks)       = Just (TkInt i       TkEnd,tks)
unconsToken (TkInt64 i tks)     = Just (TkInt64 i     TkEnd,tks)
unconsToken (TkBytes bs tks)    = Just (TkBytes bs    TkEnd,tks)
unconsToken (TkBytesBegin tks)  = Just (TkBytesBegin  TkEnd,tks)
unconsToken (TkString t tks)    = Just (TkString t    TkEnd,tks)
unconsToken (TkStringBegin tks) = Just (TkStringBegin TkEnd,tks)
unconsToken (TkListLen len tks) = Just (TkListLen len TkEnd,tks)
unconsToken (TkListBegin tks)   = Just (TkListBegin   TkEnd,tks)
unconsToken (TkMapLen len tks)  = Just (TkMapLen len  TkEnd,tks)
unconsToken (TkMapBegin tks)    = Just (TkMapBegin    TkEnd,tks)
unconsToken (TkTag w tks)       = Just (TkTag w       TkEnd,tks)
unconsToken (TkTag64 w64 tks)   = Just (TkTag64 w64   TkEnd,tks)
unconsToken (TkInteger i tks)   = Just (TkInteger i   TkEnd,tks)
unconsToken (TkNull tks)        = Just (TkNull        TkEnd,tks)
unconsToken (TkUndef tks)       = Just (TkUndef       TkEnd,tks)
unconsToken (TkBool b tks)      = Just (TkBool b      TkEnd,tks)
unconsToken (TkSimple w8 tks)   = Just (TkSimple w8   TkEnd,tks)
unconsToken (TkFloat16 f16 tks) = Just (TkFloat16 f16 TkEnd,tks)
unconsToken (TkFloat32 f32 tks) = Just (TkFloat32 f32 TkEnd,tks)
unconsToken (TkFloat64 f64 tks) = Just (TkFloat64 f64 TkEnd,tks)
unconsToken (TkBreak tks)       = Just (TkBreak       TkEnd,tks)

hexRep :: Tokens -> PP ()
hexRep tk = PP $ \toks ind ss ->
  Right (toks, ind, ss . hexBS (toStrictByteString (Encoding (const tk))),())

hexBS :: S.ByteString -> ShowS
hexBS = foldr (.) id . map (\n -> ((if n < 16 then ('0':) else id) . showHex n . (' ':))) . S.unpack
