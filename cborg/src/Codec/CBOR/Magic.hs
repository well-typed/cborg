{-# LANGUAGE BangPatterns             #-}
{-# LANGUAGE CPP                      #-}
{-# LANGUAGE MagicHash                #-}
{-# LANGUAGE UnboxedTuples            #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE ScopedTypeVariables      #-}

#include "cbor.h"

-- |
-- Module      : Codec.CBOR.Magic
-- Copyright   : (c) Duncan Coutts 2015-2017
-- License     : BSD3-style (see LICENSE.txt)
--
-- Maintainer  : duncan@community.haskell.org
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- An internal module for doing magical, low-level, and unholy things
-- in the name of efficiency.
--
module Codec.CBOR.Magic
  ( -- * Word utilities
    grabWord8         -- :: Ptr () -> Word
  , grabWord16        -- :: Ptr () -> Word
  , grabWord32        -- :: Ptr () -> Word
  , grabWord64        -- :: Ptr () -> Word64

    -- * @'ByteString'@ utilities
  , eatTailWord8      -- :: ByteString -> Word
  , eatTailWord16     -- :: ByteString -> Word
  , eatTailWord32     -- :: ByteString -> Word
  , eatTailWord64     -- :: ByteString -> Word64

    -- * Half-floats
  , wordToFloat16     -- :: Word  -> Float
  , floatToWord16     -- :: Float -> Word16

    -- * Float\/Word conversion
  , wordToFloat32     -- :: Word   -> Float
  , wordToFloat64     -- :: Word64 -> Double

    -- * Int and Word explicit conversions
  , word8ToWord       -- :: Word8  -> Word
  , word16ToWord      -- :: Word16 -> Word
  , word32ToWord      -- :: Word32 -> Word
  , word64ToWord      -- :: Word64 -> Word

  -- int*ToInt conversions are missing because they are not needed.

  , word8ToInt        -- :: Int8  -> Int
  , word16ToInt       -- :: Int16 -> Int
  , word32ToInt       -- :: Int32 -> Int
  , word64ToInt       -- :: Int64 -> Int

  , intToInt64        -- :: Int   -> Int64
#if defined(ARCH_32bit)
  , word8ToInt64      -- :: Word8  -> Int64
  , word16ToInt64     -- :: Word16 -> Int64
  , word32ToInt64     -- :: Word32 -> Int64
  , word64ToInt64     -- :: Word64 -> Maybe Int64

  , word8ToWord64     -- :: Word8  -> Word64
  , word16ToWord64    -- :: Word16 -> Word64
  , word32ToWord64    -- :: Word32 -> Word64
#endif

    -- * @'Integer'@ utilities
  , nintegerFromBytes -- :: ByteString -> Integer
  , uintegerFromBytes -- :: ByteString -> Integer

    -- * Simple mutable counters
  , Counter           -- :: * -> *
  , newCounter        -- :: Int -> ST s (Counter s)
  , readCounter       -- :: Counter s -> ST s Int
  , writeCounter      -- :: Counter s -> Int -> ST s ()
  , incCounter        -- :: Counter s -> ST s ()
  , decCounter        -- :: Counter s -> ST s ()

    -- * Array support
  , copyByteStringToByteArray
  , copyByteArrayToByteString
  ) where

import           GHC.Exts
import           GHC.ST (ST(ST))
import           GHC.IO (IO(IO), unsafeDupablePerformIO)
import           GHC.Word
import           GHC.Int
import           Foreign.Ptr

#if defined(OPTIMIZE_GMP)
import qualified GHC.Integer.GMP.Internals      as Gmp
#endif

import           Data.ByteString (ByteString)
import qualified Data.ByteString          as BS
import qualified Data.ByteString.Internal as BS
import qualified Data.ByteString.Unsafe   as BS
import           Data.Primitive.ByteArray as Prim

import           Foreign.ForeignPtr (withForeignPtr)
import           Foreign.C (CUShort)

import qualified Numeric.Half as Half

#if !defined(HAVE_BYTESWAP_PRIMOPS) || !defined(MEM_UNALIGNED_OPS)
import           Data.Bits ((.|.), unsafeShiftL)
#endif

#if defined(ARCH_32bit)
import           GHC.IntWord64 (wordToWord64#, word64ToWord#,
                                intToInt64#, int64ToInt#,
                                leWord64#, ltWord64#, word64ToInt64#)

#endif

--------------------------------------------------------------------------------

-- | Grab a 8-bit @'Word'@ given a @'Ptr'@ to some address.
grabWord8 :: Ptr () -> Word8
{-# INLINE grabWord8 #-}

-- | Grab a 16-bit @'Word'@ given a @'Ptr'@ to some address.
grabWord16 :: Ptr () -> Word16
{-# INLINE grabWord16 #-}

-- | Grab a 32-bit @'Word'@ given a @'Ptr'@ to some address.
grabWord32 :: Ptr () -> Word32
{-# INLINE grabWord32 #-}

-- | Grab a 64-bit @'Word64'@ given a @'Ptr'@ to some address.
grabWord64 :: Ptr () -> Word64
{-# INLINE grabWord64 #-}

--
-- Machine-dependent implementation
--

-- 8-bit word case is always the same...
grabWord8 (Ptr ip#) = W8# (indexWord8OffAddr# ip# 0#)

-- ... but the remaining cases arent
#if defined(HAVE_BYTESWAP_PRIMOPS) && \
    defined(MEM_UNALIGNED_OPS) && \
   !defined(WORDS_BIGENDIAN)
-- On x86 machines with GHC 7.10, we have byteswap primitives
-- available to make this conversion very fast.

grabWord16 (Ptr ip#) = W16# (narrow16Word# (byteSwap16# (indexWord16OffAddr# ip# 0#)))
grabWord32 (Ptr ip#) = W32# (narrow32Word# (byteSwap32# (indexWord32OffAddr# ip# 0#)))
#if defined(ARCH_64bit)
grabWord64 (Ptr ip#) = W64# (byteSwap# (indexWord64OffAddr# ip# 0#))
#else
grabWord64 (Ptr ip#) = W64# (byteSwap64# (indexWord64OffAddr# ip# 0#))
#endif

#elif defined(MEM_UNALIGNED_OPS) && \
      defined(WORDS_BIGENDIAN)
-- In some theoretical future-verse where there are unaligned memory
-- accesses on the machine, but it is also big-endian, we need to be
-- able to decode these numbers efficiently, still.

grabWord16 (Ptr ip#) = W16# (indexWord16OffAddr# ip# 0#)
grabWord32 (Ptr ip#) = W32# (indexWord32OffAddr# ip# 0#)
grabWord64 (Ptr ip#) = W64# (indexWord64OffAddr# ip# 0#)

#else
-- Otherwise, we fall back to the much slower, inefficient case
-- of writing out each of the 8 bits of the output word at
-- a time.

grabWord16 (Ptr ip#) =
    case indexWord8OffAddr# ip# 0# of
     w0# ->
      case indexWord8OffAddr# ip# 1# of
       w1# -> W16# w0# `unsafeShiftL` 8 .|.
              W16# w1#

grabWord32 (Ptr ip#) =
    case indexWord8OffAddr# ip# 0# of
     w0# ->
      case indexWord8OffAddr# ip# 1# of
       w1# ->
        case indexWord8OffAddr# ip# 2# of
         w2# ->
          case indexWord8OffAddr# ip# 3# of
           w3# -> W32# w0# `unsafeShiftL` 24 .|.
                  W32# w1# `unsafeShiftL` 16 .|.
                  W32# w2# `unsafeShiftL`  8 .|.
                  W32# w3#

grabWord64 (Ptr ip#) =
    case indexWord8OffAddr# ip# 0# of
     w0# ->
      case indexWord8OffAddr# ip# 1# of
       w1# ->
        case indexWord8OffAddr# ip# 2# of
         w2# ->
          case indexWord8OffAddr# ip# 3# of
           w3# ->
            case indexWord8OffAddr# ip# 4# of
             w4# ->
              case indexWord8OffAddr# ip# 5# of
               w5# ->
                case indexWord8OffAddr# ip# 6# of
                 w6# ->
                  case indexWord8OffAddr# ip# 7# of
                   w7# -> w w0# `unsafeShiftL` 56 .|.
                          w w1# `unsafeShiftL` 48 .|.
                          w w2# `unsafeShiftL` 40 .|.
                          w w3# `unsafeShiftL` 32 .|.
                          w w4# `unsafeShiftL` 24 .|.
                          w w5# `unsafeShiftL` 16 .|.
                          w w6# `unsafeShiftL`  8 .|.
                          w w7#
  where
#if defined(ARCH_64bit)
    w w# = W64# w#
#else
    w w# = W64# (wordToWord64# w#)
#endif

#endif

--------------------------------------------------------------------------------
-- ByteString shennanigans

-- | Take the tail of a @'ByteString'@ (i.e. drop the first byte) and read the
-- resulting byte(s) as an 8-bit word value. The input @'ByteString'@ MUST be at
-- least 2 bytes long: one byte to drop from the front, and one to read as a
-- @'Word'@ value. This is not checked, and failure to ensure this will result
-- in undefined behavior.
eatTailWord8 :: ByteString -> Word8
eatTailWord8 xs = withBsPtr grabWord8 (BS.unsafeTail xs)
{-# INLINE eatTailWord8 #-}

-- | Take the tail of a @'ByteString'@ (i.e. drop the first byte) and read the
-- resulting byte(s) as a 16-bit word value. The input @'ByteString'@ MUST be at
-- least 3 bytes long: one byte to drop from the front, and two to read as a
-- 16-bit @'Word'@ value. This is not checked, and failure to ensure this will
-- result in undefined behavior.
eatTailWord16 :: ByteString -> Word16
eatTailWord16 xs = withBsPtr grabWord16 (BS.unsafeTail xs)
{-# INLINE eatTailWord16 #-}

-- | Take the tail of a @'ByteString'@ (i.e. drop the first byte) and read the
-- resulting byte(s) as a 32-bit word value. The input @'ByteString'@ MUST be at
-- least 5 bytes long: one byte to drop from the front, and four to read as a
-- 32-bit @'Word'@ value. This is not checked, and failure to ensure this will
-- result in undefined behavior.
eatTailWord32 :: ByteString -> Word32
eatTailWord32 xs = withBsPtr grabWord32 (BS.unsafeTail xs)
{-# INLINE eatTailWord32 #-}

-- | Take the tail of a @'ByteString'@ (i.e. drop the first byte) and read the
-- resulting byte(s) as a 64-bit word value. The input @'ByteString'@ MUST be at
-- least 9 bytes long: one byte to drop from the front, and eight to read as a
-- 64-bit @'Word64'@ value. This is not checked, and failure to ensure this will
-- result in undefined behavior.
eatTailWord64 :: ByteString -> Word64
eatTailWord64 xs = withBsPtr grabWord64 (BS.unsafeTail xs)
{-# INLINE eatTailWord64 #-}

-- | Unsafely take a @'Ptr'@ to a @'ByteString'@ and do unholy things
-- with it.
withBsPtr :: (Ptr b -> a) -> ByteString -> a
withBsPtr f (BS.PS x off _) =
    unsafeDupablePerformIO $ withForeignPtr x $
        \(Ptr addr#) -> return $! (f (Ptr addr# `plusPtr` off))
{-# INLINE withBsPtr #-}

--------------------------------------------------------------------------------
-- Half floats

-- | Convert a @'Word16'@ to a half-sized @'Float'@.
wordToFloat16 :: Word16 -> Float
wordToFloat16 = \x -> Half.fromHalf (Half.Half (cast x))
  where
    cast :: Word16 -> CUShort
    cast = fromIntegral
{-# INLINE wordToFloat16 #-}

-- | Convert a half-sized @'Float'@ to a @'Word'@.
floatToWord16 :: Float -> Word16
floatToWord16 = \x -> cast (Half.getHalf (Half.toHalf x))
  where
    cast :: CUShort -> Word16
    cast = fromIntegral
{-# INLINE floatToWord16 #-}

--------------------------------------------------------------------------------
-- Casting words to floats

-- We have to go via a word rather than reading directly from memory because of
-- endian issues. A little endian machine cannot read a big-endian float direct
-- from memory, so we read a word, bswap it and then convert to float.
--
-- Currently there are no primops for casting word <-> float, see
-- https://ghc.haskell.org/trac/ghc/ticket/4092
--
-- In this implementation, we're avoiding doing the extra indirection (and
-- closure allocation) of the runSTRep stuff, but we have to be very careful
-- here, we cannot allow the "constant" newByteArray# 8# realWorld# to be
-- floated out and shared and aliased across multiple concurrent calls. So we
-- do manual worker/wrapper with the worker not being inlined.

-- | Cast a @'Word32'@ to a @'Float'@.
wordToFloat32 :: Word32 -> Float
wordToFloat32 (W32# w#) = F# (wordToFloat32# w#)
{-# INLINE wordToFloat32 #-}

-- | Cast a @'Word64'@ to a @'Float'@.
wordToFloat64 :: Word64 -> Double
wordToFloat64 (W64# w#) = D# (wordToFloat64# w#)
{-# INLINE wordToFloat64 #-}

-- | Cast an unboxed word to an unboxed float.
wordToFloat32# :: Word# -> Float#
wordToFloat32# w# =
    case newByteArray# 4# realWorld# of
      (# s', mba# #) ->
        case writeWord32Array# mba# 0# w# s' of
          s'' ->
            case readFloatArray# mba# 0# s'' of
              (# _, f# #) -> f#
{-# NOINLINE wordToFloat32# #-}

-- | Cast an unboxed word to an unboxed double.
#if defined(ARCH_64bit)
wordToFloat64# :: Word# -> Double#
#else
wordToFloat64# :: Word64# -> Double#
#endif
wordToFloat64# w# =
    case newByteArray# 8# realWorld# of
      (# s', mba# #) ->
        case writeWord64Array# mba# 0# w# s' of
          s'' ->
            case readDoubleArray# mba# 0# s'' of
              (# _, f# #) -> f#
{-# NOINLINE wordToFloat64# #-}

--------------------------------------------------------------------------------
-- Casting words and ints

word8ToWord  :: Word8  -> Word
word16ToWord :: Word16 -> Word
word32ToWord :: Word32 -> Word
#if defined(ARCH_64bit)
word64ToWord :: Word64 -> Word
#else
word64ToWord :: Word64 -> Maybe Word
#endif

word8ToInt  :: Word8  -> Int
word16ToInt :: Word16 -> Int
#if defined(ARCH_64bit)
word32ToInt :: Word32 -> Int
#else
word32ToInt :: Word32 -> Maybe Int
#endif
word64ToInt :: Word64 -> Maybe Int

#if defined(ARCH_32bit)
word8ToInt64  :: Word8  -> Int64
word16ToInt64 :: Word16 -> Int64
word32ToInt64 :: Word32 -> Int64
word64ToInt64 :: Word64 -> Maybe Int64

word8ToWord64  :: Word8  -> Word64
word16ToWord64 :: Word16 -> Word64
word32ToWord64 :: Word32 -> Word64
#endif

intToInt64 :: Int -> Int64
intToInt64 = fromIntegral
{-# INLINE intToInt64 #-}

word8ToWord  (W8#  w#) = W# w#
word16ToWord (W16# w#) = W# w#
word32ToWord (W32# w#) = W# w#
#if defined(ARCH_64bit)
word64ToWord (W64# w#) = W# w#
#else
word64ToWord (W64# w64#) =
  case isTrue# (w64# `leWord64#` wordToWord64# 0xffffffff##) of
    True  -> Just (W# (word64ToWord# w64#))
    False -> Nothing
#endif

{-# INLINE word8ToWord #-}
{-# INLINE word16ToWord #-}
{-# INLINE word32ToWord #-}
{-# INLINE word64ToWord #-}

word8ToInt  (W8#  w#) = I# (word2Int# w#)
word16ToInt (W16# w#) = I# (word2Int# w#)

#if defined(ARCH_64bit)
word32ToInt (W32# w#) = I# (word2Int# w#)
#else
word32ToInt (W32# w#) =
  case isTrue# (w# `ltWord#` 0x80000000##) of
    True  -> Just (I# (word2Int# w#))
    False -> Nothing
#endif

#if defined(ARCH_64bit)
word64ToInt (W64# w#) =
  case isTrue# (w# `ltWord#` 0x8000000000000000##) of
    True  -> Just (I# (word2Int# w#))
    False -> Nothing
#else
word64ToInt (W64# w#) =
  case isTrue# (w# `ltWord64#` wordToWord64# 0x80000000##) of
    True  -> Just (I# (int64ToInt# (word64ToInt64# w#)))
    False -> Nothing
#endif

{-# INLINE word8ToInt #-}
{-# INLINE word16ToInt #-}
{-# INLINE word32ToInt #-}
{-# INLINE word64ToInt #-}

#if defined(ARCH_32bit)
word8ToInt64  (W8#  w#) = I64# (intToInt64# (word2Int# w#))
word16ToInt64 (W16# w#) = I64# (intToInt64# (word2Int# w#))
word32ToInt64 (W32# w#) = I64# (word64ToInt64# (wordToWord64# w#))
word64ToInt64 (W64# w#) =
  case isTrue# (w# `ltWord64#` uncheckedShiftL64# (wordToWord64# 1##) 63#) of
    True  -> Just (I64# (word64ToInt64# w#))
    False -> Nothing

word8ToWord64  (W8#  w#) = W64# (wordToWord64# w#)
word16ToWord64 (W16# w#) = W64# (wordToWord64# w#)
word32ToWord64 (W32# w#) = W64# (wordToWord64# w#)

{-# INLINE word8ToInt64  #-}
{-# INLINE word16ToInt64 #-}
{-# INLINE word32ToInt64 #-}
{-# INLINE word64ToInt64 #-}

{-# INLINE word8ToWord64  #-}
{-# INLINE word16ToWord64 #-}
{-# INLINE word32ToWord64 #-}
#endif

--------------------------------------------------------------------------------
-- Integer utilities

-- | Create a negative @'Integer'@ out of a raw @'BS.ByteString'@.
nintegerFromBytes :: BS.ByteString -> Integer
nintegerFromBytes bs = -1 - uintegerFromBytes bs

-- | Create an @'Integer'@ out of a raw @'BS.ByteString'@.
uintegerFromBytes :: BS.ByteString -> Integer

#if defined(OPTIMIZE_GMP)
uintegerFromBytes (BS.PS fp (I# off#) (I# len#)) =
  -- This should be safe since we're simply reading from ByteString (which is
  -- immutable) and GMP allocates a new memory for the Integer, i.e., there is
  -- no mutation involved.
  unsafeDupablePerformIO $
      withForeignPtr fp $ \(Ptr addr#) ->
          let addrOff# = addr# `plusAddr#` off#
          -- The last parmaeter (`1#`) tells the import function to use big
          -- endian encoding.
          in Gmp.importIntegerFromAddr addrOff# (int2Word# len#) 1#
#else
uintegerFromBytes bs =
    case BS.uncons bs of
      Nothing        -> 0
      Just (w0, ws0) -> go (fromIntegral w0) ws0
  where
    go !acc ws =
      case BS.uncons ws of
        Nothing       -> acc
        Just (w, ws') -> go (acc `unsafeShiftL` 8 + fromIntegral w) ws'
#endif

--------------------------------------------------------------------------------
-- Mutable counters

-- | An efficient, mutable counter. Designed to be used inside
-- @'ST'@ or other primitive monads, hence it carries an abstract
-- rank-2 @s@ type parameter.
data Counter s = Counter (MutableByteArray# s)

-- | Create a new counter with a starting @'Int'@ value.
newCounter :: Int -> ST s (Counter s)
newCounter (I# n#) =
    ST (\s ->
      case newByteArray# 8# s of
        (# s', mba# #) ->
          case writeIntArray# mba# 0# n# s' of
            s'' -> (# s'', Counter mba# #))
{-# INLINE newCounter   #-}

-- | Read the current value of a @'Counter'@.
readCounter :: Counter s -> ST s Int
readCounter (Counter mba#) =
    ST (\s ->
      case readIntArray# mba# 0# s of
        (# s', n# #) -> (# s', I# n# #))
{-# INLINE readCounter  #-}

-- | Write a new value into the @'Counter'@.
writeCounter :: Counter s -> Int -> ST s ()
writeCounter (Counter mba#) (I# n#) =
    ST (\s ->
      case writeIntArray# mba# 0# n# s of
        s' -> (# s', () #))
{-# INLINE writeCounter #-}

-- | Increment a @'Counter'@ by one.
incCounter :: Counter s -> ST s ()
incCounter c = do
  x <- readCounter c
  writeCounter c (x+1)
{-# INLINE incCounter #-}

-- | Decrement a @'Counter'@ by one.
decCounter :: Counter s -> ST s ()
decCounter c = do
  x <- readCounter c
  writeCounter c (x-1)
{-# INLINE decCounter #-}

--------------------------------------------------------------------------------
-- Array support

-- | Copy a @'BS.ByteString'@ and create a primitive @'Prim.ByteArray'@ from it.
copyByteStringToByteArray :: BS.ByteString -> Prim.ByteArray
copyByteStringToByteArray (BS.PS fp off len) =
    unsafeDupablePerformIO $
      withForeignPtr fp $ \ptr -> do
        mba <- Prim.newByteArray len
        copyPtrToMutableByteArray (ptr `plusPtr` off) mba 0 len
        Prim.unsafeFreezeByteArray mba

-- TODO FIXME: can do better here: can do non-copying for larger pinned arrays
-- or copy directly into the builder buffer

-- | Copy a @'Prim.ByteArray'@ at a certain offset and length into a
-- @'BS.ByteString'@.
copyByteArrayToByteString :: Prim.ByteArray
                          -- ^ @'Prim.ByteArray'@ to copy from.
                          -> Int
                          -- ^ Offset into the @'Prim.ByteArray'@ to start with.
                          -> Int
                          -- ^ Length of the data to copy.
                          -> BS.ByteString
copyByteArrayToByteString ba off len =
    unsafeDupablePerformIO $ do
      fp <- BS.mallocByteString len
      withForeignPtr fp $ \ptr -> do
        copyByteArrayToPtr ba off ptr len
        return (BS.PS fp 0 len)

-- | Copy the data pointed to by a @'Ptr'@ into a @'MutableByteArray'.
copyPtrToMutableByteArray :: Ptr a
                          -- ^ @'Ptr'@ to buffer to copy from.
                          -> MutableByteArray RealWorld
                          -- ^ @'MutableByteArray'@ to copy into.
                          -> Int
                          -- ^ Offset to start copying from.
                          -> Int
                          -- ^ Length of the data to copy.
                          -> IO ()
copyPtrToMutableByteArray (Ptr addr#) (MutableByteArray mba#) (I# off#) (I# len#) =
    IO (\s ->
      case copyAddrToByteArray# addr# mba# off# len# s of
        s' -> (# s', () #))

-- | Copy a @'ByteArray'@ into a @'Ptr'@ with a given offset and length.
copyByteArrayToPtr :: ByteArray
                   -- ^ @'ByteArray'@ to copy.
                   -> Int
                   -- ^ Offset into the @'ByteArray'@ of where to start copying.
                   -> Ptr a
                   -- ^ Pointer to destination buffer.
                   -> Int
                   -- ^ Length of the data to copy into the destination buffer.
                   -> IO ()
copyByteArrayToPtr (ByteArray ba#) (I# off#) (Ptr addr#) (I# len#) =
    IO (\s ->
      case copyByteArrayToAddr# ba# off# addr# len# s of
        s' -> (# s', () #))
