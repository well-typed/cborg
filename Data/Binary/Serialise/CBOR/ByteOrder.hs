{-# LANGUAGE CPP #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE ForeignFunctionInterface #-}

-- |
-- Module      : Data.Binary.Serialise.CBOR.ByteOrder
-- Copyright   : (c) Duncan Coutts 2015
-- License     : BSD3-style (see LICENSE.txt)
--
-- Maintainer  : duncan@community.haskell.org
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Lorem ipsum...
--
module Data.Binary.Serialise.CBOR.ByteOrder where

#include "MachDeps.h"

#if __GLASHOW_HASKELL >= 710
#define HAVE_BYTESWAP_PRIMOPS
#endif

#if i386_HOST_ARCH || x86_64_HOST_ARCH
#define MEM_UNALIGNED_OPS
#endif

#if WORD_SIZE_IN_BITS == 64
#define ARCH_64bit
#elif WORD_SIZE_IN_BITS == 32
#else
#error expected WORD_SIZE_IN_BITS to be 32 or 64
#endif

import           GHC.Exts
import           GHC.Word
import           Foreign.C.Types
import           Foreign.Ptr
import           GHC.ForeignPtr
#if !defined(HAVE_BYTESWAP_PRIMOPS) || !defined(MEM_UNALIGNED_OPS)
import           Data.Bits ((.|.), shiftL)
#endif
#if !defined(ARCH_64bit)
import           GHC.IntWord64 (wordToWord64#)
#endif
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Internal as BS



{-# INLINE grabWord8 #-}
grabWord8 :: Ptr () -> Word
grabWord8 (Ptr ip#) =
    W# (indexWord8OffAddr# ip# 0#)

#if defined(HAVE_BYTESWAP_PRIMOPS) && \
    defined(MEM_UNALIGNED_OPS) && \
   !defined(WORDS_BIGENDIAN)

{-# INLINE grabWord16 #-}
grabWord16 :: Ptr () -> Word
grabWord16 (Ptr ip#) =
    W# (byteSwap16# (indexWord16OffAddr# ip# 0#))

{-# INLINE grabWord32 #-}
grabWord32 :: Ptr () -> Word
grabWord32 (Ptr ip#) =
    W# (byteSwap32# (indexWord32OffAddr# ip# 0#))

{-# INLINE grabWord64 #-}
grabWord64 :: Ptr () -> Word64
grabWord64 (Ptr ip#) =
    W64# (byteSwap64# (indexWord64OffAddr# ip# 0#))

#elif defined(MEM_UNALIGNED_OPS) && \
      defined(WORDS_BIGENDIAN)

{-# INLINE grabWord16 #-}
grabWord16 :: Ptr () -> Word
grabWord16 (Ptr ip#) =
    W# (indexWord16OffAddr# ip# 0#)

{-# INLINE grabWord32 #-}
grabWord32 :: Ptr () -> Word
grabWord32 (Ptr ip#) =
    W# (indexWord32OffAddr# ip# 0#)

{-# INLINE grabWord64 #-}
grabWord64 :: Ptr () -> Word64
grabWord64 (Ptr ip#) =
    W64# (indexWord64OffAddr# ip# 0#)

#else
-- fallback version:

{-# INLINE grabWord16 #-}
grabWord16 :: Ptr () -> Word
grabWord16 (Ptr ip#) =
    case indexWord8OffAddr# ip# 0# of
     w0# ->
      case indexWord8OffAddr# ip# 1# of
       w1# -> W# w0# `shiftL` 8 .|.
              W# w1#

{-# INLINE grabWord32 #-}
grabWord32 :: Ptr () -> Word
grabWord32 (Ptr ip#) =
    case indexWord8OffAddr# ip# 0# of
     w0# ->
      case indexWord8OffAddr# ip# 1# of
       w1# ->
        case indexWord8OffAddr# ip# 2# of
         w2# ->
          case indexWord8OffAddr# ip# 3# of
           w3# -> W# w0# `shiftL` 24 .|.
                  W# w1# `shiftL` 16 .|.
                  W# w2# `shiftL`  8 .|.
                  W# w3#

{-# INLINE grabWord64 #-}
grabWord64 :: Ptr () -> Word64
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
                   w7# -> w w0# `shiftL` 56 .|.
                          w w1# `shiftL` 48 .|.
                          w w2# `shiftL` 40 .|.
                          w w3# `shiftL` 32 .|.
                          w w4# `shiftL` 24 .|.
                          w w5# `shiftL` 16 .|.
                          w w6# `shiftL`  8 .|.
                          w w7#
  where
#ifdef ARCH_64bit
    w w# = W64# w#
#else
    w w# = W64# (wordToWord64# w#)
#endif

#endif

{-# INLINE withBsPtr #-}
withBsPtr :: (Ptr b -> a) -> ByteString -> a
withBsPtr f (BS.PS (ForeignPtr addr# _fpc) off _len) = f (Ptr addr# `plusPtr` off)

{-# INLINE unsafeHead #-}
unsafeHead :: ByteString -> Word8
unsafeHead (BS.PS (ForeignPtr addr# _fpc) (I# off#) _len) =
    W8# (indexWord8OffAddr# addr# off#)

--
-- Half floats
--

{-# INLINE wordToFloat16 #-}
wordToFloat16 :: Word -> Float
wordToFloat16 = halfToFloat . fromIntegral

{-# INLINE floatToWord16 #-}
floatToWord16 :: Float -> Word16
floatToWord16 = fromIntegral . floatToHalf

foreign import ccall unsafe "hs_binary_halfToFloat"
  halfToFloat :: CUShort -> Float

foreign import ccall unsafe "hs_binary_floatToHalf"
  floatToHalf :: Float -> CUShort


--
-- Casting words to floats
--

-- We have to go via a word rather than reading directly from memory because of
-- endian issues. A little endian machine cannot read a big-endian float direct
-- from memory, so we read a word, bswap it and then convert to float.

-- Currently there are no primops for casting word <-> float, see
-- https://ghc.haskell.org/trac/ghc/ticket/4092

-- In this implementation, we're avoiding doing the extra indirection (and
-- closure allocation) of the runSTRep stuff, but we have to be very careful
-- here, we cannot allow the "constsant" newByteArray# 8# realWorld# to be
-- floated out and shared and aliased across multiple concurrent calls. So we
-- do manual worker/wrapper with the worker not being inlined.

{-# INLINE wordToFloat32 #-}
wordToFloat32 :: Word -> Float
wordToFloat32 (W# w#) = F# (wordToFloat32# w#)

{-# NOINLINE wordToFloat32# #-}
wordToFloat32# :: Word# -> Float#
wordToFloat32# w# =
    case newByteArray# 4# realWorld# of
      (# s', mba# #) ->
        case writeWord32Array# mba# 0# w# s' of
          s'' ->
            case readFloatArray# mba# 0# s'' of
              (# _, f# #) -> f#

{-# INLINE wordToFloat64 #-}
wordToFloat64 :: Word64 -> Double
wordToFloat64 (W64# w#) = D# (wordToFloat64# w#)

{-# NOINLINE wordToFloat64# #-}
#ifdef ARCH_64bit
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



-- Alternative impl that goes via the FFI
{-
{-# INLINE wordToFloat32 #-}
wordToFloat32 :: Word -> Float
wordToFloat32 = toFloat

{-# INLINE wordToFloat64 #-}
wordToFloat64 :: Word64 -> Double
wordToFloat64 = toFloat

{-# INLINE toFloat #-}
toFloat :: (Storable word, Storable float) => word -> float
toFloat w =
    unsafeDupablePerformIO $ alloca $ \buf -> do
      poke (castPtr buf) w
      peek buf
-}
