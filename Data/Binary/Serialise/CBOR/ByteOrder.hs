{-# LANGUAGE CPP                      #-}
{-# LANGUAGE MagicHash                #-}
{-# LANGUAGE UnboxedTuples            #-}
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
-- An internal module for doing efficient, fiddly, low-level byte buffoonery
-- and other nonsense.
--
module Data.Binary.Serialise.CBOR.ByteOrder
  ( -- * Word utilities
    grabWord8         -- :: Ptr () -> Word
  , grabWord16        -- :: Ptr () -> Word
  , grabWord32        -- :: Ptr () -> Word
  , grabWord64        -- :: Ptr () -> Word64

    -- * @'ByteString'@ utilities
  , withBsPtr         -- :: (Ptr b -> a) -> ByteString -> a

    -- * Half-floats
  , wordToFloat16     -- :: Word  -> Float
  , floatToWord16     -- :: Float -> Word16

    -- * Float/Word conversion
  , wordToFloat32     -- :: Word   -> Float
  , wordToFloat64     -- :: Word64 -> Double
  ) where

#include "cbor.h"

import           GHC.Exts
import           GHC.Word
import           Foreign.C.Types
import           Foreign.Ptr

import           Data.ByteString (ByteString)
import qualified Data.ByteString.Internal as BS

import           Foreign.ForeignPtr (withForeignPtr)

#if !MIN_VERSION_bytestring(0,10,6)
import           System.IO.Unsafe (unsafeDupablePerformIO)
#endif

#if !defined(HAVE_BYTESWAP_PRIMOPS) || !defined(MEM_UNALIGNED_OPS)
import           Data.Bits ((.|.), unsafeShiftL)

#if defined(ARCH_32bit)
import           GHC.IntWord64 (wordToWord64#)
#endif
#endif

--------------------------------------------------------------------------------

-- | Grab a 8-bit @'Word'@ given a @'Ptr'@ to some address.
grabWord8 :: Ptr () -> Word
{-# INLINE grabWord8 #-}

-- | Grab a 16-bit @'Word'@ given a @'Ptr'@ to some address.
grabWord16 :: Ptr () -> Word
{-# INLINE grabWord16 #-}

-- | Grab a 32-bit @'Word'@ given a @'Ptr'@ to some address.
grabWord32 :: Ptr () -> Word
{-# INLINE grabWord32 #-}

-- | Grab a 64-bit @'Word64'@ given a @'Ptr'@ to some address.
grabWord64 :: Ptr () -> Word64
{-# INLINE grabWord64 #-}

--
-- Machine-dependent implementation
--

-- 8-bit word case is always the same...
grabWord8 (Ptr ip#) = W# (indexWord8OffAddr# ip# 0#)

-- ... but the remaining cases arent
#if defined(HAVE_BYTESWAP_PRIMOPS) && \
    defined(MEM_UNALIGNED_OPS) && \
   !defined(WORDS_BIGENDIAN)
-- On x86 machines with GHC 7.10, we have byteswap primitives
-- available to make this conversion very fast.

grabWord16 (Ptr ip#) = W#   (byteSwap16# (indexWord16OffAddr# ip# 0#))
grabWord32 (Ptr ip#) = W#   (byteSwap32# (indexWord32OffAddr# ip# 0#))
grabWord64 (Ptr ip#) = W64# (byteSwap64# (indexWord64OffAddr# ip# 0#))

#elif defined(MEM_UNALIGNED_OPS) && \
      defined(WORDS_BIGENDIAN)
-- In some theoretical future-verse where there are unaligned memory
-- accesses on the machine, but it is also big-endian, we need to be
-- able to decode these numbers efficiently, still.

grabWord16 (Ptr ip#) = W#   (indexWord16OffAddr# ip# 0#)
grabWord32 (Ptr ip#) = W#   (indexWord32OffAddr# ip# 0#)
grabWord64 (Ptr ip#) = W64# (indexWord64OffAddr# ip# 0#)

#else
-- Otherwise, we fall back to the much slower, inefficient case
-- of writing out each of the 8 bits of the output word at
-- a time.

grabWord16 (Ptr ip#) =
    case indexWord8OffAddr# ip# 0# of
     w0# ->
      case indexWord8OffAddr# ip# 1# of
       w1# -> W# w0# `unsafeShiftL` 8 .|.
              W# w1#

grabWord32 (Ptr ip#) =
    case indexWord8OffAddr# ip# 0# of
     w0# ->
      case indexWord8OffAddr# ip# 1# of
       w1# ->
        case indexWord8OffAddr# ip# 2# of
         w2# ->
          case indexWord8OffAddr# ip# 3# of
           w3# -> W# w0# `unsafeShiftL` 24 .|.
                  W# w1# `unsafeShiftL` 16 .|.
                  W# w2# `unsafeShiftL`  8 .|.
                  W# w3#

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

-- | Unsafely take a @'Ptr'@ to a @'ByteString'@ and do unholy things
-- with it.
withBsPtr :: (Ptr b -> a) -> ByteString -> a
withBsPtr f (BS.PS x off _) =
#if MIN_VERSION_bytestring(0,10,6)
    BS.accursedUnutterablePerformIO $ withForeignPtr x $
        \(Ptr addr#) -> return $! (f (Ptr addr# `plusPtr` off))
#else
    unsafeDupablePerformIO $ withForeignPtr x $
        \(Ptr addr#) -> return $! (f (Ptr addr# `plusPtr` off))
#endif
{-# INLINE withBsPtr #-}

--------------------------------------------------------------------------------
-- Half floats

-- | Convert a @'Word'@ to a half-sized @'Float'@.
wordToFloat16 :: Word -> Float
wordToFloat16 = halfToFloat . fromIntegral
{-# INLINE wordToFloat16 #-}

-- | Convert a half-sized @'Float'@ to a @'Word'@.
floatToWord16 :: Float -> Word16
floatToWord16 = fromIntegral . floatToHalf
{-# INLINE floatToWord16 #-}

foreign import ccall unsafe "hs_binary_halfToFloat"
  halfToFloat :: CUShort -> Float

foreign import ccall unsafe "hs_binary_floatToHalf"
  floatToHalf :: Float -> CUShort

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

-- | Cast a @'Word'@ to a @'Float'@.
wordToFloat32 :: Word -> Float
wordToFloat32 (W# w#) = F# (wordToFloat32# w#)
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


-- Alternative impl that goes via the FFI
--
-- wordToFloat32 :: Word -> Float
-- wordToFloat32 = toFloat
-- {-# INLINE wordToFloat32 #-}
--
-- wordToFloat64 :: Word64 -> Double
-- wordToFloat64 = toFloat
-- {-# INLINE wordToFloat64 #-}
--
-- toFloat :: (Storable word, Storable float) => word -> float
-- toFloat w = unsafeDupablePerformIO $ alloca $ \buf -> do
--   poke (castPtr buf) w
--   peek buf
-- {-# INLINE toFloat #-}
