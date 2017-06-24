{-# LANGUAGE BangPatterns             #-}
{-# LANGUAGE CPP                      #-}
{-# LANGUAGE MagicHash                #-}
{-# LANGUAGE UnboxedTuples            #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE ScopedTypeVariables      #-}

-- |
-- Module      : Serialise.Cborg.Magic
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
module Serialise.Cborg.Magic
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
  , copyByteStringToPrimVector
  , copyByteStringToByteArray
  , copyPrimVectorToByteString
  , copyByteArrayToByteString
  ) where

#include "cbor.h"

import           GHC.Exts
import           GHC.ST (ST(ST))
import           GHC.IO (IO(IO), unsafeDupablePerformIO)
import           GHC.Word
import           Foreign.Ptr

#if defined(OPTIMIZE_GMP)
import qualified GHC.Integer.GMP.Internals      as Gmp
#endif

import           Data.ByteString (ByteString)
import qualified Data.ByteString          as BS
import qualified Data.ByteString.Internal as BS
import qualified Data.ByteString.Unsafe   as BS
import           Data.Primitive.ByteArray as Prim
import           Data.Primitive.Types     as Prim
import           Data.Vector.Primitive    as Prim

import           Foreign.ForeignPtr (withForeignPtr)

import qualified Numeric.Half as Half

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

grabWord16 (Ptr ip#) = W#   (narrow16Word# (byteSwap16# (indexWord16OffAddr# ip# 0#)))
grabWord32 (Ptr ip#) = W#   (narrow32Word# (byteSwap32# (indexWord32OffAddr# ip# 0#)))
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

-- | Take the tail of a @'ByteString'@ (i.e. drop the first byte) and read the
-- resulting byte(s) as an 8-bit word value. The input @'ByteString'@ MUST be at
-- least 2 bytes long: one byte to drop from the front, and one to read as a
-- @'Word'@ value. This is not checked, and failure to ensure this will result
-- in undefined behavior.
eatTailWord8 :: ByteString -> Word
eatTailWord8 xs = withBsPtr grabWord8 (BS.unsafeTail xs)
{-# INLINE eatTailWord8 #-}

-- | Take the tail of a @'ByteString'@ (i.e. drop the first byte) and read the
-- resulting byte(s) as a 16-bit word value. The input @'ByteString'@ MUST be at
-- least 3 bytes long: one byte to drop from the front, and two to read as a
-- 16-bit @'Word'@ value. This is not checked, and failure to ensure this will
-- result in undefined behavior.
eatTailWord16 :: ByteString -> Word
eatTailWord16 xs = withBsPtr grabWord16 (BS.unsafeTail xs)
{-# INLINE eatTailWord16 #-}

-- | Take the tail of a @'ByteString'@ (i.e. drop the first byte) and read the
-- resulting byte(s) as a 32-bit word value. The input @'ByteString'@ MUST be at
-- least 5 bytes long: one byte to drop from the front, and four to read as a
-- 32-bit @'Word'@ value. This is not checked, and failure to ensure this will
-- result in undefined behavior.
eatTailWord32 :: ByteString -> Word
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
wordToFloat16 = \x -> Half.fromHalf (Half.Half (fromIntegral x))
{-# INLINE wordToFloat16 #-}

-- | Convert a half-sized @'Float'@ to a @'Word'@.
floatToWord16 :: Float -> Word16
floatToWord16 = \x -> fromIntegral (Half.getHalf (Half.toHalf x))
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

-- | Copy a @'BS.ByteString'@ and create a primitive @'Prim.Vector'@ from it.
copyByteStringToPrimVector :: forall a. Prim a => BS.ByteString -> Prim.Vector a
copyByteStringToPrimVector bs =
    Prim.Vector 0 len (copyByteStringToByteArray bs)
  where
    len = BS.length bs `div` I# (Prim.sizeOf# (undefined :: a))

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

-- | Copy a @'Prim.Vector'@ and create a @'BS.ByteString'@ from it.
copyPrimVectorToByteString :: forall a. Prim a => Prim.Vector a -> BS.ByteString
copyPrimVectorToByteString (Prim.Vector off len ba) =
    copyByteArrayToByteString ba (off * s) (len * s)
  where
    s = I# (Prim.sizeOf# (undefined :: a))

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
