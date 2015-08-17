{-# LANGUAGE CPP, MagicHash, UnboxedTuples, ForeignFunctionInterface #-}
module Data.Binary.Serialise.CBOR.ByteOrder where

import           GHC.Exts
import           GHC.Word
import           Foreign.C.Types
import           Foreign.Ptr
import           GHC.ForeignPtr

import           Data.ByteString (ByteString)
import qualified Data.ByteString.Internal as BS

{-# INLINE grabWord8 #-}
grabWord8 :: Ptr () -> Word
grabWord8 (Ptr ip#) =
    W# (indexWord8OffAddr# ip# 0#)

--TODO: implementations for big endian
--TODO: implementations for systems that don't do unaligned memory ops

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

{-# INLINE withBsPtr #-}
withBsPtr :: (Ptr b -> a) -> ByteString -> a
withBsPtr f (BS.PS (ForeignPtr addr# _fpc) off _len) = f (Ptr addr# `plusPtr` off)

{-# INLINE unsafeHead #-}
unsafeHead :: ByteString -> Word8
unsafeHead (BS.PS (ForeignPtr addr# _fpc) (I# off#) _len) =
    W8# (indexWord8OffAddr# addr# off#)


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


-- Casting words to floats. We have to go via a word because of the endian
-- issues.

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
wordToFloat64# :: Word# -> Double#
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
