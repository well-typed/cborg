{-# LANGUAGE CPP          #-}
{-# LANGUAGE MagicHash    #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RankNTypes   #-}

-- |
-- Module      : Codec.CBOR.Decoding
-- Copyright   : (c) Duncan Coutts 2015-2017
-- License     : BSD3-style (see LICENSE.txt)
--
-- Maintainer  : duncan@community.haskell.org
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- High level API for decoding values that were encoded with the
-- "Codec.CBOR.Encoding" module, using a 'Monad'
-- based interface.
--
module Codec.CBOR.Decoding
  ( -- * Decode primitive operations
    Decoder
  , DecodeAction(..)
  , liftST
  , getDecodeAction

  -- ** Read input tokens
  , decodeWord          -- :: Decoder s Word
  , decodeWord8         -- :: Decoder s Word8
  , decodeWord16        -- :: Decoder s Word16
  , decodeWord32        -- :: Decoder s Word32
  , decodeWord64        -- :: Decoder s Word64
  , decodeNegWord       -- :: Decoder s Word
  , decodeNegWord64     -- :: Decoder s Word64
  , decodeInt           -- :: Decoder s Int
  , decodeInt8          -- :: Decoder s Int8
  , decodeInt16         -- :: Decoder s Int16
  , decodeInt32         -- :: Decoder s Int32
  , decodeInt64         -- :: Decoder s Int64
  , decodeInteger       -- :: Decoder s Integer
  , decodeFloat         -- :: Decoder s Float
  , decodeDouble        -- :: Decoder s Double
  , decodeBytes         -- :: Decoder s ByteString
  , decodeBytesIndef    -- :: Decoder s ()
  , decodeByteArray     -- :: Decoder s ByteArray
  , decodeString        -- :: Decoder s Text
  , decodeStringIndef   -- :: Decoder s ()
  , decodeUtf8ByteArray -- :: Decoder s ByteArray
  , decodeListLen       -- :: Decoder s Int
  , decodeListLenIndef  -- :: Decoder s ()
  , decodeMapLen        -- :: Decoder s Int
  , decodeMapLenIndef   -- :: Decoder s ()
  , decodeTag           -- :: Decoder s Word
  , decodeTag64         -- :: Decoder s Word64
  , decodeBool          -- :: Decoder s Bool
  , decodeNull          -- :: Decoder s ()
  , decodeSimple        -- :: Decoder s Word8

  -- ** Specialised Read input token operations
  , decodeWordOf        -- :: Word -> Decoder s ()
  , decodeListLenOf     -- :: Int  -> Decoder s ()

  -- ** Branching operations
--, decodeBytesOrIndef
--, decodeStringOrIndef
  , decodeListLenOrIndef -- :: Decoder s (Maybe Int)
  , decodeMapLenOrIndef  -- :: Decoder s (Maybe Int)
  , decodeBreakOr        -- :: Decoder s Bool

  -- ** Inspecting the token type
  , peekTokenType        -- :: Decoder s TokenType
  , TokenType(..)

  -- ** Special operations
  , peekAvailable        -- :: Decoder s Int
  , ByteOffset
  , peekByteOffset       -- :: Decoder s ByteOffset
  , decodeWithByteSpan

  -- ** Canonical CBOR
  -- $canonical
  , decodeWordCanonical      -- :: Decoder s Word
  , decodeWord8Canonical     -- :: Decoder s Word8
  , decodeWord16Canonical    -- :: Decoder s Word16
  , decodeWord32Canonical    -- :: Decoder s Word32
  , decodeWord64Canonical    -- :: Decoder s Word64
  , decodeNegWordCanonical   -- :: Decoder s Word
  , decodeNegWord64Canonical -- :: Decoder s Word64
  , decodeIntCanonical       -- :: Decoder s Int
  , decodeInt8Canonical      -- :: Decoder s Int8
  , decodeInt16Canonical     -- :: Decoder s Int16
  , decodeInt32Canonical     -- :: Decoder s Int32
  , decodeInt64Canonical     -- :: Decoder s Int64
  , decodeBytesCanonical -- :: Decoder s ByteString
  , decodeByteArrayCanonical -- :: Decoder s ByteArray
  , decodeStringCanonical -- :: Decoder s Text
  , decodeUtf8ByteArrayCanonical -- :: Decoder s ByteArray
  , decodeListLenCanonical -- :: Decoder s Int
  , decodeMapLenCanonical -- :: Decoder s Int
  , decodeTagCanonical   -- :: Decoder s Word
  , decodeTag64Canonical -- :: Decoder s Word64
  , decodeIntegerCanonical -- :: Decoder s Integer
  , decodeFloat16Canonical -- :: Decoder s Float
  , decodeFloatCanonical   -- :: Decoder s Float
  , decodeDoubleCanonical  -- :: Decoder s Double
  , decodeSimpleCanonical  -- :: Decoder s Word8
  , decodeWordCanonicalOf    -- :: Word -> Decoder s ()
  , decodeListLenCanonicalOf -- :: Int  -> Decoder s ()

  -- * Sequence operations
  , decodeSequenceLenIndef -- :: ...
  , decodeSequenceLenN     -- :: ...
  ) where

#include "cbor.h"

import           GHC.Exts
import           GHC.Word
import           GHC.Int
import           Data.Text (Text)
import           Data.ByteString (ByteString)
import           Control.Applicative
import           Control.Monad.ST
import qualified Control.Monad.Fail as Fail

import           Codec.CBOR.ByteArray (ByteArray)

import           Prelude hiding (decodeFloat)


-- | A continuation-based decoder, used for decoding values that were
-- previously encoded using the "Codec.CBOR.Encoding"
-- module. As 'Decoder' has a 'Monad' instance, you can easily
-- write 'Decoder's monadically for building your deserialisation
-- logic.
--
-- @since 0.2.0.0
newtype Decoder s a = Decoder {
       runDecoder :: forall r. (a -> ST s (DecodeAction s r)) -> ST s (DecodeAction s r)
     }

-- | An action, representing a step for a decoder to taken and a
-- continuation to invoke with the expected value.
--
-- @since 0.2.0.0
data DecodeAction s a
    = ConsumeWord    (Word# -> ST s (DecodeAction s a))
    | ConsumeWord8   (Word# -> ST s (DecodeAction s a))
    | ConsumeWord16  (Word# -> ST s (DecodeAction s a))
    | ConsumeWord32  (Word# -> ST s (DecodeAction s a))
    | ConsumeNegWord (Word# -> ST s (DecodeAction s a))
    | ConsumeInt     (Int#  -> ST s (DecodeAction s a))
    | ConsumeInt8    (Int#  -> ST s (DecodeAction s a))
    | ConsumeInt16   (Int#  -> ST s (DecodeAction s a))
    | ConsumeInt32   (Int#  -> ST s (DecodeAction s a))
    | ConsumeListLen (Int#  -> ST s (DecodeAction s a))
    | ConsumeMapLen  (Int#  -> ST s (DecodeAction s a))
    | ConsumeTag     (Word# -> ST s (DecodeAction s a))

-- 64bit variants for 32bit machines
#if defined(ARCH_32bit)
    | ConsumeWord64    (Word64# -> ST s (DecodeAction s a))
    | ConsumeNegWord64 (Word64# -> ST s (DecodeAction s a))
    | ConsumeInt64     (Int64#  -> ST s (DecodeAction s a))
    | ConsumeListLen64 (Int64#  -> ST s (DecodeAction s a))
    | ConsumeMapLen64  (Int64#  -> ST s (DecodeAction s a))
    | ConsumeTag64     (Word64# -> ST s (DecodeAction s a))
#endif

    | ConsumeInteger       (Integer   -> ST s (DecodeAction s a))
    | ConsumeFloat         (Float#    -> ST s (DecodeAction s a))
    | ConsumeDouble        (Double#   -> ST s (DecodeAction s a))
    | ConsumeBytes         (ByteString-> ST s (DecodeAction s a))
    | ConsumeByteArray     (ByteArray -> ST s (DecodeAction s a))
    | ConsumeString        (Text      -> ST s (DecodeAction s a))
    | ConsumeUtf8ByteArray (ByteArray -> ST s (DecodeAction s a))
    | ConsumeBool          (Bool      -> ST s (DecodeAction s a))
    | ConsumeSimple        (Word#     -> ST s (DecodeAction s a))

    | ConsumeBytesIndef   (ST s (DecodeAction s a))
    | ConsumeStringIndef  (ST s (DecodeAction s a))
    | ConsumeListLenIndef (ST s (DecodeAction s a))
    | ConsumeMapLenIndef  (ST s (DecodeAction s a))
    | ConsumeNull         (ST s (DecodeAction s a))

    | ConsumeListLenOrIndef (Int# -> ST s (DecodeAction s a))
    | ConsumeMapLenOrIndef  (Int# -> ST s (DecodeAction s a))
    | ConsumeBreakOr        (Bool -> ST s (DecodeAction s a))

    | PeekTokenType  (TokenType -> ST s (DecodeAction s a))
    | PeekAvailable  (Int#      -> ST s (DecodeAction s a))
#if defined(ARCH_32bit)
    | PeekByteOffset (Int64#    -> ST s (DecodeAction s a))
#else
    | PeekByteOffset (Int#      -> ST s (DecodeAction s a))
#endif

      -- All the canonical variants
    | ConsumeWordCanonical    (Word# -> ST s (DecodeAction s a))
    | ConsumeWord8Canonical   (Word# -> ST s (DecodeAction s a))
    | ConsumeWord16Canonical  (Word# -> ST s (DecodeAction s a))
    | ConsumeWord32Canonical  (Word# -> ST s (DecodeAction s a))
    | ConsumeNegWordCanonical (Word# -> ST s (DecodeAction s a))
    | ConsumeIntCanonical     (Int#  -> ST s (DecodeAction s a))
    | ConsumeInt8Canonical    (Int#  -> ST s (DecodeAction s a))
    | ConsumeInt16Canonical   (Int#  -> ST s (DecodeAction s a))
    | ConsumeInt32Canonical   (Int#  -> ST s (DecodeAction s a))
    | ConsumeListLenCanonical (Int#  -> ST s (DecodeAction s a))
    | ConsumeMapLenCanonical  (Int#  -> ST s (DecodeAction s a))
    | ConsumeTagCanonical     (Word# -> ST s (DecodeAction s a))

#if defined(ARCH_32bit)
    | ConsumeWord64Canonical    (Word64# -> ST s (DecodeAction s a))
    | ConsumeNegWord64Canonical (Word64# -> ST s (DecodeAction s a))
    | ConsumeInt64Canonical     (Int64#  -> ST s (DecodeAction s a))
    | ConsumeListLen64Canonical (Int64#  -> ST s (DecodeAction s a))
    | ConsumeMapLen64Canonical  (Int64#  -> ST s (DecodeAction s a))
    | ConsumeTag64Canonical     (Word64# -> ST s (DecodeAction s a))
#endif

    | ConsumeIntegerCanonical       (Integer -> ST s (DecodeAction s a))
    | ConsumeFloat16Canonical       (Float#  -> ST s (DecodeAction s a))
    | ConsumeFloatCanonical         (Float#  -> ST s (DecodeAction s a))
    | ConsumeDoubleCanonical        (Double# -> ST s (DecodeAction s a))
    | ConsumeBytesCanonical         (ByteString-> ST s (DecodeAction s a))
    | ConsumeByteArrayCanonical     (ByteArray -> ST s (DecodeAction s a))
    | ConsumeStringCanonical        (Text      -> ST s (DecodeAction s a))
    | ConsumeUtf8ByteArrayCanonical (ByteArray -> ST s (DecodeAction s a))
    | ConsumeSimpleCanonical        (Word#   -> ST s (DecodeAction s a))

    | Fail String
    | Done a

-- | The type of a token, which a decoder can ask for at
-- an arbitrary time.
--
-- @since 0.2.0.0
data TokenType
  = TypeUInt
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
  | TypeSimple
  | TypeBreak
  | TypeInvalid
  deriving (Eq, Ord, Enum, Bounded, Show)

-- | @since 0.2.0.0
instance Functor (Decoder s) where
    {-# INLINE fmap #-}
    fmap f = \d -> Decoder $ \k -> runDecoder d (k . f)

-- | @since 0.2.0.0
instance Applicative (Decoder s) where
    {-# INLINE pure #-}
    pure = \x -> Decoder $ \k -> k x

    {-# INLINE (<*>) #-}
    (<*>) = \df dx -> Decoder $ \k ->
                        runDecoder df (\f -> runDecoder dx (\x -> k (f x)))

    {-# INLINE (*>) #-}
    (*>) = \dm dn -> Decoder $ \k -> runDecoder dm (\_ -> runDecoder dn k)

-- | @since 0.2.0.0
instance Monad (Decoder s) where
    return = pure

    {-# INLINE (>>=) #-}
    (>>=) = \dm f -> Decoder $ \k -> runDecoder dm (\m -> runDecoder (f m) k)

    {-# INLINE (>>) #-}
    (>>) = (*>)

#if !MIN_VERSION_base(4,13,0)
    fail = Fail.fail
#endif

-- | @since 0.2.0.0
instance Fail.MonadFail (Decoder s) where
    fail msg = Decoder $ \_ -> return (Fail msg)

-- | Lift an @ST@ action into a @Decoder@. Useful for, e.g., leveraging
-- in-place mutation to efficiently build a deserialised value.
--
-- @since 0.2.0.0
liftST :: ST s a -> Decoder s a
liftST m = Decoder $ \k -> m >>= k

-- | Given a 'Decoder', give us the 'DecodeAction'
--
-- @since 0.2.0.0
getDecodeAction :: Decoder s a -> ST s (DecodeAction s a)
getDecodeAction (Decoder k) = k (\x -> return (Done x))


-- Compatibility Shims
toInt8   :: Int# -> Int8
toInt16  :: Int# -> Int16
toInt32  :: Int# -> Int32
toWord8  :: Word# -> Word8
toWord16 :: Word# -> Word16
toWord32 :: Word# -> Word32
#if MIN_VERSION_ghc_prim(0,8,0)
toInt8   n = I8#  (intToInt8# n)
toInt16  n = I16# (intToInt16# n)
toInt32  n = I32# (intToInt32# n)
toWord8  n = W8#  (wordToWord8# n)
toWord16 n = W16# (wordToWord16# n)
toWord32 n = W32# (wordToWord32# n)
#else
toInt8   n = I8#  n
toInt16  n = I16# n
toInt32  n = I32# n
toWord8  n = W8#  n
toWord16 n = W16# n
toWord32 n = W32# n
#endif

-- $canonical
--
-- <https://tools.ietf.org/html/rfc7049#section-3.9>
--
-- In general in CBOR there can be multiple representations for the same value,
-- for example the integer @0@ can be represented in 8, 16, 32 or 64 bits. This
-- library always encoded values in the shortest representation but on
-- decoding allows any valid encoding. For some applications it is useful or
-- important to only decode the canonical encoding. The decoder primitives here
-- are to allow applications to implement canonical decoding.
--
-- It is important to note that achieving a canonical representation is /not/
-- simply about using these primitives. For example consider a typical CBOR
-- encoding of a Haskell @Set@ data type. This will be encoded as a CBOR list
-- of the set elements. A typical implementation might be:
--
-- > encodeSet = encodeList . Set.toList
-- > decodeSet = fmap Set.fromList . decodeList
--
-- This /does not/ enforce a canonical encoding. The decoder above will allow
-- set elements in any order. The use of @Set.fromList@ forgets the order.
-- To enforce that the decoder only accepts the canonical encoding it will
-- have to check that the elements in the list are /strictly/ increasing.
-- Similar issues arise in many other data types, wherever there is redundancy
-- in the external representation.
--
-- The decoder primitives in this section are not much more expensive than
-- their normal counterparts. If checking the canonical encoding property is
-- critical then a technique that is more expensive but easier to implement and
-- test is to decode normally, re-encode and check the serialised bytes are the
-- same.

---------------------------------------
-- Read input tokens of various types
--

-- | Decode a 'Word'.
--
-- @since 0.2.0.0
decodeWord :: Decoder s Word
decodeWord = Decoder (\k -> return (ConsumeWord (\w# -> k (W# w#))))
{-# INLINE decodeWord #-}

-- | Decode a 'Word8'.
--
-- @since 0.2.0.0
decodeWord8 :: Decoder s Word8
decodeWord8 = Decoder (\k -> return (ConsumeWord8 (\w# -> k (toWord8 w#))))
{-# INLINE decodeWord8 #-}

-- | Decode a 'Word16'.
--
-- @since 0.2.0.0
decodeWord16 :: Decoder s Word16
decodeWord16 = Decoder (\k -> return (ConsumeWord16 (\w# -> k (toWord16 w#))))
{-# INLINE decodeWord16 #-}

-- | Decode a 'Word32'.
--
-- @since 0.2.0.0
decodeWord32 :: Decoder s Word32
decodeWord32 = Decoder (\k -> return (ConsumeWord32 (\w# -> k (toWord32 w#))))
{-# INLINE decodeWord32 #-}

-- | Decode a 'Word64'.
--
-- @since 0.2.0.0
decodeWord64 :: Decoder s Word64
{-# INLINE decodeWord64 #-}
decodeWord64 =
#if defined(ARCH_64bit)
  Decoder (\k -> return (ConsumeWord (\w# -> k (W64# w#))))
#else
  Decoder (\k -> return (ConsumeWord64 (\w64# -> k (W64# w64#))))
#endif

-- | Decode a negative 'Word'.
--
-- @since 0.2.0.0
decodeNegWord :: Decoder s Word
decodeNegWord = Decoder (\k -> return (ConsumeNegWord (\w# -> k (W# w#))))
{-# INLINE decodeNegWord #-}

-- | Decode a negative 'Word64'.
--
-- @since 0.2.0.0
decodeNegWord64 :: Decoder s Word64
{-# INLINE decodeNegWord64 #-}
decodeNegWord64 =
#if defined(ARCH_64bit)
  Decoder (\k -> return (ConsumeNegWord (\w# -> k (W64# w#))))
#else
  Decoder (\k -> return (ConsumeNegWord64 (\w64# -> k (W64# w64#))))
#endif

-- | Decode an 'Int'.
--
-- @since 0.2.0.0
decodeInt :: Decoder s Int
decodeInt = Decoder (\k -> return (ConsumeInt (\n# -> k (I# n#))))
{-# INLINE decodeInt #-}

-- | Decode an 'Int8'.
--
-- @since 0.2.0.0
decodeInt8 :: Decoder s Int8
decodeInt8 = Decoder (\k -> return (ConsumeInt8 (\w# -> k (toInt8 w#))))
{-# INLINE decodeInt8 #-}

-- | Decode an 'Int16'.
--
-- @since 0.2.0.0
decodeInt16 :: Decoder s Int16
decodeInt16 = Decoder (\k -> return (ConsumeInt16 (\w# -> k (toInt16 w#))))
{-# INLINE decodeInt16 #-}

-- | Decode an 'Int32'.
--
-- @since 0.2.0.0
decodeInt32 :: Decoder s Int32
decodeInt32 = Decoder (\k -> return (ConsumeInt32 (\w# -> k (toInt32 w#))))
{-# INLINE decodeInt32 #-}

-- | Decode an 'Int64'.
--
-- @since 0.2.0.0
decodeInt64 :: Decoder s Int64
{-# INLINE decodeInt64 #-}
decodeInt64 =
#if defined(ARCH_64bit)
  Decoder (\k -> return (ConsumeInt (\n# -> k (I64# n#))))
#else
  Decoder (\k -> return (ConsumeInt64 (\n64# -> k (I64# n64#))))
#endif

-- | Decode canonical representation of a 'Word'.
--
-- @since 0.2.0.0
decodeWordCanonical :: Decoder s Word
decodeWordCanonical = Decoder (\k -> return (ConsumeWordCanonical (\w# -> k (W# w#))))
{-# INLINE decodeWordCanonical #-}

-- | Decode canonical representation of a 'Word8'.
--
-- @since 0.2.0.0
decodeWord8Canonical :: Decoder s Word8
decodeWord8Canonical = Decoder (\k -> return (ConsumeWord8Canonical (\w# -> k (toWord8 w#))))
{-# INLINE decodeWord8Canonical #-}

-- | Decode canonical representation of a 'Word16'.
--
-- @since 0.2.0.0
decodeWord16Canonical :: Decoder s Word16
decodeWord16Canonical = Decoder (\k -> return (ConsumeWord16Canonical (\w# -> k (toWord16 w#))))
{-# INLINE decodeWord16Canonical #-}

-- | Decode canonical representation of a 'Word32'.
--
-- @since 0.2.0.0
decodeWord32Canonical :: Decoder s Word32
decodeWord32Canonical = Decoder (\k -> return (ConsumeWord32Canonical (\w# -> k (toWord32 w#))))
{-# INLINE decodeWord32Canonical #-}

-- | Decode canonical representation of a 'Word64'.
--
-- @since 0.2.0.0
decodeWord64Canonical :: Decoder s Word64
{-# INLINE decodeWord64Canonical #-}
decodeWord64Canonical =
#if defined(ARCH_64bit)
  Decoder (\k -> return (ConsumeWordCanonical (\w# -> k (W64# w#))))
#else
  Decoder (\k -> return (ConsumeWord64Canonical (\w64# -> k (W64# w64#))))
#endif

-- | Decode canonical representation of a negative 'Word'.
--
-- @since 0.2.0.0
decodeNegWordCanonical :: Decoder s Word
decodeNegWordCanonical = Decoder (\k -> return (ConsumeNegWordCanonical (\w# -> k (W# w#))))
{-# INLINE decodeNegWordCanonical #-}

-- | Decode canonical representation of a negative 'Word64'.
--
-- @since 0.2.0.0
decodeNegWord64Canonical :: Decoder s Word64
{-# INLINE decodeNegWord64Canonical #-}
decodeNegWord64Canonical =
#if defined(ARCH_64bit)
  Decoder (\k -> return (ConsumeNegWordCanonical (\w# -> k (W64# w#))))
#else
  Decoder (\k -> return (ConsumeNegWord64Canonical (\w64# -> k (W64# w64#))))
#endif

-- | Decode canonical representation of an 'Int'.
--
-- @since 0.2.0.0
decodeIntCanonical :: Decoder s Int
decodeIntCanonical = Decoder (\k -> return (ConsumeIntCanonical (\n# -> k (I# n#))))
{-# INLINE decodeIntCanonical #-}

-- | Decode canonical representation of an 'Int8'.
--
-- @since 0.2.0.0
decodeInt8Canonical :: Decoder s Int8
decodeInt8Canonical = Decoder (\k -> return (ConsumeInt8Canonical (\w# -> k (toInt8 w#))))
{-# INLINE decodeInt8Canonical #-}

-- | Decode canonical representation of an 'Int16'.
--
-- @since 0.2.0.0
decodeInt16Canonical :: Decoder s Int16
decodeInt16Canonical = Decoder (\k -> return (ConsumeInt16Canonical (\w# -> k (toInt16 w#))))
{-# INLINE decodeInt16Canonical #-}

-- | Decode canonical representation of an 'Int32'.
--
-- @since 0.2.0.0
decodeInt32Canonical :: Decoder s Int32
decodeInt32Canonical = Decoder (\k -> return (ConsumeInt32Canonical (\w# -> k (toInt32 w#))))
{-# INLINE decodeInt32Canonical #-}

-- | Decode canonical representation of an 'Int64'.
--
-- @since 0.2.0.0
decodeInt64Canonical :: Decoder s Int64
{-# INLINE decodeInt64Canonical #-}
decodeInt64Canonical =
#if defined(ARCH_64bit)
  Decoder (\k -> return (ConsumeIntCanonical (\n# -> k (I64# n#))))
#else
  Decoder (\k -> return (ConsumeInt64Canonical (\n64# -> k (I64# n64#))))
#endif

-- | Decode an 'Integer'.
--
-- @since 0.2.0.0
decodeInteger :: Decoder s Integer
decodeInteger = Decoder (\k -> return (ConsumeInteger (\n -> k n)))
{-# INLINE decodeInteger #-}

-- | Decode a 'Float'.
--
-- @since 0.2.0.0
decodeFloat :: Decoder s Float
decodeFloat = Decoder (\k -> return (ConsumeFloat (\f# -> k (F# f#))))
{-# INLINE decodeFloat #-}

-- | Decode a 'Double'.
--
-- @since 0.2.0.0
decodeDouble :: Decoder s Double
decodeDouble = Decoder (\k -> return (ConsumeDouble (\f# -> k (D# f#))))
{-# INLINE decodeDouble #-}

-- | Decode a string of bytes as a 'ByteString'.
--
-- @since 0.2.0.0
decodeBytes :: Decoder s ByteString
decodeBytes = Decoder (\k -> return (ConsumeBytes (\bs -> k bs)))
{-# INLINE decodeBytes #-}

-- | Decode canonical representation of a string of bytes as a 'ByteString'.
--
-- @since 0.2.1.0
decodeBytesCanonical :: Decoder s ByteString
decodeBytesCanonical = Decoder (\k -> return (ConsumeBytesCanonical (\bs -> k bs)))
{-# INLINE decodeBytesCanonical #-}

-- | Decode a token marking the beginning of an indefinite length
-- set of bytes.
--
-- @since 0.2.0.0
decodeBytesIndef :: Decoder s ()
decodeBytesIndef = Decoder (\k -> return (ConsumeBytesIndef (k ())))
{-# INLINE decodeBytesIndef #-}

-- | Decode a string of bytes as a 'ByteArray'.
--
-- Also note that this will eagerly copy the content out of the input
-- to ensure that the input does not leak in the event that the 'ByteArray' is
-- live but not forced.
--
-- @since 0.2.0.0
decodeByteArray :: Decoder s ByteArray
decodeByteArray = Decoder (\k -> return (ConsumeByteArray k))
{-# INLINE decodeByteArray #-}

-- | Decode canonical representation of a string of bytes as a 'ByteArray'.
--
-- Also note that this will eagerly copy the content out of the input
-- to ensure that the input does not leak in the event that the 'ByteArray' is
-- live but not forced.
--
-- @since 0.2.1.0
decodeByteArrayCanonical :: Decoder s ByteArray
decodeByteArrayCanonical = Decoder (\k -> return (ConsumeByteArrayCanonical k))
{-# INLINE decodeByteArrayCanonical #-}

-- | Decode a textual string as a piece of 'Text'.
--
-- @since 0.2.0.0
decodeString :: Decoder s Text
decodeString = Decoder (\k -> return (ConsumeString (\str -> k str)))
{-# INLINE decodeString #-}

-- | Decode canonical representation of a textual string as a piece of 'Text'.
--
-- @since 0.2.1.0
decodeStringCanonical :: Decoder s Text
decodeStringCanonical = Decoder (\k -> return (ConsumeStringCanonical (\str -> k str)))
{-# INLINE decodeStringCanonical #-}

-- | Decode a token marking the beginning of an indefinite length
-- string.
--
-- @since 0.2.0.0
decodeStringIndef :: Decoder s ()
decodeStringIndef = Decoder (\k -> return (ConsumeStringIndef (k ())))
{-# INLINE decodeStringIndef #-}

-- | Decode a textual string as UTF-8 encoded 'ByteArray'. Note that
-- the result is not validated to be well-formed UTF-8.
--
-- Also note that this will eagerly copy the content out of the input
-- to ensure that the input does not leak in the event that the 'ByteArray' is
-- live but not forced.
--
-- @since 0.2.0.0
decodeUtf8ByteArray :: Decoder s ByteArray
decodeUtf8ByteArray = Decoder (\k -> return (ConsumeUtf8ByteArray k))
{-# INLINE decodeUtf8ByteArray #-}

-- | Decode canonical representation of a textual string as UTF-8 encoded
-- 'ByteArray'. Note that the result is not validated to be well-formed UTF-8.
--
-- Also note that this will eagerly copy the content out of the input
-- to ensure that the input does not leak in the event that the 'ByteArray' is
-- live but not forced.
--
-- @since 0.2.1.0
decodeUtf8ByteArrayCanonical :: Decoder s ByteArray
decodeUtf8ByteArrayCanonical = Decoder (\k -> return (ConsumeUtf8ByteArrayCanonical k))
{-# INLINE decodeUtf8ByteArrayCanonical #-}

-- | Decode the length of a list.
--
-- @since 0.2.0.0
decodeListLen :: Decoder s Int
decodeListLen = Decoder (\k -> return (ConsumeListLen (\n# -> k (I# n#))))
{-# INLINE decodeListLen #-}

-- | Decode canonical representation of the length of a list.
--
-- @since 0.2.0.0
decodeListLenCanonical :: Decoder s Int
decodeListLenCanonical = Decoder (\k -> return (ConsumeListLenCanonical (\n# -> k (I# n#))))
{-# INLINE decodeListLenCanonical #-}

-- | Decode a token marking the beginning of a list of indefinite
-- length.
--
-- @since 0.2.0.0
decodeListLenIndef :: Decoder s ()
decodeListLenIndef = Decoder (\k -> return (ConsumeListLenIndef (k ())))
{-# INLINE decodeListLenIndef #-}

-- | Decode the length of a map.
--
-- @since 0.2.0.0
decodeMapLen :: Decoder s Int
decodeMapLen = Decoder (\k -> return (ConsumeMapLen (\n# -> k (I# n#))))
{-# INLINE decodeMapLen #-}

-- | Decode canonical representation of the length of a map.
--
-- @since 0.2.0.0
decodeMapLenCanonical :: Decoder s Int
decodeMapLenCanonical = Decoder (\k -> return (ConsumeMapLenCanonical (\n# -> k (I# n#))))
{-# INLINE decodeMapLenCanonical #-}

-- | Decode a token marking the beginning of a map of indefinite
-- length.
--
-- @since 0.2.0.0
decodeMapLenIndef :: Decoder s ()
decodeMapLenIndef = Decoder (\k -> return (ConsumeMapLenIndef (k ())))
{-# INLINE decodeMapLenIndef #-}

-- | Decode an arbitrary tag and return it as a 'Word'.
--
-- @since 0.2.0.0
decodeTag :: Decoder s Word
decodeTag = Decoder (\k -> return (ConsumeTag (\w# -> k (W# w#))))
{-# INLINE decodeTag #-}

-- | Decode an arbitrary 64-bit tag and return it as a 'Word64'.
--
-- @since 0.2.0.0
decodeTag64 :: Decoder s Word64
{-# INLINE decodeTag64 #-}
decodeTag64 =
#if defined(ARCH_64bit)
  Decoder (\k -> return (ConsumeTag (\w# -> k (W64# w#))))
#else
  Decoder (\k -> return (ConsumeTag64 (\w64# -> k (W64# w64#))))
#endif

-- | Decode canonical representation of an arbitrary tag and return it as a
-- 'Word'.
--
-- @since 0.2.0.0
decodeTagCanonical :: Decoder s Word
decodeTagCanonical = Decoder (\k -> return (ConsumeTagCanonical (\w# -> k (W# w#))))
{-# INLINE decodeTagCanonical #-}

-- | Decode canonical representation of an arbitrary 64-bit tag and return it as
-- a 'Word64'.
--
-- @since 0.2.0.0
decodeTag64Canonical :: Decoder s Word64
{-# INLINE decodeTag64Canonical #-}
decodeTag64Canonical =
#if defined(ARCH_64bit)
  Decoder (\k -> return (ConsumeTagCanonical (\w# -> k (W64# w#))))
#else
  Decoder (\k -> return (ConsumeTag64Canonical (\w64# -> k (W64# w64#))))
#endif

-- | Decode a bool.
--
-- @since 0.2.0.0
decodeBool :: Decoder s Bool
decodeBool = Decoder (\k -> return (ConsumeBool (\b -> k b)))
{-# INLINE decodeBool #-}

-- | Decode a nullary value, and return a unit value.
--
-- @since 0.2.0.0
decodeNull :: Decoder s ()
decodeNull = Decoder (\k -> return (ConsumeNull (k ())))
{-# INLINE decodeNull #-}

-- | Decode a 'simple' CBOR value and give back a 'Word8'. You
-- probably don't ever need to use this.
--
-- @since 0.2.0.0
decodeSimple :: Decoder s Word8
decodeSimple = Decoder (\k -> return (ConsumeSimple (\w# -> k (toWord8 w#))))
{-# INLINE decodeSimple #-}

-- | Decode canonical representation of an 'Integer'.
--
-- @since 0.2.0.0
decodeIntegerCanonical :: Decoder s Integer
decodeIntegerCanonical = Decoder (\k -> return (ConsumeIntegerCanonical (\n -> k n)))
{-# INLINE decodeIntegerCanonical #-}

-- | Decode canonical representation of a half-precision 'Float'.
--
-- @since 0.2.0.0
decodeFloat16Canonical :: Decoder s Float
decodeFloat16Canonical = Decoder (\k -> return (ConsumeFloat16Canonical (\f# -> k (F# f#))))
{-# INLINE decodeFloat16Canonical #-}

-- | Decode canonical representation of a 'Float'.
--
-- @since 0.2.0.0
decodeFloatCanonical :: Decoder s Float
decodeFloatCanonical = Decoder (\k -> return (ConsumeFloatCanonical (\f# -> k (F# f#))))
{-# INLINE decodeFloatCanonical #-}

-- | Decode canonical representation of a 'Double'.
--
-- @since 0.2.0.0
decodeDoubleCanonical :: Decoder s Double
decodeDoubleCanonical = Decoder (\k -> return (ConsumeDoubleCanonical (\f# -> k (D# f#))))
{-# INLINE decodeDoubleCanonical #-}

-- | Decode canonical representation of a 'simple' CBOR value and give back a
-- 'Word8'. You probably don't ever need to use this.
--
-- @since 0.2.0.0
decodeSimpleCanonical :: Decoder s Word8
decodeSimpleCanonical = Decoder (\k -> return (ConsumeSimpleCanonical (\w# -> k (toWord8 w#))))
{-# INLINE decodeSimpleCanonical #-}

--------------------------------------------------------------
-- Specialised read operations: expect a token with a specific value
--

-- | Attempt to decode a word with 'decodeWord', and ensure the word
-- is exactly as expected, or fail.
--
-- @since 0.2.0.0
decodeWordOf :: Word -- ^ Expected value of the decoded word
             -> Decoder s ()
decodeWordOf = decodeWordOfHelper decodeWord
{-# INLINE decodeWordOf #-}

-- | Attempt to decode a list length using 'decodeListLen', and
-- ensure it is exactly the specified length, or fail.
--
-- @since 0.2.0.0
decodeListLenOf :: Int -> Decoder s ()
decodeListLenOf = decodeListLenOfHelper decodeListLen
{-# INLINE decodeListLenOf #-}

-- | Attempt to decode canonical representation of a word with 'decodeWordCanonical',
-- and ensure the word is exactly as expected, or fail.
--
-- @since 0.2.0.0
decodeWordCanonicalOf :: Word -- ^ Expected value of the decoded word
                      -> Decoder s ()
decodeWordCanonicalOf = decodeWordOfHelper decodeWordCanonical
{-# INLINE decodeWordCanonicalOf #-}

-- | Attempt to decode canonical representation of a list length using
-- 'decodeListLenCanonical', and ensure it is exactly the specified length, or
-- fail.
--
-- @since 0.2.0.0
decodeListLenCanonicalOf :: Int -> Decoder s ()
decodeListLenCanonicalOf = decodeListLenOfHelper decodeListLenCanonical
{-# INLINE decodeListLenCanonicalOf #-}

decodeListLenOfHelper :: (Show a, Eq a, Fail.MonadFail m) => m a -> a -> m ()
decodeListLenOfHelper decodeFun = \len -> do
  len' <- decodeFun
  if len == len' then return ()
                 else fail $ "expected list of length " ++ show len
{-# INLINE decodeListLenOfHelper #-}

decodeWordOfHelper :: (Show a, Eq a, Fail.MonadFail m) => m a -> a -> m ()
decodeWordOfHelper decodeFun = \n -> do
  n' <- decodeFun
  if n == n' then return ()
             else fail $ "expected word " ++ show n
{-# INLINE decodeWordOfHelper #-}

--------------------------------------------------------------
-- Branching operations

-- | Attempt to decode a token for the length of a finite, known list,
-- or an indefinite list. If 'Nothing' is returned, then an
-- indefinite length list occurs afterwords. If @'Just' x@ is
-- returned, then a list of length @x@ is encoded.
--
-- @since 0.2.0.0
decodeListLenOrIndef :: Decoder s (Maybe Int)
decodeListLenOrIndef =
    Decoder (\k -> return (ConsumeListLenOrIndef (\n# ->
                     if I# n# >= 0
                       then k (Just (I# n#))
                       else k Nothing)))
{-# INLINE decodeListLenOrIndef #-}

-- | Attempt to decode a token for the length of a finite, known map,
-- or an indefinite map. If 'Nothing' is returned, then an
-- indefinite length map occurs afterwords. If @'Just' x@ is returned,
-- then a map of length @x@ is encoded.
--
-- @since 0.2.0.0
decodeMapLenOrIndef :: Decoder s (Maybe Int)
decodeMapLenOrIndef =
    Decoder (\k -> return (ConsumeMapLenOrIndef (\n# ->
                     if I# n# >= 0
                       then k (Just (I# n#))
                       else k Nothing)))
{-# INLINE decodeMapLenOrIndef #-}

-- | Attempt to decode a @Break@ token, and if that was
-- successful, return 'True'. If the token was of any
-- other type, return 'False'.
--
-- @since 0.2.0.0
decodeBreakOr :: Decoder s Bool
decodeBreakOr = Decoder (\k -> return (ConsumeBreakOr (\b -> k b)))
{-# INLINE decodeBreakOr #-}

--------------------------------------------------------------
-- Special operations

-- | Peek at the current token we're about to decode, and return a
-- 'TokenType' specifying what it is.
--
-- @since 0.2.0.0
peekTokenType :: Decoder s TokenType
peekTokenType = Decoder (\k -> return (PeekTokenType (\tk -> k tk)))
{-# INLINE peekTokenType #-}

-- | Peek and return the length of the current buffer that we're
-- running our decoder on.
--
-- @since 0.2.0.0
peekAvailable :: Decoder s Int
peekAvailable = Decoder (\k -> return (PeekAvailable (\len# -> k (I# len#))))
{-# INLINE peekAvailable #-}


-- | A 0-based offset within the overall byte sequence that makes up the
-- input to the 'Decoder'.
--
-- This is an 'Int64' since 'Decoder' is incremental and can decode more data
-- than fits in memory at once. This is also compatible with the result type
-- of 'Data.ByteString.Lazy.length'.
--
type ByteOffset = Int64

-- | Get the current 'ByteOffset' in the input byte sequence of the 'Decoder'.
--
-- The 'Decoder' does not provide any facility to get at the input data
-- directly (since that is tricky with an incremental decoder). The next best
-- is this primitive which can be used to keep track of the offset within the
-- input bytes that makes up the encoded form of a term.
--
-- By keeping track of the byte offsets before and after decoding a subterm
-- (a pattern captured by 'decodeWithByteSpan') and if the overall input data
-- is retained then this is enables later retrieving the span of bytes for the
-- subterm.
--
-- @since 0.2.2.0
peekByteOffset :: Decoder s ByteOffset
peekByteOffset = Decoder (\k -> return (PeekByteOffset (\off# -> k (I64# off#))))
{-# INLINE peekByteOffset #-}

-- | This captures the pattern of getting the byte offsets before and after
-- decoding a subterm.
--
-- > !before <- peekByteOffset
-- > x <- decode
-- > !after  <- peekByteOffset
--
decodeWithByteSpan :: Decoder s a -> Decoder s (a, ByteOffset, ByteOffset)
decodeWithByteSpan da = do
    !before <- peekByteOffset
    x <- da
    !after  <- peekByteOffset
    return (x, before, after)

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

-- | Decode an indefinite sequence length.
--
-- @since 0.2.0.0
decodeSequenceLenIndef :: (r -> a -> r)
                       -> r
                       -> (r -> r')
                       -> Decoder s a
                       -> Decoder s r'
decodeSequenceLenIndef f z g get =
    go z
  where
    go !acc = do
      stop <- decodeBreakOr
      if stop then return $! g acc
              else do !x <- get; go (f acc x)
{-# INLINE decodeSequenceLenIndef #-}

-- | Decode a sequence length.
--
-- @since 0.2.0.0
decodeSequenceLenN :: (r -> a -> r)
                   -> r
                   -> (r -> r')
                   -> Int
                   -> Decoder s a
                   -> Decoder s r'
decodeSequenceLenN f z g c get =
    go z c
  where
    go !acc 0 = return $! g acc
    go !acc n = do !x <- get; go (f acc x) (n-1)
{-# INLINE decodeSequenceLenN #-}
