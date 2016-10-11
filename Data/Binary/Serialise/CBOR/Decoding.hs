{-# LANGUAGE CPP          #-}
{-# LANGUAGE MagicHash    #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RankNTypes   #-}

-- |
-- Module      : Data.Binary.Serialise.CBOR.Decoding
-- Copyright   : (c) Duncan Coutts 2015
-- License     : BSD3-style (see LICENSE.txt)
--
-- Maintainer  : duncan@community.haskell.org
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- High level API for decoding values that were encoded with the
-- "Data.Binary.Serialise.CBOR.Encoding" module, using a @'Monad'@
-- based interface.
--
module Data.Binary.Serialise.CBOR.Decoding
  ( -- * Decode primitive operations
    Decoder
  , DecodeAction(..)
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
  , decodeString        -- :: Decoder s Text
  , decodeStringIndef   -- :: Decoder s ()
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
  , peekAvailable        -- :: Decoder s Int
  , TokenType(..)

  -- ** Special operations
--, ignoreTerms
--, decodeTrace

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

import           Prelude hiding (decodeFloat)

#if defined(USE_ST)
import Control.Monad.ST
#else
import Control.Monad.Identity (Identity(..))
type ST s a = Identity a
#endif


-- | A continuation-based decoder, used for decoding values that were
-- previously encoded using the "Data.Binary.Serialise.CBOR.Encoding"
-- module. As @'Decoder'@ has a @'Monad'@ instance, you can easily
-- write @'Decoder'@s monadically for building your deserialisation
-- logic.
--
-- @since 0.2.0.0
data Decoder s a = Decoder {
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

    | ConsumeInteger (Integer    -> ST s (DecodeAction s a))
    | ConsumeFloat   (Float#     -> ST s (DecodeAction s a))
    | ConsumeDouble  (Double#    -> ST s (DecodeAction s a))
    | ConsumeBytes   (ByteString -> ST s (DecodeAction s a))
    | ConsumeString  (Text       -> ST s (DecodeAction s a))
    | ConsumeBool    (Bool       -> ST s (DecodeAction s a))
    | ConsumeSimple  (Word#      -> ST s (DecodeAction s a))

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

-- | @since 0.2.0.0
instance Monad (Decoder s) where
    return = pure

    {-# INLINE (>>=) #-}
    (>>=) = \dm f -> Decoder $ \k -> runDecoder dm (\m -> runDecoder (f m) k)

    {-# INLINE (>>) #-}
    (>>) = \dm dn -> Decoder $ \k -> runDecoder dm (\_ -> runDecoder dn k)

    fail msg = Decoder $ \_ -> return (Fail msg)

-- | Given a @'Decoder'@, give us the @'DecodeAction'@
--
-- @since 0.2.0.0
getDecodeAction :: Decoder s a -> ST s (DecodeAction s a)
getDecodeAction (Decoder k) = k (\x -> return (Done x))


---------------------------------------
-- Read input tokens of various types
--

-- | Decode a @'Word'@.
--
-- @since 0.2.0.0
decodeWord :: Decoder s Word
decodeWord = Decoder (\k -> return (ConsumeWord (\w# -> k (W# w#))))
{-# INLINE decodeWord #-}

-- | Decode a @'Word8'@.
--
-- @since 0.2.0.0
decodeWord8 :: Decoder s Word8
decodeWord8 = Decoder (\k -> return (ConsumeWord8 (\w# -> k (W8# w#))))
{-# INLINE decodeWord8 #-}

-- | Decode a @'Word16'@.
--
-- @since 0.2.0.0
decodeWord16 :: Decoder s Word16
decodeWord16 = Decoder (\k -> return (ConsumeWord16 (\w# -> k (W16# w#))))
{-# INLINE decodeWord16 #-}

-- | Decode a @'Word32'@.
--
-- @since 0.2.0.0
decodeWord32 :: Decoder s Word32
decodeWord32 = Decoder (\k -> return (ConsumeWord32 (\w# -> k (W32# w#))))
{-# INLINE decodeWord32 #-}

-- | Decode a @'Word64'@.
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

-- | Decode a negative @'Word'@.
--
-- @since 0.2.0.0
decodeNegWord :: Decoder s Word
decodeNegWord = Decoder (\k -> return (ConsumeNegWord (\w# -> k (W# w#))))
{-# INLINE decodeNegWord #-}

-- | Decode a negative @'Word64'@.
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

-- | Decode an @'Int'@.
--
-- @since 0.2.0.0
decodeInt :: Decoder s Int
decodeInt = Decoder (\k -> return (ConsumeInt (\n# -> k (I# n#))))
{-# INLINE decodeInt #-}

-- | Decode an @'Int8'@.
--
-- @since 0.2.0.0
decodeInt8 :: Decoder s Int8
decodeInt8 = Decoder (\k -> return (ConsumeInt8 (\w# -> k (I8# w#))))
{-# INLINE decodeInt8 #-}

-- | Decode an @'Int16'@.
--
-- @since 0.2.0.0
decodeInt16 :: Decoder s Int16
decodeInt16 = Decoder (\k -> return (ConsumeInt16 (\w# -> k (I16# w#))))
{-# INLINE decodeInt16 #-}

-- | Decode an @'Int32'@.
--
-- @since 0.2.0.0
decodeInt32 :: Decoder s Int32
decodeInt32 = Decoder (\k -> return (ConsumeInt32 (\w# -> k (I32# w#))))
{-# INLINE decodeInt32 #-}

-- | Decode an @'Int64'@.
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

-- | Decode an @'Integer'@.
--
-- @since 0.2.0.0
decodeInteger :: Decoder s Integer
decodeInteger = Decoder (\k -> return (ConsumeInteger (\n -> k n)))
{-# INLINE decodeInteger #-}

-- | Decode a @'Float'@.
--
-- @since 0.2.0.0
decodeFloat :: Decoder s Float
decodeFloat = Decoder (\k -> return (ConsumeFloat (\f# -> k (F# f#))))
{-# INLINE decodeFloat #-}

-- | Deocde a @'Double'@.
--
-- @since 0.2.0.0
decodeDouble :: Decoder s Double
decodeDouble = Decoder (\k -> return (ConsumeDouble (\f# -> k (D# f#))))
{-# INLINE decodeDouble #-}

-- | Decode a string of bytes as a @'ByteString'@.
--
-- @since 0.2.0.0
decodeBytes :: Decoder s ByteString
decodeBytes = Decoder (\k -> return (ConsumeBytes (\bs -> k bs)))
{-# INLINE decodeBytes #-}

-- | Decode a token marking the beginning of an indefinite length
-- set of bytes.
--
-- @since 0.2.0.0
decodeBytesIndef :: Decoder s ()
decodeBytesIndef = Decoder (\k -> return (ConsumeBytesIndef (k ())))
{-# INLINE decodeBytesIndef #-}

-- | Decode a textual string as a piece of @'Text'@.
--
-- @since 0.2.0.0
decodeString :: Decoder s Text
decodeString = Decoder (\k -> return (ConsumeString (\str -> k str)))
{-# INLINE decodeString #-}

-- | Decode a token marking the beginning of an indefinite length
-- string.
--
-- @since 0.2.0.0
decodeStringIndef :: Decoder s ()
decodeStringIndef = Decoder (\k -> return (ConsumeStringIndef (k ())))
{-# INLINE decodeStringIndef #-}

-- | Decode the length of a list.
--
-- @since 0.2.0.0
decodeListLen :: Decoder s Int
decodeListLen = Decoder (\k -> return (ConsumeListLen (\n# -> k (I# n#))))
{-# INLINE decodeListLen #-}

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

-- | Decode a token marking the beginning of a map of indefinite
-- length.
--
-- @since 0.2.0.0
decodeMapLenIndef :: Decoder s ()
decodeMapLenIndef = Decoder (\k -> return (ConsumeMapLenIndef (k ())))
{-# INLINE decodeMapLenIndef #-}

-- | Decode an arbitrary tag and return it as a @'Word'@.
--
-- @since 0.2.0.0
decodeTag :: Decoder s Word
decodeTag = Decoder (\k -> return (ConsumeTag (\w# -> k (W# w#))))
{-# INLINE decodeTag #-}

-- | Decode an arbitrary 64-bit tag and return it as a @'Word64'@.
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

-- | Decode a 'simple' CBOR value and give back a @'Word8'@. You
-- probably don't ever need to use this.
--
-- @since 0.2.0.0
decodeSimple :: Decoder s Word8
decodeSimple = Decoder (\k -> return (ConsumeSimple (\w# -> k (W8# w#))))
{-# INLINE decodeSimple #-}


--------------------------------------------------------------
-- Specialised read operations: expect a token with a specific value
--

-- | Attempt to decode a word with @'decodeWord'@, and ensure the word
-- is exactly as expected, or fail.
--
-- @since 0.2.0.0
decodeWordOf :: Word -- ^ Expected value of the decoded word
             -> Decoder s ()
decodeWordOf n = do
  n' <- decodeWord
  if n == n' then return ()
             else fail $ "expected word " ++ show n
{-# INLINE decodeWordOf #-}

-- | Attempt to decode a list length using @'decodeListLen'@, and
-- ensure it is exactly the specified length, or fail.
--
-- @since 0.2.0.0
decodeListLenOf :: Int -> Decoder s ()
decodeListLenOf len = do
  len' <- decodeListLen
  if len == len' then return ()
                 else fail $ "expected list of length " ++ show len
{-# INLINE decodeListLenOf #-}


--------------------------------------------------------------
-- Branching operations

-- | Attempt to decode a token for the length of a finite, known list,
-- or an indefinite list. If @'Nothing'@ is returned, then an
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
-- or an indefinite map. If @'Nothing'@ is returned, then an
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
-- successful, return @'True'@. If the token was of any
-- other type, return @'False'@.
--
-- @since 0.2.0.0
decodeBreakOr :: Decoder s Bool
decodeBreakOr = Decoder (\k -> return (ConsumeBreakOr (\b -> k b)))
{-# INLINE decodeBreakOr #-}

--------------------------------------------------------------
-- Special operations

-- | Peek at the current token we're about to decode, and return a
-- @'TokenType'@ specifying what it is.
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

