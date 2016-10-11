{-# LANGUAGE CPP       #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE RankNTypes #-}

-- |
-- Module      : Data.Binary.Serialise.CBOR.FlatTerm
-- Copyright   : (c) Duncan Coutts 2015
-- License     : BSD3-style (see LICENSE.txt)
--
-- Maintainer  : duncan@community.haskell.org
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- A simpler form than CBOR for writing out @'Enc.Encoding'@ values that allows
-- easier verification and testing. While this library primarily focuses
-- on taking @'Enc.Encoding'@ values (independent of any underlying format)
-- and serializing them into CBOR format, this module offers an alternative
-- format called @'FlatTerm'@ for serializing @'Enc.Encoding'@ values.
--
-- The @'FlatTerm'@ form is very simple and internally mirrors the original
-- @'Encoding'@ type very carefully. The intention here is that once you
-- have @'Enc.Encoding'@ and @'Dec.Decoding'@ values for your types, you can
-- round-trip values through @'FlatTerm'@ to catch bugs more easily and with
-- a smaller amount of code to look through.
--
-- For that reason, this module is primarily useful for client libraries,
-- and even then, only for their test suites to offer a simpler form for
-- doing encoding tests and catching problems in an encoder and decoder.
--
module Data.Binary.Serialise.CBOR.FlatTerm
  ( -- * Types
    FlatTerm      -- :: *
  , TermToken(..) -- :: *

    -- * Functions
  , toFlatTerm    -- :: Encoding -> FlatTerm
  , fromFlatTerm  -- :: Decoder s a -> FlatTerm -> Either String a
  , validFlatTerm -- :: FlatTerm -> Bool
  ) where

#include "cbor.h"

import           Data.Binary.Serialise.CBOR.Encoding (Encoding(..))
import qualified Data.Binary.Serialise.CBOR.Encoding as Enc
import           Data.Binary.Serialise.CBOR.Decoding as Dec

import           Data.Int
#if defined(ARCH_32bit)
import           GHC.Int   (Int64(I64#))
import           GHC.Word  (Word64(W64#))
import           GHC.Exts  (Word64#, Int64#)
#endif
import           GHC.Word  (Word(W#), Word8(W8#))
import           GHC.Exts  (Int(I#), Int#, Word#, Float#, Double#)
import           GHC.Float (Float(F#), Double(D#), float2Double)

import           Data.Word
import           Data.Text (Text)
import           Data.ByteString (ByteString)


#if defined(USE_ST)
import Control.Monad.ST
#else
import Control.Monad.Identity (Identity(..))
type ST s a = Identity a
runST :: (forall s. ST s a) -> a
runST = runIdentity
#endif

--------------------------------------------------------------------------------

-- | A "flat" representation of an @'Enc.Encoding'@ value,
-- useful for round-tripping and writing tests.
--
-- @since 0.2.0.0
type FlatTerm = [TermToken]

-- | A concrete encoding of @'Enc.Encoding'@ values, one
-- which mirrors the original @'Enc.Encoding'@ type closely.
--
-- @since 0.2.0.0
data TermToken
    = TkInt      {-# UNPACK #-} !Int
    | TkInteger                 !Integer
    | TkBytes    {-# UNPACK #-} !ByteString
    | TkBytesBegin
    | TkString   {-# UNPACK #-} !Text
    | TkStringBegin
    | TkListLen  {-# UNPACK #-} !Word
    | TkListBegin
    | TkMapLen   {-# UNPACK #-} !Word
    | TkMapBegin
    | TkBreak
    | TkTag      {-# UNPACK #-} !Word64
    | TkBool                    !Bool
    | TkNull
    | TkSimple   {-# UNPACK #-} !Word8
    | TkFloat16  {-# UNPACK #-} !Float
    | TkFloat32  {-# UNPACK #-} !Float
    | TkFloat64  {-# UNPACK #-} !Double
    deriving (Eq, Ord, Show)

--------------------------------------------------------------------------------

-- | Convert an arbitrary @'Enc.Encoding'@ into a @'FlatTerm'@.
--
-- @since 0.2.0.0
toFlatTerm :: Encoding -- ^ The input @'Enc.Encoding'@.
           -> FlatTerm -- ^ The resulting @'FlatTerm'@.
toFlatTerm (Encoding tb) = convFlatTerm (tb Enc.TkEnd)

convFlatTerm :: Enc.Tokens -> FlatTerm
convFlatTerm (Enc.TkWord     w  ts)
  | w <= maxInt                     = TkInt     (fromIntegral w) : convFlatTerm ts
  | otherwise                       = TkInteger (fromIntegral w) : convFlatTerm ts
convFlatTerm (Enc.TkWord64   w  ts)
  | w <= maxInt                     = TkInt     (fromIntegral w) : convFlatTerm ts
  | otherwise                       = TkInteger (fromIntegral w) : convFlatTerm ts
convFlatTerm (Enc.TkInt      n  ts) = TkInt       n : convFlatTerm ts
convFlatTerm (Enc.TkInt64    n  ts)
  | n <= maxInt && n >= minInt      = TkInt     (fromIntegral n) : convFlatTerm ts
  | otherwise                       = TkInteger (fromIntegral n) : convFlatTerm ts
convFlatTerm (Enc.TkInteger  n  ts)
  | n <= maxInt && n >= minInt      = TkInt (fromIntegral n) : convFlatTerm ts
  | otherwise                       = TkInteger   n : convFlatTerm ts
convFlatTerm (Enc.TkBytes    bs ts) = TkBytes    bs : convFlatTerm ts
convFlatTerm (Enc.TkBytesBegin  ts) = TkBytesBegin  : convFlatTerm ts
convFlatTerm (Enc.TkString   st ts) = TkString   st : convFlatTerm ts
convFlatTerm (Enc.TkStringBegin ts) = TkStringBegin : convFlatTerm ts
convFlatTerm (Enc.TkListLen  n  ts) = TkListLen   n : convFlatTerm ts
convFlatTerm (Enc.TkListBegin   ts) = TkListBegin   : convFlatTerm ts
convFlatTerm (Enc.TkMapLen   n  ts) = TkMapLen    n : convFlatTerm ts
convFlatTerm (Enc.TkMapBegin    ts) = TkMapBegin    : convFlatTerm ts
convFlatTerm (Enc.TkTag      n  ts) = TkTag (fromIntegral n) : convFlatTerm ts
convFlatTerm (Enc.TkTag64    n  ts) = TkTag       n : convFlatTerm ts
convFlatTerm (Enc.TkBool     b  ts) = TkBool      b : convFlatTerm ts
convFlatTerm (Enc.TkNull        ts) = TkNull        : convFlatTerm ts
convFlatTerm (Enc.TkUndef       ts) = TkSimple   23 : convFlatTerm ts
convFlatTerm (Enc.TkSimple   n  ts) = TkSimple    n : convFlatTerm ts
convFlatTerm (Enc.TkFloat16  f  ts) = TkFloat16   f : convFlatTerm ts
convFlatTerm (Enc.TkFloat32  f  ts) = TkFloat32   f : convFlatTerm ts
convFlatTerm (Enc.TkFloat64  f  ts) = TkFloat64   f : convFlatTerm ts
convFlatTerm (Enc.TkBreak       ts) = TkBreak       : convFlatTerm ts
convFlatTerm  Enc.TkEnd             = []

--------------------------------------------------------------------------------

-- | Given a @'Dec.Decoder'@, decode a @'FlatTerm'@ back into
-- an ordinary value, or return an error.
--
-- @since 0.2.0.0
fromFlatTerm :: (forall s. Decoder s a)
                                -- ^ A @'Dec.Decoder'@ for a serialised value.
             -> FlatTerm        -- ^ The serialised @'FlatTerm'@.
             -> Either String a -- ^ The deserialised value, or an error.
fromFlatTerm decoder ft =
    runST (getDecodeAction decoder >>= flip go ft)
  where
    go :: DecodeAction s a -> FlatTerm -> ST s (Either String a)
    go (ConsumeWord k)    (TkInt     n : ts)
        | n >= 0                             = k (unW# (fromIntegral n)) >>= flip go ts
    go (ConsumeWord k)    (TkInteger n : ts)
        | n >= 0                             = k (unW# (fromIntegral n)) >>= flip go ts
    go (ConsumeWord8 k)   (TkInt     n : ts)
        | n >= 0 && n <= maxWord8            = k (unW# (fromIntegral n)) >>= flip go ts
    go (ConsumeWord8 k)   (TkInteger n : ts)
        | n >= 0 && n <= maxWord8            = k (unW# (fromIntegral n)) >>= flip go ts
    go (ConsumeWord16 k)  (TkInt     n : ts)
        | n >= 0 && n <= maxWord16           = k (unW# (fromIntegral n)) >>= flip go ts
    go (ConsumeWord16 k)  (TkInteger n : ts)
        | n >= 0 && n <= maxWord16           = k (unW# (fromIntegral n)) >>= flip go ts
    go (ConsumeWord32 k)  (TkInt     n : ts)
        -- NOTE: we have to be very careful about this branch
        -- on 32 bit machines, because maxBound :: Int < maxBound :: Word32
        | intIsValidWord32 n                 = k (unW# (fromIntegral n)) >>= flip go ts
    go (ConsumeWord32 k)  (TkInteger n : ts)
        | n >= 0 && n <= maxWord32           = k (unW# (fromIntegral n)) >>= flip go ts
    go (ConsumeNegWord k) (TkInt     n : ts)
        | n <  0                             = k (unW# (fromIntegral (-1-n))) >>= flip go ts
    go (ConsumeNegWord k) (TkInteger n : ts)
        | n <  0                             = k (unW# (fromIntegral (-1-n))) >>= flip go ts
    go (ConsumeInt k)     (TkInt     n : ts) = k (unI# n) >>= flip go ts
    go (ConsumeInt k)     (TkInteger n : ts)
        | n <= maxInt                        = k (unI# (fromIntegral n)) >>= flip go ts
    go (ConsumeInt8 k)    (TkInt     n : ts)
        | n >= minInt8 && n <= maxInt8       = k (unI# n) >>= flip go ts
    go (ConsumeInt8 k)    (TkInteger n : ts)
        | n >= minInt8 && n <= maxInt8       = k (unI# (fromIntegral n)) >>= flip go ts
    go (ConsumeInt16 k)   (TkInt     n : ts)
        | n >= minInt16 && n <= maxInt16     = k (unI# n) >>= flip go ts
    go (ConsumeInt16 k)    (TkInteger n : ts)
        | n >= minInt16 && n <= maxInt16     = k (unI# (fromIntegral n)) >>= flip go ts
    go (ConsumeInt32 k)    (TkInt     n : ts)
        | n >= minInt32 && n <= maxInt32     = k (unI# n) >>= flip go ts
    go (ConsumeInt32 k)    (TkInteger n : ts)
        | n >= minInt32 && n <= maxInt32     = k (unI# (fromIntegral n)) >>= flip go ts
    go (ConsumeInteger k) (TkInt     n : ts) = k (fromIntegral n) >>= flip go ts
    go (ConsumeInteger k) (TkInteger n : ts) = k n >>= flip go ts
    go (ConsumeListLen k) (TkListLen n : ts)
        | n <= maxInt                        = k (unI# (fromIntegral n)) >>= flip go ts
    go (ConsumeMapLen  k) (TkMapLen  n : ts)
        | n <= maxInt                        = k (unI# (fromIntegral n)) >>= flip go ts
    go (ConsumeTag     k) (TkTag     n : ts)
        | n <= maxWord                       = k (unW# (fromIntegral n)) >>= flip go ts

#if defined(ARCH_32bit)
    -- 64bit variants for 32bit machines
    go (ConsumeWord64    k) (TkInt       n : ts)
      | n >= 0                                   = k (unW64# (fromIntegral n)) >>= flip go ts
    go (ConsumeWord64    k) (TkInteger   n : ts)
      | n >= 0                                   = k (unW64# (fromIntegral n)) >>= flip go ts
    go (ConsumeNegWord64 k) (TkInt       n : ts)
      | n < 0                                    = k (unW64# (fromIntegral (-1-n))) >>= flip go ts
    go (ConsumeNegWord64 k) (TkInteger   n : ts)
      | n < 0                                    = k (unW64# (fromIntegral (-1-n))) >>= flip go ts

    go (ConsumeInt64     k) (TkInt       n : ts) = k (unI64# (fromIntegral n)) >>= flip go ts
    go (ConsumeInt64     k) (TkInteger   n : ts) = k (unI64# (fromIntegral n)) >>= flip go ts

    go (ConsumeTag64     k) (TkTag       n : ts) = k (unW64# n) >>= flip go ts

    -- TODO FIXME (aseipp/dcoutts): are these going to be utilized?
    -- see fallthrough case below if/when fixed.
    go (ConsumeListLen64 _) ts                   = unexpected "decodeListLen64" ts
    go (ConsumeMapLen64  _) ts                   = unexpected "decodeMapLen64"  ts
#endif

    go (ConsumeFloat  k) (TkFloat16 f : ts) = k (unF# f) >>= flip go ts
    go (ConsumeFloat  k) (TkFloat32 f : ts) = k (unF# f) >>= flip go ts
    go (ConsumeDouble k) (TkFloat16 f : ts) = k (unD# (float2Double f)) >>= flip go ts
    go (ConsumeDouble k) (TkFloat32 f : ts) = k (unD# (float2Double f)) >>= flip go ts
    go (ConsumeDouble k) (TkFloat64 f : ts) = k (unD# f) >>= flip go ts
    go (ConsumeBytes  k) (TkBytes  bs : ts) = k bs >>= flip go ts
    go (ConsumeString k) (TkString st : ts) = k st >>= flip go ts
    go (ConsumeBool   k) (TkBool    b : ts) = k b >>= flip go ts
    go (ConsumeSimple k) (TkSimple  n : ts) = k (unW8# n) >>= flip go ts

    go (ConsumeBytesIndef   da) (TkBytesBegin  : ts) = da >>= flip go ts
    go (ConsumeStringIndef  da) (TkStringBegin : ts) = da >>= flip go ts
    go (ConsumeListLenIndef da) (TkListBegin   : ts) = da >>= flip go ts
    go (ConsumeMapLenIndef  da) (TkMapBegin    : ts) = da >>= flip go ts
    go (ConsumeNull         da) (TkNull        : ts) = da >>= flip go ts

    go (ConsumeListLenOrIndef k) (TkListLen n : ts)
        | n <= maxInt                               = k (unI# (fromIntegral n)) >>= flip go ts
    go (ConsumeListLenOrIndef k) (TkListBegin : ts) = k (-1#) >>= flip go ts
    go (ConsumeMapLenOrIndef  k) (TkMapLen  n : ts)
        | n <= maxInt                               = k (unI# (fromIntegral n)) >>= flip go ts
    go (ConsumeMapLenOrIndef  k) (TkMapBegin  : ts) = k (-1#) >>= flip go ts
    go (ConsumeBreakOr        k) (TkBreak     : ts) = k True >>= flip go ts
    go (ConsumeBreakOr        k) ts@(_        : _ ) = k False >>= flip go ts

    go (PeekTokenType k) ts@(tk:_) = k (tokenTypeOf tk) >>= flip go ts
    go (PeekTokenType _) ts        = unexpected "peekTokenType" ts
    go (PeekAvailable k) ts        = k (unI# (length ts)) >>= flip go ts

    go (Fail msg) _  = return $ Left msg
    go (Done x)   [] = return $ Right x
    go (Done _)   ts = return $ Left ("trailing tokens: " ++ show (take 5 ts))

    ----------------------------------------------------------------------------
    -- Fallthrough cases: unhandled token/DecodeAction combinations

    go (ConsumeWord    _) ts = unexpected "decodeWord"    ts
    go (ConsumeWord8   _) ts = unexpected "decodeWord8"   ts
    go (ConsumeWord16  _) ts = unexpected "decodeWord16"  ts
    go (ConsumeWord32  _) ts = unexpected "decodeWord32"  ts
    go (ConsumeNegWord _) ts = unexpected "decodeNegWord" ts
    go (ConsumeInt     _) ts = unexpected "decodeInt"     ts
    go (ConsumeInt8    _) ts = unexpected "decodeInt8"    ts
    go (ConsumeInt16   _) ts = unexpected "decodeInt16"   ts
    go (ConsumeInt32   _) ts = unexpected "decodeInt32"   ts
    go (ConsumeInteger _) ts = unexpected "decodeInteger" ts

    go (ConsumeListLen _) ts = unexpected "decodeListLen" ts
    go (ConsumeMapLen  _) ts = unexpected "decodeMapLen"  ts
    go (ConsumeTag     _) ts = unexpected "decodeTag"     ts

    go (ConsumeFloat  _) ts = unexpected "decodeFloat"  ts
    go (ConsumeDouble _) ts = unexpected "decodeDouble" ts
    go (ConsumeBytes  _) ts = unexpected "decodeBytes"  ts
    go (ConsumeString _) ts = unexpected "decodeString" ts
    go (ConsumeBool   _) ts = unexpected "decodeBool"   ts
    go (ConsumeSimple _) ts = unexpected "decodeSimple" ts

#if defined(ARCH_32bit)
    -- 64bit variants for 32bit machines
    go (ConsumeWord64    _) ts = unexpected "decodeWord64"    ts
    go (ConsumeNegWord64 _) ts = unexpected "decodeNegWord64" ts
    go (ConsumeInt64     _) ts = unexpected "decodeInt64"     ts
    go (ConsumeTag64     _) ts = unexpected "decodeTag64"     ts
  --go (ConsumeListLen64 _) ts = unexpected "decodeListLen64" ts
  --go (ConsumeMapLen64  _) ts = unexpected "decodeMapLen64"  ts
#endif

    go (ConsumeBytesIndef   _) ts = unexpected "decodeBytesIndef"   ts
    go (ConsumeStringIndef  _) ts = unexpected "decodeStringIndef"  ts
    go (ConsumeListLenIndef _) ts = unexpected "decodeListLenIndef" ts
    go (ConsumeMapLenIndef  _) ts = unexpected "decodeMapLenIndef"  ts
    go (ConsumeNull         _) ts = unexpected "decodeNull"         ts

    go (ConsumeListLenOrIndef _) ts = unexpected "decodeListLenOrIndef" ts
    go (ConsumeMapLenOrIndef  _) ts = unexpected "decodeMapLenOrIndef"  ts
    go (ConsumeBreakOr        _) ts = unexpected "decodeBreakOr"        ts

    unexpected name []      = return $ Left $ name ++ ": unexpected end of input"
    unexpected name (tok:_) = return $ Left $ name ++ ": unexpected token " ++ show tok

-- | Map a @'TermToken'@ to the underlying CBOR @'TokenType'@
tokenTypeOf :: TermToken -> TokenType
tokenTypeOf (TkInt n)
    | n >= 0                = TypeUInt
    | otherwise             = TypeNInt
tokenTypeOf TkInteger{}     = TypeInteger
tokenTypeOf TkBytes{}       = TypeBytes
tokenTypeOf TkBytesBegin{}  = TypeBytesIndef
tokenTypeOf TkString{}      = TypeString
tokenTypeOf TkStringBegin{} = TypeStringIndef
tokenTypeOf TkListLen{}     = TypeListLen
tokenTypeOf TkListBegin{}   = TypeListLenIndef
tokenTypeOf TkMapLen{}      = TypeMapLen
tokenTypeOf TkMapBegin{}    = TypeMapLenIndef
tokenTypeOf TkTag{}         = TypeTag
tokenTypeOf TkBool{}        = TypeBool
tokenTypeOf TkNull          = TypeNull
tokenTypeOf TkBreak         = TypeBreak
tokenTypeOf TkSimple{}      = TypeSimple
tokenTypeOf TkFloat16{}     = TypeFloat16
tokenTypeOf TkFloat32{}     = TypeFloat32
tokenTypeOf TkFloat64{}     = TypeFloat64

--------------------------------------------------------------------------------

-- | Ensure a @'FlatTerm'@ is internally consistent and was created in a valid
-- manner.
--
-- @since 0.2.0.0
validFlatTerm :: FlatTerm -- ^ The input @'FlatTerm'@
              -> Bool     -- ^ @'True'@ if valid, @'False'@ otherwise.
validFlatTerm ts =
   either (const False) (const True) $ do
     ts' <- validateTerm TopLevelSingle ts
     case ts' of
       [] -> return ()
       _  -> Left "trailing data"

-- | A data type used for tracking the position we're at
-- as we traverse a @'FlatTerm'@ and make sure it's valid.
data Loc = TopLevelSingle
         | TopLevelSequence
         | InString   Int     Loc
         | InBytes    Int     Loc
         | InListN    Int Int Loc
         | InList     Int     Loc
         | InMapNKey  Int Int Loc
         | InMapNVal  Int Int Loc
         | InMapKey   Int     Loc
         | InMapVal   Int     Loc
         | InTagged   Word64  Loc
  deriving Show

-- | Validate an arbitrary @'FlatTerm'@ at an arbitrary location.
validateTerm :: Loc -> FlatTerm -> Either String FlatTerm
validateTerm _loc (TkInt       _   : ts) = return ts
validateTerm _loc (TkInteger   _   : ts) = return ts
validateTerm _loc (TkBytes     _   : ts) = return ts
validateTerm  loc (TkBytesBegin    : ts) = validateBytes loc 0 ts
validateTerm _loc (TkString    _   : ts) = return ts
validateTerm  loc (TkStringBegin   : ts) = validateString loc 0 ts
validateTerm  loc (TkListLen   len : ts)
    | len <= maxInt                      = validateListN loc 0 (fromIntegral len) ts
    | otherwise                          = Left "list len too long (> max int)"
validateTerm  loc (TkListBegin     : ts) = validateList  loc 0     ts
validateTerm  loc (TkMapLen    len : ts)
    | len <= maxInt                      = validateMapN  loc 0 (fromIntegral len) ts
    | otherwise                          = Left "map len too long (> max int)"
validateTerm  loc (TkMapBegin      : ts) = validateMap   loc 0     ts
validateTerm  loc (TkTag       w   : ts) = validateTerm  (InTagged w loc) ts
validateTerm _loc (TkBool      _   : ts) = return ts
validateTerm _loc (TkNull          : ts) = return ts
validateTerm  loc (TkBreak         : _)  = unexpectedToken TkBreak loc
validateTerm _loc (TkSimple  _     : ts) = return ts
validateTerm _loc (TkFloat16 _     : ts) = return ts
validateTerm _loc (TkFloat32 _     : ts) = return ts
validateTerm _loc (TkFloat64 _     : ts) = return ts
validateTerm  loc                    []  = unexpectedEof loc

unexpectedToken :: TermToken -> Loc -> Either String a
unexpectedToken tok loc = Left $ "unexpected token " ++ show tok
                              ++ ", in context " ++ show loc

unexpectedEof :: Loc -> Either String a
unexpectedEof loc = Left $ "unexpected end of input in context " ++ show loc

validateBytes :: Loc -> Int -> [TermToken] -> Either String [TermToken]
validateBytes _    _ (TkBreak   : ts) = return ts
validateBytes ploc i (TkBytes _ : ts) = validateBytes ploc (i+1) ts
validateBytes ploc i (tok       : _)  = unexpectedToken tok (InBytes i ploc)
validateBytes ploc i []               = unexpectedEof       (InBytes i ploc)

validateString :: Loc -> Int -> [TermToken] -> Either String [TermToken]
validateString _    _ (TkBreak    : ts) = return ts
validateString ploc i (TkString _ : ts) = validateString ploc (i+1) ts
validateString ploc i (tok        : _)  = unexpectedToken tok (InString i ploc)
validateString ploc i []                = unexpectedEof       (InString i ploc)

validateListN :: Loc -> Int -> Int -> [TermToken] -> Either String [TermToken]
validateListN    _ i len ts | i == len = return ts
validateListN ploc i len ts = do
    ts' <- validateTerm (InListN i len ploc) ts
    validateListN ploc (i+1) len ts'

validateList :: Loc -> Int -> [TermToken] -> Either String [TermToken]
validateList _    _ (TkBreak : ts) = return ts
validateList ploc i ts = do
    ts' <- validateTerm (InList i ploc) ts
    validateList ploc (i+1) ts'

validateMapN :: Loc -> Int -> Int -> [TermToken] -> Either String [TermToken]
validateMapN    _ i len ts  | i == len = return ts
validateMapN ploc i len ts  = do
    ts'  <- validateTerm (InMapNKey i len ploc) ts
    ts'' <- validateTerm (InMapNVal i len ploc) ts'
    validateMapN ploc (i+1) len ts''

validateMap :: Loc -> Int -> [TermToken] -> Either String [TermToken]
validateMap _    _ (TkBreak : ts) = return ts
validateMap ploc i ts = do
    ts'  <- validateTerm (InMapKey i ploc) ts
    ts'' <- validateTerm (InMapVal i ploc) ts'
    validateMap ploc (i+1) ts''

--------------------------------------------------------------------------------
-- Utilities

maxInt, minInt, maxWord :: Num n => n
maxInt    = fromIntegral (maxBound :: Int)
minInt    = fromIntegral (minBound :: Int)
maxWord   = fromIntegral (maxBound :: Word)

maxInt8, minInt8, maxWord8 :: Num n => n
maxInt8    = fromIntegral (maxBound :: Int8)
minInt8    = fromIntegral (minBound :: Int8)
maxWord8   = fromIntegral (maxBound :: Word8)

maxInt16, minInt16, maxWord16 :: Num n => n
maxInt16    = fromIntegral (maxBound :: Int16)
minInt16    = fromIntegral (minBound :: Int16)
maxWord16   = fromIntegral (maxBound :: Word16)

maxInt32, minInt32, maxWord32 :: Num n => n
maxInt32    = fromIntegral (maxBound :: Int32)
minInt32    = fromIntegral (minBound :: Int32)
maxWord32   = fromIntegral (maxBound :: Word32)

-- | Do a careful check to ensure an @'Int'@ is in the
-- range of a @'Word32'@.
intIsValidWord32 :: Int -> Bool
intIsValidWord32 n = b1 && b2
  where
    -- NOTE: this first comparison must use Int for
    -- the check, not Word32, in case a negative value
    -- is given. Otherwise this check would fail due to
    -- overflow.
    b1 = n >= 0
    -- NOTE: we must convert n to Word32, otherwise,
    -- maxWord32 is inferred as Int, and because
    -- the maxBound of Word32 is greater than Int,
    -- it overflows and this check fails.
    b2 = (fromIntegral n :: Word32) <= maxWord32

unI# :: Int -> Int#
unI#   (I#   i#) = i#

unW# :: Word -> Word#
unW#   (W#  w#) = w#

unW8# :: Word8 -> Word#
unW8#  (W8# w#) = w#

unF# :: Float -> Float#
unF#   (F#   f#) = f#

unD# :: Double -> Double#
unD#   (D#   f#) = f#

#if defined(ARCH_32bit)
unW64# :: Word64 -> Word64#
unW64# (W64# w#) = w#

unI64# :: Int64 -> Int64#
unI64# (I64# i#) = i#
#endif
