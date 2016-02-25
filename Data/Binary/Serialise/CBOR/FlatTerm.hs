{-# LANGUAGE CPP, MagicHash #-}
module Data.Binary.Serialise.CBOR.FlatTerm (
    FlatTerm,
    TermToken(..),
    toFlatTerm,
    fromFlatTerm,

    Valid,
    Loc(..),
    validFlatTerm,
    validateTerm
  ) where

import Data.Binary.Serialise.CBOR.Encoding (Encoding(..))
import qualified Data.Binary.Serialise.CBOR.Encoding as Enc
import Data.Binary.Serialise.CBOR.Decoding as Dec

import GHC.Word  (Word(W#), Word8(W8#))
import GHC.Exts  (Int(I#), Int#, Word#, Float#, Double#)
import GHC.Float (Float(F#), Double(D#), float2Double)

import Data.Word
import Data.Text (Text)
import Data.ByteString (ByteString)

#include "MachDeps.h"

#if WORD_SIZE_IN_BITS == 64
#define ARCH_64bit
#elif WORD_SIZE_IN_BITS == 32
#else
#error expected WORD_SIZE_IN_BITS to be 32 or 64
#endif


type FlatTerm = [TermToken]

data TermToken =

      TkInt      {-# UNPACK #-} !Int
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
    | TkUndef
    | TkSimple   {-# UNPACK #-} !Word8
    | TkFloat16  {-# UNPACK #-} !Float
    | TkFloat32  {-# UNPACK #-} !Float
    | TkFloat64  {-# UNPACK #-} !Double

  deriving (Eq, Ord, Show)


toFlatTerm :: Encoding -> FlatTerm
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
  | n <= maxInt                     = TkInt     (fromIntegral n) : convFlatTerm ts
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
convFlatTerm (Enc.TkUndef       ts) = TkUndef       : convFlatTerm ts
convFlatTerm (Enc.TkSimple   n  ts) = TkSimple    n : convFlatTerm ts
convFlatTerm (Enc.TkFloat16  f  ts) = TkFloat16   f : convFlatTerm ts
convFlatTerm (Enc.TkFloat32  f  ts) = TkFloat32   f : convFlatTerm ts
convFlatTerm (Enc.TkFloat64  f  ts) = TkFloat64   f : convFlatTerm ts
convFlatTerm (Enc.TkBreak       ts) = TkBreak       : convFlatTerm ts
convFlatTerm  Enc.TkEnd             = []

maxInt, minInt, maxWord :: Num n => n
maxInt    = fromIntegral (maxBound :: Int)
minInt    = fromIntegral (minBound :: Int)
maxWord   = fromIntegral (maxBound :: Word)

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

fromFlatTerm :: Decoder a -> FlatTerm -> Either String a
fromFlatTerm decoder =
    go (getDecodeAction decoder)
  where
    go (ConsumeWord k)    (TkInt     n : ts)
        | n >= 0                             = go (k (unW# (fromIntegral n))) ts
    go (ConsumeNegWord k) (TkInt     n : ts)
        | n <  0                             = go (k (unW# (fromIntegral (-1-n)))) ts
    go (ConsumeInt k)     (TkInt     n : ts) = go (k (unI# n)) ts
    go (ConsumeInteger k) (TkInt     n : ts) = go (k (fromIntegral n)) ts
    go (ConsumeInteger k) (TkInteger n : ts) = go (k n) ts
    go (ConsumeListLen k) (TkListLen n : ts)
        | n <= maxInt                        = go (k (unI# (fromIntegral n))) ts
    go (ConsumeMapLen  k) (TkMapLen  n : ts)
        | n <= maxInt                        = go (k (unI# (fromIntegral n))) ts
    go (ConsumeTag     k) (TkTag     n : ts)
        | n <= maxWord                       = go (k (unW# (fromIntegral n))) ts

-- 64bit variants for 32bit machines
#ifndef ARCH_64bit
    go (ConsumeWord64 k) =
    go (ConsumeNegWord64 k) =
    go (ConsumeInt64 k) =
    go (ConsumeListLen64 k) =
    go (ConsumeMapLen64 k) =
    go (ConsumeTag64 k) =
#endif

    go (ConsumeFloat  k) (TkFloat16 f : ts) = go (k (unF# f)) ts
    go (ConsumeFloat  k) (TkFloat32 f : ts) = go (k (unF# f)) ts
    go (ConsumeDouble k) (TkFloat16 f : ts) = go (k (unD# (float2Double f))) ts
    go (ConsumeDouble k) (TkFloat32 f : ts) = go (k (unD# (float2Double f))) ts
    go (ConsumeDouble k) (TkFloat64 f : ts) = go (k (unD# f)) ts
    go (ConsumeBytes  k) (TkBytes  bs : ts) = go (k bs) ts
    go (ConsumeString k) (TkString st : ts) = go (k st) ts
    go (ConsumeBool   k) (TkBool    b : ts) = go (k b) ts
    go (ConsumeSimple k) (TkSimple  n : ts) = go (k (unW8# n)) ts

    go (ConsumeBytesIndef   da) (TkBytesBegin  : ts) = go da ts
    go (ConsumeStringIndef  da) (TkStringBegin : ts) = go da ts
    go (ConsumeListLenIndef da) (TkListBegin   : ts) = go da ts
    go (ConsumeMapLenIndef  da) (TkMapBegin    : ts) = go da ts
    go (ConsumeNull         da) (TkNull        : ts) = go da ts

    go (ConsumeListLenOrIndef k) (TkListLen n : ts)
        | n <= maxInt                               = go (k (unI# (fromIntegral n))) ts
    go (ConsumeListLenOrIndef k) (TkListBegin : ts) = go (k (-1#)) ts
    go (ConsumeMapLenOrIndef  k) (TkMapLen  n : ts)
        | n <= maxInt                               = go (k (unI# (fromIntegral n))) ts
    go (ConsumeMapLenOrIndef  k) (TkMapBegin  : ts) = go (k (-1#)) ts
    go (ConsumeBreakOr        k) (TkBreak     : ts) = go (k True) ts
    go (ConsumeBreakOr        k) ts@(_        : _ ) = go (k False) ts

    go (PeekTokenType k) ts@(tk:_) = go (k (tokenTypeOf tk)) ts
    go (PeekTokenType _) ts        = unexpected "peekTokenType" ts

    go (Fail msg) _  = Left msg
    go (Done x)   [] = Right x
    go (Done _)   ts = Left ("trailing tokens: " ++ show (take 5 ts))

    go (ConsumeWord    _) ts = unexpected "decodeWord"    ts
    go (ConsumeNegWord _) ts = unexpected "decodeNegWord" ts
    go (ConsumeInt     _) ts = unexpected "decodeInt"     ts
    go (ConsumeInteger _) ts = unexpected "decodeInteger" ts

    go (ConsumeListLen _) ts = unexpected "decodeListLen" ts
    go (ConsumeMapLen  _) ts = unexpected "decodeMapLen"  ts
    go (ConsumeTag     _) ts = unexpected "decodeTag"     ts
#ifndef ARCH_64bit
    go (ConsumeWord64    _) = unexpected "decodeWord64"    ts
    go (ConsumeNegWord64 _) = unexpected "decodeNegWord64" ts
    go (ConsumeInt64     _) = unexpected "decodeInt64"     ts
    go (ConsumeListLen64 _) = unexpected "decodeListLen64" ts
    go (ConsumeMapLen64  _) = unexpected "decodeMapLen64"  ts
    go (ConsumeTag64     _) = unexpected "decodeTag64"     ts
#endif

    go (ConsumeFloat  _) ts = unexpected "decodeFloat"  ts
    go (ConsumeDouble _) ts = unexpected "decodeDouble" ts
    go (ConsumeBytes  _) ts = unexpected "decodeBytes"  ts
    go (ConsumeString _) ts = unexpected "decodeString" ts
    go (ConsumeBool   _) ts = unexpected "decodeBool"   ts
    go (ConsumeSimple _) ts = unexpected "decodeSimple" ts

    go (ConsumeBytesIndef   _) ts = unexpected "decodeBytesIndef"   ts
    go (ConsumeStringIndef  _) ts = unexpected "decodeStringIndef"  ts
    go (ConsumeListLenIndef _) ts = unexpected "decodeListLenIndef" ts
    go (ConsumeMapLenIndef  _) ts = unexpected "decodeMapLenIndef"  ts
    go (ConsumeNull         _) ts = unexpected "decodeNull"         ts

    go (ConsumeListLenOrIndef _) ts = unexpected "decodeListLenOrIndef" ts
    go (ConsumeMapLenOrIndef  _) ts = unexpected "decodeMapLenOrIndef"  ts
    go (ConsumeBreakOr        _) ts = unexpected "decodeBreakOr"        ts


    unexpected name []      = Left $ name ++ ": unexpected end of input"
    unexpected name (tok:_) = Left $ name ++ ": unexpected token " ++ show tok


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
tokenTypeOf TkUndef         = TypeUndef
tokenTypeOf TkBreak         = TypeBreak
tokenTypeOf TkSimple{}      = TypeSimple
tokenTypeOf TkFloat16{}     = TypeFloat16
tokenTypeOf TkFloat32{}     = TypeFloat32
tokenTypeOf TkFloat64{}     = TypeFloat64


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

validFlatTerm :: FlatTerm -> Bool
validFlatTerm ts =
   either (const False) (const True) $ do
     ts' <- validateTerm TopLevelSingle ts
     case ts' of
       [] -> return ()
       _  -> Left "trailing data"

type Valid a = Either String a

validateTerm :: Loc -> FlatTerm -> Valid FlatTerm
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
validateTerm _loc (TkUndef         : ts) = return ts
validateTerm  loc (TkBreak         : _)  = unexpectedToken TkBreak loc
validateTerm _loc (TkSimple  _     : ts) = return ts
validateTerm _loc (TkFloat16 _     : ts) = return ts
validateTerm _loc (TkFloat32 _     : ts) = return ts
validateTerm _loc (TkFloat64 _     : ts) = return ts
validateTerm  loc                    []  = unexpectedEof loc

unexpectedToken :: TermToken -> Loc -> Valid a
unexpectedToken tok loc = Left $ "unexpected token " ++ show tok
                              ++ ", in context " ++ show loc

unexpectedEof :: Loc -> Valid a
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

