{-# LANGUAGE CPP                 #-}
{-# LANGUAGE DeriveFunctor       #-}
{-# LANGUAGE MagicHash           #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving  #-}
{-# LANGUAGE UnboxedTuples       #-}

-- |
-- Module      : Codec.CBOR.Pretty
-- Copyright   : (c) Duncan Coutts 2015-2017
-- License     : BSD3-style (see LICENSE.txt)
--
-- Maintainer  : duncan@community.haskell.org
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Pretty printing tools for debugging and analysis.
--
module Codec.CBOR.Pretty
  ( prettyHexEnc -- :: Encoding -> String
  ) where

#include "cbor.h"

import           Data.Word

import qualified Data.ByteString                     as S
import qualified Data.Text                           as T

import           Codec.CBOR.ByteArray.Sliced
import           Codec.CBOR.Encoding
import           Codec.CBOR.Write

import qualified Control.Monad.Fail as Fail
import           Control.Monad                       (replicateM_)
import           GHC.Int (Int64)
import           Numeric
#if !MIN_VERSION_base(4,8,0)
import           Control.Applicative
#endif

--------------------------------------------------------------------------------

newtype PP a = PP (Tokens -> Int -> ShowS -> Either String (Tokens,Int,ShowS,a))

-- | Pretty prints an 'Encoding' in an annotated, hexadecimal format
-- that maps CBOR values to their types. The output format is similar
-- to the format used on http://cbor.me/.
--
-- For example, with the term:
--
-- @
-- 'Prelude.putStrLn' . 'prettyHexEnc' . 'Codec.CBOR.encode' $
--   ( True
--   , [1,2,3::Int]
--   , ('Data.Map.fromList' [(\"Hello\",True),(\"World\",False)], "This is a long string which wraps")
--   )
-- @
--
-- You get:
--
-- @
-- 83      # list(3)
--    f5   # bool(true)
--    9f   # list(*)
--       01        # int(1)
--       02        # int(2)
--       03        # int(3)
--    ff   # break
--    82   # list(2)
--       a2        # map(2)
--          65 48 65 6c 6c 6f      # text(\"Hello\")
--          f5     # bool(true)
--          65 57 6f 72 6c 64      # text(\"World\")
--          f4     # bool(false)
--       78 21 54 68 69 73 20 69 73 20 61 20 6c 6f 6e 67
--       20 73 74 72 69 6e 67 20 77 68 69 63 68 20 77 72
--       61 70 73          # text("This is a long string which wraps")
-- @
--
-- @since 0.2.0.0
prettyHexEnc :: Encoding -> String
prettyHexEnc e = case runPP pprint e of
  Left s -> s
  Right (TkEnd,_,ss,_) -> ss ""
  Right (toks,_,ss,_) -> ss $ "\nprettyEnc: Not all input was consumed (this is probably a problem with the pretty printing code). Tokens left: " ++ show toks

runPP :: PP a -> Encoding -> Either String (Tokens, Int, ShowS, a)
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
#if !MIN_VERSION_base(4,13,0)
  fail = Fail.fail
#endif

instance Fail.MonadFail PP where
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

appShowS :: ShowS -> PP ()
appShowS s = PP $ \toks ind ss -> Right (toks,ind,ss . s,())

str :: String -> PP ()
str = appShowS . showString

shown :: Show a => a -> PP ()
shown = appShowS . shows

parens :: PP a -> PP a
parens pp = str "(" *> pp <* str ")"

indef :: PP () -> PP ()
indef pp = do
  tk <- peekTerm
  case tk of
    TkBreak TkEnd -> dec 3 >> pprint
    _ -> pp >> indef pp


pprint :: PP ()
pprint = do
  nl
  term <- getTerm
  hexRep term
  str " "
  case term of
    TkInt      i   TkEnd     -> ppTkInt i
    TkInt      _   _         -> termFailure term
    TkInt64    i   TkEnd     -> ppTkInt64 i
    TkInt64    _   _         -> termFailure term
    TkInteger  i   TkEnd     -> ppTkInteger i
    TkInteger  _   _         -> termFailure term
    TkWord64   w   TkEnd     -> ppTkWord64 w
    TkWord64   _   _         -> termFailure term
    TkWord     w   TkEnd     -> ppTkWord w
    TkWord     _   _         -> termFailure term
    TkBytes    bs  TkEnd     -> ppTkBytes bs
    TkBytes    _   _         -> termFailure term
    TkBytesBegin   TkEnd     -> ppTkBytesBegin
    TkBytesBegin   _         -> termFailure term
    TkByteArray ba TkEnd     -> ppTkByteArray ba
    TkByteArray _   _        -> termFailure term
    TkUtf8ByteArray ba TkEnd -> ppTkUtf8ByteArray ba
    TkUtf8ByteArray _   _    -> termFailure term
    TkString   t   TkEnd     -> ppTkString t
    TkString   _   _         -> termFailure term
    TkStringBegin  TkEnd     -> ppTkStringBegin
    TkStringBegin  _         -> termFailure term
    TkListLen  w   TkEnd     -> ppTkListLen w
    TkListLen  _   _         -> termFailure term
    TkListBegin    TkEnd     -> ppTkListBegin
    TkListBegin    _         -> termFailure term
    TkMapLen   w   TkEnd     -> ppTkMapLen w
    TkMapLen   _   _         -> termFailure term
    TkMapBegin     TkEnd     -> ppTkMapBegin
    TkMapBegin     _         -> termFailure term
    TkBreak        TkEnd     -> ppTkBreak
    TkBreak        _         -> termFailure term
    TkTag      w   TkEnd     -> ppTkTag w
    TkTag      _   _         -> termFailure term
    TkTag64    w   TkEnd     -> ppTkTag64 w
    TkTag64    _   _         -> termFailure term
    TkBool     b   TkEnd     -> ppTkBool b
    TkBool     _   _         -> termFailure term
    TkNull         TkEnd     -> ppTkNull
    TkNull         _         -> termFailure term
    TkUndef        TkEnd     -> ppTkUndef
    TkUndef        _         -> termFailure term
    TkSimple   w   TkEnd     -> ppTkSimple w
    TkSimple   _   _         -> termFailure term
    TkFloat16  f   TkEnd     -> ppTkFloat16 f
    TkFloat16  _   _         -> termFailure term
    TkFloat32  f   TkEnd     -> ppTkFloat32 f
    TkFloat32  _   _         -> termFailure term
    TkFloat64  f   TkEnd     -> ppTkFloat64 f
    TkFloat64  _   _         -> termFailure term
    TkEncoded  _   TkEnd     -> ppTkEncoded
    TkEncoded  _   _         -> termFailure term
    TkEnd                    -> str "# End of input"
 where
   termFailure t = fail $ unwords ["pprint: Unexpected token:", show t]

ppTkInt        :: Int        -> PP ()
ppTkInt i = str "# int" >> parens (shown i)

ppTkInt64      :: Int64      -> PP ()
ppTkInt64 i = str "# int" >> parens (shown i)

ppTkInteger    :: Integer    -> PP ()
ppTkInteger i = str "# integer" >> parens (shown i)

ppTkWord64     :: Word64     -> PP ()
ppTkWord64 w = str "# word" >> parens (shown w)

ppTkWord       :: Word       -> PP ()
ppTkWord w = str "# word" >> parens (shown w)

ppTkByteArray  :: SlicedByteArray -> PP ()
ppTkByteArray bs = str "# bytes" >> parens (shown $ sizeofSlicedByteArray bs)

ppTkUtf8ByteArray  :: SlicedByteArray -> PP ()
ppTkUtf8ByteArray bs = str "# text" >> parens (shown $ sizeofSlicedByteArray bs)

ppTkBytes      :: S.ByteString -> PP ()
ppTkBytes bs = str "# bytes" >> parens (shown (S.length bs))

ppTkBytesBegin ::               PP ()
ppTkBytesBegin = str "# bytes(*)" >> inc 3 >> indef pprint

ppTkString     :: T.Text     -> PP ()
ppTkString t = str "# text" >> parens (shown t)

ppTkStringBegin::               PP ()
ppTkStringBegin = str "# text(*)" >> inc 3 >> indef pprint

ppTkEncoded    ::               PP ()
ppTkEncoded = str "# pre-encoded CBOR term"

ppTkListLen    :: Word       -> PP ()
ppTkListLen n = do
  str "# list"
  parens (shown n)
  inc 3
  replicateM_ (fromIntegral n) pprint
  dec 3

ppTkListBegin  ::               PP ()
ppTkListBegin = str "# list(*)" >> inc 3 >> indef pprint

ppMapPairs :: PP ()
ppMapPairs = do
  nl
  inc 3
  indent
  str " # key"
  pprint
  dec 3
  -- str " [end map key]"
  nl
  inc 3
  indent
  str " # value"
  pprint
  dec 3
  -- str " [end map value]"

ppTkMapLen     :: Word       -> PP ()
ppTkMapLen w = do
  str "# map"
  parens (shown w)
  -- inc 3
  replicateM_ (fromIntegral w) ppMapPairs
  -- dec 3

ppTkMapBegin   ::               PP ()
ppTkMapBegin = str "# map(*)" >> inc 3 >> indef ppMapPairs

ppTkBreak      ::               PP ()
ppTkBreak = str "# break"

ppTkTag        :: Word       -> PP ()
ppTkTag w = do
  str "# tag"
  parens (shown w)
  inc 3
  pprint
  dec 3

ppTkTag64      :: Word64     -> PP ()
ppTkTag64 w = do
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

ppTkUndef       ::              PP ()
ppTkUndef = str "# undefined"

ppTkSimple     :: Word8      -> PP ()
ppTkSimple w = str "# simple" >> parens (shown w)

ppTkFloat16    :: Float      -> PP ()
ppTkFloat16 f = str "# float16" >> parens (shown f)

ppTkFloat32    :: Float      -> PP ()
ppTkFloat32 f = str "# float32" >> parens (shown f)

ppTkFloat64    :: Double     -> PP ()
ppTkFloat64 f = str "# float64" >> parens (shown f)

unconsToken :: Tokens -> Maybe (Tokens, Tokens)
unconsToken TkEnd               = Nothing
unconsToken (TkWord w      tks) = Just (TkWord w      TkEnd,tks)
unconsToken (TkWord64 w    tks) = Just (TkWord64 w    TkEnd,tks)
unconsToken (TkInt i       tks) = Just (TkInt i       TkEnd,tks)
unconsToken (TkInt64 i     tks) = Just (TkInt64 i     TkEnd,tks)
unconsToken (TkBytes bs    tks) = Just (TkBytes bs    TkEnd,tks)
unconsToken (TkBytesBegin  tks) = Just (TkBytesBegin  TkEnd,tks)
unconsToken (TkByteArray a tks) = Just (TkByteArray a TkEnd,tks)
unconsToken (TkString t    tks) = Just (TkString t    TkEnd,tks)
unconsToken (TkStringBegin tks) = Just (TkStringBegin TkEnd,tks)
unconsToken (TkUtf8ByteArray a tks) = Just (TkUtf8ByteArray a TkEnd,tks)
unconsToken (TkListLen len tks) = Just (TkListLen len TkEnd,tks)
unconsToken (TkListBegin   tks) = Just (TkListBegin   TkEnd,tks)
unconsToken (TkMapLen len  tks) = Just (TkMapLen len  TkEnd,tks)
unconsToken (TkMapBegin    tks) = Just (TkMapBegin    TkEnd,tks)
unconsToken (TkTag w       tks) = Just (TkTag w       TkEnd,tks)
unconsToken (TkTag64 w64   tks) = Just (TkTag64 w64   TkEnd,tks)
unconsToken (TkInteger i   tks) = Just (TkInteger i   TkEnd,tks)
unconsToken (TkNull        tks) = Just (TkNull        TkEnd,tks)
unconsToken (TkUndef       tks) = Just (TkUndef       TkEnd,tks)
unconsToken (TkBool b      tks) = Just (TkBool b      TkEnd,tks)
unconsToken (TkSimple w8   tks) = Just (TkSimple w8   TkEnd,tks)
unconsToken (TkFloat16 f16 tks) = Just (TkFloat16 f16 TkEnd,tks)
unconsToken (TkFloat32 f32 tks) = Just (TkFloat32 f32 TkEnd,tks)
unconsToken (TkFloat64 f64 tks) = Just (TkFloat64 f64 TkEnd,tks)
unconsToken (TkEncoded bs  tks) = Just (TkEncoded bs  TkEnd,tks)
unconsToken (TkBreak       tks) = Just (TkBreak       TkEnd,tks)

hexRep :: Tokens -> PP ()
hexRep tk = go . toStrictByteString . Encoding $ const tk where
  go bs | S.length bs > 16 = case S.splitAt 16 bs of
          (h,t) -> indent >> appShowS (hexBS h) >> nl >> go t
        | otherwise = indent >> appShowS (hexBS bs)

hexBS :: S.ByteString -> ShowS
hexBS = foldr (.) id . map (\n -> ((if n < 16 then ('0':) else id) . showHex n . (' ':))) . S.unpack
