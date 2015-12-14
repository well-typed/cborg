{-# LANGUAGE BangPatterns #-}

-- |
-- Module      : Data.Binary.Serialise.CBOR.Term
-- Copyright   : (c) Duncan Coutts 2015
-- License     : BSD3-style (see LICENSE.txt)
--
-- Maintainer  : duncan@community.haskell.org
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Lorem ipsum...
--
module Data.Binary.Serialise.CBOR.Term (
    Term(..),
    encodeTerm,
    decodeTerm,
    ignoreTerm,
  ) where

import Data.Binary.Serialise.CBOR.Encoding hiding (Tokens(..))
import Data.Binary.Serialise.CBOR.Decoding
import Data.Binary.Serialise.CBOR.Class (Serialise(..))

import           Data.Word
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import           Data.Monoid
import           Control.Applicative
import           Control.Monad

import Prelude hiding (encodeFloat, decodeFloat)


-- | A general CBOR term
--
data Term = TInt     {-# UNPACK #-} !Int
          | TInteger                !Integer
          | TBytes                  !BS.ByteString
          | TBytesI                 !LBS.ByteString
          | TString                 !T.Text
          | TStringI                !LT.Text
          | TList                   ![Term]
          | TListI                  ![Term]
          | TMap                    ![(Term, Term)]
          | TMapI                   ![(Term, Term)]
          | TTagged  {-# UNPACK #-} !Word64 !Term
          | TBool                   !Bool
          | TNull
          | TUndef
          | TSimple  {-# UNPACK #-} !Word8
          | THalf    {-# UNPACK #-} !Float
          | TFloat   {-# UNPACK #-} !Float
          | TDouble  {-# UNPACK #-} !Double
  deriving (Eq, Ord, Show, Read)


instance Serialise Term where
  encode = encodeTerm
  decode = decodeTerm

encodeTerm :: Term -> Encoding
encodeTerm (TInt      n)  = encodeInt n
encodeTerm (TInteger  n)  = encodeInteger n
encodeTerm (TBytes   bs)  = encodeBytes bs
encodeTerm (TString  st)  = encodeString st
encodeTerm (TBytesI bss)  = encodeBytesIndef
                            <> mconcat [ encodeBytes bs
                                       | bs <- LBS.toChunks bss ]
                            <> encodeBreak
encodeTerm (TStringI sts) = encodeStringIndef
                            <> mconcat [ encodeString str
                                       | str <- LT.toChunks sts ]
                            <> encodeBreak
encodeTerm (TList    ts)  = encodeListLen (fromIntegral $ length ts)
                            <> mconcat [ encodeTerm t | t <- ts ]
encodeTerm (TListI   ts)  = encodeListLenIndef
                            <> mconcat [ encodeTerm t | t <- ts ]
                            <> encodeBreak
encodeTerm (TMap     ts)  = encodeMapLen (fromIntegral $ length ts)
                            <> mconcat [ encodeTerm t <> encodeTerm t'
                                       | (t, t') <- ts ]
encodeTerm (TMapI ts)     = encodeMapLenIndef
                            <> mconcat [ encodeTerm t <> encodeTerm t'
                                       | (t, t') <- ts ]
                            <> encodeBreak
encodeTerm (TTagged w t)  = encodeTag64 w <> encodeTerm t
encodeTerm (TBool     b)  = encodeBool b
encodeTerm  TNull         = encodeNull
encodeTerm  TUndef        = encodeUndef
encodeTerm (TSimple   w)  = encodeSimple w
encodeTerm (THalf     f)  = encodeFloat16 f
encodeTerm (TFloat    f)  = encodeFloat   f
encodeTerm (TDouble   f)  = encodeDouble  f


ignoreTerm :: Decoder ()
ignoreTerm = decodeTerm >> return () --TODO: optimised impl

decodeTerm :: Decoder Term
decodeTerm = do
    tkty <- peekTokenType
    case tkty of
      TypeUInt   -> do w <- decodeWord
                       return $! fromWord w
                    where
                      fromWord :: Word -> Term
                      fromWord w
                        | w <= fromIntegral (maxBound :: Int)
                                    = TInt     (fromIntegral w)
                        | otherwise = TInteger (fromIntegral w)

      TypeUInt64 -> do w <- decodeWord64
                       return $! fromWord64 w
                    where
                      fromWord64 w
                        | w <= fromIntegral (maxBound :: Int)
                                    = TInt     (fromIntegral w)
                        | otherwise = TInteger (fromIntegral w)

      TypeNInt   -> do w <- decodeNegWord
                       return $! fromNegWord w
                    where
                      fromNegWord w
                        | w <= fromIntegral (maxBound :: Int)
                                    = TInt     (-1 - fromIntegral w)
                        | otherwise = TInteger (-1 - fromIntegral w)

      TypeNInt64 -> do w <- decodeNegWord64
                       return $! fromNegWord64 w
                    where
                      fromNegWord64 w
                        | w <= fromIntegral (maxBound :: Int)
                                    = TInt     (-1 - fromIntegral w)
                        | otherwise = TInteger (-1 - fromIntegral w)

      TypeInteger -> do !x <- decodeInteger
                        return (TInteger x)
      TypeFloat16 -> do !x <- decodeFloat
                        return (TFloat x)
      TypeFloat32 -> do !x <- decodeFloat
                        return (TFloat x)
      TypeFloat64 -> do !x <- decodeDouble
                        return (TDouble x)

      TypeBytes        -> do !x <- decodeBytes
                             return (TBytes x)
      TypeBytesIndef   -> decodeBytesIndef >> decodeBytesIndefLen []
      TypeString       -> do !x <- decodeString
                             return (TString x)
      TypeStringIndef  -> decodeStringIndef >> decodeStringIndefLen []

      TypeListLen      -> decodeListLen      >>= flip decodeListN []
      TypeListLen64    -> decodeListLen      >>= flip decodeListN []
      TypeListLenIndef -> decodeListLenIndef >>  decodeListIndefLen []
      TypeMapLen       -> decodeMapLen       >>= flip decodeMapN []
      TypeMapLen64     -> decodeMapLen       >>= flip decodeMapN []
      TypeMapLenIndef  -> decodeMapLenIndef  >>  decodeMapIndefLen []
      TypeTag          -> do !x <- decodeTag64
                             !y <- decodeTerm
                             return (TTagged x y)
      TypeTag64        -> do !x <- decodeTag64
                             !y <- decodeTerm
                             return (TTagged x y)

      TypeBool    -> do !x <- decodeBool
                        return (TBool x)
      TypeNull    -> TNull   <$  decodeNull
      TypeUndef   -> TUndef  <$  decodeSimple
      TypeSimple  -> do !x <- decodeSimple
                        return (TSimple x)
      TypeBreak   -> fail "unexpected break"
      TypeInvalid -> fail "invalid token encoding"


decodeBytesIndefLen :: [BS.ByteString] -> Decoder Term
decodeBytesIndefLen acc = do
    stop <- decodeBreakOr
    if stop then return $! TBytesI (LBS.fromChunks (reverse acc))
            else do !bs <- decodeBytes
                    decodeBytesIndefLen (bs : acc)


decodeStringIndefLen :: [T.Text] -> Decoder Term
decodeStringIndefLen acc = do
    stop <- decodeBreakOr
    if stop then return $! TStringI (LT.fromChunks (reverse acc))
            else do !str <- decodeString
                    decodeStringIndefLen (str : acc)


decodeListN :: Int -> [Term] -> Decoder Term
decodeListN !n acc =
    case n of
      0 -> return $! TList (reverse acc)
      _ -> do !t <- decodeTerm
              decodeListN (n-1) (t : acc)


decodeListIndefLen :: [Term] -> Decoder Term
decodeListIndefLen acc = do
    stop <- decodeBreakOr
    if stop then return $! TListI (reverse acc)
            else do !tm <- decodeTerm
                    decodeListIndefLen (tm : acc)


decodeMapN :: Int -> [(Term, Term)] -> Decoder Term
decodeMapN !n acc =
    case n of
      0 -> return $! TMap (reverse acc)
      _ -> do !tm   <- decodeTerm
              !tm'  <- decodeTerm
              decodeMapN (n-1) ((tm, tm') : acc)


decodeMapIndefLen :: [(Term, Term)] -> Decoder Term
decodeMapIndefLen acc = do
    stop <- decodeBreakOr
    if stop then return $! TMapI (reverse acc)
            else do !tm  <- decodeTerm
                    !tm' <- decodeTerm
                    decodeMapIndefLen ((tm, tm') : acc)

