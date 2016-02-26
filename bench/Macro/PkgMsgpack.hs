{-# LANGUAGE TemplateHaskell, StandaloneDeriving, GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Macro.PkgMsgpack where

import Macro.Types
import qualified Data.MessagePack as MsgPack
import Data.MessagePack (deriveObject, Packable(..), Unpackable(..), get)
import Data.ByteString.Lazy as BS
import Blaze.ByteString.Builder
import Data.Bits
import qualified Data.Attoparsec.ByteString as A
import Data.Monoid
import Data.Word
import Text.Printf


serialise :: [GenericPackageDescription] -> BS.ByteString
serialise pkgs = MsgPack.pack pkgs

deserialise :: BS.ByteString -> [GenericPackageDescription]
deserialise = MsgPack.unpack

{-
deserialiseNull :: BS.ByteString -> ()
deserialiseNull bs =
    case Cereal.runGetLazy decodeListNull bs of
      Right () -> ()
  where
    decodeListNull = do
      n <- get :: Get Int
      go n
    go 0 = return ()
    go i = do x <- get :: Get GenericPackageDescription
              x `seq` go (i-1)
-}
deriveObject False ''Either
deriveObject False ''Version
deriving instance MsgPack.Packable PackageName
deriving instance MsgPack.Unpackable PackageName
deriveObject False ''PackageId
deriveObject False ''VersionRange
deriveObject False ''Dependency
deriveObject False ''CompilerFlavor
deriveObject False ''License
deriveObject False ''SourceRepo
deriveObject False ''RepoKind
deriveObject False ''RepoType
deriveObject False ''BuildType
deriveObject False ''Library
deriveObject False ''Executable
deriveObject False ''TestSuite
deriveObject False ''TestSuiteInterface
deriveObject False ''TestType
deriveObject False ''Benchmark
deriveObject False ''BenchmarkInterface
deriveObject False ''BenchmarkType
deriveObject False ''BuildInfo
deriving instance MsgPack.Packable ModuleName
deriving instance MsgPack.Unpackable ModuleName
deriveObject False ''Language
deriveObject False ''Extension
deriveObject False ''KnownExtension
deriveObject False ''PackageDescription
deriveObject False ''OS
deriveObject False ''Arch
deriveObject False ''Flag
deriving instance MsgPack.Packable FlagName
deriving instance MsgPack.Unpackable FlagName
deriveObject False ''CondTree
deriveObject False ''ConfVar
deriveObject False ''Condition
deriveObject False ''GenericPackageDescription



instance (Packable a1, Packable a2, Packable a3, Packable a4, Packable a5,
          Packable a6, Packable a7, Packable a8, Packable a9, Packable a10,
          Packable a11, Packable a12, Packable a13, Packable a14, Packable a15,
          Packable a16, Packable a17, Packable a18, Packable a19, Packable a20,
          Packable a21, Packable a22, Packable a23, Packable a24, Packable a25)
       => Packable (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10,
                    a11, a12, a13, a14, a15, a16, a17, a18, a19, a20,
                    a21, a22, a23, a24, a25) where
  from = fromArray (const 25) f
    where
      f (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, a20, a21, a22, a23, a24, a25) =
        from a1 <> from a2 <> from a3 <> from a4 <> from a5 <> from a6 <> from a7 <> from a8 <> from a9 <> from a10 <>
        from a11 <> from a12 <> from a13 <> from a14 <> from a15 <> from a16 <> from a17 <> from a18 <> from a19 <> from a20 <>
        from a21 <> from a22 <> from a23 <> from a24 <> from a25

instance (Packable a1, Packable a2, Packable a3, Packable a4, Packable a5,
          Packable a6, Packable a7, Packable a8, Packable a9, Packable a10,
          Packable a11, Packable a12, Packable a13, Packable a14, Packable a15,
          Packable a16, Packable a17, Packable a18, Packable a19, Packable a20,
          Packable a21, Packable a22, Packable a23, Packable a24, Packable a25,
          Packable a26, Packable a27, Packable a28)
       => Packable (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10,
                    a11, a12, a13, a14, a15, a16, a17, a18, a19, a20,
                    a21, a22, a23, a24, a25, a26, a27, a28) where
  from = fromArray (const 28) f
    where
      f (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10,
         a11, a12, a13, a14, a15, a16, a17, a18, a19, a20,
         a21, a22, a23, a24, a25, a26, a27, a28) =
        from a1 <> from a2 <> from a3 <> from a4 <> from a5 <> from a6 <> from a7 <> from a8 <> from a9 <> from a10 <>
        from a11 <> from a12 <> from a13 <> from a14 <> from a15 <> from a16 <> from a17 <> from a18 <> from a19 <> from a20 <>
        from a21 <> from a22 <> from a23 <> from a24 <> from a25 <> from a26 <> from a27 <> from a28

fromArray :: (a -> Int) -> (a -> Builder) -> a -> Builder
fromArray lf pf arr = do
  case lf arr of
    len | len <= 15 ->
      fromWord8 $ 0x90 .|. fromIntegral len
    len | len < 0x10000 ->
      fromWord8 0xDC <>
      fromWord16be (fromIntegral len)
    len ->
      fromWord8 0xDD <>
      fromWord32be (fromIntegral len)
  <> pf arr

instance (Unpackable a1, Unpackable a2, Unpackable a3, Unpackable a4, Unpackable a5,
          Unpackable a6, Unpackable a7, Unpackable a8, Unpackable a9, Unpackable a10,
          Unpackable a11, Unpackable a12, Unpackable a13, Unpackable a14, Unpackable a15,
          Unpackable a16, Unpackable a17, Unpackable a18, Unpackable a19, Unpackable a20,
          Unpackable a21, Unpackable a22, Unpackable a23, Unpackable a24, Unpackable a25)
       => Unpackable (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10,
                      a11, a12, a13, a14, a15, a16, a17, a18, a19, a20,
                      a21, a22, a23, a24, a25) where

  get = parseArray f where
    f 25 = get >>= \a1 -> get >>= \a2 -> get >>= \a3 -> get >>= \a4 -> get >>= \a5 ->
           get >>= \a6 -> get >>= \a7 -> get >>= \a8 -> get >>= \a9 -> get >>= \a10 ->
           get >>= \a11 -> get >>= \a12 -> get >>= \a13 -> get >>= \a14 -> get >>= \a15 ->
           get >>= \a16 -> get >>= \a17 -> get >>= \a18 -> get >>= \a19 -> get >>= \a20 ->
           get >>= \a21 -> get >>= \a22 -> get >>= \a23 -> get >>= \a24 -> get >>= \a25 ->
           return (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10,
                   a11, a12, a13, a14, a15, a16, a17, a18, a19, a20,
                   a21, a22, a23, a24, a25)
    f n = fail $ printf "wrong tuple size: expected 25 but got %d" n

instance (Unpackable a1, Unpackable a2, Unpackable a3, Unpackable a4, Unpackable a5,
          Unpackable a6, Unpackable a7, Unpackable a8, Unpackable a9, Unpackable a10,
          Unpackable a11, Unpackable a12, Unpackable a13, Unpackable a14, Unpackable a15,
          Unpackable a16, Unpackable a17, Unpackable a18, Unpackable a19, Unpackable a20,
          Unpackable a21, Unpackable a22, Unpackable a23, Unpackable a24, Unpackable a25,
          Unpackable a26, Unpackable a27, Unpackable a28)
       => Unpackable (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10,
                      a11, a12, a13, a14, a15, a16, a17, a18, a19, a20,
                      a21, a22, a23, a24, a25, a26, a27, a28) where

  get = parseArray f where
    f 28 = get >>= \a1 -> get >>= \a2 -> get >>= \a3 -> get >>= \a4 -> get >>= \a5 ->
           get >>= \a6 -> get >>= \a7 -> get >>= \a8 -> get >>= \a9 -> get >>= \a10 ->
           get >>= \a11 -> get >>= \a12 -> get >>= \a13 -> get >>= \a14 -> get >>= \a15 ->
           get >>= \a16 -> get >>= \a17 -> get >>= \a18 -> get >>= \a19 -> get >>= \a20 ->
           get >>= \a21 -> get >>= \a22 -> get >>= \a23 -> get >>= \a24 -> get >>= \a25 ->
           get >>= \a26 -> get >>= \a27 -> get >>= \a28 ->
           return (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10,
                   a11, a12, a13, a14, a15, a16, a17, a18, a19, a20,
                   a21, a22, a23, a24, a25, a26, a27, a28)
    f n = fail $ printf "wrong tuple size: expected 28 but got %d" n

parseArray :: (Int -> A.Parser a) -> A.Parser a
parseArray aget = do
  c <- A.anyWord8
  case c of
    _ | c .&. 0xF0 == 0x90 ->
      aget . fromIntegral $ c .&. 0x0F
    0xDC ->
      aget . fromIntegral =<< parseUint16
    0xDD ->
      aget . fromIntegral =<< parseUint32
    _ ->
      fail $ printf "invalid array tag: 0x%02X" c

parseUint16 :: A.Parser Word16
parseUint16 = do
  b0 <- A.anyWord8
  b1 <- A.anyWord8
  return $ (fromIntegral b0 `shiftL` 8) .|. fromIntegral b1

parseUint32 :: A.Parser Word32
parseUint32 = do
  b0 <- A.anyWord8
  b1 <- A.anyWord8
  b2 <- A.anyWord8
  b3 <- A.anyWord8
  return $ (fromIntegral b0 `shiftL` 24) .|.
           (fromIntegral b1 `shiftL` 16) .|.
           (fromIntegral b2 `shiftL` 8) .|.
           fromIntegral b3

