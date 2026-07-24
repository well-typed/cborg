{-# LANGUAGE RankNTypes #-}

module Tests.Regress.Issue263 ( testTree ) where

import           Data.Word
import           Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import           Data.Foldable (forM_)
import           Control.Monad.ST (ST, runST)
import           Codec.CBOR.Decoding (decodeInteger)
import           Codec.CBOR.Read
import           Codec.CBOR.Term (Term(..), decodeTerm)
import           Test.Tasty
import           Test.Tasty.HUnit
import qualified Tests.Reference.Implementation as Reference
import           Tests.Util (splits2, splits3)

mkRepr :: Integer -> [Word8]
mkRepr int =
    [ -- Tag(2), 0xc2 — positive bigint, 0xc3 — negative bigint
      if int>=0 then 0xc2  else 0xc3
      -- Indefinite-length byte string
    , 0x5f
      -- Bytes
    ] ++ (let b = if int >0
                  then Reference.integerToBytes int
                  else Reference.integerToBytes (-(int+1))
              l = Reference.lengthUInt b
           in Reference.encodeToken (Reference.MT2_ByteString l b)) ++
    [ 0xff ]

shouldDecode :: Integer -> IO ()
shouldDecode int =
  case deserialiseFromBytes decodeTerm (LBS.pack (mkRepr int)) of
    Left err -> fail ("Deserialisation failed for " ++ (show (mkRepr int)) ++ ": " ++ show err)
    Right (b,x)
      | LBS.null b -> (TInteger int) @=? x
      | otherwise -> fail "Trailing bytes"

shouldDecodeIncremental :: (LBS.ByteString -> [LBS.ByteString])
                        -> Integer -> IO ()
shouldDecodeIncremental mksplits int =
  forM_ splits $ \chunks ->
    case runST (deserialiseIncremental decodeInteger >>= go chunks) of
      Left err -> fail ("Incremental deserialisation failed for " ++ (show (mkRepr int)) ++ ": " ++ show err)
      Right (b, x)
        | BS.null b -> int @=? x
        | otherwise -> fail "Trailing bytes"

  where
    -- Each split is a list of chunks which represents encoding of the given
    -- `int`
    splits :: [[ByteString]]
    splits = LBS.toChunks <$> mksplits (LBS.pack (mkRepr int))

    go :: [ByteString]
       -> IDecode s Integer
       -> ST s (Either DeserialiseFailure (ByteString, Integer))
    go [] Partial{} = pure $ Left (DeserialiseFailure 0 "not enough bytes")
    go (bs:rest) (Partial f) = f (Just bs) >>= go rest
    go _ (Done b _ a) = pure $ Right (b, a)
    go _ (Fail _ _ e) = pure $ Left e

shouldDecodeIncremental_splits2 :: Integer -> IO ()
shouldDecodeIncremental_splits2 = shouldDecodeIncremental splits2

shouldDecodeIncremental_splits3 :: Integer -> IO ()
shouldDecodeIncremental_splits3 = shouldDecodeIncremental splits3

testTree :: TestTree
testTree =
    testGroup "Issue 263 - bigint with indefinite length"
        [ testCase "small bigint" $ shouldDecode 1231
        , testCase "big bigint" $ shouldDecode 123123123123123123123123123
        , testCase "small negative bigint" $ shouldDecode (-123)
        , testCase "big negative bigint" $ shouldDecode (-12312312311231231231231234)
        , testGroup "incremental"
          [ testCase "small bigint with all 2-splits" $ shouldDecodeIncremental_splits2 1231
          , testCase "big bigint with all 2-splits" $ shouldDecodeIncremental_splits2 123123123123123123123123123
          , testCase "small bigint with all 3-splits" $ shouldDecodeIncremental_splits3 1231
          , testCase "big bigint with all 3-splits" $ shouldDecodeIncremental_splits3 123123123123123123123123123
          , testCase "small negative bigint with all 2-splits" $ shouldDecodeIncremental_splits2 (-1231)
          , testCase "big negative bigint with all 2-splits" $ shouldDecodeIncremental_splits2 (-123123123123123123123123123)
          , testCase "small negative bigint with all 3-splits" $ shouldDecodeIncremental_splits3 (-1231)
          , testCase "big negative bigint with all 3-splits" $ shouldDecodeIncremental_splits3 (-123123123123123123123123123)
          ]
        ]
