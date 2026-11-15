module Tests.Regress.Issue263 ( testTree ) where

import           Data.Word
import qualified Data.ByteString.Lazy as LBS
import           Codec.CBOR.Read
import           Codec.CBOR.Term (Term(..), decodeTerm)
import           Test.Tasty
import           Test.Tasty.HUnit
import qualified Tests.Reference.Implementation as Reference

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


testTree :: TestTree
testTree =
    testGroup "Issue 263 - bigint with indefinite length"
        [ testCase "small bigint" $ shouldDecode 1231
        , testCase "big bigint" $ shouldDecode 123123123123123123123123123
        , testCase "small negative bigint" $ shouldDecode (-123)
        , testCase "big negative bigint" $ shouldDecode (-12312312311231231231231234)
        ]
