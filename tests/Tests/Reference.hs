{-# LANGUAGE OverloadedStrings, NamedFieldPuns #-}
module Tests.Reference where

import qualified Data.ByteString            as BS
import qualified Data.ByteString.Lazy       as LBS
import qualified Data.ByteString.Base64     as Base64
import qualified Data.ByteString.Base64.URL as Base64url
import qualified Data.ByteString.Base16     as Base16
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Vector as V
import           Data.Scientific (fromFloatDigits, toRealFloat)
import           Data.Aeson as Aeson
import           Control.Applicative
import           Control.Monad
import           Data.Word
import qualified Numeric.Half as Half

import Test.Tasty.HUnit

import Tests.Reference.Implementation as CBOR


data TestCase = TestCase {
       encoded   :: !LBS.ByteString,
       decoded   :: !(Either Aeson.Value String),
       roundTrip :: !Bool
     }
  deriving Show

instance FromJSON TestCase where
  parseJSON =
    withObject "cbor test" $ \obj -> do
      encoded64 <- T.encodeUtf8 <$> obj .: "cbor"
      encoded   <- either (fail "invalid base64") return $
                   Base64.decode encoded64
      encoded16 <- T.encodeUtf8 <$> obj .: "hex"
      let encoded' = fst (Base16.decode encoded16)
      when (encoded /= encoded') $
        fail "hex and cbor encoding mismatch in input"
      roundTrip <- obj .: "roundtrip"
      decoded   <- Left  <$> obj .: "decoded"
               <|> Right <$> obj .: "diagnostic"
      return $! TestCase {
        encoded = LBS.fromStrict encoded,
        roundTrip,
        decoded
      }

loadTestCases :: IO [TestCase]
loadTestCases = do
    content <- LBS.readFile "tests/test-vectors/appendix_a.json"
    either fail return (Aeson.eitherDecode' content)

externalTestCase :: TestCase -> Assertion
externalTestCase TestCase { encoded, decoded = Left expectedJson } = do
  let term       = deserialise encoded
      actualJson = termToJson term
      reencoded  = serialise term
  
  expectedJson `equalJson` actualJson
  encoded @=? reencoded

externalTestCase TestCase { encoded, decoded = Right expectedDiagnostic } = do
  let term             = deserialise encoded
      actualDiagnostic = diagnosticNotation term
      reencoded        = serialise term

  expectedDiagnostic @=? actualDiagnostic
  encoded @=? reencoded

equalJson :: Aeson.Value -> Aeson.Value -> Assertion
equalJson (Aeson.Number expected) (Aeson.Number actual)
  | toRealFloat expected == promoteDouble (toRealFloat actual)
                          = return ()
  where
    -- This is because the expected JSON output is always using double precision
    -- where as Aeson's Scientific type preserves the precision of the input.
    -- So for tests using Float, we're more precise than the reference values.
    promoteDouble :: Float -> Double
    promoteDouble = realToFrac

equalJson expected actual = expected @=? actual


termToJson :: CBOR.Term -> Aeson.Value
termToJson (TUInt    n)   = Aeson.Number (fromIntegral (fromUInt n))
termToJson (TNInt    n)   = Aeson.Number (-1 - fromIntegral (fromUInt n))
termToJson (TBigInt  n)   = Aeson.Number (fromIntegral n)
termToJson (TBytes   ws)  = Aeson.String (bytesToBase64Text ws)
termToJson (TBytess  wss) = Aeson.String (bytesToBase64Text (concat wss))
termToJson (TString  cs)  = Aeson.String (T.pack cs)
termToJson (TStrings css) = Aeson.String (T.pack (concat css))
termToJson (TArray   ts)  = Aeson.Array  (V.fromList (map termToJson ts))
termToJson (TArrayI  ts)  = Aeson.Array  (V.fromList (map termToJson ts))
termToJson (TMap     kvs) = Aeson.object [ (T.pack k, termToJson v)
                                         | (TString k,v) <- kvs ]
termToJson (TMapI    kvs) = Aeson.object [ (T.pack k, termToJson v)
                                         | (TString k,v) <- kvs ]
termToJson (TTagged _ t)  = termToJson t
termToJson  TTrue         = Aeson.Bool True
termToJson  TFalse        = Aeson.Bool False
termToJson  TNull         = Aeson.Null
termToJson  TUndef        = Aeson.Null -- replacement value
termToJson (TSimple _)    = Aeson.Null -- replacement value
termToJson (TFloat16 f)   = Aeson.Number (fromFloatDigits (Half.fromHalf f))
termToJson (TFloat32 f)   = Aeson.Number (fromFloatDigits f)
termToJson (TFloat64 f)   = Aeson.Number (fromFloatDigits f)

bytesToBase64Text :: [Word8] -> T.Text
bytesToBase64Text = T.decodeLatin1 . Base64url.encode . BS.pack

expectedDiagnosticNotation :: String -> [Word8] -> Assertion
expectedDiagnosticNotation expectedDiagnostic encoded = do
  let Just (term, [])  = runDecoder decodeTerm encoded
      actualDiagnostic = diagnosticNotation term

  expectedDiagnostic @=? actualDiagnostic

-- | The reference implementation satisfies the roundtrip property for most
-- examples (all the ones from Appendix A). It does not satisfy the roundtrip
-- property in general however, non-canonical over-long int encodings for
-- example.
-- 
--
encodedRoundtrip :: String -> [Word8] -> Assertion
encodedRoundtrip expectedDiagnostic encoded = do
  let Just (term, [])  = runDecoder decodeTerm encoded
      reencoded        = encodeTerm term

  assertEqual ("for CBOR: " ++ expectedDiagnostic) encoded reencoded

-- | The examples from the CBOR spec RFC7049 Appendix A.
-- The diagnostic notation and encoded bytes.
--
specTestVector :: [(String, [Word8])]
specTestVector =
  [ ("0",    [0x00])
  , ("1",    [0x01])
  , ("10",   [0x0a])
  , ("23",   [0x17])
  , ("24",   [0x18, 0x18])
  , ("25",   [0x18, 0x19])
  , ("100",  [0x18, 0x64])
  , ("1000", [0x19, 0x03, 0xe8])
  , ("1000000",               [0x1a, 0x00, 0x0f, 0x42, 0x40])
  , ("1000000000000",         [0x1b, 0x00, 0x00, 0x00, 0xe8, 0xd4, 0xa5, 0x10, 0x00])

  , ("18446744073709551615",  [0x1b, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff])
  , ("18446744073709551616",  [0xc2, 0x49, 0x01, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00])
  , ("-18446744073709551616", [0x3b, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff])
  , ("-18446744073709551617", [0xc3, 0x49, 0x01, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00])

  , ("-1",      [0x20])
  , ("-10",     [0x29])
  , ("-100",    [0x38, 0x63])
  , ("-1000",   [0x39, 0x03, 0xe7])

  , ("0.0",     [0xf9, 0x00, 0x00])
  , ("-0.0",    [0xf9, 0x80, 0x00])
  , ("1.0",     [0xf9, 0x3c, 0x00])
  , ("1.1",     [0xfb, 0x3f, 0xf1, 0x99, 0x99, 0x99, 0x99, 0x99, 0x9a])
  , ("1.5",     [0xf9, 0x3e, 0x00])
  , ("65504.0", [0xf9, 0x7b, 0xff])
  , ("100000.0",               [0xfa, 0x47, 0xc3, 0x50, 0x00])
  , ("3.4028234663852886e38", [0xfa, 0x7f, 0x7f, 0xff, 0xff])
  , ("1.0e300",               [0xfb, 0x7e, 0x37, 0xe4, 0x3c, 0x88, 0x00, 0x75, 0x9c])
  , ("5.960464477539063e-8",   [0xf9, 0x00, 0x01])
  , ("0.00006103515625",       [0xf9, 0x04, 0x00])
  , ("-4.0",                   [0xf9, 0xc4, 0x00])
  , ("-4.1",                   [0xfb, 0xc0, 0x10, 0x66, 0x66, 0x66, 0x66, 0x66, 0x66])

  , ("Infinity",  [0xf9, 0x7c, 0x00])
  , ("NaN",       [0xf9, 0x7e, 0x00])
  , ("-Infinity", [0xf9, 0xfc, 0x00])
  , ("Infinity",  [0xfa, 0x7f, 0x80, 0x00, 0x00])
  , ("NaN",       [0xfa, 0x7f, 0xc0, 0x00, 0x00])
  , ("-Infinity", [0xfa, 0xff, 0x80, 0x00, 0x00])
  , ("Infinity",  [0xfb, 0x7f, 0xf0, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00])
  , ("NaN",       [0xfb, 0x7f, 0xf8, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00])
  , ("-Infinity", [0xfb, 0xff, 0xf0, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00])

  , ("false",       [0xf4])
  , ("true",        [0xf5])
  , ("null",        [0xf6])
  , ("undefined",   [0xf7])
  , ("simple(16)",  [0xf0])
  , ("simple(24)",  [0xf8, 0x18])
  , ("simple(255)", [0xf8, 0xff])

  , ("0(\"2013-03-21T20:04:00Z\")",
         [0xc0, 0x74, 0x32, 0x30, 0x31, 0x33, 0x2d, 0x30, 0x33, 0x2d, 0x32, 0x31,
          0x54, 0x32, 0x30, 0x3a, 0x30, 0x34, 0x3a, 0x30, 0x30, 0x5a])
  , ("1(1363896240)",   [0xc1, 0x1a, 0x51, 0x4b, 0x67, 0xb0])
  , ("1(1363896240.5)", [0xc1, 0xfb, 0x41, 0xd4, 0x52, 0xd9, 0xec, 0x20, 0x00, 0x00])
  , ("23(h'01020304')", [0xd7, 0x44, 0x01, 0x02, 0x03, 0x04])
  , ("24(h'6449455446')", [0xd8, 0x18, 0x45, 0x64, 0x49, 0x45, 0x54, 0x46])
  , ("32(\"http://www.example.com\")",
         [0xd8, 0x20, 0x76, 0x68, 0x74, 0x74, 0x70, 0x3a, 0x2f, 0x2f, 0x77, 0x77,
          0x77, 0x2e, 0x65, 0x78, 0x61, 0x6d, 0x70, 0x6c, 0x65, 0x2e, 0x63, 0x6f, 0x6d])

  , ("h''",          [0x40])
  , ("h'01020304'",  [0x44, 0x01, 0x02, 0x03, 0x04])
  , ("\"\"",         [0x60])
  , ("\"a\"",        [0x61, 0x61])
  , ("\"IETF\"",     [0x64, 0x49, 0x45, 0x54, 0x46])
  , ("\"\\\"\\\\\"", [0x62, 0x22, 0x5c])
  , ("\"\\252\"",    [0x62, 0xc3, 0xbc])
  , ("\"\\27700\"",  [0x63, 0xe6, 0xb0, 0xb4])
  , ("\"\\65873\"",  [0x64, 0xf0, 0x90, 0x85, 0x91])

  , ("[]",                  [0x80])
  , ("[1, 2, 3]",           [0x83, 0x01, 0x02, 0x03])
  , ("[1, [2, 3], [4, 5]]", [0x83, 0x01, 0x82, 0x02, 0x03, 0x82, 0x04, 0x05])
  , ("[1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25]",
         [0x98, 0x19, 0x01, 0x02, 0x03, 0x04, 0x05, 0x06, 0x07, 0x08, 0x09, 0x0a,
          0x0b, 0x0c, 0x0d, 0x0e, 0x0f, 0x10, 0x11, 0x12, 0x13, 0x14, 0x15, 0x16,
          0x17, 0x18, 0x18, 0x18, 0x19])

  , ("{}",           [0xa0])
  , ("{1: 2, 3: 4}", [0xa2, 0x01, 0x02, 0x03, 0x04])
  , ("{\"a\": 1, \"b\": [2, 3]}", [0xa2, 0x61, 0x61, 0x01, 0x61, 0x62, 0x82, 0x02, 0x03])
  , ("[\"a\", {\"b\": \"c\"}]",   [0x82, 0x61, 0x61, 0xa1, 0x61, 0x62, 0x61, 0x63])
  , ("{\"a\": \"A\", \"b\": \"B\", \"c\": \"C\", \"d\": \"D\", \"e\": \"E\"}",
         [0xa5, 0x61, 0x61, 0x61, 0x41, 0x61, 0x62, 0x61, 0x42, 0x61, 0x63, 0x61,
          0x43, 0x61, 0x64, 0x61, 0x44, 0x61, 0x65, 0x61, 0x45])

  , ("(_ h'0102', h'030405')",  [0x5f, 0x42, 0x01, 0x02, 0x43, 0x03, 0x04, 0x05, 0xff])
  , ("(_ \"strea\", \"ming\")", [0x7f, 0x65, 0x73, 0x74, 0x72, 0x65, 0x61, 0x64, 0x6d, 0x69, 0x6e, 0x67, 0xff])

  , ("[_ ]", [0x9f, 0xff])
  , ("[_ 1, [2, 3], [_ 4, 5]]", [0x9f, 0x01, 0x82, 0x02, 0x03, 0x9f, 0x04, 0x05, 0xff, 0xff])
  , ("[_ 1, [2, 3], [4, 5]]", [0x9f, 0x01, 0x82, 0x02, 0x03, 0x82, 0x04, 0x05, 0xff])
  , ("[1, [2, 3], [_ 4, 5]]", [0x83, 0x01, 0x82, 0x02, 0x03, 0x9f, 0x04, 0x05, 0xff])
  , ("[1, [_ 2, 3], [4, 5]]", [0x83, 0x01, 0x9f, 0x02, 0x03, 0xff, 0x82, 0x04, 0x05])
  , ("[_ 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25]", 
         [0x9f, 0x01, 0x02, 0x03, 0x04, 0x05, 0x06, 0x07, 0x08, 0x09, 0x0a, 0x0b,
          0x0c, 0x0d, 0x0e, 0x0f, 0x10, 0x11, 0x12, 0x13, 0x14, 0x15, 0x16, 0x17,
          0x18, 0x18, 0x18, 0x19, 0xff])
  , ("{_ \"a\": 1, \"b\": [_ 2, 3]}", [0xbf, 0x61, 0x61, 0x01, 0x61, 0x62, 0x9f, 0x02, 0x03, 0xff, 0xff])

  , ("[\"a\", {_ \"b\": \"c\"}]",      [0x82, 0x61, 0x61, 0xbf, 0x61, 0x62, 0x61, 0x63, 0xff])
  , ("{_ \"Fun\": true, \"Amt\": -2}", [0xbf, 0x63, 0x46, 0x75, 0x6e, 0xf5, 0x63, 0x41, 0x6d, 0x74, 0x21, 0xff])
  ]

--TODO: test redundant encodings e.g.
-- bigint with zero-length bytestring
-- bigint with leading zeros
-- bigint using indefinate bytestring encoding
-- larger than necessary ints, lengths, tags, simple etc

