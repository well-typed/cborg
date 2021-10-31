{-# LANGUAGE CPP                #-}
{-# LANGUAGE NamedFieldPuns     #-}
module Tests.Reference (
    testTree
  , termToJson
  , equalJson
  ) where

import           Test.Tasty as Tasty
import           Test.Tasty.HUnit
import           Test.Tasty.QuickCheck

import qualified Data.ByteString            as BS
import qualified Data.ByteString.Base64.URL as Base64url
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Vector as V
import           Data.Scientific (fromFloatDigits, toRealFloat)
import           Data.Aeson as Aeson
#if MIN_VERSION_aeson(2,0,0)
import           Data.Aeson.Key as Aeson.Key
#endif
import           Data.Word
import qualified Numeric.Half as Half

import           Tests.Reference.Implementation as CBOR
import           Tests.Reference.Generators
                   ( HalfSpecials(..), FloatSpecials(..), DoubleSpecials(..) )
import           Tests.Reference.TestVectors


-------------------------------------------------------------------------------
-- Unit tests for test vector from https://github.com/cbor/test-vectors/
--

unit_externalTestVector :: [ExternalTestCase] -> Assertion
unit_externalTestVector = mapM_ unit_externalTestCase

unit_externalTestCase :: ExternalTestCase -> Assertion
unit_externalTestCase ExternalTestCase {
                        encoded,
                        decoded = Left expectedJson
                      } = do
  let term       = deserialise encoded
      actualJson = termToJson term
      reencoded  = serialise term

  expectedJson `equalJson` actualJson
  encoded @=? reencoded

unit_externalTestCase ExternalTestCase {
                        encoded,
                        decoded = Right expectedDiagnostic
                      } = do
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

#if MIN_VERSION_aeson(2,0,0)
stringToJsonKey :: String -> Aeson.Key.Key
stringToJsonKey = Aeson.Key.fromString
#else
stringToJsonKey :: String -> T.Text
stringToJsonKey = T.pack
#endif

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
termToJson (TMap     kvs) = Aeson.object [ (stringToJsonKey k, termToJson v)
                                         | (TString k,v) <- kvs ]
termToJson (TMapI    kvs) = Aeson.object [ (stringToJsonKey k, termToJson v)
                                         | (TString k,v) <- kvs ]
termToJson (TTagged _ t)  = termToJson t
termToJson  TTrue         = Aeson.Bool True
termToJson  TFalse        = Aeson.Bool False
termToJson  TNull         = Aeson.Null
termToJson  TUndef        = Aeson.Null -- replacement value
termToJson (TSimple _)    = Aeson.Null -- replacement value
termToJson (TFloat16 f)   = Aeson.Number (fromFloatDigits (Half.fromHalf (getHalfSpecials f)))
termToJson (TFloat32 f)   = Aeson.Number (fromFloatDigits (getFloatSpecials f))
termToJson (TFloat64 f)   = Aeson.Number (fromFloatDigits (getDoubleSpecials f))

bytesToBase64Text :: [Word8] -> T.Text
bytesToBase64Text = T.decodeLatin1 . Base64url.encode . BS.pack


-------------------------------------------------------------------------------
-- Unit tests for test vector from CBOR spec RFC7049 Appendix A
--

unit_expectedDiagnosticNotation :: RFC7049TestCase -> Assertion
unit_expectedDiagnosticNotation RFC7049TestCase {
                                  expectedDiagnostic,
                                  encodedBytes
                                } = do
  let Just (term, [])  = runDecoder decodeTerm encodedBytes
      actualDiagnostic = diagnosticNotation term

  expectedDiagnostic @=? actualDiagnostic

-- | The reference implementation satisfies the roundtrip property for most
-- examples (all the ones from Appendix A). It does not satisfy the roundtrip
-- property in general however, non-canonical over-long int encodings for
-- example.
--
unit_encodedRoundtrip :: RFC7049TestCase -> Assertion
unit_encodedRoundtrip RFC7049TestCase {
                        expectedDiagnostic,
                        encodedBytes
                      } = do
  let Just (term, [])  = runDecoder decodeTerm encodedBytes
      reencodedBytes   = encodeTerm term

  assertEqual ("for CBOR: " ++ expectedDiagnostic) encodedBytes reencodedBytes


--------------------------------------------------------------------------------
-- TestTree API

testTree :: TestTree
testTree =
  testGroup "Reference implementation"
    [ testGroup "internal properties"
        [ testProperty "Integer to/from bytes" prop_integerToFromBytes
        , testProperty "Word16 to/from network byte order" prop_word16ToFromNet
        , testProperty "Word32 to/from network byte order" prop_word32ToFromNet
        , testProperty "Word64 to/from network byte order" prop_word64ToFromNet
        , testProperty "Numeric.Half to/from Float"        prop_halfToFromFloat
        ]

    , testGroup "properties"
        [ testProperty "encoding/decoding initial byte"    prop_InitialByte
        , testProperty "encoding/decoding additional info" prop_AdditionalInfo
        , testProperty "encoding/decoding token header"    prop_TokenHeader
        , testProperty "encoding/decoding token header 2"  prop_TokenHeader2
        , testProperty "encoding/decoding tokens"          prop_Token
        , --localOption (QuickCheckTests  1000) $
          localOption (QuickCheckMaxSize 150) $
          testProperty "encoding/decoding terms"           prop_Term
        ]

    , testCase "RFC7049 test vector: decode" $
        mapM_ unit_expectedDiagnosticNotation rfc7049TestVector

    , testCase "RFC7049 test vector: roundtrip" $
        mapM_ unit_encodedRoundtrip rfc7049TestVector

    , withExternalTestVector $ \getTestVector ->
      testCase "external test vector" $
        getTestVector >>= unit_externalTestVector
    ]
