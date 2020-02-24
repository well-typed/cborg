{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

module Codec.CBOR.JSON
 ( encodeValue
 , decodeValue
 ) where

import           Control.Monad                       ( replicateM )
import           Data.Monoid
import           Control.Applicative
import           Prelude hiding (decodeFloat)

import           Codec.CBOR.Encoding
import           Codec.CBOR.Decoding
import           Data.Aeson                          ( Value(..) )
import qualified Data.Aeson                          as Aeson
import qualified Data.ByteString                     as BS
import           Data.ByteString.Base16              ( encodeBase16 )
import           Data.ByteString.Base64              ( encodeBase64 )
import           Data.ByteString.Base64.URL          ( encodeBase64Unpadded' )
import           Data.ByteString.Char8               ( unpack )
import qualified Data.HashMap.Lazy                   as HM
import           Data.Scientific                     as Scientific
import qualified Data.Text                           as T
import qualified Data.Text.Lazy                      as LT
import           Data.Text.Lazy.Builder
import qualified Data.Vector                         as V
import           Numeric                             ( showHex )


-- | Encode a JSON value into CBOR.
encodeValue :: Value -> Encoding
encodeValue (Object vs) = encodeObject vs
encodeValue (Array  vs) = encodeArray  vs
encodeValue (String s)  = encodeString s
encodeValue (Number n)  = case Scientific.floatingOrInteger n of
                            Left  d -> encodeDouble  d
                            Right i -> encodeInteger i
encodeValue (Bool   b)  = encodeBool b
encodeValue  Null       = encodeNull

encodeObject :: Aeson.Object -> Encoding
encodeObject vs =
    encodeMapLen (fromIntegral (HM.size vs))
 <> HM.foldrWithKey (\k v r -> encodeString k <> encodeValue v <> r) mempty vs

encodeArray :: Aeson.Array -> Encoding
encodeArray vs =
    encodeListLen (fromIntegral (V.length vs))
 <> V.foldr (\v r -> encodeValue v <> r) mempty vs

-- | Decode an arbitrary CBOR value into JSON.
decodeValue :: Bool -> Decoder s Value
decodeValue lenient = do
    tkty <- peekTokenType
    case tkty of
      -- Positive and negative integers (type 0,1)
      -- become a JSON number.
      TypeUInt         -> decodeNumberIntegral
      TypeUInt64       -> decodeNumberIntegral
      TypeNInt         -> decodeNumberIntegral
      TypeNInt64       -> decodeNumberIntegral

      -- Type 2 - A bytestring that is not embedded in a tag that specifies
      -- a proposed encoding is encoded in base64url without padding & becomes
      -- a JSON String.
      TypeBytes        -> decodeUntaggedByteString
      TypeBytesIndef   -> decodeUntaggedByteStringIndef []

      -- Type 3 - A UTF-8 string becomes a JSON string.
      TypeString       -> decodeAndEscapeString
      TypeStringIndef  -> decodeStringIndef' []

      -- Type 4 (array) becomes a JSON array
      TypeListLen      -> decodeListLen >>= decodeListN lenient
      TypeListLen64    -> decodeListLen >>= decodeListN lenient
      TypeListLenIndef -> decodeListLenIndef >> decodeListIndef lenient []

      -- Type 5 (map) becomes a JSON object
      TypeMapLen       -> decodeMapLen >>= flip (decodeMapN lenient) HM.empty
      TypeMapLen64     -> decodeMapLen >>= flip (decodeMapN lenient) HM.empty
      TypeMapLenIndef  -> decodeMapLenIndef >> decodeMapIndefLen lenient HM.empty

      -- Tagged values (type 6)
      TypeTag          -> decodeTag >>= decodeTaggedValue lenient
      TypeTag64        -> decodeTag64'
      TypeInteger      -> decodeNumberIntegral

      -- Simple and floats (type 7)
      TypeNull         -> pure Null
      TypeFloat16      -> decodeNumberFloat16
      TypeFloat32      -> decodeNumberFloating
      TypeFloat64      -> decodeNumberFloating
      TypeBool         -> Bool   <$> decodeBool
      TypeSimple       -> decodeSimple'
      TypeBreak        -> Bool   <$> decodeBreakOr

      TypeInvalid      -> fail $ "Unexpected CBOR token type for a JSON value: "
                               <> show tkty

decodeAndEscapeString :: Decoder s Value
decodeAndEscapeString = do
    unescapedString <- decodeString
    pure . String . LT.toStrict . toLazyText $ escapeString unescapedString

decodeBigFloat :: Int -> Decoder s Value
decodeBigFloat n = do
    (exponent':significand':_) <- replicateM n decodeDouble
    let bigFloat = significand' * (2 ** exponent')
    return . Number $ fromFloatDigits bigFloat

decodeNumberIntegral :: Decoder s Value
decodeNumberIntegral = Number . fromInteger <$> decodeInteger

decodeNumberFloating :: Decoder s Value
decodeNumberFloating = Number . Scientific.fromFloatDigits <$> decodeDouble

decodeNumberFloat16 :: Decoder s Value
decodeNumberFloat16 = do
    f <- decodeFloat
    if isNaN f || isInfinite f
        then return Null
        else return $ Number (Scientific.fromFloatDigits f)

decodeListN :: Bool -> Int -> Decoder s Value
decodeListN !lenient !n = do
    vec <- V.replicateM n (decodeValue lenient)
    return $! Array vec

decodeListIndef :: Bool -> [Value] -> Decoder s Value
decodeListIndef !lenient acc = do
    stop <- decodeBreakOr
    if stop then return $! Array (V.fromList (reverse acc))
            else do !tm <- decodeValue lenient
                    decodeListIndef lenient (tm : acc)

decodeMapN :: Bool -> Int -> Aeson.Object -> Decoder s Value
decodeMapN !lenient !n acc =
    case n of
      0 -> return $! Object acc
      _ -> do
        !tk <- decodeValue lenient >>= \v -> case v of
                 String s           -> return s
                 -- These cases are only allowed when --lenient is passed,
                 -- as printing them as strings may result in key collisions.
                 Number d | lenient -> return $ T.pack (show d)
                 Bool   b | lenient -> return $ T.pack (show b)
                 _        -> fail "Could not decode map key type"
        !tv  <- decodeValue lenient
        decodeMapN lenient (n-1) (HM.insert tk tv acc)

-- A byte string (major type 2) that is not embedded in a tag that
-- specifies a proposed encoding is encoded in base64url without
-- padding and becomes a JSON string.
decodeUntaggedByteString :: Decoder s Value
decodeUntaggedByteString = do
    bs <- decodeBytes
    return . String . T.pack . unpack $ encodeBase64Unpadded' bs

decodeUntaggedByteStringBase16 :: Decoder s Value
decodeUntaggedByteStringBase16 = do
    bs <- decodeBytes
    return . String $ encodeBase16 bs

decodeUntaggedByteStringBase64 :: Decoder s Value
decodeUntaggedByteStringBase64 = do
    bs <- decodeBytes
    return . String $ encodeBase64 bs

decodeUntaggedByteStringIndef :: [BS.ByteString] -> Decoder s Value
decodeUntaggedByteStringIndef acc = do
    stop <- decodeBreakOr
    if stop then return . String . T.pack . unpack . BS.concat $ reverse acc
            else do bs <- decodeBytes
                    decodeUntaggedByteStringIndef (encodeBase64Unpadded' bs : acc)

decodeStringIndef' :: [T.Text] -> Decoder s Value
decodeStringIndef' acc = do
    stop <- decodeBreakOr
    if stop then return . String . LT.toStrict . toLazyText . escapeString . T.concat $ reverse acc
            else do !str <- decodeString
                    decodeStringIndef' (str : acc)

decodeNegativeBignum :: Decoder s Value
decodeNegativeBignum = do bs <- decodeBytes
                          let negBigNum = "~" <> (T.pack . unpack $ encodeBase64Unpadded' bs)
                          return $ String negBigNum
decodePositiveBignum :: Decoder s Value
decodePositiveBignum = decodeUntaggedByteString

decodeDecimalFraction :: Int -> Decoder s Value
decodeDecimalFraction n = do
    (exponent':significand':_) <- replicateM n decodeInteger
    return . Number $ scientific significand' (fromIntegral exponent')

decodeMapIndefLen :: Bool -> Aeson.Object -> Decoder s Value
decodeMapIndefLen lenient acc = do
    stop <- decodeBreakOr
    if stop then return $! Object acc
            else do
              !tk <- decodeValue lenient >>= \v -> case v of
                 String s           -> return s
                 -- These cases are only allowed when --lenient is passed,
                 -- as printing them as strings may result in key collisions.
                 Number d | lenient -> return $ T.pack (show d)
                 Bool   b | lenient -> return $ T.pack (show b)
                 _        -> fail "Could not decode map key type"
              !tv  <- decodeValue lenient
              decodeMapIndefLen lenient $ HM.insert tk tv acc

decodeSimple' :: Decoder s Value
decodeSimple' = Number . fromIntegral <$> decodeSimple

decodeTaggedValue :: Bool -> Word -> Decoder s Value
decodeTaggedValue lenient t =
    case t of
      -- Standard date/time string
      0     -> decodeAndEscapeString
      -- Epoch-based date/time
      1     -> do tag <- decodeTag
                  case tag of
                      0 -> decodeTaggedValue lenient 0
                      1 -> decodeTaggedValue lenient 1
                      7 -> decodeTaggedValue lenient 7
                      _ -> pure Null
      -- Positive bignum
      2     -> decodePositiveBignum
      -- Negative bignum
      3     -> decodeNegativeBignum
      -- Decimal fraction
      4     -> decodeListLen >>= decodeDecimalFraction
      -- Big float
      5     -> decodeListLen >>= decodeBigFloat
      -- base64url encoding
      21    -> multiple lenient decodeUntaggedByteString
      -- base64 encoding
      22    -> multiple lenient decodeUntaggedByteStringBase64
      -- base16 encoding
      23    -> multiple lenient decodeUntaggedByteStringBase16
      -- Encoded CBOR data item
      24    -> String <$> decodeString
      -- URI
      32    -> decodeAndEscapeString
      -- base64url
      33    -> decodeAndEscapeString
      -- base64
      34    -> decodeAndEscapeString
      -- regular expression
      35    -> decodeAndEscapeString
      -- MIME message
      36    -> decodeAndEscapeString
      -- Self-describe CBOR
      55799 -> undefined
      -- Unassigned
      _     -> pure Null

decodeTag64' :: Decoder s Value
decodeTag64' = Number . fromIntegral <$> decodeTag64

escapeString :: T.Text -> Builder
escapeString s = singleton '"' <> quote s <> singleton '"'
  where
    quote q = case T.uncons t of
                Nothing      -> fromText h
                Just (!c,t') -> fromText h <> escape c <> quote t'
        where (h,t) = T.break isEscape q
    isEscape c = c == '\"' ||
                 c == '\\' ||
                 c < '\x20'
    escape '\"' = "\\\""
    escape '\\' = "\\\\"
    escape '\n' = "\\n"
    escape '\r' = "\\r"
    escape '\t' = "\\t"

    escape c
        | c < '\x20' = fromString $ "\\u" ++ replicate (4 - length h) '0' ++ h
        | otherwise  = singleton c
        where h = showHex (fromEnum c) ""

multiple :: Bool -> Decoder s Value -> Decoder s Value
multiple lenient decoder = do
    tktype'' <- peekTokenType
    case tktype'' of
      TypeBytes        -> decoder
      TypeListLen      -> decodeListLen >>= decodeListN lenient
      TypeListLen64    -> decodeListLen >>= decodeListN lenient
      TypeListLenIndef -> decodeListLenIndef >> decodeListIndef lenient []
      TypeMapLen       -> decodeMapLen >>= flip (decodeMapN lenient) HM.empty
      TypeMapLen64     -> decodeMapLen >>= flip (decodeMapN lenient) HM.empty
      TypeMapLenIndef  -> decodeMapLenIndef >> decodeMapIndefLen lenient HM.empty
      _          -> Null <$ decodeNull
