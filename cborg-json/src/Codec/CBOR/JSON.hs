{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP          #-}

module Codec.CBOR.JSON
 ( encodeValue
 , decodeValue
 ) where

import           Data.Monoid
import           Control.Applicative
import           Prelude hiding (decodeFloat)

import           Codec.CBOR.Encoding
import           Codec.CBOR.Decoding
import           Data.Aeson                          ( Value(..) )
import qualified Data.Aeson                          as Aeson
import           Data.Scientific                     as Scientific
import qualified Data.Text                           as T
import qualified Data.Vector                         as V

#if MIN_VERSION_aeson(2,0,0)
import qualified Data.Aeson.Key                      as K
import qualified Data.Aeson.KeyMap                   as KM
#else
import qualified Data.HashMap.Lazy                   as HM
#endif

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
     encodeMapLen (fromIntegral size)
  <> foldrWithKey (\k v r -> encodeString' k <> encodeValue v <> r) mempty vs
  where
#if MIN_VERSION_aeson(2,0,0)
    size = KM.size vs
    foldrWithKey = KM.foldrWithKey
    encodeString' = encodeString . K.toText
#else
    size = HM.size vs
    foldrWithKey = HM.foldrWithKey
    encodeString' = encodeString
#endif

encodeArray :: Aeson.Array -> Encoding
encodeArray vs =
    encodeListLen (fromIntegral (V.length vs))
 <> V.foldr (\v r -> encodeValue v <> r) mempty vs

-- | Decode an arbitrary CBOR value into JSON.
decodeValue :: Bool -> Decoder s Value
decodeValue lenient = do
    tkty <- peekTokenType
    case tkty of
      TypeUInt    -> decodeNumberIntegral
      TypeUInt64  -> decodeNumberIntegral
      TypeNInt    -> decodeNumberIntegral
      TypeNInt64  -> decodeNumberIntegral
      TypeInteger -> decodeNumberIntegral
      TypeFloat16 -> decodeNumberFloat16
      TypeFloat32 -> decodeNumberFloating
      TypeFloat64 -> decodeNumberFloating
      TypeBool    -> Bool   <$> decodeBool
      TypeNull    -> Null   <$  decodeNull
      TypeString  -> String <$> decodeString

      TypeListLen      -> decodeListLen >>= decodeListN lenient
      TypeListLenIndef -> decodeListLenIndef >> decodeListIndef lenient []
      TypeMapLen       -> decodeMapLen >>= flip (decodeMapN lenient) mempty

      _           -> fail $ "unexpected CBOR token type for a JSON value: "
                         ++ show tkty

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
        decodeMapN lenient (n-1) (insert tk tv acc)
  where
#if MIN_VERSION_aeson(2,0,0)
    insert k v m = KM.insert (K.fromText k) v m
#else
    insert k v m = HM.insert k v m
#endif
