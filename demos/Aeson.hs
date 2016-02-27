{-# LANGUAGE CPP          #-}
{-# LANGUAGE BangPatterns #-}
module Main
  ( main -- :: IO ()
  ) where
import           Data.Monoid
import           System.FilePath
import           System.Environment

#if !MIN_VERSION_base(4,8,0)
import           Control.Applicative
#endif

#if MIN_VERSION_aeson(0,10,0)
import           Data.Aeson                          as Aeson hiding (Encoding)
#else
import           Data.Aeson                          as Aeson
#endif
import           Data.Aeson.Encode.Pretty            as Aeson.Pretty
import           Data.Scientific
import qualified Data.Vector                         as Vec
import qualified Data.HashMap.Lazy                   as HashMap
import qualified Data.ByteString.Lazy                as LB

import qualified Data.Binary.Serialise.CBOR.Read     as CBOR.Read
import qualified Data.Binary.Serialise.CBOR.Write    as CBOR.Write
import           Data.Binary.Serialise.CBOR.Encoding
import           Data.Binary.Serialise.CBOR.Decoding

--------------------------------------------------------------------------------
-- Encoder from JSON values to CBOR values

encodeValue :: Value -> Encoding
encodeValue (Object vs) = encodeMapLen (fromIntegral $ HashMap.size vs)
                       <> mconcat [ encodeString k <> encodeValue v
                                  | (k,v) <- HashMap.toList vs ]
encodeValue (Array  vs) = encodeListLen (fromIntegral $ Vec.length vs)
                       <> mconcat [ encodeValue v | v <- Vec.toList vs ]

encodeValue (String s)  = encodeString s
encodeValue (Number n)  = case floatingOrInteger n of
                            Left  d -> encodeDouble d
                            Right i -> encodeInteger i
encodeValue (Bool   b)  = encodeBool b
encodeValue  Null       = encodeNull

--------------------------------------------------------------------------------
-- Decoder from CBOR values to JSON values

decodeValue :: Decoder Value
decodeValue = do
    tkty <- peekTokenType
    case tkty of
      TypeUInt    -> decodeNumberIntegral
      TypeUInt64  -> decodeNumberIntegral
      TypeNInt    -> decodeNumberIntegral
      TypeNInt64  -> decodeNumberIntegral
      TypeInteger -> decodeNumberIntegral
      TypeFloat64 -> decodeNumberFloating

      TypeString  -> String <$> decodeString
      TypeListLen -> decodeListLen >>= decodeArray
      TypeMapLen  -> decodeMapLen  >>= decodeObject

      TypeBool    -> Bool   <$> decodeBool
      TypeNull    -> Null   <$  decodeNull
      _           -> fail $ "unexpected CBOR token type for a JSON value: "
                         ++ show tkty

decodeNumberIntegral :: Decoder Value
decodeNumberIntegral = Number . fromInteger <$> decodeInteger

decodeNumberFloating :: Decoder Value
decodeNumberFloating = Number . fromFloatDigits <$> decodeDouble

decodeArray :: Int -> Decoder Value
decodeArray n0 = do
    vs <- go n0 []
    return $! Array (Vec.fromListN n0 (reverse vs))
  where
    go 0 acc = return acc
    go n acc = do t <- decodeValue
                  go (n-1) (t : acc)

decodeObject :: Int -> Decoder Value
decodeObject n0 = do
    kvs <- go n0 []
    return $! Object (HashMap.fromList kvs)
  where
    go 0 acc = return acc
    go n acc = do k <- decodeString
                  v <- decodeValue
                  go (n-1) ((k, v) : acc)

--------------------------------------------------------------------------------
-- Driver program

-- | Convert an arbitrary JSON file into CBOR format.
jsonToCbor :: FilePath -> IO ()
jsonToCbor file = do
  bs <- LB.readFile file
  case (Aeson.decode bs) of
    Nothing -> fail "invalid JSON file!"
    Just v  -> do
      let encVal   = encodeValue v
          cborFile = dropExtension file <.> ".cbor"
      -- Now write the blob
      LB.writeFile cborFile (CBOR.Write.toLazyByteString encVal)

-- | Convert a CBOR file to JSON, and echo it to @stdout@.
cborToJson :: FilePath -> IO ()
cborToJson file = do
  bs <- LB.readFile file
  case (CBOR.Read.deserialiseFromBytes decodeValue bs) of
    Left err -> fail $ "deserialization error: " ++ err
    -- TODO FIXME: encoding nonsense
    Right v  -> LB.putStrLn (Aeson.Pretty.encodePretty v)

-- | Main entry point.
main :: IO ()
main = do
  args <- getArgs
  case args of
    ("encode":file:_) -> jsonToCbor file
    ("decode":file:_) -> cborToJson file
    _ -> fail "invalid args: use 'encode <file>.json' or 'decode <file>.cbor'"
