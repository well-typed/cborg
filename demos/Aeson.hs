{-# LANGUAGE CPP          #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}  -- instance Serialise Value
module Main
  ( main -- :: IO ()
  ) where
import           Control.Monad (when)
import           System.FilePath
import           System.Environment

#if !MIN_VERSION_base(4,8,0)
import           Control.Applicative
#endif

import           Data.Aeson (Value(..))
import qualified Data.Aeson                          as Aeson
import           Data.Aeson.Encode.Pretty            as Aeson.Pretty
import           Data.Scientific
import qualified Data.ByteString.Lazy                as LB

import qualified Data.Text.Lazy.IO                   as T
import qualified Data.Text.Lazy.Builder              as T
import qualified Data.Vector                         as V

import qualified Data.Binary.Serialise.CBOR.Read     as CBOR.Read
import qualified Data.Binary.Serialise.CBOR.Write    as CBOR.Write
import           Data.Binary.Serialise.CBOR.Encoding
import           Data.Binary.Serialise.CBOR.Decoding

import           Data.Binary.Serialise.CBOR.Class

--------------------------------------------------------------------------------
-- Encoder from JSON values to CBOR values


instance Serialise Value where
  encode = encodeValue
  decode = decodeValue

encodeValue :: Value -> Encoding
encodeValue (Object vs) = encode vs
encodeValue (Array  vs) = encode vs

encodeValue (String s)  = encode s
encodeValue (Number n)  = case floatingOrInteger n of
                            Left  d -> encode (d::Double)
                            Right i -> encode (i::Integer)
encodeValue (Bool   b)  = encode b
encodeValue  Null       = encodeNull

--------------------------------------------------------------------------------
-- Decoder from CBOR values to JSON values

decodeValue :: Decoder s Value
decodeValue = do
    tkty <- peekTokenType
    case tkty of
      TypeUInt    -> decodeNumberIntegral
      TypeUInt64  -> decodeNumberIntegral
      TypeNInt    -> decodeNumberIntegral
      TypeNInt64  -> decodeNumberIntegral
      TypeInteger -> decodeNumberIntegral
      TypeFloat64 -> decodeNumberFloating

      TypeString       -> String <$> decode
      TypeListLen      -> Array  <$> decode
      TypeListLenIndef -> decodeIndefList
      TypeMapLen       -> Object <$> decode

      TypeBool    -> Bool   <$> decodeBool
      TypeNull    -> Null   <$  decodeNull
      _           -> fail $ "unexpected CBOR token type for a JSON value: "
                         ++ show tkty

decodeIndefList :: Decoder s Value
decodeIndefList = Array . V.fromList <$> decode

decodeNumberIntegral :: Decoder s Value
decodeNumberIntegral = Number . fromInteger <$> decode

decodeNumberFloating :: Decoder s Value
decodeNumberFloating = Number . fromFloatDigits <$> (decode :: Decoder s Double)


--------------------------------------------------------------------------------
-- Driver program

-- | Convert an arbitrary JSON file into CBOR format.
jsonToCbor :: FilePath -> IO ()
jsonToCbor file = do
  -- Ensure the extension is sane, so that dropExtension doesn't
  -- end up making some weird file.
  when (takeExtension file /= ".json") $ do
    fail "Input file expected to have .json extension; exiting"

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
  -- Ensure the extension is sane
  when (takeExtension file /= ".cbor") $ do
    fail "Input file expected to have .cbor extension; exiting"

  bs <- LB.readFile file
  case (CBOR.Read.deserialiseFromBytes decodeValue bs) of
    Left err -> fail $ "deserialization error: " ++ show err
    Right v  -> do
      let builder = Aeson.Pretty.encodePrettyToTextBuilder v
      T.putStrLn (T.toLazyText builder)

-- | Main entry point.
main :: IO ()
main = do
  let help = fail $ unlines
        [ "usage: demo-aeson [encode <file>.json | decode <file>.cbor]"
        , ""
        , "  encode <file>.json"
        , "    Read <file>.json and encode it as CBOR, into <file>.cbor"
        , ""
        , "  decode <file>.cbor"
        , "    Read <file>.cbor and decode it into JSON, to stdout"
        ]

  -- Dispatch; note the pattern matching to ensure we have at least
  -- two args
  args <- getArgs
  case args of
    ("encode":file:_) -> jsonToCbor file
    ("decode":file:_) -> cborToJson file
    _                 -> help
