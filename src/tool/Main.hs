{-# LANGUAGE CPP          #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}  -- instance Serialise Value
module Main
  ( main -- :: IO ()
  ) where
import           System.FilePath
import           System.Environment
import           Text.Printf                         ( printf )

#if !MIN_VERSION_base(4,8,0)
import           Control.Applicative
#endif

import           Data.Aeson                          ( Value(..) )
import qualified Data.Aeson                          as Aeson
import           Data.Aeson.Encode.Pretty            as Aeson.Pretty
import           Data.Scientific
import qualified Data.ByteString.Lazy                as LB

import qualified Data.Text.Lazy.IO                   as T
import qualified Data.Text.Lazy.Builder              as T
import qualified Data.Vector                         as V

import           Data.Binary.Serialise.CBOR.Class
import           Data.Binary.Serialise.CBOR.Encoding
import           Data.Binary.Serialise.CBOR.Decoding
import           Data.Binary.Serialise.CBOR.Pretty
import qualified Data.Binary.Serialise.CBOR.Read     as CBOR.Read
import qualified Data.Binary.Serialise.CBOR.Write    as CBOR.Write
import           Data.Binary.Serialise.CBOR.Term     ( decodeTerm, encodeTerm )

--------------------------------------------------------------------------------
-- Aeson adapter code

instance Serialise Value where
  encode = encodeValue
  decode = decodeValue

-- | Encode a JSON value into CBOR.
encodeValue :: Value -> Encoding
encodeValue (Object vs) = encode vs
encodeValue (Array  vs) = encode vs
encodeValue (String s)  = encode s
encodeValue (Number n)  = case floatingOrInteger n of
                            Left  d -> encode (d::Double)
                            Right i -> encode (i::Integer)
encodeValue (Bool   b)  = encode b
encodeValue  Null       = encodeNull

-- | Decode an arbitrary CBOR value into JSON.
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
-- CBOR <-> JSON conversion

-- | Convert an arbitrary JSON file into CBOR format.
jsonToCbor :: FilePath -> IO ()
jsonToCbor file = do
  bs <- LB.readFile file
  case (Aeson.decode bs) of
    Nothing -> fail "invalid JSON file!"
    Just v  -> do
      let encVal   = encodeValue v
          cborFile = dropExtension file <.> "cbor"
      -- Now write the blob
      LB.writeFile cborFile (CBOR.Write.toLazyByteString encVal)

-- | Convert a CBOR file to JSON, and echo it to @stdout@.
cborToJson :: FilePath -> IO ()
cborToJson file = do
  bs <- LB.readFile file
  case (CBOR.Read.deserialiseFromBytes decodeValue bs) of
    Left err -> fail $ "deserialization error: " ++ (show err)
    Right v  -> do
      let builder = Aeson.Pretty.encodePrettyToTextBuilder v
      T.putStrLn (T.toLazyText builder)

--------------------------------------------------------------------------------
-- Dumping code

-- | Dump a CBOR file.
dumpCborFile :: Bool -> FilePath -> IO ()
dumpCborFile pretty file = do
  bs <- LB.readFile file
  case (CBOR.Read.deserialiseFromBytes decodeTerm bs) of
    Left err -> fail $ "deserialization error: " ++ (show err)
    Right v | pretty -> putStrLn (prettyHexEnc $ encodeTerm v)
    Right v          -> print v

dumpAsHex :: FilePath -> IO ()
dumpAsHex file = do
  bs <- LB.readFile file
  putStrLn (showHexString bs)

-- | Show a @'ByteString'@ as a hex string.
showHexString :: LB.ByteString -> String
showHexString = concat . toHex
  where
    toHex = map (printf "%02X ") . LB.unpack

--------------------------------------------------------------------------------
-- Driver code

-- | Main entry point.
main :: IO ()
main = do
  let help = fail $ unlines
        [ "usage: cbor-tool <action> [options] <file>"
        , ""
        , "  Useful tool for dealing with CBOR values. <action> and <file>"
        , "  are mandatory options."
        , ""
        , "  Actions:"
        , ""
        , "  dump [--json|--hex|--pretty|--term] <file>"
        , "    Read the CBOR values inside <file> and dump them in the"
        , "    specified format. --json will dump JSON files, while --hex"
        , "    will dump the output in hexadecimal format. --pretty will"
        , "    use the internal 'Encoding' pretty-printer. --term is the"
        , "    internal 'Term' format type. You must specify either"
        , "    --json, --hex, -pretty, or --term (there is no default)."
        , ""
        , "  encode [--json] <file>"
        , "    Read the <file> in a specified format and dump a copy of"
        , "    the input in CBOR form under <file>.cbor. --json reads"
        , "    JSON files. You must specify --json (there is no default"
        , "    for future compatibility."
        ]

  args <- getArgs
  case args of
    ("dump"   : "--json"   : file : _) -> cborToJson         file
    ("dump"   : "--hex"    : file : _) -> dumpAsHex          file
    ("dump"   : "--pretty" : file : _) -> dumpCborFile True  file
    ("dump"   : "--term"   : file : _) -> dumpCborFile False file
    ("encode" : "--json"   : file : _) -> jsonToCbor         file
    _ -> help
