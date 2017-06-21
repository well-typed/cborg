{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE CPP          #-}
{-# LANGUAGE BangPatterns #-}
module Main
  ( main -- :: IO ()
  ) where
import           System.FilePath
import           System.Environment
import           System.Exit                         ( exitFailure )
import           System.IO                           ( hPutStrLn, stderr )
import           Text.Printf                         ( printf )

#if !MIN_VERSION_base(4,8,0)
import           Control.Applicative
#endif

import           Data.Aeson                          ( Value(..), Object )
import qualified Data.Aeson                          as Aeson
import           Data.Aeson.Encode.Pretty            as Aeson.Pretty
import           Data.Scientific
import qualified Data.ByteString.Lazy                as LB
import qualified Data.HashMap.Lazy                   as HM

import qualified Data.Text                           as T
import qualified Data.Text.Lazy.IO                   as LT
import qualified Data.Text.Lazy.Builder              as LT
import qualified Data.Vector                         as V

import           Serialise.Cborg.Class
import           Serialise.Cborg.Encoding
import           Serialise.Cborg.Decoding
import           Serialise.Cborg.Pretty
import qualified Serialise.Cborg.Read     as Read
import qualified Serialise.Cborg.Write    as Write
import           Serialise.Cborg.Term     ( decodeTerm, encodeTerm )

--------------------------------------------------------------------------------
-- Aeson adapter code

instance Serialise Value where
  encode = encodeValue
  decode = decodeValue False -- not lenient by default, so JSON is always valid

-- | Encode a JSON value into CBOR.
encodeValue :: Value -> Encoding
encodeValue (Object vs) = encode vs
encodeValue (Array  vs) = encode vs
encodeValue (String s)  = encodeString s
encodeValue (Number n)  = case floatingOrInteger n of
                            Left  d -> encode (d::Double)
                            Right i -> encode (i::Integer)
encodeValue (Bool   b)  = encodeBool b
encodeValue  Null       = encodeNull

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
      TypeFloat64 -> decodeNumberFloating
      TypeBool    -> Bool   <$> decodeBool
      TypeNull    -> Null   <$  decodeNull
      TypeString  -> String <$> decodeString

      TypeListLen      -> decodeListLen >>= flip (decodeListN lenient) []
      TypeListLenIndef -> decodeListLenIndef >> (decodeListIndef lenient) []
      TypeMapLen       -> decodeMapLen >>= flip (decodeMapN lenient) HM.empty

      _           -> fail $ "unexpected CBOR token type for a JSON value: "
                         ++ show tkty

decodeNumberIntegral :: Decoder s Value
decodeNumberIntegral = Number . fromInteger <$> decode

decodeNumberFloating :: Decoder s Value
decodeNumberFloating = Number . fromFloatDigits <$> (decode :: Decoder s Double)

decodeListN :: Bool -> Int -> [Value] -> Decoder s Value
decodeListN !lenient !n acc =
    case n of
      0 -> return $! Array (V.fromList (reverse acc))
      _ -> do !t <- decodeValue lenient
              decodeListN lenient (n-1) (t : acc)

decodeListIndef :: Bool -> [Value] -> Decoder s Value
decodeListIndef !lenient acc = do
    stop <- decodeBreakOr
    if stop then return $! Array (V.fromList (reverse acc))
            else do !tm <- decodeValue lenient
                    decodeListIndef lenient (tm : acc)

decodeMapN :: Bool -> Int -> Object -> Decoder s Value
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
      LB.writeFile cborFile (Write.toLazyByteString encVal)

-- | Convert a CBOR file to JSON, and echo it to @stdout@.
cborToJson :: Bool -> FilePath -> IO ()
cborToJson lenient file = do
  bs <- LB.readFile file
  case (Read.deserialiseFromBytes (decodeValue lenient) bs) of
    Left err -> fail $ "deserialization error: " ++ (show err)
    Right v  -> do
      let builder = Aeson.Pretty.encodePrettyToTextBuilder v
      LT.putStrLn (LT.toLazyText builder)

--------------------------------------------------------------------------------
-- Dumping code

-- | Dump a CBOR file.
dumpCborFile :: Bool -> FilePath -> IO ()
dumpCborFile pretty file = do
  bs <- LB.readFile file
  case (Read.deserialiseFromBytes decodeTerm bs) of
    -- print normally or in pretty-mode, if asked.
    Right v | pretty -> putStrLn (prettyHexEnc $ encodeTerm v)
    Right v          -> print v

    -- otherwise, give a detailed error message
    Left (Read.DeserialiseFailure off err) ->
      fail $ "deserialization error (at offset " ++ show off ++ "): " ++ err

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
  let help = unlines
        [ "usage: cbor-tool <action> [options] <file>"
        , ""
        , "  Useful tool for dealing with and analyzing raw files containing"
        , "  CBOR values. <action> and <file> are both mandatory options."
        , ""
        , "  Actions:"
        , ""
        , "  dump [--json|--hex|--pretty|--term] [--lenient] <file>"
        , "    Read the CBOR values inside <file> and dump them in the"
        , "    specified format. --json will dump JSON files, while --hex"
        , "    will dump the output in hexadecimal format. --pretty will"
        , "    use the internal 'Encoding' pretty-printer. --term is the"
        , "    internal 'Term' format type. You must specify either"
        , "    --json, --hex, --pretty, or --term (there is no default)."
        , ""
        , "    When printing CBOR as JSON, the only valid type for JSON"
        , "    object keys is a UTF8 string. If you have a CBOR map that"
        , "    uses non-String types for keys, this will cause the decoder"
        , "    to fail when creating JSON. Use the --lenient option to"
        , "    remove this restriction -- this will allow numbers to also"
        , "    be valid as CBOR keys, by printing them as strings. However,"
        , "    this may risk collision of some keys as CBOR is more general,"
        , "    result in in invalid JSON -- so use it with caution."
        , ""
        , "  encode [--json] <file>"
        , "    Read the <file> in a specified format and dump a copy of"
        , "    the input in CBOR form under <file>.cbor. --json reads"
        , "    JSON files. You must specify --json (there is no default,"
        , "    for future compatibility.)"
        ]

  args <- getArgs
  case args of
    ("dump"   : "--json"   : "--lenient" : file : _) -> cborToJson True file
    ("dump"   : "--json"   : file : _)               -> cborToJson False file
    ("dump"   : "--hex"    : file : _) -> dumpAsHex          file
    ("dump"   : "--pretty" : file : _) -> dumpCborFile True  file
    ("dump"   : "--term"   : file : _) -> dumpCborFile False file
    ("encode" : "--json"   : file : _) -> jsonToCbor         file
    _ -> hPutStrLn stderr help >> exitFailure
