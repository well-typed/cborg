{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RankNTypes   #-}
module Main
  ( main -- :: IO ()
  ) where

import           Control.Monad
import           System.FilePath
import           System.Environment
import           System.Exit                         ( exitFailure )
import           System.IO                           ( hPutStrLn, stderr )
import           Text.Printf                         ( printf )

import qualified Data.Aeson                          as Aeson
import           Data.Aeson.Encode.Pretty            as Aeson.Pretty
import qualified Data.ByteString.Lazy                as LB

import qualified Data.Text.Lazy.IO                   as LT
import qualified Data.Text.Lazy.Builder              as LT

import           Codec.CBOR.JSON
import           Codec.CBOR.Pretty
import           Codec.CBOR.Read     as Read
import qualified Codec.CBOR.Write    as Write
import           Codec.CBOR.Term     ( decodeTerm, encodeTerm )

--------------------------------------------------------------------------------
-- JSON -> CBOR conversion

-- | Convert an arbitrary JSON file into CBOR format.
jsonToCbor :: FilePath -> IO ()
jsonToCbor file = do
  bs <- LB.readFile file
  case Aeson.decode bs of
    Nothing -> fail "invalid JSON file!"
    Just v  -> do
      let encVal   = encodeValue v
          cborFile = dropExtension file <.> "cbor"
      -- Now write the blob
      LB.writeFile cborFile (Write.toLazyByteString encVal)

--------------------------------------------------------------------------------
-- Dumping code

-- | Dump a CBOR file as JSON
dumpCborAsJson :: Bool -> FilePath -> IO ()
dumpCborAsJson lenient file = LB.readFile file >>= go
  where go :: LB.ByteString -> IO ()
        go bs =
          case deserialiseFromBytes (decodeValue lenient) bs of
            Left (Read.DeserialiseFailure off err) ->
                fail $ "deserialization error (at offset " ++ show off ++ "): " ++ err
            Right (trailing, v) -> do
                let builder = Aeson.Pretty.encodePrettyToTextBuilder v
                LT.putStrLn (LT.toLazyText builder)
                unless (LB.null trailing) (go trailing)

-- | Dump a CBOR file as a Term, or optionally as pretty hex
dumpCborFile :: Bool -> FilePath -> IO ()
dumpCborFile pretty file = LB.readFile file >>= go
  where go :: LB.ByteString -> IO ()
        go bs =
          case deserialiseFromBytes decodeTerm bs of
            Left (Read.DeserialiseFailure off err) ->
                fail $ "deserialization error (at offset " ++ show off ++ "): " ++ err
            Right (trailing, v) -> do
                if pretty
                  then putStrLn (prettyHexEnc $ encodeTerm v)
                  else print v
                unless (LB.null trailing) (go trailing)

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
        , "    resulting in invalid JSON -- so use it with caution."
        , ""
        , "  encode [--json] <file>"
        , "    Read the <file> in a specified format and dump a copy of"
        , "    the input in CBOR form under <file>.cbor. --json reads"
        , "    JSON files. You must specify --json (there is no default,"
        , "    for future compatibility.)"
        ]

  args <- getArgs
  case args of
    ("dump"   : "--json"   : "--lenient" : file : _) -> dumpCborAsJson True file
    ("dump"   : "--json"   : file : _) -> dumpCborAsJson False file
    ("dump"   : "--hex"    : file : _) -> dumpAsHex file
    ("dump"   : "--pretty" : file : _) -> dumpCborFile True  file
    ("dump"   : "--term"   : file : _) -> dumpCborFile False file
    ("encode" : "--json"   : file : _) -> jsonToCbor file
    _ -> hPutStrLn stderr help >> exitFailure
