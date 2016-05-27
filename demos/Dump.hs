{-# LANGUAGE CPP          #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}  -- instance Serialise Value
module Main
  ( main -- :: IO ()
  ) where
import           Text.Printf                     (printf)
import           Control.Monad                   (when)
import           System.FilePath                 (takeExtension)
import           System.Environment              (getArgs)

import qualified Data.ByteString.Lazy            as LB

import           Data.Binary.Serialise.CBOR.Term (decodeTerm)
import           Data.Binary.Serialise.CBOR.Read (deserialiseFromBytes)

--------------------------------------------------------------------------------
-- Driver program

-- | Dump a CBOR file.
dumpCborFile :: FilePath -> IO ()
dumpCborFile file = do
  -- Ensure the extension is sane, so that dropExtension doesn't
  -- end up making some weird file.
  when (takeExtension file /= ".cbor") $ do
    fail "Input file expected to have .cbor extension; exiting"

  bs <- LB.readFile file
  case (deserialiseFromBytes decodeTerm bs) of
    Left err -> fail $ "deserialization error: " ++ show err
    Right v  -> print v

dumpAsHex :: FilePath -> IO ()
dumpAsHex file = do
  bs <- LB.readFile file
  putStrLn (showHexString bs)

-- | Show a @'ByteString'@ as a hex string.
showHexString :: LB.ByteString -> String
showHexString = concat . toHex
  where
    toHex = map (printf "%02X ") . LB.unpack

-- | Main entry point.
main :: IO ()
main = do
  let help = fail $ unlines
        [ "usage: demo-dump-cbor <file>.cbor"
        , ""
        , "  Dump any arbitrary CBOR value out to stdout"
        ]

  args <- getArgs
  case args of
    ("hex":file:_) -> dumpAsHex file
    (file:_)       -> dumpCborFile file
    _              -> help
