{-# LANGUAGE DeriveDataTypeable #-}

-- |
-- Module      : Data.Binary.Serialise.CBOR.IO
-- Copyright   : (c) Duncan Coutts 2015
-- License     : BSD3-style (see LICENSE.txt)
--
-- Maintainer  : duncan@community.haskell.org
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- High-level file-based API for serialising and deserialising values.
--
module Data.Binary.Serialise.CBOR.IO
  ( -- * High level file API
    writeFileSerialise,
    readFileDeserialise,

    -- * High level @'Handle'@ API
    hPutSerialise
  ) where

import           Control.Exception          (throwIO)
import           System.IO                  (Handle, IOMode (..), withFile)

import qualified Data.ByteString.Lazy       as BS

import           Data.Binary.Serialise.CBOR

--------------------------------------------------------------------------------

-- | Serialise a @'BS.ByteString'@ (via @'serialise'@) and write it directly
-- to the specified @'Handle'@.
hPutSerialise :: Serialise a => Handle -> a -> IO ()
hPutSerialise hnd x = BS.hPut hnd (serialise x)

-- | Serialise a @'BS.ByteString'@ and write it directly to the
-- specified file.
writeFileSerialise :: Serialise a => FilePath -> a -> IO ()
writeFileSerialise fname x =
    withFile fname WriteMode $ \hnd -> hPutSerialise hnd x

-- | Read the specified file (internally, by reading a @'BS.ByteString'@)
-- and attempt to decode it into a Haskell value using @'deserialise'@
-- (the type of which is determined by the choice of the result type).
readFileDeserialise :: Serialise a => FilePath -> IO a
readFileDeserialise fname =
    withFile fname ReadMode $ \hnd -> do
      input <- BS.hGetContents hnd
      case deserialiseOrFail input of
        Left  err -> throwIO err
        Right x   -> return x
