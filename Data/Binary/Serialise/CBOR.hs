{-# LANGUAGE DeriveDataTypeable #-}

-- |
-- Module      : Data.Binary.Serialise.CBOR.ByteOrder
-- Copyright   : (c) Duncan Coutts 2015
-- License     : BSD3-style (see LICENSE.txt)
--
-- Maintainer  : duncan@community.haskell.org
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- This module provides functions to serialise and deserialise Haskell
-- values for storage or transmission. It also provides a type class
-- and utilities to help you make your types serialisable.
--
module Data.Binary.Serialise.CBOR (
    -- * Serialization and derialization of Haskell values

    -- ** Convenience functions
    serialise,
    deserialise,
    deserialiseOrFail,
    writeFileSerialise,
    readFileDeserialise,
    hPutSerialise,

    -- ** The primitives
    serialiseIncremental,
    deserialiseIncremental,

    -- * Making types serializable
    -- ** The Serialise class
    -- | 
    Serialise(..),
  ) where

--import Data.Serialise.Serialise.CBOR.Encode
--import Data.Serialise.Serialise.CBOR.Decode
import           Data.Binary.Serialise.CBOR.Class
import qualified Data.Binary.Serialise.CBOR.Read  as CBOR.Read
import qualified Data.Binary.Serialise.CBOR.Write as CBOR.Write
import qualified Data.Binary.Get as Bin


import qualified Data.ByteString.Lazy as BS
import qualified Data.ByteString.Lazy.Internal as BS
import qualified Data.ByteString.Builder as BS

import System.IO
import Data.Typeable
import Control.Exception


-------------------
-- The primitives
--

-- | Serialise a Haskell value to an external binary representation.
--
-- The output is represented as a 'BS.Builder' and is constructed incrementally.
-- The representation as a 'BS.Builder' allows efficient concatenation with
-- other data.
--
serialiseIncremental :: Serialise a => a -> BS.Builder
serialiseIncremental = CBOR.Write.toBuilder . encode

-- | Deserialise a Haskell value from the external binary representation.
--
-- This allows /input/ data to be provided incrementally, rather than all in one
-- go. It also gives an explicit representation of deserialisation errors.
--
-- Note that the incremental behaviour is only for the input data, not the
-- output value: the final deserialised value is constructed and returned as a
-- whole, not incrementally.
--
deserialiseIncremental :: Serialise a => Bin.Decoder a
deserialiseIncremental = CBOR.Read.deserialiseIncremental decode

--------------------------
-- Convenience functions
--

-- | Serialise a Haskell value to an external binary representation.
--
-- The output is represented as a lazy 'BS.ByteString' and is constructed
-- incrementally.
--
serialise :: Serialise a => a -> BS.ByteString
serialise = BS.toLazyByteString . serialiseIncremental

-- | Deserialise a Haskell value from the external binary representation
-- (which must have been made using 'serialise' or related function).
--
-- This is a convenience function; it takes all the input at once and it will
-- fail with an exception if the given external representation is invalid or
-- does not correspond to a value of the expected type.
--
deserialise :: Serialise a => BS.ByteString -> a
deserialise =
    supplyAllInput deserialiseIncremental
  where
    supplyAllInput (Bin.Done _ _ x) _bs = x
    supplyAllInput (Bin.Partial k)   bs =
      case bs of
        BS.Chunk chunk bs' ->  supplyAllInput (k (Just chunk)) bs'
        BS.Empty           ->  supplyAllInput (k Nothing)      BS.Empty
    supplyAllInput (Bin.Fail _ off msg) _ =
      error $ "Data.Binary.Serialise.CBOR.deserialise: failed at offset "
           ++ show off ++ " : " ++ msg

data DeserialiseFailure =
       DeserialiseFailure Bin.ByteOffset String
  deriving (Show, Typeable)

instance Exception DeserialiseFailure

deserialiseOrFail :: Serialise a => BS.ByteString -> Either DeserialiseFailure a
deserialiseOrFail = supplyAllInput deserialiseIncremental
  where
    supplyAllInput (Bin.Done _ _ x) _bs = Right x
    supplyAllInput (Bin.Partial k)   bs =
      case bs of
        BS.Chunk chunk bs' ->  supplyAllInput (k (Just chunk)) bs'
        BS.Empty           ->  supplyAllInput (k Nothing)      BS.Empty
    supplyAllInput (Bin.Fail _ offset msg) _ =
        Left (DeserialiseFailure offset msg)


hPutSerialise :: Serialise a => Handle -> a -> IO ()
hPutSerialise hnd x = BS.hPut hnd (serialise x)

writeFileSerialise :: Serialise a => FilePath -> a -> IO ()
writeFileSerialise fname x =
    withFile fname WriteMode $ \hnd -> hPutSerialise hnd x

readFileDeserialise :: Serialise a => FilePath -> IO a
readFileDeserialise fname =
    withFile fname ReadMode $ \hnd -> do
      input <- BS.hGetContents hnd
      case deserialiseOrFail input of
        Left  err -> throwIO err
        Right x   -> return x

