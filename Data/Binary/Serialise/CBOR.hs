{-# LANGUAGE DeriveDataTypeable #-}

-- |
-- Module      : Data.Binary.Serialise.CBOR
-- Copyright   : (c) Duncan Coutts 2015
-- License     : BSD3-style (see LICENSE.txt)
--
-- Maintainer  : duncan@community.haskell.org
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- This module provides functions to serialise and deserialise Haskell
-- values for storage or transmission, to and from lazy
-- @'Data.ByteString.Lazy.ByteString'@s. It also provides a type class
-- and utilities to help you make your types serialisable.
--
-- For a full tutorial on using this module, see
-- "Data.Binary.Serialise.CBOR.Tutorial".
--
module Data.Binary.Serialise.CBOR
  ( -- * High level API
    -- $highlevel
    serialise,
    deserialise,
    deserialiseOrFail,

    -- * Deserialisation exceptions
    DeserialiseFailure(..),

    -- * Primitive, incremental interface
    -- $primitives
    serialiseIncremental,
    deserialiseIncremental,

    -- * The @'Serialise'@ class
    Serialise(..),
  ) where

import           Control.Exception                (Exception)
import           Data.Typeable                    (Typeable)

import qualified Data.Binary.Get                  as Bin
import qualified Data.ByteString.Builder          as BS
import qualified Data.ByteString.Lazy             as BS
import qualified Data.ByteString.Lazy.Internal    as BS

import           Data.Binary.Serialise.CBOR.Class
import qualified Data.Binary.Serialise.CBOR.Read  as CBOR.Read
import qualified Data.Binary.Serialise.CBOR.Write as CBOR.Write

--------------------------------------------------------------------------------

-- $primitives
-- The following API...
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

-- $highlevel
-- This is a test
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
