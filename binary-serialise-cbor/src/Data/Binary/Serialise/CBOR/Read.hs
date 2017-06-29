{-# LANGUAGE RankNTypes #-}

-- |
-- Module      : Data.Binary.Serialise.CBOR.Read
-- Copyright   : (c) Duncan Coutts 2015-2017
-- License     : BSD3-style (see LICENSE.txt)
--
-- Maintainer  : duncan@community.haskell.org
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Tools for reading values in a CBOR-encoded format
-- back into ordinary values.
--
module Data.Binary.Serialise.CBOR.Read
 ( module X
 , deserialiseFromBytes
 ) where

import           Codec.CBOR.Read as X hiding (deserialiseFromBytes)
import qualified Codec.CBOR.Read as Read
import           Codec.CBOR.Decoding (Decoder)
import qualified Data.ByteString.Lazy as LBS

deserialiseFromBytes :: (forall s. Decoder s a)
                     -> LBS.ByteString
                     -> Either Read.DeserialiseFailure a
deserialiseFromBytes f = fmap snd . Read.deserialiseFromBytes f
