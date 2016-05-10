-- |
-- Module      : Data.Binary.Serialise.CBOR.Properties
-- Copyright   : (c) Duncan Coutts 2015
-- License     : BSD3-style (see LICENSE.txt)
--
-- Maintainer  : duncan@community.haskell.org
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- This module contains a set of generally useful properties, which
-- instance authors are encouraged to use in order to test their
-- instances of the @'Serialise'@ class.
--
module Data.Binary.Serialise.CBOR.Properties
  ( -- * CBOR Properties
    serialiseIdentity -- :: (Serialise a, Eq a) => a -> Bool
    -- * @'FlatTerm'@ Properties
  , flatTermIdentity  -- :: (Serialise a, Eq a) => a -> Bool
  , hasValidFlatTerm  -- ::  Serialise a        => a -> Bool
  ) where

import           Data.Binary.Serialise.CBOR          (deserialise, serialise)
import           Data.Binary.Serialise.CBOR.Class
import           Data.Binary.Serialise.CBOR.FlatTerm

--------------------------------------------------------------------------------

-- | Ensure that serializing and deserializing some value results in
-- the original value being returned.
serialiseIdentity :: (Serialise a, Eq a) => a -> Bool
serialiseIdentity a = a == (deserialise . serialise) a

-- | Ensure that serializing and deserializing a value with the
-- @'FlatTerm'@ form results in the original value being returned.
flatTermIdentity :: (Serialise a, Eq a) => a -> Bool
flatTermIdentity  a = Right a == (fromFlat . toFlat) a
  where
    toFlat   = toFlatTerm . encode
    fromFlat = fromFlatTerm decode

-- | Ensure that serializing a value into a @'FlatTerm'@ gives us a
-- valid @'FlatTerm'@ back.
hasValidFlatTerm :: Serialise a => a -> Bool
hasValidFlatTerm = validFlatTerm . toFlatTerm . encode
