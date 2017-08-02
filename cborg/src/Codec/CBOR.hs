{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- |
-- Module      : Codec.CBOR
-- Copyright   : (c) Duncan Coutts 2015-2017
-- License     : BSD3-style (see LICENSE.txt)
--
-- Maintainer  : duncan@community.haskell.org
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- A library for working with CBOR.
--
module Codec.CBOR
 ( -- $intro
   -- * Library Structure
   -- $structure
 ) where

-- used by Haddocks
import Codec.CBOR.Decoding (Decoder)
import Codec.CBOR.Encoding (Encoding)
import Codec.CBOR.FlatTerm (FlatTerm)
import Codec.CBOR.Term     (Term, encodeTerm, decodeTerm)

{- $intro

The @cborg@ library is a low-level parsing and encoding library for the
Compact Binary Object Representation (CBOR) defined in RFC 7049. CBOR is a
language-agnostic, extensible, and size- and computation-efficient encoding
for arbitrary data, with a well-defined bijection to the ubiquitous JSON format
and a precisely specified canonical form.

Note, however, that @cborg@ does not itself aim to be a serialisation
library; it merely serves as the substrate on which such a library might be
built. See the [serialise](/package/serialise) library if you are looking for
convenient serialisation of Haskell values.

Instead, @cborg@ targets cases where precise control over the CBOR object
structure is needed such as when working with externally-specified CBOR formats.

-}

{- $structure

The library is split into a number of modules,

* Decoding

    * "Codec.CBOR.Decoding" defines the machinery for decoding primitive CBOR terms
      into Haskell values. In particular, the 'Decoder' type and associated decoders,

      @
      data 'Decoder' s a

      -- for, e.g., safe in-place mutation during decoding
      liftST      :: ST s a -> 'Decoder' s a

      -- primitive decoders
      decodeWord  :: 'Decoder' s Word
      decodeBytes :: 'Decoder' s ByteString
      -- et cetera
      @
    * "Codec.CBOR.Read" defines the low-level wire-format decoder, e.g.

      @
      'Codec.CBOR.Read.deserialiseFromBytes' :: 'Decoder' a
                           -> ByteString
                           -> Either String (ByteString, a)
      @

* Encoding

      * "Codec.CBOR.Encoding" defines the 'Encoding' type, which is in essence
        difference-list of CBOR tokens and is used to construct CBOR encodings.

        @
        data 'Encoding'
        instance Monoid 'Encoding'

        encodeWord  :: Word       -> Encoding
        encodeBytes :: ByteString -> Encoding
        -- et cetera
        @

      * "Codec.CBOR.Write" defines the low-level wire-format encoder, e.g.

        @
        'Codec.CBOR.Write.toBuilder' :: 'Encoding' a -> Data.ByteString.Builder.Builder
        @

* Capturing arbitrary terms

      * "Codec.CBOR.Term" provides the 'Term' type, which provides a type for
        capturing arbitrary CBOR terms. 'Term's can be encoded and decoded with,

        @
        data 'Term'
          = TInt   Int
          | TBytes ByteString
          -- et cetera

        'encodeTerm' :: 'Term' -> 'Encoding'
        'decodeTerm' :: 'Decoder' 'Term'
        @

* Debugging

    * "Codec.CBOR.FlatTerm" contains the 'FlatTerm' type, which provides a
      concrete AST for capturing primitive CBOR wire encodings. This can be
      useful when testing decoders and encoders.

-}
