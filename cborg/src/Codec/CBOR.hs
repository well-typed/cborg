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

{- $intro

The @cborg@ library is a low-level parsing and encoding library for the
Compact Binary Object Representation (CBOR) defined in RFC 7049. CBOR is a
language-agnostic, extensible, and size- and computation-efficient encoding
for arbitrary data, with bijections to ubiquitous JSON and well-defined
canonical representation.

Note, however, that @cborg@ does not itself aim to be a serialisation
library; it merely serves as the substrate on which one might be built. See
the @serialise@ library if you are looking for convenient serialisation of
Haskell values.

-}

{- $structure

The library is split into a number of modules,

* Decoding

    * "Codec.CBOR.Decoding" defines the machinery for decoding primitive CBOR terms
      into Haskell values. In particular, the 'Decoder' type.
    * "Codec.CBOR.Read" defines the low-level wire-format decoder, e.g.

      @
      'Codec.CBOR.Read.deserialiseFromBytes' :: 'Decoder' a -> ByteString -> Either String (ByteString, a)
      @
* Encoding

      * "Codec.CBOR.Encoding" defines the 'Encoding' type, which is in essence
        difference-list of CBOR tokens and is used to construct CBOR encodings.

      * "Codec.CBOR.Write" defines the low-level wire-format encoder, e.g.

        @
        'Codec.CBOR.Write.toBuilder' :: 'Encoding' a -> Data.ByteString.Builder.Builder
        @

* Debugging

    * "Codec.CBOR.FlatTerm" contains the 'FlatTerm' type, which provides a
      concrete AST for capturing primitive CBOR terms. This can be useful when
      testing decoders and encoders.

-}
