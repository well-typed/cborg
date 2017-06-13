{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- |
-- Module      : Codec.CBOR.Tutorial
-- Copyright   : (c) Duncan Coutts 2015-2017
-- License     : BSD3-style (see LICENSE.txt)
--
-- Maintainer  : duncan@community.haskell.org
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- @cborg@ is a library for the serialisation of Haskell values.
--
module Codec.Serialise.Tutorial
  ( -- * Introduction
    -- $introduction

    -- ** The CBOR format
    -- $cbor_format

    -- * The 'Serialise' class
    -- $serialise

    -- ** Encoding terms
    -- $encoding

    -- ** Decoding terms
    -- $decoding

    -- * Migrations
    -- $migrations

    -- * Working with foreign encodings
    -- | While @cborg@ is primarily designed to be a Haskell serialisation
    -- library, the fact that it uses the standard CBOR encoding means that it can also
    -- find uses in interacting with foreign non-@cborg@ producers and
    -- consumers. In this section we will describe a few features of the library
    -- which may be useful in such applications.

    -- ** Working with arbitrary terms
    -- $arbitrary_terms

    -- ** Examining encodings
    -- $examining_encodings
  ) where

-- These are necessary for haddock to properly hyperlink
import Codec.Serialise.Decoding
import Codec.Serialise.Class

{- $introduction

As in modern serialisation libraries, @cborg@ offers
instance derivation via GHC's 'GHC.Generic' mechanism,

> import Serialise.Cborg
> import qualified Data.ByteString.Lazy as BSL
>
> data Animal = HoppingAnimal { animalName :: String, hoppingHeight :: Int }
>             | WalkingAnimal { animalName :: String, walkingSpeed :: Int }
>             deriving (Generic)
>
> instance Serialise Animal
>
> fredTheFrog :: Animal
> fredTheFrog = HoppingAnimal "Fred" 4
>
> main = BSL.writeFile "hi" (serialise fredTheFrog)

We can then later read Fred,

> main = do
>     fred <- deserialise <$> BSL.readFile "hi"
>     print fred

-}

{- $cbor_format

@cborg@ uses the Concise Binary Object Representation, CBOR
(IETF RFC 7049, <https://tools.ietf.org/html/rfc7049>), as its serialised
representation. This encoding is efficient in both encoding\/decoding complexity
as well as space, and is generally machine-independent.

The CBOR data model resembles that of JSON, having arrays, key\/value maps,
integers, floating point numbers, binary strings, and text. In addition,
CBOR allows items to be /tagged/ with a number which describes the type of data
that follows. This can be used both to identify which data constructor of a type
an encoding represents, as well as representing different versions of the same
constructor.

=== A note on interoperability

@cborg@ is intended primarily as a /serialisation/ library for
Haskell values. That is, a means of stably storing Haskell values for later
reading by @cborg@. While it uses the CBOR encoding format, the
library is /not/ primarily aimed to facilitate serialisation and
deserialisation across different CBOR implementations.

If you want to use @cborg@ to serialise\/deserialise values
for\/from another CBOR implementation (either in Haskell or another language),
you should keep a few things in mind,

1. The 'Serialise' instances for some "basic" Haskell types (e.g. 'Maybe',
   'Data.ByteString.ByteString', tuples) don't carry a tag, in contrast to common
   convention. This is an intentional design decision to minimize encoding size
   for types which are primitive enough that their representation can be
   considered stable.

2. The library reserves the right to change encodings in
   non-backwards-compatible ways across super-major versions. For example the
   library may start producing a new representation for some type. The new
   version of the library will be able to decode the old and new representation,
   but your different CBOR decoder would not be expecting the new representation
   and would have to be updated to match.

3. While the library tries to use standard encodings in its instances wherever possible,
   these instances aren't guaranteed to implement all valid variants of the
   encodings in the specification. For instance, the 'UTCTime' instance only
   implements a small subset of the encodings described by the Extended Date
   RFC.

-}

{- $serialise

@cborg@ provides a 'Serialise' class for convenient access to serialisers and
deserialisers. Writing a serialiser can be as simple as deriving 'Generic' and
'Serialise',

> -- with DerivingStrategies (GHC 8.2 and newer)
> data Animal = ...
>             deriving stock (Generic)
>             deriving anyclass (Serialise)
>
> -- older GHCs
> data MyType = ...
>             deriving (Generic)
> instance Serialise MyType

Of course, you can also write the equivalent serialisers manually.
A hand-rolled 'Serialise' instance may be desireable for a variety
of reasons,

 * Deviating from the type-guided encoding that the 'Generic' instance will
   provide

 * Interfacing with other CBOR implementations

 * Enabling migrations for future changes to the type or its encoding

A minimal hand-rolled instance will define the 'encode' and 'decode' methods,

> instance Serialise Animal where
>     encode = encodeAnimal
>     decode = decodeAnimal

Below we will describe how to write these pieces.

-}

{- $encoding

For the purposes of encoding, abstract CBOR representations are embodied by the
'Serialise.Cborg.Encoding.Tokens' type. Such a representation can be efficiently
built using the 'Serialise.Cborg.Encoding.Encoding' 'Monoid'.

For instance, to implement an encoder for the @Animal@ type above we might write,

> encodeAnimal :: Animal -> Encoding
> encodeAnimal (HoppingAnimal name height) =
>     encodeListLen 3 <> encodeTag 0 <> encode name <> encode height
> encodeAnimal (WalkingAnimal name speed) =
>     encodeListLen 3 <> encodeTag 1 <> encode name <> encode speed

Here we see that each encoding begins with a /length/, describing how many
values belonging to our @Animal@ will follow. We then encode a /tag/, which
identifies which constructor. We then encode the fields using their respective
'Serialise' instance.

It is recommended that you not deviate from this encoding scheme, including both
the length and tag, to ensure that you have the option to migrate your types
later on.

-}

{- $decoding

Decoding CBOR representations to Haskell values is done in the 'Decoder'
'Monad'. We can write a 'Decoder' for the @Animal@ type defined above as
follows,

> decodeAnimal :: Decoder s Animal
> decodeAnimal = do
>     len <- decodeListLen
>     tag <- decodeTag
>     case (len, tag) of
>       (3, 0) -> HoppingAnimal <$> decode <*> decode
>       (3, 1) -> WalkingAnimal <$> decode <*> decode
>       _      -> fail "invalid Animal encoding"

-}

{- $migrations

One eventuality that data serialisation schemes need to account for is the need
for changes in the data's structure. There are two types of compatibility
which we might want to strive for in our serialisers,

 * Backward compatibility, such that newer versions of the serialiser can read
   older versions of an encoding

 * Forward compatibility, such that older versions of the serialiser can read
   (or at least tolerate) newer versions of an encoding

Below we will look at a few of the types of changes which we may need to make
and describe how these can be handled in a backwards-compatible manner with
@cborg@.

=== Adding a constructor

Say we want to add a new constructor to our @Animal@ type, @SwimmingAnimal@,

> data Animal = HoppingAnimal { animalName :: String, hoppingHeight :: Int }
>             | WalkingAnimal { animalName :: String, walkingSpeed :: Int }
>             | SwimmingAnimal { numberOfFins :: Int }
>             deriving (Generic)

We can account for this in our hand-rolled serialiser by simply adding a new tag
to our encoder and decoder,

> encodeAnimal :: Animal -> Encoding
> -- HoppingAnimal, SwimmingAnimal cases are unchanged...
> encodeAnimal (HoppingAnimal name height) =
>     encodeListLen 3 <> encodeTag 0 <> encode name <> encode height
> encodeAnimal (WalkingAnimal name speed) =
>     encodeListLen 3 <> encodeTag 1 <> encode name <> encode speed
> -- Here is out new case...
> encodeAnimal (SwimmingAnimal numberOfFins) =
>     encodeListLen 2 <> encodeTag 2 <> encode numberOfFins
>
> decodeAnimal :: Decoder s Animal
> decodeAnimal = do
>     len <- decodeListLen
>     tag <- decodeTag
>     case (len, tag) of
>       -- these cases are unchanged...
>       (3, 0) -> HoppingAnimal <$> decode <*> decode
>       (3, 1) -> WalkingAnimal <$> decode <*> decode
>       -- this is new...
>       (2, 2) -> SwimmingAnimal <$> decode
>       _      -> fail "invalid Animal encoding"

=== Adding\/removing\/modifying fields

Say then we want to add a new field to our @WalkingAnimal@ constructor,

> data Animal = HoppingAnimal { animalName :: String, hoppingHeight :: Int }
>             | WalkingAnimal { animalName :: String, walkingSpeed :: Int, numberOfFeet :: Int }
>             | SwimmingAnimal { numberOfFins :: Int }
>             deriving (Generic)

We can account for this by representing @WalkingAnimal@ with a new encoding with
a new tag,

> encodeAnimal :: Animal -> Encoding
> -- HoppingAnimal, SwimmingAnimal cases are unchanged...
> encodeAnimal (HoppingAnimal name height) =
>     encodeListLen 3 <> encodeTag 0 <> encode name <> encode height
> encodeAnimal (WalkingAnimal name speed) =
>     encodeListLen 3 <> encodeTag 1 <> encode name <> encode speed
> -- This is new...
> encodeAnimal (WalkingAnimal animalName walkingSpeed numberOfFeet) =
>     encodeListLen 4 <> encodeTag 3 <> encode animalName <> encode walkingSpeed <> encode numberOfFins
>
> decodeAnimal :: Decoder s Animal
> decodeAnimal = do
>     len <- decodeListLen
>     tag <- decodeTag
>     case (len, tag) of
>       -- this cases are unchanged...
>       (3, 0) -> HoppingAnimal <$> decode <*> decode
>       (2, 2) -> SwimmingAnimal <$> decode
>       -- this is new...
>       (3, 1) -> WalkingAnimal <$> decode <*> decode <*> pure 4
>                                                      -- ^ note the default for backwards compat
>       (4, 3) -> WalkingAnimal <$> decode <*> decode
>       _      -> fail "invalid Animal encoding"

We can use this same approach to handle field removal and type changes.

-}

{- $arbitrary_terms

When working with foreign encodings, it can sometimes be useful to capture a
serialised CBOR term verbatim (for instance, so you can later re-serialize it in
some later result). The 'Serialise.Cborg.Term.Term' type provides such a
representation, losslessly capturing a CBOR AST. It can be serialised and
deserialised with its 'Serialise' instance.

-}

{- $examining_encodings

We can also look In addition to serialisation and deserialisation, @cborg@
provides a variety of tools for representing arbitrary CBOR encodings in the
"Serialise.Cborg.FlatTerm" and "Serialise.Cborg.Pretty" modules.

The 'Serialise.Cborg.FlatTerm.FlatTerm' type represents a single CBOR /term/, as
would be found in the ultimate CBOR representation. For instance, we can easily
look at the structure of our @Animal@ encoding above,

>>> toFlatTerm $ encode $ HoppingAnimal "Fred" 42
[TkListLen 3,TkInt 0,TkString "Fred",TkInt 42]
>>> fromFlatTerm (decode @Animal) $ toFlatTerm $ encode (HoppingAnimal "Fred" 42)
Right (HoppingAnimal {animalName = "Fred", hoppingHeight = 42})

This can be useful both for understanding external CBOR formats, as well as
understanding and testing your own hand-rolled encodings.

The package also includes a pretty-printer in "Serialise.Cborg.Pretty", for
visualising the CBOR wire protocol alongside its semantic structure. For instance,

>>> putStrLn $ Serialise.Cborg.Pretty.prettyHexEnc $ encode $ HoppingAnimal "Fred" 42
83  # list(3)
   00  # word(0)
   64 46 72 65 64  # text("Fred")
   18 2a  # int(42)

-}
