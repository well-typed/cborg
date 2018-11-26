{-# LANGUAGE CPP                  #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE DefaultSignatures    #-}
{-# LANGUAGE AllowAmbiguousTypes  #-}
{-# LANGUAGE TypeApplications     #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Tests.Properties (
    testTree
  ) where

import qualified Data.ByteString.Lazy as LBS

import           Codec.CBOR.Term
import           Codec.CBOR.Read
import           Codec.CBOR.Write
import           Codec.CBOR.Decoding
import           Codec.CBOR.Encoding

import           Test.Tasty (TestTree, testGroup, localOption)
import           Test.Tasty.QuickCheck (testProperty, QuickCheckMaxSize(..))
import           Test.QuickCheck

import qualified Tests.Reference.Implementation as Ref
import qualified Tests.Term as Term (serialise, deserialise)
import           Tests.Term
                   ( fromRefTerm, toRefTerm, eqTerm
                   , prop_toFromRefTerm, prop_fromToRefTerm
                   , canonicaliseTerm )
import           Tests.Util

#if !MIN_VERSION_base(4,8,0)
import           Control.Applicative
#endif


-- | The CBOR implementation and its reference implementation satisfy all the
-- properties implied in the following commuting diagram.
--
-- The properties in this module exercise various paths throguh this diagram,
-- and do so for various different types.
--
-- >        canon          id
-- >  Ref──────────▶Ref───────────▶Ref
-- >    │            ▲ ╲  (ref)  ╱ │
-- >    │            │  ╲enc dec╱  │
-- >    │            │   ╲     ╱   │
-- >    │from      to│    ▶Enc▶    │from
-- >    │            │   ╱     ╲   │
-- >    │            │  ╱enc dec╲  │
-- >    ▼            │ ╱  (imp)  ╲ ▼
-- >  Imp──────────▶Imp───────────▶Imp
-- >         id           canon
--
-- Key
--
--  * Imp:  Implementation token type
--  * Ref:  Reference token type
--  * Enc:  Encoding (ie bytes)
--  * canon: canonicaliseRef or canonicaliseImp
--  * enc:   encodeRef       or encodeImp
--  * dec:   decodeRef       or decodeImp
--
-- We capture these types and arrows with a type class and an associated type.
--
class (Eq t, Show t) => Token t where
  type Imp t :: *

  encodeImp :: Imp t -> Encoding
  encodeRef :: Ref.Encoder t

  decodeImp :: forall s. Decoder s (Imp t)
  decodeRef :: Ref.Decoder t

  canonicaliseImp :: Imp t -> Imp t
  canonicaliseRef ::     t ->     t

  eqImp   :: Imp t -> Imp t -> Bool

  toRef   :: Imp t -> t
  fromRef :: t -> Imp t

  -- defaults
  canonicaliseImp = id
  canonicaliseRef = toRef . fromRef

  default eqImp :: Eq (Imp t) => Imp t -> Imp t -> Bool
  eqImp = (==)


-- A few derived utils

serialiseRef :: forall t. Token t => t -> LBS.ByteString
serialiseRef = LBS.pack . encodeRef

serialiseImp :: forall t. Token t => Imp t -> LBS.ByteString
serialiseImp = toLazyByteString . encodeImp @t

deserialiseRef :: forall t. Token t => LBS.ByteString -> t
deserialiseRef bytes =
  case Ref.runDecoder (decodeRef @t) (LBS.unpack bytes) of
    Just (x, trailing)
      | null trailing -> x
      | otherwise     -> error "deserialiseRef: trailing bytes"
    Nothing           -> error "deserialiseRef: decode failure"

deserialiseImp :: forall t. Token t => LBS.ByteString -> Imp t
deserialiseImp bytes =
    case deserialiseFromBytes (decodeImp @ t) bytes of
      Right (trailing, x)
        | LBS.null trailing -> x
        | otherwise         -> error "deserialiseImp: trailing data"
      Left _failure         -> error "deserialiseImp: decode failure"


--------------------------------------------------------------------------------
-- Properties
--


-- | The property corresponding to the following part of the commuting diagram.
--
-- >        canon
-- >  Ref──────────▶Ref . . . . . ▷.
-- >    │            ▲ .         . .
-- >    │            │  .       .  .
-- >    │            │   .     .   .
-- >    │from      to│    ▷   ▷    .
-- >    │            │   .     .   .
-- >    │            │  .       .  .
-- >    ▼            │ .         . ▽
-- >  Imp──────────▶Imp . . . . . ▷.
-- >         id
--
-- > to . id . from = canon_ref
--
prop_fromRefToRef :: Token t => t -> Bool
prop_fromRefToRef x =

    (toRef . fromRef) x == canonicaliseRef x


-- | The property corresponding to the following part of the commuting diagram.
--
-- >                       id
-- >    . . . . . .▷Ref───────────▶Ref
-- >    .            ▲ .         . │
-- >    .            │  .       .  │
-- >    .            │   .     .   │
-- >    .          to│    ▷   ▷    │from
-- >    .            │   .     .   │
-- >    .            │  .       .  │
-- >    ▽            │ .         . ▼
-- >    . . . . . .▶Imp───────────▶Imp
-- >                      canon
--
-- > from . id . to = canon_imp
--
prop_toRefFromRef :: forall t. Token t => Imp t -> Bool
prop_toRefFromRef x =

    (fromRef . toRef @t) x  `eq`  canonicaliseImp @t x

  where
    eq = eqImp @t


-- | The property corresponding to the following part of the commuting diagram.
--
-- This is a round trip property, with the reference implementation of the
-- encoder and decoder.
--
-- >                       id
-- >    . . . . . .▷Ref───────────▶Ref
-- >    .            △ ╲         ╱ .
-- >    .            .  ╲enc dec╱  .
-- >    .            .   ╲     ╱   .
-- >    .            .    ▶Enc▶    .
-- >    .            .   .     .   .
-- >    .            .  .       .  .
-- >    ▽            . .         . ▽
-- >    . . . . . . ▷.. . . . . . ▷.
--
-- > dec_ref . enc_ref = id
--
prop_encodeRefdecodeRef :: forall t. Token t => t -> Bool
prop_encodeRefdecodeRef x =

    (deserialiseRef . serialiseRef) x  ==  x


-- | The property corresponding to the following part of the commuting diagram.
--
-- This is a round trip property, with the production implementation of the
-- encoder and decoder.
--
-- >    . . . . . . ▷. . . . . . .▷.
-- >    .            △ .         . .
-- >    .            .  .       .  .
-- >    .            .   .     .   .
-- >    .            .    ▶Enc▶    .
-- >    .            .   ╱     ╲   .
-- >    .            .  ╱enc dec╲  .
-- >    ▽            . ╱         ╲ ▽
-- >    . . . . . .▷Imp───────────▶Imp
-- >                      canon
--
-- > dec_imp . enc_imp = canon_imp
--
prop_encodeImpdecodeImp :: forall t. Token t => Imp t -> Bool
prop_encodeImpdecodeImp x =

    (deserialiseImp @t . serialiseImp @t) x  `eq`  canonicaliseImp @t x

  where
    eq = eqImp @t


-- | This is the same property as 'prop_encodeImpdecodeImp' but the encoded
-- data is split into two chunks provided as input into the decoder. All
-- possible 2-chunk splits are tried. This checks that the decoder gives the
-- same result irrespective of the chunk boundaries.
--
prop_encodeImpdecodeImp_splits2 :: forall t. Token t => Imp t -> Bool
prop_encodeImpdecodeImp_splits2 x =
    and [ deserialiseImp @t enc'  `eq`  x'
        | let enc = serialiseImp    @t x
              x'  = canonicaliseImp @t x
        , enc' <- splits2 enc ]
  where
    eq = eqImp @t


-- | This is the same idea as 'prop_encodeImpdecodeImp_splits2' but with all
-- possible 3-chunk splits of the input data. This test is of course more
-- expensive and so the size of the input must be limited.
--
prop_encodeImpdecodeImp_splits3 :: forall t. Token t => Imp t -> Bool
prop_encodeImpdecodeImp_splits3 x =
    and [ deserialiseImp @t enc'  `eq`  x'
        | let enc = serialiseImp    @t x
              x'  = canonicaliseImp @t x
        , enc' <- splits3 enc ]
  where
    eq = eqImp @t


-- | The property corresponding to the following part of the commuting diagram.
--
-- This checks that the reference and real implementation produce the same
-- encoded bytes. It starts from a value in the reference implementation.
--
-- >        canon
-- >  Ref──────────▶Ref . . . . . ▷.
-- >    │            △ ╲         . .
-- >    │            .  ╲enc    .  .
-- >    │            .   ╲     .   .
-- >    │from        .    ▶Enc▷    .
-- >    │            .   ╱     .   .
-- >    │            .  ╱enc    .  .
-- >    ▼            . ╱         . ▽
-- >  Imp──────────▶Imp . . . . . ▷.
-- >         id
--
-- > enc_imp . id . from = enc_ref . canon_ref
--
prop_encodeRefencodeImp1 :: forall t. Token t => t -> Bool
prop_encodeRefencodeImp1 x =

    (serialiseImp @t . fromRef) x  ==  (serialiseRef . canonicaliseRef) x


-- | The property corresponding to the following part of the commuting diagram.
--
-- This checks that the reference and real implementation produce the same
-- encoded bytes.  It starts from a value in the real implementation.
--
-- >    . . . . . .▷Ref . . . . . ▷.
-- >    .            ▲ ╲         . .
-- >    .            │  ╲enc    .  .
-- >    .            │   ╲     .   .
-- >    .          to│    ▶Enc▷    .
-- >    .            │   ╱     .   .
-- >    .            │  ╱enc    .  .
-- >    ▽            │ ╱         . ▽
-- >    . . . . . .▷Imp . . . . . ▷.
--
-- > enc_ref . id . to = enc_imp
--
prop_encodeRefencodeImp2 :: forall t. Token t => Imp t -> Bool
prop_encodeRefencodeImp2 x =

    (serialiseRef . toRef @t) x == serialiseImp @t x


-- | The property corresponding to the following part of the commuting diagram.
--
-- This checks that starting from the same encoding, the reference and real
-- implementation deserialise to equivalent values.
--
-- >    . . . . . .▷Ref . . . . . ▶Ref
-- >    .            △ ╲         ╱ │
-- >    .            .  ╲enc dec╱  │
-- >    .            .   ╲     ╱   │
-- >    .            .    ▶Enc▶    │from
-- >    .            .   .     ╲   │
-- >    .            .  .    dec╲  │
-- >    ▽            . .         ╲ ▼
-- >    . . . . . . ▷.. . . . . . ▶Imp
--
-- > dec_imp . enc_ref = from . dec_ref . enc_ref
--
prop_decodeRefdecodeImp :: forall t. Token t => t -> Bool
prop_decodeRefdecodeImp x =

    deserialiseImp @t enc  `eq`  (fromRef . deserialiseRef @t) enc

  where
    enc = serialiseRef x
    eq  = eqImp @t


--------------------------------------------------------------------------------
-- Properties
--

prop_encodeDecodeTermRoundtrip :: Term -> Bool
prop_encodeDecodeTermRoundtrip term =
             (Term.deserialise . Term.serialise) term
    `eqTerm` canonicaliseTerm term

prop_encodeDecodeTermRoundtrip_splits2 :: Term -> Bool
prop_encodeDecodeTermRoundtrip_splits2 term =
    and [          Term.deserialise thedata'
          `eqTerm` canonicaliseTerm term
        | let thedata = Term.serialise term
        , thedata' <- splits2 thedata ]

prop_encodeDecodeTermRoundtrip_splits3 :: Term -> Bool
prop_encodeDecodeTermRoundtrip_splits3 term =
    and [          Term.deserialise thedata'
          `eqTerm` canonicaliseTerm term
        | let thedata = Term.serialise term
        , thedata' <- splits3 thedata ]

prop_encodeTermMatchesRefImpl :: Ref.Term -> Bool
prop_encodeTermMatchesRefImpl term =
    let encoded  = Term.serialise (fromRefTerm term)
        encoded' = Ref.serialise (Ref.canonicaliseTerm term)
     in encoded == encoded'

prop_encodeTermMatchesRefImpl2 :: Term -> Bool
prop_encodeTermMatchesRefImpl2 term =
    let encoded  = Term.serialise term
        encoded' = Ref.serialise (toRefTerm term)
     in encoded == encoded'

prop_decodeTermMatchesRefImpl :: Ref.Term -> Bool
prop_decodeTermMatchesRefImpl term0 =
    let encoded = Ref.serialise term0
        term    = Ref.deserialise encoded
        term'   = Term.deserialise encoded
     in term' `eqTerm` fromRefTerm term

prop_decodeTermNonCanonical :: Ref.Term -> Property
prop_decodeTermNonCanonical term0 =
    let encoded = Ref.serialise term0
        term    = Ref.deserialise encoded
        term'   = Term.deserialise encoded
        encoded'= Term.serialise term'
        isCanonical = encoded == encoded'
     in not isCanonical ==>
        -- This property holds without this pre-condition, as demonstrated by
        -- prop_decodeTermMatchesRefImpl, but using it ensures we get good
        -- coverage of the non-canonical cases
        term' `eqTerm` fromRefTerm term


--------------------------------------------------------------------------------
-- Token class instances for Term type
--

instance Token Ref.Term where
    type Imp Ref.Term = Term

    eqImp = eqTerm

    fromRef = fromRefTerm
    toRef   = toRefTerm

    canonicaliseImp = canonicaliseTerm
    canonicaliseRef = Ref.canonicaliseTerm

    encodeImp = encodeTerm
    decodeImp = decodeTerm

    encodeRef = Ref.encodeTerm
    decodeRef = Ref.decodeTerm


--------------------------------------------------------------------------------
-- TestTree API

testTree :: TestTree
testTree =
  testGroup "properties"
  [ testGroup "to . id . from = canon_ref"
    [ testProperty "Term"    (prop_fromRefToRef @ Ref.Term)
    ]

  , testGroup "from . id . to = canon_imp"
    [ testProperty "Term"    (prop_toRefFromRef @ Ref.Term)
    ]

  , testGroup "dec_ref . enc_ref = id"
    [ testProperty "Term"    (prop_encodeRefdecodeRef @ Ref.Term)
    ]

  , testGroup "dec_imp . enc_imp = canon_imp"
    [ testProperty "Term"    (prop_encodeImpdecodeImp @ Ref.Term)
    ]

  , testGroup "dec_imp . enc_imp = canon_imp (all 2-splits)"
    [ localOption (QuickCheckMaxSize 100) $
      testProperty "Term"    (prop_encodeImpdecodeImp_splits2 @ Ref.Term)
    ]

  , testGroup "dec_imp . enc_imp = canon_imp (all 3-splits)"
    [ localOption (QuickCheckMaxSize 25) $
      testProperty "Term"    (prop_encodeImpdecodeImp_splits3 @ Ref.Term)
    ]

  , testGroup "enc_imp . from = enc_ref . canon_ref"
    [ testProperty "Term"    (prop_encodeRefencodeImp1 @ Ref.Term)
    ]

  , testGroup "enc_ref . to = enc_imp"
    [ testProperty "Term"    (prop_encodeRefencodeImp2 @ Ref.Term)
    ]

  , testGroup "dec_imp . enc_ref = from . dec_ref . enc_ref"
    [ testProperty "Term"    (prop_decodeRefdecodeImp @ Ref.Term)
    ]

  , testGroup "Terms"
    [ testProperty "from/to reference terms"        prop_fromToRefTerm
    , testProperty "to/from reference terms"        prop_toFromRefTerm
    , testProperty "rountrip de/encoding terms"     prop_encodeDecodeTermRoundtrip
      -- TODO FIXME: need to fix the generation of terms to give
      -- better size distribution some get far too big for the
      -- splits properties.
    , localOption (QuickCheckMaxSize 30) $
      testProperty "decoding with all 2-chunks"     prop_encodeDecodeTermRoundtrip_splits2
    , localOption (QuickCheckMaxSize 20) $
      testProperty "decoding with all 3-chunks"     prop_encodeDecodeTermRoundtrip_splits3
    , testProperty "encode term matches ref impl 1" prop_encodeTermMatchesRefImpl
    , testProperty "encode term matches ref impl 2" prop_encodeTermMatchesRefImpl2
    , testProperty "decoding term matches ref impl" prop_decodeTermMatchesRefImpl
    , testProperty "non-canonical encoding"         prop_decodeTermNonCanonical
    ]
  ]
