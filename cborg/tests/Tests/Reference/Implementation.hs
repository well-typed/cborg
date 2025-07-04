{-# LANGUAGE BangPatterns #-}
-----------------------------------------------------------------------------
-- |
-- Module      : Codec.CBOR
-- Copyright   : 2013 Simon Meier <iridcode@gmail.com>,
--               2013-2014 Duncan Coutts,
-- License     : BSD3-style (see LICENSE.txt)
--
-- Maintainer  : Duncan Coutts
-- Stability   :
-- Portability : portable
--
-- CBOR format support.
--
-----------------------------------------------------------------------------

module Tests.Reference.Implementation (
    serialise,
    deserialise,

    Term(..),
    Token(..),
    canonicaliseTerm,
    isCanonicalTerm,

    UInt(..),
    fromUInt,
    toUInt,
    canonicaliseUInt,
    lengthUInt,

    Simple(..),
    fromSimple,
    toSimple,
    reservedSimple,
    unassignedSimple,
    reservedTag,

    Decoder,
    runDecoder,
    testDecode,

    decodeTerm,
    decodeTokens,
    decodeToken,
    decodeTagged,

    diagnosticNotation,

    Encoder,
    encodeTerm,
    encodeToken,

    prop_InitialByte,
    prop_AdditionalInfo,
    prop_TokenHeader,
    prop_TokenHeader2,
    prop_Token,
    prop_Term,

    -- properties of internal helpers
    prop_integerToFromBytes,
    prop_word16ToFromNet,
    prop_word32ToFromNet,
    prop_word64ToFromNet,
    prop_halfToFromFloat,
    ) where


import qualified Control.Monad.Fail as Fail
import           Data.Bits
import           Data.Word
import qualified Numeric.Half as Half
import           Data.List
import           Numeric
import           GHC.Float (float2Double)
import qualified Data.ByteString      as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import           Data.Monoid ((<>))
import           Control.Monad (ap)

import           Test.QuickCheck.Arbitrary
import           Test.QuickCheck.Gen

import           Tests.Reference.Generators


serialise :: Term -> LBS.ByteString
serialise = LBS.pack . encodeTerm

deserialise :: LBS.ByteString -> Term
deserialise bytes =
    case runDecoder decodeTerm (LBS.unpack bytes) of
      Just (term, []) -> term
      Just _          -> error "ReferenceImpl.deserialise: trailing data"
      Nothing         -> error "ReferenceImpl.deserialise: decoding failed"


------------------------------------------------------------------------

newtype Decoder a = Decoder { runDecoder :: [Word8] -> Maybe (a, [Word8]) }

instance Functor Decoder where
  fmap f a = a >>= return . f

instance Applicative Decoder where
  pure  = return
  (<*>) = ap

instance Monad Decoder where
  return x = Decoder (\ws -> Just (x, ws))
  d >>= f  = Decoder (\ws -> case runDecoder d ws of
                               Nothing       -> Nothing
                               Just (x, ws') -> runDecoder (f x) ws')

instance Fail.MonadFail Decoder where
  fail _   = Decoder (\_ -> Nothing)

getByte :: Decoder Word8
getByte =
  Decoder $ \ws ->
    case ws of
      w:ws' -> Just (w, ws')
      _     -> Nothing

getBytes :: Integral n => n -> Decoder [Word8]
getBytes n =
  Decoder $ \ws ->
    case genericSplitAt n ws of
      (ws', [])   | genericLength ws' == n -> Just (ws', [])
                  | otherwise              -> Nothing
      (ws', ws'')                          -> Just (ws', ws'')

eof :: Decoder Bool
eof = Decoder $ \ws -> Just (null ws, ws)

type Encoder a = a -> [Word8]

-- The initial byte of each data item contains both information about
-- the major type (the high-order 3 bits, described in Section 2.1) and
-- additional information (the low-order 5 bits).

data MajorType = MajorType0 | MajorType1 | MajorType2 | MajorType3
               | MajorType4 | MajorType5 | MajorType6 | MajorType7
  deriving (Show, Eq, Ord, Enum)

instance Arbitrary MajorType where
  arbitrary = elements [MajorType0 .. MajorType7]

encodeInitialByte :: MajorType -> Word -> Word8
encodeInitialByte mt ai
  | ai < 2^(5 :: Int)
  = fromIntegral (fromIntegral (fromEnum mt) `shiftL` 5 .|. ai)

  | otherwise
  = error "encodeInitialByte: invalid additional info value"

decodeInitialByte :: Word8 -> (MajorType, Word)
decodeInitialByte ib = ( toEnum $ fromIntegral $ ib `shiftR` 5
                       , fromIntegral $ ib .&. 0x1f)

prop_InitialByte :: Bool
prop_InitialByte =
    and [ (uncurry encodeInitialByte . decodeInitialByte) w8 == w8
        | w8 <- [minBound..maxBound] ]

-- When the value of the
-- additional information is less than 24, it is directly used as a
-- small unsigned integer.  When it is 24 to 27, the additional bytes
-- for a variable-length integer immediately follow; the values 24 to 27
-- of the additional information specify that its length is a 1-, 2-,
-- 4-, or 8-byte unsigned integer, respectively.  Additional information
-- value 31 is used for indefinite-length items, described in
-- Section 2.2.  Additional information values 28 to 30 are reserved for
-- future expansion.
--
-- In all additional information values, the resulting integer is
-- interpreted depending on the major type.  It may represent the actual
-- data: for example, in integer types, the resulting integer is used
-- for the value itself.  It may instead supply length information: for
-- example, in byte strings it gives the length of the byte string data
-- that follows.

data UInt =
       UIntSmall Word
     | UInt8     Word8
     | UInt16    Word16
     | UInt32    Word32
     | UInt64    Word64
  deriving (Eq, Show)

data AdditionalInformation =
       AiValue    UInt
     | AiIndefLen
     | AiReserved Word
  deriving (Eq, Show)

instance Arbitrary UInt where
  arbitrary =
    sized $ \n ->
      oneof $ take (1 + n `div` 2)
        [ UIntSmall <$> choose (0, 23)
        , UInt8     <$> arbitraryBoundedIntegral
        , UInt16    <$> arbitraryBoundedIntegral
        , UInt32    <$> arbitraryBoundedIntegral
        , UInt64    <$> arbitraryBoundedIntegral
        ]
  shrink (UIntSmall n) = [ UIntSmall n' | n' <- shrink n ]
  shrink (UInt8  n)    = [ UInt8  n'    | n' <- shrink n ]
                      ++ [ UIntSmall (fromIntegral n) | n <= 23 ]
  shrink (UInt16 n)    = [ UInt16 n'    | n' <- shrink n ]
                      ++ [ UInt8 (fromIntegral n)
                         | n <= fromIntegral (maxBound :: Word8) ]
  shrink (UInt32 n)    = [ UInt32 n'    | n' <- shrink n ]
                      ++ [ UInt16 (fromIntegral n)
                         | n <= fromIntegral (maxBound :: Word16) ]
  shrink (UInt64 n)    = [ UInt64 n'    | n' <- shrink n ]
                      ++ [ UInt32 (fromIntegral n)
                         | n <= fromIntegral (maxBound :: Word32) ]

instance Arbitrary AdditionalInformation where
  arbitrary =
    frequency
      [ (7, AiValue <$> arbitrary)
      , (2, pure AiIndefLen)
      , (1, AiReserved <$> choose (28, 30))
      ]

decodeAdditionalInfo :: Word -> Decoder AdditionalInformation
decodeAdditionalInfo = dec
  where
    dec n
      | n < 24 = return (AiValue (UIntSmall n))
    dec 24     = do w <- getByte
                    return (AiValue (UInt8 w))
    dec 25     = do [w1,w0] <- getBytes (2 :: Int)
                    let w = word16FromNet w1 w0
                    return (AiValue (UInt16 w))
    dec 26     = do [w3,w2,w1,w0] <- getBytes (4 :: Int)
                    let w = word32FromNet w3 w2 w1 w0
                    return (AiValue (UInt32 w))
    dec 27     = do [w7,w6,w5,w4,w3,w2,w1,w0] <- getBytes (8 :: Int)
                    let w = word64FromNet w7 w6 w5 w4 w3 w2 w1 w0
                    return (AiValue (UInt64 w))
    dec 31     = return AiIndefLen
    dec n
      | n < 31 = return (AiReserved n)
    dec _      = fail ""

encodeAdditionalInfo :: AdditionalInformation -> (Word, [Word8])
encodeAdditionalInfo = enc
  where
    enc (AiValue (UIntSmall n))
      | n < 24               = (n, [])
      | otherwise            = error "invalid UIntSmall value"
    enc (AiValue (UInt8  w)) = (24, [w])
    enc (AiValue (UInt16 w)) = (25, [w1, w0])
                               where (w1, w0) = word16ToNet w
    enc (AiValue (UInt32 w)) = (26, [w3, w2, w1, w0])
                               where (w3, w2, w1, w0) = word32ToNet w
    enc (AiValue (UInt64 w)) = (27, [w7, w6, w5, w4,
                                     w3, w2, w1, w0])
                               where (w7, w6, w5, w4,
                                      w3, w2, w1, w0) = word64ToNet w
    enc  AiIndefLen          = (31, [])
    enc (AiReserved n)
      | n >= 28 && n < 31    = (n,  [])
      | otherwise            = error "invalid AiReserved value"

prop_AdditionalInfo :: AdditionalInformation -> Bool
prop_AdditionalInfo ai =
    let (w, ws) = encodeAdditionalInfo ai
        Just (ai', _) = runDecoder (decodeAdditionalInfo w) ws
     in ai == ai'


data TokenHeader = TokenHeader MajorType AdditionalInformation
  deriving (Show, Eq)

instance Arbitrary TokenHeader where
  arbitrary = TokenHeader <$> arbitrary <*> arbitrary

decodeTokenHeader :: Decoder TokenHeader
decodeTokenHeader = do
    b <- getByte
    let (mt, ai) = decodeInitialByte b
    ai' <- decodeAdditionalInfo ai
    return (TokenHeader mt ai')

encodeTokenHeader :: Encoder TokenHeader
encodeTokenHeader (TokenHeader mt ai) =
    let (w, ws) = encodeAdditionalInfo ai
     in encodeInitialByte mt w : ws

prop_TokenHeader :: TokenHeader -> Bool
prop_TokenHeader header =
    let ws                = encodeTokenHeader header
        Just (header', _) = runDecoder decodeTokenHeader ws
     in header == header'

prop_TokenHeader2 :: Bool
prop_TokenHeader2 =
    and [ w8 : extraused == encoded
        | w8 <- [minBound..maxBound]
        , let extra = [1..8]
              Just (header, unused) = runDecoder decodeTokenHeader (w8 : extra)
              encoded   = encodeTokenHeader header
              extraused = take (8 - length unused) extra
        ]

data Simple = SimpleSmall Word  --  0 .. 23
            | SimpleLarge Word8 --  0 .. 255, but  0..23 are non-canonical
                                --            and 24..31 are reserved
  deriving (Eq, Show)

fromSimple :: Simple -> Word8
fromSimple (SimpleSmall w) = fromIntegral w
fromSimple (SimpleLarge w) = w

toSimple :: Word8 -> Simple
toSimple w | w <= 23   = SimpleSmall (fromIntegral w)
           | otherwise = SimpleLarge w

reservedSimple :: Word8 -> Bool
reservedSimple w = w >= 24 && w <= 31

unassignedSimple :: Word8 -> Bool
unassignedSimple w = w < 20 || w > 31

instance Arbitrary Simple where
  arbitrary = oneof [ SimpleSmall <$> choose (0, 23)
                    , SimpleLarge <$> choose (0, 31)
                    , SimpleLarge <$> choose (32, 255)
                    ]
  shrink (SimpleSmall n) = [ SimpleSmall n' | n' <- shrink n ]
  shrink (SimpleLarge n) = [ SimpleSmall (fromIntegral n')
                           | n' <- shrink n, n' <= 23 ]
                        ++ [ SimpleLarge n' | n' <- shrink n ]


data Token =
     MT0_UnsignedInt UInt
   | MT1_NegativeInt UInt
   | MT2_ByteString  UInt [Word8]
   | MT2_ByteStringIndef
   | MT3_String      UInt [Word8]
   | MT3_StringIndef
   | MT4_ArrayLen    UInt
   | MT4_ArrayLenIndef
   | MT5_MapLen      UInt
   | MT5_MapLenIndef
   | MT6_Tag     UInt
   | MT7_Simple  Simple
   | MT7_Float16 HalfSpecials
   | MT7_Float32 FloatSpecials
   | MT7_Float64 DoubleSpecials
   | MT7_Break
  deriving (Show, Eq)

instance Arbitrary Token where
  arbitrary =
    oneof
      [ MT0_UnsignedInt <$> arbitrary
      , MT1_NegativeInt <$> arbitrary
      , do ws <- arbitrary
           MT2_ByteString <$> arbitraryLengthUInt ws <*> pure ws
      , pure MT2_ByteStringIndef
      , do cs <- arbitrary
           let ws = encodeUTF8 cs
           MT3_String <$> arbitraryLengthUInt ws <*> pure ws
      , pure MT3_StringIndef
      , MT4_ArrayLen <$> arbitrary
      , pure MT4_ArrayLenIndef
      , MT5_MapLen <$> arbitrary
      , pure MT5_MapLenIndef
      , MT6_Tag     <$> arbitrary
      , MT7_Simple  <$> arbitrary
      , MT7_Float16 <$> arbitrary
      , MT7_Float32 <$> arbitrary
      , MT7_Float64 <$> arbitrary
      , pure MT7_Break
      ]
    where
      arbitraryLengthUInt xs =
        let n = length xs in
        elements $
             [ UIntSmall (fromIntegral n) | n < 24  ]
          ++ [ UInt8     (fromIntegral n) | n < 255 ]
          ++ [ UInt16    (fromIntegral n) | n < 65536 ]
          ++ [ UInt32    (fromIntegral n)
             , UInt64    (fromIntegral n) ]

testDecode :: [Word8] -> Term
testDecode ws =
    case runDecoder decodeTerm ws of
      Just (x, []) -> x
      _            -> error "testDecode: parse error"

decodeTokens :: Decoder [Token]
decodeTokens = do
    done <- eof
    if done
      then return []
      else do tok  <- decodeToken
              toks <- decodeTokens
              return (tok:toks)

decodeToken :: Decoder Token
decodeToken = do
    header <- decodeTokenHeader
    extra  <- getBytes (tokenExtraLen header)
    either fail return (packToken header extra)

tokenExtraLen :: TokenHeader -> Word64
tokenExtraLen (TokenHeader MajorType2 (AiValue n)) = fromUInt n  -- bytestrings
tokenExtraLen (TokenHeader MajorType3 (AiValue n)) = fromUInt n  -- unicode strings
tokenExtraLen _                                    = 0

packToken :: TokenHeader -> [Word8] -> Either String Token
packToken (TokenHeader mt ai) extra = case (mt, ai) of
    -- Major type 0:  an unsigned integer.  The 5-bit additional information
    -- is either the integer itself (for additional information values 0
    -- through 23) or the length of additional data.
    (MajorType0, AiValue n)  -> return (MT0_UnsignedInt n)

    -- Major type 1:  a negative integer.  The encoding follows the rules
    -- for unsigned integers (major type 0), except that the value is
    -- then -1 minus the encoded unsigned integer.
    (MajorType1, AiValue n)  -> return (MT1_NegativeInt n)

    -- Major type 2:  a byte string.  The string's length in bytes is
    -- represented following the rules for positive integers (major type 0).
    (MajorType2, AiValue n)  -> return (MT2_ByteString n extra)
    (MajorType2, AiIndefLen) -> return MT2_ByteStringIndef

    -- Major type 3:  a text string, specifically a string of Unicode
    -- characters that is encoded as UTF-8 [RFC3629].  The format of this
    -- type is identical to that of byte strings (major type 2), that is,
    -- as with major type 2, the length gives the number of bytes.
    (MajorType3, AiValue n)  -> return (MT3_String n extra)
    (MajorType3, AiIndefLen) -> return MT3_StringIndef

    -- Major type 4:  an array of data items. The array's length follows the
    -- rules for byte strings (major type 2), except that the length
    -- denotes the number of data items, not the length in bytes that the
    -- array takes up.
    (MajorType4, AiValue n)  -> return (MT4_ArrayLen n)
    (MajorType4, AiIndefLen) -> return  MT4_ArrayLenIndef

    -- Major type 5:  a map of pairs of data items. A map is comprised of
    -- pairs of data items, each pair consisting of a key that is
    -- immediately followed by a value. The map's length follows the
    -- rules for byte strings (major type 2), except that the length
    -- denotes the number of pairs, not the length in bytes that the map
    -- takes up.
    (MajorType5, AiValue n)  -> return (MT5_MapLen n)
    (MajorType5, AiIndefLen) -> return  MT5_MapLenIndef

    -- Major type 6:  optional semantic tagging of other major types.
    -- The initial bytes of the tag follow the rules for positive integers
    -- (major type 0).
    (MajorType6, AiValue n)  -> return (MT6_Tag n)

    -- Major type 7 is for two types of data: floating-point numbers and
    -- "simple values" that do not need any content.  Each value of the
    -- 5-bit additional information in the initial byte has its own separate
    -- meaning, as defined in Table 1.
    --   | 0..23       | Simple value (value 0..23)                       |
    --   | 24          | Simple value (value 32..255 in following byte)   |
    --   | 25          | IEEE 754 Half-Precision Float (16 bits follow)   |
    --   | 26          | IEEE 754 Single-Precision Float (32 bits follow) |
    --   | 27          | IEEE 754 Double-Precision Float (64 bits follow) |
    --   | 28-30       | (Unassigned)                                     |
    --   | 31          | "break" stop code for indefinite-length items    |
    (MajorType7, AiValue (UIntSmall w)) -> return (MT7_Simple (SimpleSmall w))
    (MajorType7, AiValue (UInt8     w)) -> return (MT7_Simple (SimpleLarge w))
    (MajorType7, AiValue (UInt16    w)) -> return (MT7_Float16 (HalfSpecials (wordToHalf w)))
    (MajorType7, AiValue (UInt32    w)) -> return (MT7_Float32 (FloatSpecials (wordToFloat w)))
    (MajorType7, AiValue (UInt64    w)) -> return (MT7_Float64 (DoubleSpecials (wordToDouble w)))
    (MajorType7, AiIndefLen)            -> return (MT7_Break)
    _                                   -> Left "invalid token header"


encodeToken :: Encoder Token
encodeToken tok =
    let (header, extra) = unpackToken tok
     in encodeTokenHeader header ++ extra


unpackToken :: Token -> (TokenHeader, [Word8])
unpackToken tok = (\(mt, ai, ws) -> (TokenHeader mt ai, ws)) $ case tok of
    (MT0_UnsignedInt n)    -> (MajorType0, AiValue n,  [])
    (MT1_NegativeInt n)    -> (MajorType1, AiValue n,  [])
    (MT2_ByteString  n ws) -> (MajorType2, AiValue n,  ws)
    MT2_ByteStringIndef    -> (MajorType2, AiIndefLen, [])
    (MT3_String      n ws) -> (MajorType3, AiValue n,  ws)
    MT3_StringIndef        -> (MajorType3, AiIndefLen, [])
    (MT4_ArrayLen    n)    -> (MajorType4, AiValue n,  [])
    MT4_ArrayLenIndef      -> (MajorType4, AiIndefLen, [])
    (MT5_MapLen      n)    -> (MajorType5, AiValue n,  [])
    MT5_MapLenIndef        -> (MajorType5, AiIndefLen, [])
    (MT6_Tag     n)        -> (MajorType6, AiValue n,  [])
    (MT7_Simple
        (SimpleSmall n))   -> (MajorType7, AiValue (UIntSmall (fromIntegral n)), [])
    (MT7_Simple
        (SimpleLarge n))   -> (MajorType7, AiValue (UInt8  n), [])
    (MT7_Float16
        (HalfSpecials f))  -> (MajorType7, AiValue (UInt16 (halfToWord f)),   [])
    (MT7_Float32
        (FloatSpecials f)) -> (MajorType7, AiValue (UInt32 (floatToWord f)),  [])
    (MT7_Float64
        (DoubleSpecials f))-> (MajorType7, AiValue (UInt64 (doubleToWord f)), [])
    MT7_Break              -> (MajorType7, AiIndefLen, [])


fromUInt :: UInt -> Word64
fromUInt (UIntSmall w) = fromIntegral w
fromUInt (UInt8     w) = fromIntegral w
fromUInt (UInt16    w) = fromIntegral w
fromUInt (UInt32    w) = fromIntegral w
fromUInt (UInt64    w) = fromIntegral w

toUInt :: Word64 -> UInt
toUInt n
  | n < 24                                 = UIntSmall (fromIntegral n)
  | n <= fromIntegral (maxBound :: Word8)  = UInt8     (fromIntegral n)
  | n <= fromIntegral (maxBound :: Word16) = UInt16    (fromIntegral n)
  | n <= fromIntegral (maxBound :: Word32) = UInt32    (fromIntegral n)
  | otherwise                              = UInt64    n

lengthUInt :: [a] -> UInt
lengthUInt = toUInt . fromIntegral . length

decodeUTF8 :: [Word8] -> Either String [Char]
decodeUTF8 = either (Left . show) (return . T.unpack) . T.decodeUtf8' . BS.pack

encodeUTF8 :: [Char] -> [Word8]
encodeUTF8 = BS.unpack . T.encodeUtf8 . T.pack

reservedTag :: Word64 -> Bool
reservedTag w = w <= 5

prop_Token :: Token -> Bool
prop_Token token =
    let ws = encodeToken token
        Just (token', []) = runDecoder decodeToken ws
     in token == token'

data Term = TUInt   UInt
          | TNInt   UInt
          | TBigInt Integer
          | TBytes    [Word8]
          | TBytess  [[Word8]]
          | TString   [Char]
          | TStrings [[Char]]
          | TArray  [Term]
          | TArrayI [Term]
          | TMap    [(Term, Term)]
          | TMapI   [(Term, Term)]
          | TTagged UInt Term
          | TTrue
          | TFalse
          | TNull
          | TUndef
          | TSimple  Simple
          | TFloat16 HalfSpecials
          | TFloat32 FloatSpecials
          | TFloat64 DoubleSpecials
  deriving (Show, Eq)

instance Arbitrary Term where
  arbitrary =
      frequency
        [ (1, TUInt    <$> arbitrary)
        , (1, TNInt    <$> arbitrary)
        , (1, TBigInt . getLargeInteger <$> arbitrary)
        , (1, TBytes   <$> arbitrary)
        , (1, TBytess  <$> arbitrary)
        , (1, TString  <$> arbitrary)
        , (1, TStrings <$> arbitrary)
        , (2, TArray   <$> listOfSmaller arbitrary)
        , (2, TArrayI  <$> listOfSmaller arbitrary)
        , (2, TMap     <$> listOfSmaller ((,) <$> arbitrary <*> arbitrary))
        , (2, TMapI    <$> listOfSmaller ((,) <$> arbitrary <*> arbitrary))
        , (1, TTagged  <$> arbitraryTag <*> sized (\sz -> resize (max 0 (sz-1)) arbitrary))
        , (1, pure TFalse)
        , (1, pure TTrue)
        , (1, pure TNull)
        , (1, pure TUndef)
        , (1, TSimple  <$> arbitrary `suchThat` (unassignedSimple . fromSimple))
        , (1, TFloat16 <$> arbitrary)
        , (1, TFloat32 <$> arbitrary)
        , (1, TFloat64 <$> arbitrary)
        ]
    where
      listOfSmaller :: Gen a -> Gen [a]
      listOfSmaller gen =
        sized $ \n -> do
          k <- choose (0,n)
          vectorOf k (resize (n `div` (k+1)) gen)

      arbitraryTag = arbitrary `suchThat` (not . reservedTag . fromUInt)

  shrink (TUInt   n)    = [ TUInt    n'   | n' <- shrink n ]
  shrink (TNInt   n)    = [ TNInt    n'   | n' <- shrink n ]
  shrink (TBigInt n)    = [ TBigInt  n'   | n' <- shrink n ]

  shrink (TBytes  ws)   = [ TBytes   ws'  | ws'  <- shrink ws  ]
  shrink (TBytess wss)  = [ TBytess  wss' | wss' <- shrink wss ]
  shrink (TString  ws)  = [ TString  ws'  | ws'  <- shrink ws  ]
  shrink (TStrings wss) = [ TStrings wss' | wss' <- shrink wss ]

  shrink (TArray  xs@[x]) = x : [ TArray  xs' | xs' <- shrink xs ]
  shrink (TArray  xs)     =     [ TArray  xs' | xs' <- shrink xs ]
  shrink (TArrayI xs@[x]) = x : [ TArrayI xs' | xs' <- shrink xs ]
  shrink (TArrayI xs)     =     [ TArrayI xs' | xs' <- shrink xs ]

  shrink (TMap  xys@[(x,y)]) = x : y : [ TMap  xys' | xys' <- shrink xys ]
  shrink (TMap  xys)         =         [ TMap  xys' | xys' <- shrink xys ]
  shrink (TMapI xys@[(x,y)]) = x : y : [ TMapI xys' | xys' <- shrink xys ]
  shrink (TMapI xys)         =         [ TMapI xys' | xys' <- shrink xys ]

  shrink (TTagged w t) = [ TTagged w' t' | (w', t') <- shrink (w, t)
                                         , not (reservedTag (fromUInt w')) ]

  shrink TFalse = []
  shrink TTrue  = []
  shrink TNull  = []
  shrink TUndef = []

  shrink (TSimple  n) = [ TSimple  n' | n' <- shrink n
                                      , unassignedSimple (fromSimple n') ]
  shrink (TFloat16 f) = [ TFloat16 f' | f' <- shrink f ]
  shrink (TFloat32 f) = [ TFloat32 f' | f' <- shrink f ]
  shrink (TFloat64 f) = [ TFloat64 f' | f' <- shrink f ]


decodeTerm :: Decoder Term
decodeTerm = decodeToken >>= decodeTermFrom

decodeTermFrom :: Token -> Decoder Term
decodeTermFrom tk =
    case tk of
      MT0_UnsignedInt n  -> return (TUInt n)
      MT1_NegativeInt n  -> return (TNInt n)

      MT2_ByteString _ bs -> return (TBytes bs)
      MT2_ByteStringIndef -> decodeBytess []

      MT3_String _ ws    -> either fail (return . TString) (decodeUTF8 ws)
      MT3_StringIndef    -> decodeStrings []

      MT4_ArrayLen len   -> decodeArrayN (fromUInt len) []
      MT4_ArrayLenIndef  -> decodeArray []

      MT5_MapLen  len    -> decodeMapN (fromUInt len) []
      MT5_MapLenIndef    -> decodeMap  []

      MT6_Tag     tag    -> decodeTagged tag

      MT7_Simple  n
        | n' == 20       -> return TFalse
        | n' == 21       -> return TTrue
        | n' == 22       -> return TNull
        | n' == 23       -> return TUndef
        | otherwise      -> return (TSimple n)
        where
          n' = fromSimple n
      MT7_Float16 f      -> return (TFloat16 f)
      MT7_Float32 f      -> return (TFloat32 f)
      MT7_Float64 f      -> return (TFloat64 f)
      MT7_Break          -> fail "unexpected"


decodeBytess :: [[Word8]] -> Decoder Term
decodeBytess acc = do
    tk <- decodeToken
    case tk of
      MT7_Break            -> return $! TBytess (reverse acc)
      MT2_ByteString _ bs  -> decodeBytess (bs : acc)
      _                    -> fail "unexpected"

decodeStrings :: [String] -> Decoder Term
decodeStrings acc = do
    tk <- decodeToken
    case tk of
      MT7_Break        -> return $! TStrings (reverse acc)
      MT3_String _ ws  -> do cs <- either fail return (decodeUTF8 ws)
                             decodeStrings (cs : acc)
      _                -> fail "unexpected"

decodeArrayN :: Word64 -> [Term] -> Decoder Term
decodeArrayN n acc =
    case n of
      0 -> return $! TArray (reverse acc)
      _ -> do t <- decodeTerm
              decodeArrayN (n-1) (t : acc)

decodeArray :: [Term] -> Decoder Term
decodeArray acc = do
    tk <- decodeToken
    case tk of
      MT7_Break -> return $! TArrayI (reverse acc)
      _         -> do
        tm <- decodeTermFrom tk
        decodeArray (tm : acc)

decodeMapN :: Word64 -> [(Term, Term)] -> Decoder Term
decodeMapN n acc =
    case n of
      0 -> return $! TMap (reverse acc)
      _ -> do
        tm   <- decodeTerm
        tm'  <- decodeTerm
        decodeMapN (n-1) ((tm, tm') : acc)

decodeMap :: [(Term, Term)] -> Decoder Term
decodeMap acc = do
    tk <- decodeToken
    case tk of
      MT7_Break -> return $! TMapI (reverse acc)
      _         -> do
        tm  <- decodeTermFrom tk
        tm' <- decodeTerm
        decodeMap ((tm, tm') : acc)

decodeTagged :: UInt -> Decoder Term
decodeTagged tag | fromUInt tag == 2 = do
    MT2_ByteString _ bs <- decodeToken
    let !n = integerFromBytes bs
    return (TBigInt n)
decodeTagged tag | fromUInt tag == 3 = do
    MT2_ByteString _ bs <- decodeToken
    let !n = integerFromBytes bs
    return (TBigInt (-1 - n))
decodeTagged tag = do
    tm <- decodeTerm
    return (TTagged tag tm)

integerFromBytes :: [Word8] -> Integer
integerFromBytes []       = 0
integerFromBytes (w0:ws0) = go (fromIntegral w0) ws0
  where
    go !acc []     = acc
    go !acc (w:ws) = go (acc `shiftL` 8 + fromIntegral w) ws

integerToBytes :: Integer -> [Word8]
integerToBytes n0
  | n0 == 0   = [0]
  | n0 < 0    = reverse (go (-n0))
  | otherwise = reverse (go n0)
  where
    go n | n == 0    = []
         | otherwise = narrow n : go (n `shiftR` 8)

    narrow :: Integer -> Word8
    narrow = fromIntegral

prop_integerToFromBytes :: LargeInteger -> Bool
prop_integerToFromBytes (LargeInteger n)
  | n >= 0 =
    let ws = integerToBytes n
        n' = integerFromBytes ws
     in n == n'
  | otherwise =
    let ws = integerToBytes n
        n' = integerFromBytes ws
     in n == -n'

-------------------------------------------------------------------------------

encodeTerm :: Encoder Term
encodeTerm (TUInt n)       = encodeToken (MT0_UnsignedInt n)
encodeTerm (TNInt n)       = encodeToken (MT1_NegativeInt n)
encodeTerm (TBigInt n)
               | n >= 0    = encodeToken (MT6_Tag (UIntSmall 2))
                          <> let ws  = integerToBytes n
                                 len = lengthUInt ws in
                             encodeToken (MT2_ByteString len ws)
               | otherwise = encodeToken (MT6_Tag (UIntSmall 3))
                          <> let ws  = integerToBytes (-1 - n)
                                 len = lengthUInt ws in
                             encodeToken (MT2_ByteString len ws)
encodeTerm (TBytes ws)     = let len = lengthUInt ws in
                             encodeToken (MT2_ByteString len ws)
encodeTerm (TBytess wss)   = encodeToken MT2_ByteStringIndef
                          <> mconcat [ encodeToken (MT2_ByteString len ws)
                                     | ws <- wss
                                     , let len = lengthUInt ws ]
                          <> encodeToken MT7_Break
encodeTerm (TString  cs)   = let ws  = encodeUTF8 cs
                                 len = lengthUInt ws in
                             encodeToken (MT3_String len ws)
encodeTerm (TStrings css)  = encodeToken MT3_StringIndef
                          <> mconcat [ encodeToken (MT3_String len ws)
                                     | cs <- css
                                     , let ws  = encodeUTF8 cs
                                           len = lengthUInt ws ]
                          <> encodeToken MT7_Break
encodeTerm (TArray  ts)    = let len = lengthUInt ts in
                             encodeToken (MT4_ArrayLen len)
                          <> mconcat (map encodeTerm ts)
encodeTerm (TArrayI ts)    = encodeToken MT4_ArrayLenIndef
                          <> mconcat (map encodeTerm ts)
                          <> encodeToken MT7_Break
encodeTerm (TMap    kvs)   = let len = lengthUInt kvs in
                             encodeToken (MT5_MapLen len)
                          <> mconcat [ encodeTerm k <> encodeTerm v
                                     | (k,v) <- kvs ]
encodeTerm (TMapI   kvs)   = encodeToken MT5_MapLenIndef
                          <> mconcat [ encodeTerm k <> encodeTerm v
                                     | (k,v) <- kvs ]
                          <> encodeToken MT7_Break
encodeTerm (TTagged tag t) = encodeToken (MT6_Tag tag)
                          <> encodeTerm t
encodeTerm  TFalse         = encodeToken (MT7_Simple (SimpleSmall 20))
encodeTerm  TTrue          = encodeToken (MT7_Simple (SimpleSmall 21))
encodeTerm  TNull          = encodeToken (MT7_Simple (SimpleSmall 22))
encodeTerm  TUndef         = encodeToken (MT7_Simple (SimpleSmall 23))
encodeTerm (TSimple  w)    = encodeToken (MT7_Simple w)
encodeTerm (TFloat16 f)    = encodeToken (MT7_Float16 f)
encodeTerm (TFloat32 f)    = encodeToken (MT7_Float32 f)
encodeTerm (TFloat64 f)    = encodeToken (MT7_Float64 f)


-------------------------------------------------------------------------------

prop_Term :: Term -> Bool
prop_Term term =
    let ws = encodeTerm term
        Just (term', []) = runDecoder decodeTerm ws
     in term == term'

isCanonicalTerm :: Term -> Bool
isCanonicalTerm t = canonicaliseTerm t == t

canonicaliseTerm :: Term -> Term
canonicaliseTerm (TUInt n) = TUInt (canonicaliseUInt n)
canonicaliseTerm (TNInt n) = TNInt (canonicaliseUInt n)
canonicaliseTerm (TBigInt n)
  | n >= 0 && n <= fromIntegral (maxBound :: Word64)
                           = TUInt (toUInt (fromIntegral n))
  | n <  0 && n >= -1 - fromIntegral (maxBound :: Word64)
                           = TNInt (toUInt (fromIntegral (-1 - n)))
  | otherwise              = TBigInt n
canonicaliseTerm (TSimple  n)   = TSimple  (canonicaliseSimple n)
canonicaliseTerm (TFloat16 f)   = canonicaliseFloat TFloat16 f
canonicaliseTerm (TFloat32 f)   = canonicaliseFloat TFloat32 f
canonicaliseTerm (TFloat64 f)   = canonicaliseFloat TFloat64 f
canonicaliseTerm (TBytess  wss) = TBytess  (filter (not . null) wss)
canonicaliseTerm (TStrings css) = TStrings (filter (not . null) css)
canonicaliseTerm (TArray  ts) = TArray  (map canonicaliseTerm ts)
canonicaliseTerm (TArrayI ts) = TArrayI (map canonicaliseTerm ts)
canonicaliseTerm (TMap    ts) = TMap    (map canonicaliseTermPair ts)
canonicaliseTerm (TMapI   ts) = TMapI   (map canonicaliseTermPair ts)
canonicaliseTerm (TTagged tag t) = TTagged (canonicaliseUInt tag) (canonicaliseTerm t)
canonicaliseTerm t = t

canonicaliseUInt :: UInt -> UInt
canonicaliseUInt = toUInt . fromUInt

canonicaliseSimple :: Simple -> Simple
canonicaliseSimple = toSimple . fromSimple

canonicaliseFloat :: RealFloat t => (t -> Term) -> t -> Term
canonicaliseFloat tfloatNN f
  | isNaN f   = TFloat16 canonicalNaN
  | otherwise = tfloatNN f

canonicaliseTermPair :: (Term, Term) -> (Term, Term)
canonicaliseTermPair (x,y) = (canonicaliseTerm x, canonicaliseTerm y)


-------------------------------------------------------------------------------

diagnosticNotation :: Term -> String
diagnosticNotation = \t -> showsTerm t ""
  where
    showsTerm tm = case tm of
      TUInt    n     -> shows (fromUInt n)
      TNInt    n     -> shows (-1 - fromIntegral (fromUInt n) :: Integer)
      TBigInt  n     -> shows n
      TBytes   bs    -> showsBytes bs
      TBytess  bss   -> surround '(' ')' (underscoreSpace . commaSep showsBytes bss)
      TString  cs    -> shows cs
      TStrings css   -> surround '(' ')' (underscoreSpace . commaSep shows css)
      TArray   ts    -> surround '[' ']' (commaSep showsTerm ts)
      TArrayI  ts    -> surround '[' ']' (underscoreSpace . commaSep showsTerm ts)
      TMap     ts    -> surround '{' '}' (commaSep showsMapElem ts)
      TMapI    ts    -> surround '{' '}' (underscoreSpace . commaSep showsMapElem ts)
      TTagged  tag t -> shows (fromUInt tag) . surround '(' ')' (showsTerm t)
      TTrue          -> showString "true"
      TFalse         -> showString "false"
      TNull          -> showString "null"
      TUndef         -> showString "undefined"
      TSimple  n     -> showString "simple" . surround '(' ')' (shows (fromSimple n))
      -- convert to float to work around https://github.com/ekmett/half/issues/2
      TFloat16 f     -> showFloatCompat (float2Double (Half.fromHalf (getHalfSpecials f)))
      TFloat32 f     -> showFloatCompat (float2Double (getFloatSpecials f))
      TFloat64 f     -> showFloatCompat (getDoubleSpecials f)

    surround a b x = showChar a . x . showChar b

    commaSpace = showChar ',' . showChar ' '
    underscoreSpace = showChar '_' . showChar ' '

    showsMapElem (k,v) = showsTerm k . showChar ':' . showChar ' ' . showsTerm v

    catShows :: (a -> ShowS) -> [a] -> ShowS
    catShows f xs = \s -> foldr (\x r -> f x . r) id xs s

    sepShows :: ShowS -> (a -> ShowS) -> [a] -> ShowS
    sepShows sep f xs = foldr (.) id (intersperse sep (map f xs))

    commaSep = sepShows commaSpace

    showsBytes :: [Word8] -> ShowS
    showsBytes bs = showChar 'h' . showChar '\''
                                 . catShows showFHex bs
                                 . showChar '\''

    showFHex n | n < 16    = showChar '0' . showHex n
               | otherwise = showHex n

    showFloatCompat n
      | exponent' >= -5 && exponent' <= 15 = showFFloat Nothing n
      | otherwise                          = showEFloat Nothing n
      where exponent' = snd (floatToDigits 10 n)


word16FromNet :: Word8 -> Word8 -> Word16
word16FromNet w1 w0 =
      fromIntegral w1 `shiftL` (8*1)
  .|. fromIntegral w0 `shiftL` (8*0)

word16ToNet :: Word16 -> (Word8, Word8)
word16ToNet w =
    ( fromIntegral ((w `shiftR` (8*1)) .&. 0xff)
    , fromIntegral ((w `shiftR` (8*0)) .&. 0xff)
    )

word32FromNet :: Word8 -> Word8 -> Word8 -> Word8 -> Word32
word32FromNet w3 w2 w1 w0 =
      fromIntegral w3 `shiftL` (8*3)
  .|. fromIntegral w2 `shiftL` (8*2)
  .|. fromIntegral w1 `shiftL` (8*1)
  .|. fromIntegral w0 `shiftL` (8*0)

word32ToNet :: Word32 -> (Word8, Word8, Word8, Word8)
word32ToNet w =
    ( fromIntegral ((w `shiftR` (8*3)) .&. 0xff)
    , fromIntegral ((w `shiftR` (8*2)) .&. 0xff)
    , fromIntegral ((w `shiftR` (8*1)) .&. 0xff)
    , fromIntegral ((w `shiftR` (8*0)) .&. 0xff)
    )

word64FromNet :: Word8 -> Word8 -> Word8 -> Word8 ->
                 Word8 -> Word8 -> Word8 -> Word8 -> Word64
word64FromNet w7 w6 w5 w4 w3 w2 w1 w0 =
      fromIntegral w7 `shiftL` (8*7)
  .|. fromIntegral w6 `shiftL` (8*6)
  .|. fromIntegral w5 `shiftL` (8*5)
  .|. fromIntegral w4 `shiftL` (8*4)
  .|. fromIntegral w3 `shiftL` (8*3)
  .|. fromIntegral w2 `shiftL` (8*2)
  .|. fromIntegral w1 `shiftL` (8*1)
  .|. fromIntegral w0 `shiftL` (8*0)

word64ToNet :: Word64 -> (Word8, Word8, Word8, Word8,
                          Word8, Word8, Word8, Word8)
word64ToNet w =
    ( fromIntegral ((w `shiftR` (8*7)) .&. 0xff)
    , fromIntegral ((w `shiftR` (8*6)) .&. 0xff)
    , fromIntegral ((w `shiftR` (8*5)) .&. 0xff)
    , fromIntegral ((w `shiftR` (8*4)) .&. 0xff)
    , fromIntegral ((w `shiftR` (8*3)) .&. 0xff)
    , fromIntegral ((w `shiftR` (8*2)) .&. 0xff)
    , fromIntegral ((w `shiftR` (8*1)) .&. 0xff)
    , fromIntegral ((w `shiftR` (8*0)) .&. 0xff)
    )

prop_word16ToFromNet :: Word8 -> Word8 -> Bool
prop_word16ToFromNet w1 w0 =
    word16ToNet (word16FromNet w1 w0) == (w1, w0)

prop_word32ToFromNet :: Word8 -> Word8 -> Word8 -> Word8 -> Bool
prop_word32ToFromNet w3 w2 w1 w0 =
    word32ToNet (word32FromNet w3 w2 w1 w0) == (w3, w2, w1, w0)

prop_word64ToFromNet :: Word8 -> Word8 -> Word8 -> Word8 ->
                        Word8 -> Word8 -> Word8 -> Word8 -> Bool
prop_word64ToFromNet w7 w6 w5 w4 w3 w2 w1 w0 =
    word64ToNet (word64FromNet w7 w6 w5 w4 w3 w2 w1 w0)
 == (w7, w6, w5, w4, w3, w2, w1, w0)

-- Note: some NaNs do not roundtrip https://github.com/ekmett/half/issues/3
-- but all the others had better
prop_halfToFromFloat :: Bool
prop_halfToFromFloat =
    all (\w -> roundTrip w || isNaN (Half.Half w)) [minBound..maxBound]
  where
    roundTrip w =
      w == (Half.getHalf . Half.toHalf . Half.fromHalf . Half.Half $ w)

