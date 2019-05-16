{-# Language FlexibleInstances #-}
module Tests.FlatTerm
  ( testTree -- :: TestTree
  ) where

import           Control.Applicative
import           Control.Monad
import           Codec.CBOR.FlatTerm ( TermToken(..), fromFlatTerm, validFlatTerm, toFlatTerm )
import           Codec.CBOR.Write    ( toLazyByteString)
import           Codec.CBOR.Term     ( decodeTerm, encodeTerm )
import           Codec.CBOR.Encoding ( Encoding(..)
                                     , encodeInt  
                                     , encodeInteger
                                     , encodeBytes
                                     , encodeBytesIndef
                                     , encodeString
                                     , encodeStringIndef
                                     , encodeListLen
                                     , encodeListLenIndef
                                     , encodeMapLen
                                     , encodeMapLenIndef
                                     , encodeBreak 
                                     , encodeTag64
                                     , encodeBool
                                     , encodeNull
                                     , encodeSimple
                                     , encodeFloat16
                                     , encodeFloat
                                     , encodeDouble )
import qualified Tests.Reference.Implementation as Imp
import           Tests.Term ( toRefTerm, fromRefTerm )
import qualified Data.ByteString      as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text as T
import           Data.Word
import           Prelude         hiding (encodeFloat)

import           Test.QuickCheck
import           Test.Tasty
import           Test.Tasty.QuickCheck

--------------------------------------------------------------------------------

instance {-# OVERLAPS #-} Arbitrary [TermToken] where
  arbitrary = frequency
    [ (60, termToTermTokens <$> arbitrary ) -- Generates valid FlatTerms
    , (40, genTermTokenList )               -- Generates mostly invalid FlatTerms
    ]

genTermTokenList :: Gen [TermToken]
genTermTokenList = do
  size <- choose (1, 100 ::Int)
  flatTerm <- forM [1.. size] $ const genArbitraryTermToken
  return flatTerm
 
genArbitraryTermToken :: Gen TermToken
genArbitraryTermToken = oneof
  [ TkInt <$> arbitrary
  , TkInteger <$> arbitrary
  , TkBytes . BS.pack <$> arbitrary 
  , pure TkBytesBegin
  , TkString . T.pack <$> arbitrary
  , pure TkStringBegin
  , TkListLen <$> arbitrary
  , pure TkListBegin
  , TkMapLen <$> arbitrary
  , pure TkMapBegin
  , pure TkBreak
  , TkTag <$> arbitrary
  , TkBool <$> arbitrary
  , pure TkNull
  , TkSimple <$> arbitrary
  , TkFloat16 <$> arbitrary
  , TkFloat32 <$> arbitrary
  , TkFloat64 <$> arbitrary ]

-- | Converts a FlatTerm to Encoding
tokenToEncoding :: TermToken -> Encoding
tokenToEncoding (TkInt n)      = encodeInt n  
tokenToEncoding (TkInteger i)  = encodeInteger i
tokenToEncoding (TkBytes bs)   = encodeBytes bs
tokenToEncoding TkBytesBegin   = encodeBytesIndef
tokenToEncoding (TkString txt) = encodeString txt
tokenToEncoding TkStringBegin  = encodeStringIndef
tokenToEncoding (TkListLen w)  = encodeListLen w
tokenToEncoding TkListBegin    = encodeListLenIndef
tokenToEncoding (TkMapLen w)   = encodeMapLen w
tokenToEncoding TkMapBegin     = encodeMapLenIndef 
tokenToEncoding TkBreak        = encodeBreak 
tokenToEncoding (TkTag w64)    = encodeTag64 w64
tokenToEncoding (TkBool bool)  = encodeBool bool
tokenToEncoding TkNull         = encodeNull
tokenToEncoding (TkSimple w8)  = encodeSimple w8
tokenToEncoding (TkFloat16 f)  = encodeFloat16 f
tokenToEncoding (TkFloat32 f)  = encodeFloat f
tokenToEncoding (TkFloat64 d)  = encodeDouble d

-- | converts a @Term@ to @FlatTerm@
termToTermTokens :: Imp.Term -> [TermToken]
termToTermTokens = toFlatTerm . encodeTerm . fromRefTerm


-- | Changes a FlatTerm to List of Word8.    
termTokensToWord :: [TermToken] -> [Word8]
termTokensToWord ts = LBS.unpack bytes
  where
    bytes = toLazyByteString enc
    enc = mconcat $ tokenToEncoding <$> ts 

-- | Given a FlatTerm checks whether the results of Refernce and Implementation
-- decoders tally or not, also matches the result with @validFlatTerm@ function.
prop_validFlatTerm :: [TermToken] -> Property
prop_validFlatTerm ts = property $ case (refDecoded, impDecoded) of
  (Right x, Just (term, []))   -> ((Imp.canonicaliseTerm . toRefTerm $ x) == Imp.canonicaliseTerm term) 
                                  && validFlatTerm ts
  (Left _ , _)                 -> not $ validFlatTerm ts
  _                            -> False
  where
    impDecoded = Imp.runDecoder Imp.decodeTerm (termTokensToWord ts)
    refDecoded = fromFlatTerm decodeTerm ts

testTree :: TestTree
testTree = testGroup "tests the function validFlatTerm and different decoders"
  [ testProperty "all should agree: RefDecoder, ImpDecoder, validFlatTerm " prop_validFlatTerm ]
