-- This is indended to be the simplest possible use of
-- Codec.CBOR.
module Main
  ( main -- :: IO ()
  ) where

import           Codec.Serialise            as S
import qualified Data.ByteString.Lazy       as BL

a :: [ Int ] -- full type definition required
a = [ 1, 2, 3 ]

-- Don't output to the terminal: the created ByteString will contain non 7-bit
-- ASCII characters.
main :: IO ()
main = BL.writeFile "outfile.cbor" (S.serialise a)
