-- This is indended to be about the simplest possible use of
-- Data.Binary.Serialise.

import Data.Binary.Serialise.CBOR as CBOR
import qualified Data.ByteString.Lazy as Bs

a :: [ Int ] -- full type definition required
a = [1,2,3]

-- we don't output to the terminal because the byte string created will
-- contain non 7-bit ASCII characters.  

main=Bs.writeFile "outfile.cbor" a_serialised
     where
       a_serialised = CBOR.serialise a
