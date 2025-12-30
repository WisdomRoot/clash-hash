{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | Runtime software implementation from the crypton library
module Reference.Crypton
  ( sha3,
    shake256,
  )
where

import Clash.Prelude hiding (fromList)
import Clash.Sized.Vector qualified as V
import Crypto.Hash (Digest, hash)
import Crypto.Hash.Algorithms (SHA3_256, SHAKE256 (..))
import Data.Bits qualified as Bits
import Data.ByteArray (convert)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.Constraint (Dict (..))
import Data.Maybe (fromJust)
import Data.Proxy (Proxy (..))
import Data.Word (Word8)
import GHC.TypeLits (SomeNat (..), someNatVal)
import Prelude (Int, fromIntegral, ($), (.), (<>))
import Prelude qualified as P
import Reference.SHA3 qualified as SHA3
import Unsafe.Coerce (unsafeCoerce)

-- | Compute SHA3-256 hash (using crypton library)
--
-- >>> sha3 "test"  -- 32 bytes (256 bits) output
sha3 :: ByteString -> ByteString
sha3 input = convert (hash input :: Digest SHA3_256)

-- | Compute SHAKE256 hash with variable output length (using crypton library)
--
-- The first argument is the output length in bytes.
-- The second argument is the input message.
--
-- >>> shake256 32 "test"  -- 32 bytes (256 bits) output
shake256 :: Int -> ByteString -> ByteString
shake256 outputBytes input =
  let outputBits = outputBytes P.* 8
   in case fromJust (someNatVal (fromIntegral outputBits)) of
        SomeNat (_ :: Proxy n) ->
          convert (hash input :: Digest (SHAKE256 n))