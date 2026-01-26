{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Runtime software implementation from the crypton library
module Reference.Crypton
  ( sha3,
    sha3_512,
    shake128,
    shake256,
  )
where

import Clash.Prelude hiding (fromList)
import Crypto.Hash (Digest, hash)
import Crypto.Hash.Algorithms (SHA3_256, SHA3_512, SHAKE128 (..), SHAKE256 (..))
import Data.ByteArray (convert)
import Data.ByteString (ByteString)
import Data.Maybe (fromJust)
import Data.Proxy (Proxy (..))
import Prelude qualified as P

-- | Compute SHA3-256 hash (using crypton library)
--
-- >>> sha3 "test"  -- 32 bytes (256 bits) output
sha3 :: ByteString -> ByteString
sha3 input = convert (hash input :: Digest SHA3_256)

-- | Compute SHA3-512 hash (using crypton library)
--
-- >>> sha3_512 "test"  -- 64 bytes (512 bits) output
sha3_512 :: ByteString -> ByteString
sha3_512 input = convert (hash input :: Digest SHA3_512)

-- | Compute SHAKE128 hash with variable output length (using crypton library)
--
-- The first argument is the output length in bytes.
-- The second argument is the input message.
--
-- >>> shake128 32 "test"  -- 32 bytes (256 bits) output
shake128 :: Int -> ByteString -> ByteString
shake128 outputBytes input =
  let outputBits = outputBytes P.* 8
   in case fromJust (someNatVal (fromIntegral outputBits)) of
        SomeNat (_ :: Proxy n) ->
          convert (hash input :: Digest (SHAKE128 n))

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
