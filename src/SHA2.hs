module SHA2
  (
    sha256
  ) where

import Clash.Prelude

-- $

-- $setup
-- >>> import Numeric (showHex)
-- >>> import Clash.Prelude

-- | SHA256
-- >>> showHex (unpack @(Unsigned 256) $ sha256 @0 @0 0) ""
-- "e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855"

sha256 :: forall n ell k. (KnownNat n, KnownNat ell, KnownNat k, ell + 1 + k + 64 ~ 512 * (n + 1)) => BitVector ell -> BitVector 256
sha256 msg = pack $ compfunc k (head chunks) inithash where
  h0 = 0x6a09e667 :: Unsigned 32
  h1 = 0xbb67ae85 :: Unsigned 32
  h2 = 0x3c6ef372 :: Unsigned 32
  h3 = 0xa54ff53a :: Unsigned 32
  h4 = 0x510e527f :: Unsigned 32
  h5 = 0x9b05688c :: Unsigned 32
  h6 = 0x1f83d9ab :: Unsigned 32
  h7 = 0x5be0cd19 :: Unsigned 32
  k =
    0x428a2f98 :> 0x71374491 :> 0xb5c0fbcf :> 0xe9b5dba5 :> 0x3956c25b :> 0x59f111f1 :> 0x923f82a4 :> 0xab1c5ed5 :>
    0xd807aa98 :> 0x12835b01 :> 0x243185be :> 0x550c7dc3 :> 0x72be5d74 :> 0x80deb1fe :> 0x9bdc06a7 :> 0xc19bf174 :>
    0xe49b69c1 :> 0xefbe4786 :> 0x0fc19dc6 :> 0x240ca1cc :> 0x2de92c6f :> 0x4a7484aa :> 0x5cb0a9dc :> 0x76f988da :>
    0x983e5152 :> 0xa831c66d :> 0xb00327c8 :> 0xbf597fc7 :> 0xc6e00bf3 :> 0xd5a79147 :> 0x06ca6351 :> 0x14292967 :>
    0x27b70a85 :> 0x2e1b2138 :> 0x4d2c6dfc :> 0x53380d13 :> 0x650a7354 :> 0x766a0abb :> 0x81c2c92e :> 0x92722c85 :>
    0xa2bfe8a1 :> 0xa81a664b :> 0xc24b8b70 :> 0xc76c51a3 :> 0xd192e819 :> 0xd6990624 :> 0xf40e3585 :> 0x106aa070 :>
    0x19a4c116 :> 0x1e376c08 :> 0x2748774c :> 0x34b0bcb5 :> 0x391c0cb3 :> 0x4ed8aa4a :> 0x5b9cca4f :> 0x682e6ff3 :>
    0x748f82ee :> 0x78a5636f :> 0x84c87814 :> 0x8cc70208 :> 0x90befffa :> 0xa4506ceb :> 0xbef9a3f7 :> 0xc67178f2 :> (Nil :: Vec 0 (Unsigned 32))
  padded = msg ++# (pack high) ++# (resize @_ @_ @k $ pack low) ++# (resize @_ @_ @64 . pack . size# $ msg)
  chunks = unpack padded :: Vec (n + 1) (Vec 16 (Unsigned 32))
  inithash = h0 :> h1 :> h2 :> h3 :> h4 :> h5 :> h6 :> h7 :> Nil

--

compfunc :: Vec 64 (Unsigned 32) -> Vec 16 (Unsigned 32) -> Vec 8 (Unsigned 32) -> Vec 8 (Unsigned 32)
compfunc ks chunk inithash = zipWith (+) inithash $ foldl compfuncround inithash $ zip ks ws where
  ws = chunk ++ unfoldrI f chunk where
    -- f :: Vec 16 (Unsigned 32) -> (Unsigned 32, Vec 16 (Unsigned 32))
    f xs = (x, xs <<+ x) where
      s0 = (x1 `rotateR` 7) `xor` (x1 `rotateR` 18) `xor` (x1 `shiftR` 3) where x1 = d1 `at` xs
      s1 = (x14 `rotateR` 17) `xor` (x14 `rotateR` 19) `xor` (x14 `shiftR` 10) where x14 = d14 `at` xs
      x = d0 `at` xs + s0 + d9 `at` xs + s1

compfuncround :: Vec 8 (Unsigned 32) -> (Unsigned 32, Unsigned 32) -> Vec 8 (Unsigned 32)
compfuncround input (k, w) = output where
  a = d0 `at` input
  b = d1 `at` input
  c = d2 `at` input
  d = d3 `at` input
  e = d4 `at` input
  f = d5 `at` input
  g = d6 `at` input
  h = d7 `at` input
  s1 = (e `rotateR` 6) `xor` (e `rotateR` 11) `xor` (e `rotateR` 25)
  ch = (e .&. f) `xor` (complement e .&. g)
  temp1 = h + s1 + ch + k + w
  s0 = (a `rotateR` 2) `xor` (a `rotateR` 13) `xor` (a `rotateR` 22)
  maj = (a .&. b) `xor` (a .&. c) `xor` (b .&. c)
  temp2 = s0 + maj
  output = temp1 + temp2 :> a :> b :> c :> d + temp1 :> e :> f :> g :> Nil
