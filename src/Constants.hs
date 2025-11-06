module Constants
  ( iota
  ) where

import Clash.Prelude
import qualified Prelude as P

-- | All 24 round constants for Keccak-f, 64 bits each
--
-- Generated using LFSR (Linear Feedback Shift Register) with polynomial x^8 + x^6 + x^5 + x^4 + 1.
-- Bits are placed at positions 2^j - 1 for j in 0..6 (i.e., positions 0, 1, 3, 7, 15, 31, 63).
--
-- Can be sliced for different Keccak-f variants:
-- - Keccak-f[25]:   w=1,  use bits 0
-- - Keccak-f[50]:   w=2,  use bits 0-1
-- - Keccak-f[100]:  w=4,  use bits 0-3
-- - Keccak-f[200]:  w=8,  use bits 0-7
-- - Keccak-f[400]:  w=16, use bits 0-15
-- - Keccak-f[800]:  w=32, use bits 0-31
-- - Keccak-f[1600]: w=64, use all bits 0-63
--
-- Number of rounds for each variant:
-- - Keccak-f[25]:   12 + 2*0 = 12 rounds (use indices 0-11)
-- - Keccak-f[50]:   12 + 2*1 = 14 rounds (use indices 0-13)
-- - Keccak-f[100]:  12 + 2*2 = 16 rounds (use indices 0-15)
-- - Keccak-f[200]:  12 + 2*3 = 18 rounds (use indices 0-17)
-- - Keccak-f[400]:  12 + 2*4 = 20 rounds (use indices 0-19)
-- - Keccak-f[800]:  12 + 2*5 = 22 rounds (use indices 0-21)
-- - Keccak-f[1600]: 12 + 2*6 = 24 rounds (use all indices 0-23)
iota :: Vec 24 (BitVector 64)
iota = fmap (v2bv . reverse . ifoldl g (repeat 0)) lfsr
  where
    lfsr = unconcatI . unfoldrI f $ bv2v $(bLit "10000000") :: Vec 24 (Vec 7 Bit)
    f t = (head t, zipWith xor (0 +>> t) . fmap (last t .&.) $ bv2v $(bLit "10001110"))
    g t j b = replace @_ @(Unsigned 7) (2 P.^ j - 1) b t
