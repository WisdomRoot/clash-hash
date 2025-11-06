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
iota = unfoldrI generateRoundConstant (0b10000000 :: BitVector 8)
  where
    -- Generate one round constant and next LFSR state
    generateRoundConstant :: BitVector 8 -> (BitVector 64, BitVector 8)
    generateRoundConstant lfsr = (expandLFSR lfsr, lfsrStep lfsr)

    -- LFSR step with feedback polynomial x^8 + x^6 + x^5 + x^4 + 1
    lfsrStep :: BitVector 8 -> BitVector 8
    lfsrStep s =
      let fb = testBit s 0
          s' = s `rotateR` 1
          mask = 0b10001110 :: BitVector 8
      in if fb then s' `xor` mask else s'

    -- Expand 8-bit LFSR to 64-bit round constant
    -- Bits placed at positions 2^j - 1 for j in 0..6
    expandLFSR :: BitVector 8 -> BitVector 64
    expandLFSR s = P.foldl setBitAt 0 (P.zip [0..6] bitPositions)
      where
        bitPositions :: [Int]
        bitPositions = [0, 1, 3, 7, 15, 31, 63]

        setBitAt acc (j, pos) =
          -- `BitVector` stores bit 0 at the LSB, but the Keccak spec numbers
          -- the taps from the MSB side. Map j=0..6 onto bits 7..1 accordingly.
          let tap = 7 - j
          in if testBit s tap
               then setBit acc pos
               else acc
