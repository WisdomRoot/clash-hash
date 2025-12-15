{-# LANGUAGE TypeApplications #-}

module Permutation.ThetaTwoStage
  ( thetaF1600TwoStage,
  )
where

import Clash.Prelude

-- | Two-stage theta transformation matching VHDL structure
--
-- Stage 1: Compute 5 column parities (320 bits = 5 columns × 64 bits)
--   sumSheet[x][z] = state[0][x][z] XOR state[1][x][z] XOR state[2][x][z] XOR state[3][x][z] XOR state[4][x][z]
--
-- Stage 2: Apply parities to each cell (1600 outputs)
--   output[y][x][z] = state[y][x][z] XOR sumSheet[(x-1) mod 5][z] XOR sumSheet[(x+1) mod 5][(z-1) mod 64]
--
-- This makes the shared column parity computation explicit,
-- allowing the synthesizer to recognize and optimize it.
thetaF1600TwoStage :: Vec 1600 Bit -> Vec 1600 Bit
thetaF1600TwoStage bv =
  let -- Unpack to 5×5×64 structure: 5 rows (y) × 5 columns (x) × 64 bits (z)
      state :: Vec 5 (Vec 5 (Vec 64 Bit))
      state = unconcatI (unconcatI bv)

      -- Stage 1: Compute column parities
      -- For each column x (0..4), XOR all 5 lanes across y (0..4)
      -- sumSheet :: Vec 5 (Vec 64 Bit) = 5 columns × 64 bits each
      sumSheet :: Vec 5 (Vec 64 Bit)
      sumSheet = map computeColumnParity (indicesI @5)
        where
          computeColumnParity :: Index 5 -> Vec 64 Bit
          computeColumnParity x =
            let lane0 = state !! 0 !! x -- y=0, column x
                lane1 = state !! 1 !! x -- y=1, column x
                lane2 = state !! 2 !! x -- y=2, column x
                lane3 = state !! 3 !! x -- y=3, column x
                lane4 = state !! 4 !! x -- y=4, column x
             in zipWith xor lane0 (zipWith xor lane1 (zipWith xor lane2 (zipWith xor lane3 lane4)))

      -- Stage 2: Apply parities to compute output
      -- For each cell (y,x,z), XOR with two column parity bits
      output :: Vec 5 (Vec 5 (Vec 64 Bit))
      output = imap applyToRow state
        where
          applyToRow :: Index 5 -> Vec 5 (Vec 64 Bit) -> Vec 5 (Vec 64 Bit)
          applyToRow y row = imap (applyToLane y) row

          applyToLane :: Index 5 -> Index 5 -> Vec 64 Bit -> Vec 64 Bit
          applyToLane y x lane = imap (applyToBit y x) lane

          applyToBit :: Index 5 -> Index 5 -> Index 64 -> Bit -> Bit
          applyToBit _y x z inputBit =
            let -- Wraparound for column indices (mod 5)
                xMinus1 = if x == 0 then 4 else x - 1
                xPlus1 = if x == 4 then 0 else x + 1
                -- Wraparound for bit indices (mod 64)
                zMinus1 = if z == 0 then 63 else z - 1
                -- Get the two column parity bits
                parity1 = sumSheet !! xMinus1 !! z
                parity2 = sumSheet !! xPlus1 !! zMinus1
             in inputBit `xor` parity1 `xor` parity2

   in -- Flatten back to Vec 1600 Bit
      concat (concat output)
