{-# LANGUAGE BinaryLiterals #-}
{-# LANGUAGE TemplateHaskell #-}

module Constants.TH
  ( iotaTH
  ) where

import Clash.Prelude hiding (Exp)
import qualified Prelude as P
import Language.Haskell.TH (Exp, Q)

-- | Template Haskell generator that produces the Vec of 24 Keccak round constants.
iotaTH :: Q Exp
iotaTH = do
  let seed :: BitVector 8
      seed = 0b10000000

      bitPositions :: [Int]
      bitPositions = [0, 1, 3, 7, 15, 31, 63]

      expandLFSR :: BitVector 8 -> BitVector 64
      expandLFSR s =
        P.foldl setBitAt 0 (P.zip [0 .. 6] bitPositions)
        where
          setBitAt acc (j, pos) =
            let tap = 7 - j
            in if testBit s tap
                 then setBit acc pos
                 else acc

      lfsrStep :: BitVector 8 -> BitVector 8
      lfsrStep s =
        let fb = testBit s 0
            s' = s `rotateR` 1
            mask = 0b10001110 :: BitVector 8
        in if fb then s' `xor` mask else s'

      generateRoundConstant :: BitVector 8 -> (BitVector 64, BitVector 8)
      generateRoundConstant lfsr = (expandLFSR lfsr, lfsrStep lfsr)

      constants :: Vec 24 (BitVector 64)
      constants = unfoldrI generateRoundConstant seed

  listToVecTH (toList constants)
