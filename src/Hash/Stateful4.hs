{-# LANGUAGE TypeApplications #-}

module Hash.Stateful4
  ( -- * Stateful4 SHA3 Top Entity
    topEntity,
  )
where

import Clash.Prelude
import qualified Permutation.KeccakF1600 as Perm
import qualified Sponge.Stateful4

--------------------------------------------------------------------------------
-- Stateful4 SHA3-256 Top Entity (24 single-round iterations)
--------------------------------------------------------------------------------

-- For "abc" test: 24-bit message, 256-bit digest (SHA3-256)
type MsgBits = 24
type DigestBits = 256

{-# ANN
  topEntity
  ( Synthesize
      { t_name = "Stateful4_SHA3",
        t_inputs =
          [ PortName "CLK",
            PortName "RST",
            PortName "EN",
            PortName "MSG"
          ],
        t_output = PortName "DIGEST"
      }
  )
  #-}
{-# NOINLINE topEntity #-}
topEntity ::
  Clock System ->
  Reset System ->
  Enable System ->
  Signal System (BitVector MsgBits) -> -- Input message
  Signal System (BitVector DigestBits) -- Output digest (256 bits for SHA3-256)
topEntity clk rst en msgSig =
  Sponge.Stateful4.topEntity clk rst en Perm.keccakF1600Round msgSig
