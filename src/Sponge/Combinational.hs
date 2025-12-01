{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Purely combinational non-AXI SHA3-256
module Sponge.Combinational (topEntity, spongeCore256, BitString) where

import Clash.Prelude
import qualified Sponge.Incremental as Inc

-- Re-export for convenience
type BitString n = Vec n Bit

-- | Fixed-width digest: SHA3-256 over a 64-bit message (no suffix needed).
-- You can change 'MsgBits' to specialize for other fixed-width messages.
type MsgBits = 64

-- | Convenience wrapper returning a 256-bit digest for a fixed-size message.
spongeCore256 :: BitVector MsgBits -> BitVector 256
spongeCore256 msgBV =
  let msgBits = unpack msgBV :: BitString MsgBits
      digestBits = Inc.sponge4 @256 @MsgBits msgBits
   in pack digestBits

-- | Clash topEntity for synthesis/simulation.
topEntity :: BitVector MsgBits -> BitVector 256
topEntity = spongeCore256

{-# ANN topEntity
  (Synthesize
    { t_name   = "sponge_core"
    , t_inputs = [PortName "msg"]
    , t_output = PortName "digest"
    })
  #-}
