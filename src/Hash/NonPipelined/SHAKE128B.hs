{-# LANGUAGE TypeApplications #-}

module Hash.NonPipelined.SHAKE128B
  ( -- * NonPipelined SHAKE128B (Byte-Stream) Top Entity
    topEntity,
    hash,
  )
where

import AXI4Stream (AXI4Stream)
import Clash.Prelude
import Permutation.Reversed qualified as Perm
import Sponge.NonPipelined.SHAKE128B qualified

--------------------------------------------------------------------------------
-- NonPipelined SHAKE128B Top Entity (Byte-Stream Version)
--------------------------------------------------------------------------------

type MsgBits = 8 -- Byte stream

type DigestBits = 8 -- Byte stream

{-# OPAQUE spongeFSM #-}
spongeFSM :: Index 24 -> BitVector 1600 -> BitVector 1600
spongeFSM = Perm.keccakF1600RoundReversed

{-# ANN
  topEntity
  ( Synthesize
      { t_name = "SHAKE_128B_NonPipelined",
        t_inputs =
          [ PortName "CLK",
            PortName "RST",
            PortName "EN",
            PortName "TREADY",
            PortProduct
              "MSG"
              [ PortName "MSG_TDATA",
                PortName "MSG_TVALID",
                PortName "MSG_TLAST",
                PortName "MSG_FLUSH"
              ]
          ],
        t_output =
          PortProduct
            ""
            [ PortName "DIGEST_TDATA",
              PortName "DIGEST_TVALID",
              PortName "DIGEST_TLAST"
            ]
      }
  )
  #-}
{-# NOINLINE topEntity #-}
topEntity ::
  Clock System ->
  Reset System ->
  Enable System ->
  Signal System Bool ->
  Signal System (AXI4Stream MsgBits, Bool) ->
  Signal System (AXI4Stream DigestBits, Bool)
topEntity clk rst en treadySig inputSig =
  withClockResetEnable clk rst en $
    let (msgSig, flushSig) = unbundle inputSig
        inputWithFlush = bundle (msgSig, treadySig, flushSig)
     in Sponge.NonPipelined.SHAKE128B.sponge @System spongeFSM inputWithFlush

hash :: (HiddenClockResetEnable dom) => Signal dom (AXI4Stream MsgBits, Bool, Bool) -> Signal dom (AXI4Stream DigestBits, Bool)
hash = Sponge.NonPipelined.SHAKE128B.sponge spongeFSM
