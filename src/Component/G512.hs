{-# LANGUAGE TypeApplications #-}

module Component.G512
  ( i256o256,
    i256o256Stream,
  )
where

import AXI4Stream (AXI4Stream (..))
import Clash.Prelude hiding (tlast)
import Component.G.Common qualified as Common
import Parameter
import Permutation qualified

--------------------------------------------------------------------------------
-- Component G512 (MK-KEM-512 / 256-bit input / 256-bit output)
--------------------------------------------------------------------------------

-- Wrapper function for module naming control
-- OPAQUE ensures module boundary; function name determines module name
{-# OPAQUE spongeFSM #-}
spongeFSM :: Index 24 -> BitVector 1600 -> BitVector 1600
spongeFSM = Permutation.keccakF1600

{-# NOINLINE i256o256Stream #-}
i256o256Stream ::
  Clock System ->
  Reset System ->
  Enable System ->
  Signal System Bool -> -- output tready
  Signal System (AXI4Stream 256, Bool) -> -- Input message with flush signal
  Signal System (AXI4Stream 256, Bool) -- Output digest (AXI4-Stream), input tready
i256o256Stream clk rst en treadySig inputSig =
  withClockResetEnable clk rst en
    $ let (msgSig, flushSig) = unbundle inputSig
       in Common.sponge @System MLKEM512 spongeFSM (bundle (msgSig, treadySig, flushSig))

{-# ANN
  i256o256
  ( Synthesize
      { t_name = "G512_I256_O256",
        t_inputs =
          [ PortName "CLK",
            PortName "RST",
            PortName "EN",
            PortName "TREADY",
            PortName "MSG_TDATA",
            PortName "MSG_TVALID",
            PortName "MSG_TLAST",
            PortName "MSG_FLUSH"
          ],
        t_output =
          PortProduct
            ""
            [ PortName "DIGEST_TDATA",
              PortName "DIGEST_TVALID",
              PortName "DIGEST_TLAST",
              PortName "MSG_TREADY"
            ]
      }
  )
  #-}
{-# NOINLINE i256o256 #-}
i256o256 ::
  Clock System ->
  Reset System ->
  Enable System ->
  Signal System Bool -> -- TREADY
  Signal System (BitVector 256) -> -- MSG_TDATA
  Signal System Bool -> -- MSG_TVALID
  Signal System Bool -> -- MSG_TLAST
  Signal System Bool -> -- MSG_FLUSH
  (Signal System (BitVector 256), Signal System Bool, Signal System Bool, Signal System Bool)
i256o256 clk rst en treadySig msgTdataSig msgTvalidSig msgTlastSig msgFlushSig =
  let msgSig = AXI4Stream <$> msgTdataSig <*> msgTvalidSig <*> msgTlastSig
      outSig = i256o256Stream clk rst en treadySig (bundle (msgSig, msgFlushSig))
      (outStream, msgTreadySig) = unbundle outSig
   in (tdata <$> outStream, tvalid <$> outStream, tlast <$> outStream, msgTreadySig)
