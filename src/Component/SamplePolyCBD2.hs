module Component.SamplePolyCBD2
  ( topEntity,
  )
where

import AXI4Stream
import Clash.Prelude hiding (tlast)
import Component.SamplePolyCBD.Common
  ( absorb33,
    cbd2,
    extractBits4,
    squeezeSlice,
  )
import Permutation qualified

{-# ANN
  topEntity
  ( Synthesize
      { t_name = "Component_SamplePolyCBD2",
        t_inputs =
          [ PortName "CLK",
            PortName "RST",
            PortName "EN",
            PortName "MSG_33B",
            PortName "DIGEST_TREADY"
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
  Signal System (BitVector 264) ->
  Signal System Bool ->
  Signal System (AXI4Stream 12, Bool)
topEntity clk rst en msgSig treadySig =
  withClockResetEnable clk rst en (samplePolyCBD2 msgSig treadySig)

-- | State machine for SamplePolyCBD2
-- PRF(eta=2) outputs 128 bytes = 16 64-bit words
-- Each 64-bit word produces 16 coefficients via CBD(eta=2)
-- Total: 16 * 16 = 256 coefficients
data State
  = Absorb
  | Permute (Index 24) (BitVector 1600)
  | Squeeze (Index 16) (Index 16) (BitVector 1600) -- wordIdx, coeffIdx within word, state
  | Done
  deriving (Show, Eq, Generic, NFDataX)

samplePolyCBD2 ::
  forall dom.
  (HiddenClockResetEnable dom) =>
  Signal dom (BitVector 264) ->
  Signal dom Bool ->
  Signal dom (AXI4Stream 12, Bool)
samplePolyCBD2 msgSig treadySig = mealy step Absorb (bundle (msgSig, treadySig))
  where
    step ::
      State ->
      (BitVector 264, Bool) ->
      (State, (AXI4Stream 12, Bool))
    step st (msg, tready) =
      case st of
        Absorb ->
          let initState = absorb33 msg
           in (Permute 0 initState, (idleAXI4Stream, True))
        Permute roundIdx state ->
          let state' = Permutation.keccakF1600 roundIdx state
           in if roundIdx == maxBound
                then (Squeeze 0 0 state', (idleAXI4Stream, False))
                else (Permute (roundIdx + 1) state', (idleAXI4Stream, False))
        Squeeze wordIdx coeffIdx state ->
          let -- Extract 64-bit word from state
              word64 = squeezeSlice (resize wordIdx) state
              -- Extract 4-bit chunk for this coefficient
              bits4 = extractBits4 coeffIdx word64
              -- Apply CBD(eta=2)
              coeff = cbd2 bits4
              -- Check if this is the last coefficient (wordIdx=15, coeffIdx=15)
              isLast = wordIdx == 15 && coeffIdx == 15
              outStream =
                AXI4Stream
                  { tdata = coeff,
                    tvalid = True,
                    tlast = isLast
                  }
              nextState =
                if tready
                  then
                    if isLast
                      then Done
                      else
                        if coeffIdx == 15
                          then Squeeze (wordIdx + 1) 0 state
                          else Squeeze wordIdx (coeffIdx + 1) state
                  else Squeeze wordIdx coeffIdx state
           in (nextState, (outStream, False))
        Done -> (Done, (idleAXI4Stream, False))
