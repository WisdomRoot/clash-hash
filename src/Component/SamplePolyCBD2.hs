module Component.SamplePolyCBD2
  ( i264o12,
    i264o12Core,
    i264o24,
    i264o24Core,
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
  i264o12
  ( Synthesize
      { t_name = "dut",
        t_inputs =
          [ PortName "CLK",
            PortName "RST",
            PortName "EN",
            PortProduct
              ""
              [ PortProduct "MSG" [PortName "TDATA", PortName "TVALID", PortName "TLAST"],
                PortName "DIGEST_TREADY"
              ]
          ],
        t_output =
          PortProduct
            ""
            [ PortProduct "DIGEST" [PortName "TDATA", PortName "TVALID", PortName "TLAST"],
              PortName "MSG_TREADY"
            ]
      }
  )
  #-}
{-# NOINLINE i264o12 #-}
i264o12 ::
  Clock System ->
  Reset System ->
  Enable System ->
  Signal System (AXI4Stream 264, Bool) ->
  Signal System (AXI4Stream 12, Bool)
i264o12 = toDUT i264o12Core

{-# ANN
  i264o24
  ( Synthesize
      { t_name = "dut",
        t_inputs =
          [ PortName "CLK",
            PortName "RST",
            PortName "EN",
            PortProduct
              ""
              [ PortProduct "MSG" [PortName "TDATA", PortName "TVALID", PortName "TLAST"],
                PortName "DIGEST_TREADY"
              ]
          ],
        t_output =
          PortProduct
            ""
            [ PortProduct "DIGEST" [PortName "TDATA", PortName "TVALID", PortName "TLAST"],
              PortName "MSG_TREADY"
            ]
      }
  )
  #-}
{-# NOINLINE i264o24 #-}
i264o24 ::
  Clock System ->
  Reset System ->
  Enable System ->
  Signal System (AXI4Stream 264, Bool) ->
  Signal System (AXI4Stream 24, Bool)
i264o24 = toDUT i264o24Core

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

data PairState
  = NeedFirst
  | NeedSecond (BitVector 12)
  | HavePair (BitVector 24) Bool
  deriving (Show, Eq, Generic, NFDataX)

samplePolyCBD2 ::
  forall dom.
  (HiddenClockResetEnable dom) =>
  Pipe dom 264 12
samplePolyCBD2 (outReady, inStream) = mealyB step Absorb (outReady, inStream)
  where
    step ::
      State ->
      (Bool, AXI4Stream 264) ->
      (State, (Bool, AXI4Stream 12))
    step st (tready, inBeat) =
      case st of
        Absorb ->
          if tvalid inBeat
            then
              let initState = absorb33 (tdata inBeat)
               in (Permute 0 initState, (False, idleAXI4Stream))
            else (Absorb, (True, idleAXI4Stream))
        Permute roundIdx state ->
          let state' = Permutation.keccakF1600Reversed roundIdx state
           in if roundIdx == maxBound
                then (Squeeze 0 0 state', (False, idleAXI4Stream))
                else (Permute (roundIdx + 1) state', (False, idleAXI4Stream))
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
           in (nextState, (False, outStream))
        Done -> (Done, (False, idleAXI4Stream))

i264o12Core ::
  HiddenClockResetEnable dom =>
  Pipe dom 264 12
i264o12Core = samplePolyCBD2

pairStep ::
  PairState ->
  (Bool, AXI4Stream 12) ->
  (PairState, (Bool, AXI4Stream 24))
pairStep st (outReady, inBeat) =
  case st of
    NeedFirst ->
      if tvalid inBeat
        then (NeedSecond (tdata inBeat), (True, idleAXI4Stream))
        else (NeedFirst, (True, idleAXI4Stream))
    NeedSecond c0 ->
      if tvalid inBeat
        then
          let pairData = tdata inBeat ++# c0
              outBeat = validBeat pairData (tlast inBeat)
           in if outReady
                then (NeedFirst, (True, outBeat))
                else (HavePair pairData (tlast inBeat), (True, outBeat))
        else (NeedSecond c0, (True, idleAXI4Stream))
    HavePair pairData isLast ->
      let outBeat = validBeat pairData isLast
       in if outReady
            then (NeedFirst, (False, outBeat))
            else (HavePair pairData isLast, (False, outBeat))

i264o24Core ::
  HiddenClockResetEnable dom =>
  Pipe dom 264 24
i264o24Core = i264o12Core ~> mealyB pairStep NeedFirst
