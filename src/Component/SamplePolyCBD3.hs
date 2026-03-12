module Component.SamplePolyCBD3
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
    cbd3,
    extractTop6,
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

-- | State machine for SamplePolyCBD3
-- PRF(eta=3) outputs 192 bytes = 24 64-bit words
-- This requires two Keccak blocks: 17 words from first, 7 from second
-- Each coefficient uses 6 bits via CBD(eta=3)
-- Total: 256 coefficients
--
-- We use a 128-bit buffer to handle 6-bit extraction across word boundaries
data State
  = Absorb
  | Permute
      (Index 24) -- round index
      (Unsigned 9) -- coefficient count (0-256)
      (Unsigned 5) -- word index (0-24)
      (Unsigned 1) -- block index (0 or 1)
      (BitVector 128) -- bit buffer
      (Unsigned 8) -- valid bits in buffer (0-128)
      (BitVector 1600) -- Keccak state
  | Squeeze
      (Unsigned 9) -- coefficient count (0-256)
      (Unsigned 5) -- word index (0-24)
      (Unsigned 1) -- block index (0 or 1)
      (BitVector 128) -- bit buffer
      (Unsigned 8) -- valid bits in buffer (0-128)
      (BitVector 1600) -- Keccak state
  | Done
  deriving (Show, Eq, Generic, NFDataX)

data PairState
  = NeedFirst
  | NeedSecond (BitVector 12)
  | HavePair (BitVector 24) Bool
  deriving (Show, Eq, Generic, NFDataX)

samplePolyCBD3 ::
  forall dom.
  (HiddenClockResetEnable dom) =>
  Pipe dom 264 12
samplePolyCBD3 (outReady, inStream) = mealyB step Absorb (outReady, inStream)
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
               in (Permute 0 0 0 0 0 0 initState, (False, idleAXI4Stream))
            else (Absorb, (True, idleAXI4Stream))
        Permute roundIdx coeffIdx wordIdx blockIdx buffer validBits state ->
          let state' = Permutation.keccakF1600Reversed roundIdx state
           in if roundIdx == maxBound
                then (Squeeze coeffIdx wordIdx blockIdx buffer validBits state', (False, idleAXI4Stream))
                else (Permute (roundIdx + 1) coeffIdx wordIdx blockIdx buffer validBits state', (False, idleAXI4Stream))
        Squeeze coeffIdx wordIdx blockIdx buffer validBits state ->
          let -- Can we output a coefficient?
              canOutput = validBits >= 6
              -- Extract top 6 bits from buffer
              bits6 = extractTop6 buffer
              coeffVal = cbd3 bits6
              isLast = coeffIdx == 255

              outStream =
                if canOutput
                  then
                    AXI4Stream
                      { tdata = coeffVal,
                        tvalid = True,
                        tlast = isLast
                      }
                  else idleAXI4Stream

              -- Update after potential output
              (coeffIdx', buffer', validBits') =
                if canOutput && tready
                  then (coeffIdx + 1, buffer `shiftL` 6, validBits - 6)
                  else (coeffIdx, buffer, validBits)

              -- Check if we need to load more bits
              needLoad = validBits' <= 64 && wordIdx < 24
              -- Need to permute when entering second block
              needPermute = wordIdx == 17 && blockIdx == 0 && needLoad
              needLoadFromState = needLoad && not needPermute

              -- Calculate in-block index for word extraction
              inBlockIdx :: Index 17
              inBlockIdx = fromIntegral (if wordIdx < 17 then wordIdx else wordIdx - 17)

              -- Determine next state
              nextState
                | canOutput && tready && isLast = Done
                | needPermute =
                    -- Go to Permute for second block
                    Permute 0 coeffIdx' wordIdx 1 buffer' validBits' state
                | needLoadFromState =
                    -- Load next word into buffer
                    let word = squeezeSlice inBlockIdx state
                        shiftAmt = 64 - validBits'
                        buffer'' = buffer' .|. (resize word `shiftL` fromIntegral shiftAmt)
                     in Squeeze coeffIdx' (wordIdx + 1) blockIdx buffer'' (validBits' + 64) state
                | otherwise = Squeeze coeffIdx' wordIdx blockIdx buffer' validBits' state
           in (nextState, (False, outStream))
        Done -> (Done, (False, idleAXI4Stream))

i264o12Core ::
  HiddenClockResetEnable dom =>
  Pipe dom 264 12
i264o12Core = samplePolyCBD3

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
