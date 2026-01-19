{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Sponge.NonPipelined.SHAKE128B
  ( sponge,
    pad,
    squeezeSlice,
  )
where

import AXI4Stream
import Clash.Prelude hiding (permute, tlast)
import Sponge.NonPipelined (Phase (..), SeenTLAST (..), State (..), complementAt)

type RateBeats = 168 -- 168 bytes = 1344 bits (SHAKE128 rate)

type PadBeats = 168

-- | Padding function for byte-stream SHAKE128
-- XORs domain separator (0x1F) and final pad bit (0x80)
pad :: Index PadBeats -> BitVector 1600 -> BitVector 1600
pad beatIndex =
  let -- Domain separator 0x1F goes at bit position after last absorbed byte
      -- Final pad bit 0x80 goes at bit 256 (last bit of rate)
      domainBitPos = 1599 - (resize beatIndex * 8 + 7) :: Index 1600
   in complementAt 256 -- Final pad bit
        . complementAt domainBitPos -- DS bit 0
        . complementAt (domainBitPos - 1) -- DS bit 1
        . complementAt (domainBitPos - 2) -- DS bit 2
        . complementAt (domainBitPos - 3) -- DS bit 3
        . complementAt (domainBitPos - 4) -- DS bit 4

-- | Squeeze operation: extract one 8-bit byte from state
squeezeSlice :: KnownNat n => Index n -> BitVector 1600 -> BitVector 8
squeezeSlice counter state =
  let -- Each counter value extracts 8 bits (from MSB side)
      counterU :: Unsigned 16
      counterU = fromIntegral counter
      bitPos = fromIntegral (counterU * 8) :: Int
      -- Shift right to align the target byte to LSB position, then truncate
   in truncateB (state `shiftR` (1592 - bitPos))

-- | Main sponge construction for byte-stream SHAKE128
sponge ::
  forall dom.
  (HiddenClockResetEnable dom, KnownDomain dom) =>
  (Index 24 -> BitVector 1600 -> BitVector 1600) ->
  Signal dom (AXI4Stream 8, Bool, Bool) ->
  Signal dom (AXI4Stream 8, Bool)
sponge permuteFn input = mealy step (State (Absorb 0) 0) input
  where
    step ::
      State RateBeats (Index RateBeats) ->
      (AXI4Stream 8, Bool, Bool) ->
      (State RateBeats (Index RateBeats), (AXI4Stream 8, Bool))
    step (State phase state) (inputStream, tready, flush) =
      case phase of
        Absorb counter ->
          absorb pad xorByte counter state inputStream flush
        Permute permuteCounter seenTLAST ->
          permute permuteFn pad permuteCounter seenTLAST state tready
        Squeeze squeezeCounter ->
          squeeze squeezeCounter state tready

    -- XOR an 8-bit byte into the state at the given beat index
    xorByte :: BitVector 1600 -> BitVector 8 -> Index RateBeats -> BitVector 1600
    xorByte state byte beatIndex =
      let -- Calculate bit position from MSB (beat 0 starts at bit 1599)
          beatU :: Unsigned 16
          beatU = fromIntegral beatIndex
          bitPos = fromIntegral (beatU * 8) :: Int
          shiftAmt = 1592 - bitPos
          -- Extract current byte, XOR it, then reconstruct state
          currentByte = truncateB (state `shiftR` shiftAmt) :: BitVector 8
          xoredByte = currentByte `xor` byte
          -- Create mask to clear the target byte, then OR in the new value
          mask = complement (resize (maxBound :: BitVector 8) `shiftL` shiftAmt) :: BitVector 1600
          cleared = state .&. mask
          newValue = resize xoredByte `shiftL` shiftAmt
       in cleared .|. newValue

    squeeze :: Index RateBeats -> BitVector 1600 -> Bool -> (State RateBeats (Index RateBeats), (AXI4Stream 8, Bool))
    squeeze counter state tready' =
      let outByte = squeezeSlice counter state
          outStream = AXI4Stream {tdata = outByte, tvalid = True, tlast = False}
          nextCounter = if tready' then counter + 1 else counter
       in if tready'
            then
              if nextCounter >= maxBound
                then (State (Permute 0 SeenTLASTAndPadded) state, (outStream, False))
                else (State (Squeeze nextCounter) state, (outStream, False))
            else (State (Squeeze counter) state, (outStream, False))

    -- | Absorb phase for byte-stream (8-bit)
    absorb ::
      (Index RateBeats -> BitVector 1600 -> BitVector 1600) ->
      (BitVector 1600 -> BitVector 8 -> Index RateBeats -> BitVector 1600) ->
      Index RateBeats ->
      BitVector 1600 ->
      AXI4Stream 8 ->
      Bool ->
      (State RateBeats (Index RateBeats), (AXI4Stream 8, Bool))
    absorb pad' xorFn counter state' inputMsg flush
      | flush && counter == 0 =
          let padded = pad' maxAbsorbBeat state'
           in (State (Permute 0 SeenTLASTAndPadded) padded, (idleAXI4Stream, False))
      | not (tvalid inputMsg) = (State (Absorb counter) state', (idleAXI4Stream, True))
      | tlast inputMsg && counter < maxAbsorbBeat =
          let state'' = xorFn state' (tdata inputMsg) counter
              padded = pad' counter state''
           in (State (Permute 0 SeenTLASTAndPadded) padded, (idleAXI4Stream, False))
      | tlast inputMsg && otherwise =
          let state'' = xorFn state' (tdata inputMsg) counter
           in (State (Permute 0 SeenTLASTNotPadded) state'', (idleAXI4Stream, False))
      | counter < maxAbsorbBeat =
          let state'' = xorFn state' (tdata inputMsg) counter
           in (State (Absorb (counter + 1)) state'', (idleAXI4Stream, True))
      | otherwise =
          let state'' = xorFn state' (tdata inputMsg) counter
           in (State (Permute 0 NotSeenTLAST) state'', (idleAXI4Stream, False))
      where
        maxAbsorbBeat = maxBound

    -- | Permute phase for byte-stream (8-bit output)
    permute ::
      (Index 24 -> BitVector 1600 -> BitVector 1600) ->
      (Index RateBeats -> BitVector 1600 -> BitVector 1600) ->
      Index 24 ->
      SeenTLAST ->
      BitVector 1600 ->
      Bool ->
      (State RateBeats (Index RateBeats), (AXI4Stream 8, Bool))
    permute permModule pad' counter seenTLAST state' tready' =
      let state'' = permModule counter state'
       in if counter == 23
            then case seenTLAST of
              SeenTLASTAndPadded ->
                let outByte = squeezeSlice (0 :: Index RateBeats) state''
                    outStream = AXI4Stream {tdata = outByte, tvalid = True, tlast = False}
                    nextState = if tready' then State (Squeeze 1) state'' else State (Squeeze 0) state''
                 in (nextState, (outStream, False))
              SeenTLASTNotPadded ->
                let padded = pad' maxBound state''
                 in (State (Permute 0 SeenTLASTAndPadded) padded, (idleAXI4Stream, False))
              NotSeenTLAST -> (State (Absorb 0) state'', (idleAXI4Stream, True))
            else (State (Permute (counter + 1) seenTLAST) state'', (idleAXI4Stream, False))
