module DUT (topEntity, driveMessage, driveMessageDebug) where

import Clash.Explicit.Testbench
import Clash.Prelude
import qualified Clash.Explicit.Signal as ES
import qualified KeccakF1600 as Core

-- | This is the DUT for KeccakF1600 SHA3-256
-- It wraps the core implementation and is the synthesizable circuit under test
topEntity ::
  Clock System ->
  Reset System ->
  Enable System ->
  Signal System Bool ->             -- S_AXIS_TVALID
  Signal System (BitVector 64) ->   -- S_AXIS_TDATA
  Signal System Bool ->             -- S_AXIS_TLAST
  Signal System Bool ->             -- M_AXIS_TREADY
  ( Signal System Bool              -- S_AXIS_TREADY
  , Signal System Bool              -- M_AXIS_TVALID
  , Signal System (BitVector 64)    -- M_AXIS_TDATA
  , Signal System Bool              -- M_AXIS_TLAST
  )
topEntity = Core.topEntity

-- | Test harness: Drive the DUT with a 64-bit message and collect the digest
driveMessage ::
  ( Clock System -> Reset System -> Enable System ->
    Signal System Bool ->
    Signal System (BitVector 64) ->
    Signal System Bool ->
    Signal System Bool ->
    ( Signal System Bool
    , Signal System Bool
    , Signal System (BitVector 64)
    , Signal System Bool
    )
  )
  -> BitVector 64
  -> (Signal System (BitVector 256), Signal System Bool)
driveMessage dut inputMsg =
  let (digestSig, doneSig, _, _, _, _, _) = driveMessageDebug dut inputMsg
   in (digestSig, doneSig)

-- | Debug-friendly harness that exposes handshake and beat counter signals.
--   Useful for probing simulation step-by-step without altering the core tests.
driveMessageDebug ::
  ( Clock System -> Reset System -> Enable System ->
    Signal System Bool ->
    Signal System (BitVector 64) ->
    Signal System Bool ->
    Signal System Bool ->
    ( Signal System Bool
    , Signal System Bool
    , Signal System (BitVector 64)
    , Signal System Bool
    )
  )
  -> BitVector 64
  -> ( Signal System (BitVector 256)  -- Concatenated digest
     , Signal System Bool             -- Done collecting all beats
     , Signal System Bool             -- s_axis_tready
     , Signal System Bool             -- m_axis_tvalid
     , Signal System (BitVector 64)   -- m_axis_tdata
     , Signal System Bool             -- m_axis_tlast
     , Signal System (Unsigned 3)     -- beat counter
     )
driveMessageDebug dut inputMsg = (actualDigest, allBeatsCollected, sReady, mValid, mData, mLast, beatNum)
  where
    clk = tbSystemClockGen (not <$> allBeatsCollected)
    rst = systemResetGen
    en  = enableGen

    -- AXI input: send one beat with TLAST=1, then TVALID=0
    sValid = stimuliGenerator clk rst (True :> False :> Nil)
    sData  = stimuliGenerator clk rst (inputMsg :> 0 :> Nil)
    sLast  = stimuliGenerator clk rst (True :> False :> Nil)
    mReady = pure True

    (sReady, mValid, mData, mLast) =
      dut clk rst en sValid sData sLast mReady

    -- Collect 4 output beats into individual registers
    beat0 :: Signal System (BitVector 64)
    beat0 = ES.register clk rst en 0 (mux (mValid .&&. (beatNum .==. pure 0)) mData beat0)

    beat1 :: Signal System (BitVector 64)
    beat1 = ES.register clk rst en 0 (mux (mValid .&&. (beatNum .==. pure 1)) mData beat1)

    beat2 :: Signal System (BitVector 64)
    beat2 = ES.register clk rst en 0 (mux (mValid .&&. (beatNum .==. pure 2)) mData beat2)

    beat3 :: Signal System (BitVector 64)
    beat3 = ES.register clk rst en 0 (mux (mValid .&&. (beatNum .==. pure 3)) mData beat3)

    -- Count which beat we're on
    beatNum :: Signal System (Unsigned 3)
    beatNum = ES.register clk rst en 0 nextBeatNum
      where
        nextBeatNum = mux (mValid .&&. (beatNum .<. pure 4))
                          (beatNum + 1)
                          beatNum

    -- Concatenate beats into final digest
    actualDigest :: Signal System (BitVector 256)
    actualDigest = liftA2 (++#) (liftA2 (++#) (liftA2 (++#) beat0 beat1) beat2) beat3

    -- Done when we've collected all 4 beats
    allBeatsCollected :: Signal System Bool
    allBeatsCollected = beatNum .==. pure 4
