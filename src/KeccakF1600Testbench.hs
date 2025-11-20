{-# LANGUAGE TypeApplications #-}

module KeccakF1600Testbench (testBench) where

import Clash.Explicit.Testbench
import Clash.Prelude
import KeccakF1600 (topEntity)
import SHA3internal (BitString, v2bs)
import qualified SHA3

-- | For now, test a single message: empty string
emptyMsg :: Vec 0 (Unsigned 8)
emptyMsg = Nil

-- | Golden digest for empty string using SHA3 reference
-- SHA3-256("") = a7ffc6f8bf1ed76651c14756a061d662f580ff4de43b49fa82d80a4b80f8434a
expectedDigest :: BitVector 256
expectedDigest = v2bv (SHA3.sha3_256 @0 (v2bs emptyMsg :: BitString 0))

{-# ANN testBench (TestBench 'topEntity) #-}
{-# NOINLINE testBench #-}
testBench :: Signal System Bool
testBench = done
  where
    clk = tbSystemClockGen (not <$> done)
    rst = systemResetGen
    en = enableGen

    -- AXI stream for empty message:
    -- For empty string, we send TLAST=1 on the first beat with any data
    -- The sponge will pad this into a full block
    sAxisValid = stimuliGenerator clk rst (True :> False :> Nil)
    sAxisData = stimuliGenerator clk rst ((0 :: BitVector 64) :> 0 :> Nil)
    sAxisLast = stimuliGenerator clk rst (True :> False :> Nil)
    mAxisReady = pure True

    (sReady, mValid, mData, mLast) =
      topEntity clk rst en sAxisValid sAxisData sAxisLast mAxisReady

    -- TODO: Implement multi-cycle AXI output collection
    -- The digest comes out as 4 beats of 64-bit data (256 bits total)
    -- Need to:
    -- 1. Collect 4 beats when mValid is high
    -- 2. Concatenate into a BitVector 256
    -- 3. Compare with expectedDigest
    -- 4. Set done=True when comparison complete

    -- Placeholder: just never finish
    done = pure False
