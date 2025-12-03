{-# LANGUAGE TypeApplications #-}

module Hash.Stateful5
  ( -- * Stateful5 SHA3 Top Entity
    topEntity,
  )
where

import Clash.Prelude
import qualified Permutation.KeccakF1600 as Perm
import qualified Sponge.Stateful5

--------------------------------------------------------------------------------
-- Stateful5 SHA3-256 Top Entity (24 single-round iterations)
--------------------------------------------------------------------------------

type MsgBits = 1084
type DigestBits = 256

--------------------------------------------------------------------------------
-- Module Naming Strategy (Lessons Learned)
--------------------------------------------------------------------------------
-- Problem: Clash derives generated module names from the calling context.
-- When passing a function as a parameter, Clash names the module after that parameter.
--
-- Attempted Solutions:
--   1. ❌ Adding {-# OPAQUE stateful5Sponge #-} in Sponge.Stateful5
--      - Creates module boundary but doesn't fix naming
--      - Module still named after parameter in calling context
--
--   2. ❌ Marking topEntity with {-# OPAQUE topEntity #-}
--      - Merges FSM + permutation into single huge module (714K)
--      - Loses module separation entirely
--
--   3. ✓ Creating wrapper function with descriptive name
--      - Wrapper function name becomes the module name
--      - Clean 3-module separation:
--        * Stateful5_SHA3.v (576B) - top wrapper
--        * Hash_Stateful5_topEntity_spongeFSM.v (9.2K) - FSM logic
--        * Hash_Stateful5_topEntity_keccakF1600Round.v (699K) - permutation
--
-- Note: The wrapper name should reflect what the MODULE contains, not what
-- the function does. Hence "spongeFSM" describes the FSM module, even though
-- the function just delegates to keccakF1600Round.
--------------------------------------------------------------------------------

-- Wrapper function for module naming control
-- OPAQUE ensures module boundary; function name determines module name
{-# OPAQUE spongeFSM #-}
spongeFSM :: Index 24 -> BitVector 1600 -> BitVector 1600
spongeFSM = Perm.keccakF1600Round

{-# ANN
  topEntity
  ( Synthesize
      { t_name = "Stateful5_SHA3",
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
  withClockResetEnable clk rst en $
    Sponge.Stateful5.sponge @System @DigestBits @MsgBits spongeFSM msgSig
