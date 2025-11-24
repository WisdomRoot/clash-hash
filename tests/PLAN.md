# Staged Sponge Rewrite Plan

## Current Situation

### Test Status
- 5 out of 6 tests currently **failing** with incorrect digest outputs
- Only the "debug liveness test" is passing (confirms circuit doesn't hang)
- Digest mismatches indicate issues in sponge construction, padding, or byte ordering

### Current Implementation Complexity
The existing `Sponge.hs` is production-grade but complex:
- 4-state FSM (AbsIdle, AbsBusy, SqIdle, SqBusy)
- Multi-beat gather/scatter pattern (bus=64 bits, rate=1088 bits)
- AXI4-Stream handshaking with backpressure
- Automatic padding injection after TLAST
- 17 beats per absorb block, 4 beats per squeeze

This complexity makes it difficult to isolate the root cause of digest mismatches.

### What's Working
- **KeccakF1600 Permutation** (`src/KeccakF1600/Permutation.hs`) - Verified correct
- **Reference Implementation** (`src/SHA3.hs`) - Pure software implementation
- **Test Infrastructure** (`tests/Driver.hs`) - AXI stimulus and beat collection

## Staged Rewrite Strategy

The goal is to rewrite the sponge in **small, testable increments** to isolate bugs and keep tests green as we grow features.

---

## Phase 1: Pure Reference Sponge (No AXI, No Multi-beat)

**Goal:** Establish correctness baseline with the simplest possible implementation

### Design Decisions for Phase 1

To eliminate ambiguity and maximize testability, **Phase 1 is SHA3-only** (no SHAKE/cSHAKE):

1. **SHA3-only scope:**
   - 2-bit suffix (`0b01` for SHA3 domain separation)
   - Single-block squeeze: `digest <= rate` (SHA3-256/384/512 all satisfy this)
   - SHAKE support deferred to later phases (requires multi-block squeeze, variable digest length)

2. **Padding is internal:** `pureSponge` takes raw messages, applies pad10*1+suffix internally
   - Eliminates double-padding risk
   - Simplifies test code
   - All padding logic in one place

3. **Returns exact digest length:** `BitVector digest` output, not `Vec` of rate-blocks
   - No caller truncation needed
   - Type system enforces correct digest size
   - Simpler API

4. **Permutation is stateless:** Takes state in, returns permuted state out
   - Permutation performs all rounds internally (24 for Keccak-f[1600])
   - No round counter needed in API
   - Cleaner separation: sponge controls absorb/squeeze, permutation is pure function

5. **All type-level parameters explicit:**
   - `PaddedBlocks rate msgBits = DivRU (msgBits + 4) rate` defined (2-bit suffix + 2-bit pad10*1 = 4 bits overhead)
   - All forall parameters have `KnownNat` constraints
   - Examples provided for SHA3-256 instantiation

6. **Bit layout locked down:**
   - Exact bit positions specified for padding (section 2 below)
   - LSB-first endianness documented
   - Edge cases covered (exact-rate messages, multi-block padding)

### Tasks

1. **Create `src/Sponge/Pure.hs`**
   - **API Design Decision:** Phase 1 simplifies testing by taking raw messages and returning exact-length digests. No caller truncation needed.

   - Type-level helpers:
     ```haskell
     -- Number of rate-blocks needed for message + padding
     -- msgBits message + 2 suffix bits + 1 pad start + at least 1 pad end = msgBits + 4 minimum
     type PaddedBlocks rate msgBits = DivRU (msgBits + 4) rate
     ```

   - Pure sponge function signature:
     ```haskell
     pureSponge ::
       forall b rate digest msgBits.
       ( KnownNat b, KnownNat rate, KnownNat digest, KnownNat msgBits
       , rate <= b, digest <= rate
       ) =>
       BitVector 2 ->                    -- suffix (0b01 for SHA3-256/384/512)
       (BitVector b -> BitVector b) ->   -- permutation function (performs all rounds)
       BitVector msgBits ->              -- raw message (NOT pre-padded)
       BitVector digest                  -- exact-length digest (no truncation needed)
     ```

   - **Critical: pureSponge handles ALL padding internally**
     - Takes raw message of any length `msgBits`
     - Applies pad10*1 rule: `M || suffix[1:0] || 1 || 0...0 || 1`
     - Pads to multiple of `rate` bits (1088 for SHA3-256)
     - Returns exactly `digest` bits (256 for SHA3-256)
     - This eliminates padding/truncation bugs from test code

   - **Sponge operation:**
     ```haskell
     -- Pseudocode
     pureSponge suffix permute msg = extractDigest squeezeState
       where
         paddedBlocks = padToRateBlocks suffix msg  -- Pad to Vec n (BitVector rate)
         absorbState  = foldl absorbBlock zeroState paddedBlocks
         squeezeState = absorbState

         absorbBlock :: BitVector b -> BitVector rate -> BitVector b
         absorbBlock st block = permute (st `xor` (zeroExtend block))
         -- permute performs all 24 rounds internally

         extractDigest :: BitVector b -> BitVector digest
         extractDigest st = truncateB st  -- Take low digest bits from rate portion
     ```

2. **Padding bit layout (CRITICAL for correctness)**
   - **For SHA3-256 (b=1600, rate=1088, digest=256):**
     - State layout: `[1599:1088] capacity (512 bits) || [1087:0] rate (1088 bits)`
     - Padding format: `M || suffix[1:0] || 1 || 0...0 || 1`

   - **Bit-level example: 64-bit message on 1088-bit rate**
     ```
     Message:        M[63:0]                    (64 bits at [63:0])
     Suffix:         0b01                       (2 bits at [65:64])
     Pad start:      0b1                        (1 bit at [66])
     Pad zeros:      0b0...0                    (1020 bits at [1086:67])
     Pad end:        0b1                        (1 bit at [1087])
     ```
     - Total: 64 + 2 + 1 + 1020 + 1 = 1088 bits (one rate block)
     - **Endianness:** LSB-first (bit 0 is rightmost/least significant)
     - **In BitVector 1088:** `padded[1087:0] = 1 || 0...0 || 1 || 01 || M[63:0]`

   - **Bit-level example: 1024-bit message on 1088-bit rate**
     ```
     Message:        M[1023:0]                  (1024 bits at [1023:0])
     Suffix:         0b01                       (2 bits at [1025:1024])
     Pad start:      0b1                        (1 bit at [1026])
     Pad zeros:      0b0...0                    (60 bits at [1086:1027])
     Pad end:        0b1                        (1 bit at [1087])
     ```
     - Total: 1024 + 2 + 1 + 60 + 1 = 1088 bits (one rate block)

   - **Multi-block example: 1088-bit message on 1088-bit rate**
     ```
     Block 1:        M[1087:0]                  (1088 bits, fills rate exactly)
     Block 2 (pad):  1 || 0...0 || 1 || 01      (1088 bits, all padding)
     ```
     - When message is exact multiple of rate, full padding block required

   - **Implementation helper:**
     ```haskell
     padToRateBlocks ::
       forall rate msgBits.
       (KnownNat rate, KnownNat msgBits) =>
       BitVector 2 ->                          -- suffix
       BitVector msgBits ->                    -- message
       Vec (PaddedBlocks rate msgBits) (BitVector rate)

     -- For testing/verification: extract individual padding components
     getSuffixBits :: BitVector 2 -> (Bool, Bool)  -- (bit1, bit0)
     getPadStartBit :: Bool                         -- always True
     getPadEndBit :: Bool                           -- always True
     ```

   - **Permutation wrapper:**
     ```haskell
     -- The existing KeccakF1600.Permutation.topEntity is round-by-round
     -- We need a wrapper that applies all 24 rounds at once for Phase 1
     keccakF1600 :: BitVector 1600 -> BitVector 1600
     keccakF1600 state = foldl (\st rnd -> keccakF1600Round rnd st) state [0..23]
       where
         keccakF1600Round :: Index 24 -> BitVector 1600 -> BitVector 1600
         keccakF1600Round = -- from KeccakF1600.Permutation
     ```
     - Phase 1 needs the full permutation as a pure function
     - Later phases will use the round-by-round interface for pipelining

3. **Create `tests/Test/Sponge/Properties.hs`**
   - Property tests against `SHA3.sha3_256` reference
   - Known test vectors:
     - Empty string: `0-bit message` → `a7ffc6f8bf1ed766...`
     - NIST "abc" test: `0x616263` (24 bits) → `3a985da74fe225b2...`
     - 64-bit test: `0x0123456789ABCDEF`
     - 1024-bit test: near-full-block message
     - 1088-bit test: exact-full-block message (requires 2-block padding)
     - NIST test vectors from FIPS 202
   - **Simplified test workflow (no manual padding):**
     ```haskell
     -- keccakF1600 is the full 24-round permutation
     keccakF1600 :: BitVector 1600 -> BitVector 1600
     keccakF1600 = -- imported from KeccakF1600.Permutation

     testVector :: KnownNat n => BitVector n -> BitVector 256 -> Expectation
     testVector msg expected =
       let actual = pureSponge @1600 @1088 @256 (0b01 :: BitVector 2) keccakF1600 msg
        in actual `shouldBe` expected
     ```

4. **Lock down correctness fundamentals**
   - **Padding construction (verified by tests above):**
     - Format: `M || suffix[1:0] || 1 || 0...0 || 1`
     - For SHA3-256: suffix = `0b01`
     - **Exact bit positions from section 2 above:**
       - Message at `[msgBits-1:0]`
       - Suffix at `[msgBits+1:msgBits]`
       - Pad start at `[msgBits+2]`
       - Pad zeros at `[rate-2:msgBits+3]`
       - Pad end at `[rate-1]`
     - **Edge case:** If `msgBits + 4 > rate`, spans multiple blocks
     - **Edge case:** If `msgBits == rate`, need full padding block

   - **Byte ordering (little-endian within lanes):**
     - Keccak uses 64-bit lanes in little-endian byte order
     - BitVector bit 0 = LSB, bit 63 = MSB of first lane
     - Match the `v2bs`/`bs2v` functions in `SHA3.hs`
     - Test with known byte-pattern messages (e.g., `0x0123456789ABCDEF`)

   - **State initialization:**
     - All 1600 bits start at zero: `zeroState = 0 :: BitVector 1600`

   - **Capacity isolation:**
     - Only bits `[rate-1:0]` are XORed with message/padding
     - Bits `[1599:rate]` (capacity) never touched by XOR
     - Verified by: permutation is only operation that mixes capacity bits

### Verification
- Property tests pass: `pureSponge` output matches `SHA3.sha3_256` exactly
- Padding test: Verify bit positions for 64-bit, 1024-bit, 1088-bit messages
- Byte order test: Known vectors with specific byte patterns (e.g., `0x0123456789ABCDEF`)
- Edge case test: Empty message, exact-rate message, multi-block message

---

## Phase 2: Add Simple Sequential Logic

**Goal:** Convert pure function to hardware-ready Mealy machine

### Tasks

1. **Create `src/Sponge/Simple.hs`**
   - Convert `pureSponge` to stateful Mealy FSM
   - States: `Absorb | Permute | Squeeze`
   - Input: `Maybe (BitVector rate, Bool)`  -- (data, isLast)
   - Output: `Maybe (BitVector rate, Bool)`  -- (data, isLast)
   - Still rate = bus width (single-cycle absorb/squeeze per beat)
   - Register state between operations
   - Add round counter for permutation (24 cycles)

2. **Internal State:**
   ```haskell
   -- Type-level computation: how many rate-blocks needed for digest
   type DigestBlocks rate digest = DivRU digest rate

   data Phase = AbsIdle | Permute | SqIdle

   data SpongeState rate b digest rounds = SpongeState
     { stState       :: BitVector b                      -- Full 1600-bit state
     , stPhase       :: Phase                            -- Current FSM phase
     , stRoundCnt    :: Index (rounds + 1)               -- Permutation round counter (0 = done, rounds = start)
     , stSeenLast    :: Bool                             -- Have we received TLAST on input?
     , stSqueezeRem  :: Index (DigestBlocks rate digest + 1)  -- Remaining squeeze blocks (0 = done)
     }
   -- Example for SHA3-256: DigestBlocks 1088 256 = 1, so stSqueezeRem :: Index 2 (values 0 or 1)
   ```

3. **Update property tests**
   - Same test vectors, now tested through sequential circuit
   - Use `simulate` to run Mealy machine over multiple cycles
   - Output should still match `SHA3.sha3_256`

### Verification
- Sequential version produces identical results to Phase 1 pure function
- Cycle count matches expectation (absorb + 24*permute + squeeze)

---

## Phase 3: Add Multi-beat Support

**Goal:** Decouple bus width from rate (bus=64 bits, rate=1088 bits)

### Tasks

1. **Update `Sponge/Simple.hs` for multi-beat**
   - Change input: `Maybe (BitVector bus, Bool)` -- bus=64 bits
   - Add beat counter to state: `beatIdx :: Index (BeatsPerBlock rate bus + 1)`
     - For SHA3-256: `BeatsPerBlock 1088 64 = 17`, so counter ranges 0-17
     - 17 = ready for first beat, 16-1 = gathering, 0 = block complete
   - **Gather logic (Absorb phase):**
     - Accumulate 17 beats (17 × 64 = 1088 bits)
     - XOR each beat into correct slice of rate portion using `writeSlice`
     - Decrement `beatIdx` after each beat
     - When `beatIdx` reaches 0: full block gathered, transition to Permute
   - **Scatter logic (Squeeze phase):**
     - Emit digest using `readSlice` to extract 64-bit beats
     - For SHA3-256: 4 beats (4 × 64 = 256 bits)
     - Track remaining squeeze beats in state
   - **Multi-block absorb handling (CRITICAL FOR LONG MESSAGES):**
     - After Permute completes, FSM decision point:
       ```
       if stSeenLast then
         -- We saw TLAST, no more input coming
         transition to SqIdle, reset stSqueezeRem to DigestBlocks
       else
         -- More data blocks expected
         transition to AbsIdle, reset beatIdx to BeatsPerBlock
       ```
     - This loop allows messages longer than rate (e.g., 2176-bit message = 2 absorb blocks)
   - **TLAST handling (padding injection):**
     - TLAST can arrive at ANY beat (e.g., beat 3 of 17 for 3×64=192-bit message)
     - When TLAST received mid-block:
       1. Set `stSeenLast := True`
       2. Continue gathering beats, but XOR padding bits instead of data
       3. Use `padMessage` logic to generate padding for beats (beatIdx-1) down to 0
       4. When `beatIdx` reaches 0, transition to Permute
       5. After Permute, see `stSeenLast=True`, so go to Squeeze (not back to Absorb)
     - Example: 192-bit message (3 beats) on 1088-bit rate (17 beats):
       - Beats 17-15: XOR data
       - Beat 14: XOR padding block (starts with suffix `0b01`, pad10*1 rule)
       - Beats 13-1: XOR padding continuation (all zeros)
       - Beat 0: XOR final padding beat (sets bit at rate-1)
       - Permute, then Squeeze

2. **Slice operations:**
   ```haskell
   type BeatsPerBlock rate bus = DivRU rate bus

   writeSlice ::
     forall rate bus.
     (KnownNat rate, KnownNat bus, bus <= rate) =>
     BitVector rate ->
     Index (BeatsPerBlock rate bus) ->
     BitVector bus ->
     BitVector rate

   readSlice ::
     forall rate bus.
     (KnownNat rate, KnownNat bus, bus <= rate) =>
     BitVector rate ->
     Index (BeatsPerBlock rate bus) ->
     BitVector bus
   ```

3. **Update tests - explicit single and multi-block cases**
   - Feed same messages but beat-by-beat (64 bits at a time)
   - **Single-block tests (message + padding fits in 17 beats):**
     - 64-bit input: 1 beat data + 16 beats padding → 1 absorb cycle
     - 128-bit input: 2 beats data + 15 beats padding → 1 absorb cycle
     - 1024-bit input: 16 beats data + 1 beat padding → 1 absorb cycle
   - **Multi-block tests (message spans multiple rate blocks):**
     - 1088-bit input: 17 beats data + 17 beats padding → 2 absorb cycles
     - 1152-bit input: 18 beats data + 16 beats padding → 2 absorb cycles (17 + 17)
     - 2176-bit input: 34 beats data + 17 beats padding → 3 absorb cycles (17 + 17 + 17)
   - **Padding injection tests:**
     - TLAST at beat 17: no padding beats needed (message fills block)
     - TLAST at beat 1: 16 beats of padding needed
     - Verify pad10*1 bits land at correct offsets within beats
   - Output should match reference for all cases

### Verification
- Multi-beat version matches Phase 2 single-beat version (for same test vectors)
- Beat counters work correctly:
  - Absorb: count down 17→0 per block
  - Squeeze: count down 4→0 for 256-bit digest
- Padding injection works across multiple beats (suffix, pad10*1 bits verified)
- **Multi-block messages work:**
  - FSM loops back to AbsIdle after Permute when `stSeenLast=False`
  - Cycles through: Absorb(17 beats) → Permute(24 cycles) → Absorb(17 beats) → ...
  - Finally: Absorb → Permute → Squeeze when `stSeenLast=True`
- TLAST handling correct: Can arrive at any beat, triggers padding + final permute only
- No hangs: FSM always progresses to done state

---

## Phase 4: Add AXI Streaming Interface

**Goal:** Production-ready AXI4-Stream wrapper

### Tasks

1. **Create `src/Sponge/Axi.hs` (or update `Sponge.hs`)**
   - Wrap Phase 3 core with AXI handshaking:
     ```haskell
     spongeAxi ::
       ... type parameters ...
       -> Signal dom Bool              -- s_axis_tvalid
       -> Signal dom (BitVector bus)   -- s_axis_tdata
       -> Signal dom Bool              -- s_axis_tlast
       -> Signal dom Bool              -- m_axis_tready
       -> ( Signal dom Bool            -- s_axis_tready
          , Signal dom Bool            -- m_axis_tvalid
          , Signal dom (BitVector bus) -- m_axis_tdata
          , Signal dom Bool            -- m_axis_tlast
          )
     ```
   - **Backpressure handling:**
     - Only consume input when both `tvalid && tready`
     - Only emit output when `mAxisTReady` is high
   - **Automatic padding injection:**
     - After receiving `TLAST`, inject padding beats
     - Calculate how many padding beats needed based on `beatCount`
   - **FSM states:** May keep 4-state design or simplify to 3-state

2. **Re-enable all tests in `tests/Test/SHA3.hs`**
   - Uncomment the 5 comprehensive test cases
   - All should now pass with correct digests

3. **Add streaming property tests**
   - Test backpressure: random ready/valid handshaking
   - Test TLAST positioning: various message lengths
   - Test continuous streaming: multiple messages back-to-back

### Verification
- All 6 tests pass (5 comprehensive + 1 debug)
- AXI version matches Phase 3 non-AXI version
- Handles all backpressure scenarios correctly

---

## Key Verification Points Summary

| Phase | What's Verified | How |
|-------|----------------|-----|
| 1 | Padding, byte order, sponge math | Property tests vs `SHA3.sha3_256` |
| 2 | Sequential logic, state transitions | Simulate vs Phase 1 pure function |
| 3 | Multi-beat gather/scatter | Feed beat-by-beat vs Phase 2 |
| 4 | AXI handshaking, backpressure | Original test suite + new stream tests |

## Expected Outcomes

This staged approach provides:

1. **Clear blame assignment:** If tests fail in Phase N, the bug is in new code added in Phase N
2. **Incremental confidence:** Each phase builds on proven-correct foundation
3. **Simplified debugging:** Test pure logic first, add hardware complexity later
4. **Better documentation:** Each phase's code is simpler and easier to understand
5. **Regression prevention:** Keep all tests from previous phases passing

## Current Architecture to Preserve

- **Permutation:** `src/KeccakF1600/Permutation.hs` - already correct
- **Constants:** `src/Constants.hs`, `src/SHA3internal.hs` - verified
- **Test infrastructure:** `tests/Driver.hs`, `tests/DUT.hs` - good framework
- **Reference impl:** `src/SHA3.hs` - our source of truth

## Files to Create/Update

### New Files
- `src/Sponge/Pure.hs` (Phase 1)
- `tests/Test/Sponge/Properties.hs` (Phase 1)
- `src/Sponge/Simple.hs` (Phase 2)
- `src/Sponge/Axi.hs` (Phase 4, or update existing `Sponge.hs`)

### Updated Files
- `tests/Main.hs` - uncomment test cases in Phase 4
- `src/KeccakF1600.hs` - switch to new sponge in Phase 4

### Reference Files (Read-only)
- `src/SHA3.hs` - software reference
- `src/KeccakF1600/Permutation.hs` - hardware permutation
- `tests/Driver.hs` - test harness

## Notes

- **Rate vs Bus Width:** SHA3-256 has rate=1088 bits, but we use bus=64 bits for AXI streaming
- **Padding beats:** For 64-bit input, need ceil((64 + 2 + 1 + 1) / 1088) = 1 block = 17 beats total (1 data + 16 padding)
- **Digest beats:** 256-bit digest needs 4 beats of 64 bits each
- **Permutation cycles:** KeccakF1600 takes 24 rounds × 1 cycle = 24 cycles per permutation
- **Little-endian:** The Keccak spec uses little-endian byte ordering within lanes (64-bit words)
