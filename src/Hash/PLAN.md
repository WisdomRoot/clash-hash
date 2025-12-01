Plan: Incremental Bottom-Up AXI4-Stream SHA3-256 Implementation

 Goal

 Build a working AXI4-stream SHA3-256 implementation by starting from the proven Hash.Combinational.truncated and making it stateful/streaming incrementally.

 Context: What's Working

 ✅ Combinational Implementation (Hash.Combinational)
 - All 4 steps passing: padded → absorbed → squeezed → truncated
 - Pure functional implementation with Vec types
 - Tests in Test.Combinational verify correctness at each layer
 - truncated is the complete working hash function

 ❌ AXI Implementation (Hash.KeccakF1600.topEntity + Sponge.spongeAxi)
 - End-to-end test in Test.Complete is failing
 - Complex FSM with 4 states: AbsIdle, AbsBusy, SqIdle, SqBusy
 - Interleaves AXI4-stream protocol with permutation rounds
 - Hard to debug without incremental tests

 Strategy: Start from Working Code

 Key Insight from User: "can we start from Hash.Combinational.truncated?"

 Instead of building state machine from scratch, we:
 1. Start with Hash.Combinational.truncated (already working!)
 2. Make small modifications to make it stateful
 3. Test against SHA3 reference after each change
 4. Gradually add streaming/AXI capabilities

 This is MUCH simpler than building from scratch because:
 - We start with verified correct logic
 - We know the test should pass initially
 - Any failure immediately shows what broke
 - Changes are minimal and controlled

 Incremental Steps

 Step 0: Copy and Verify Baseline

 Goal: Copy Hash.Combinational.truncated to new module and verify it still works

 Implementation:
 -- src/Hash/Stateful.hs
 stateful0 :: Vec msgBits Bit -> Vec 256 Bit
 stateful0 = Hash.Combinational.truncated @256

 Test:
 it "stateful0 = truncated = SHA3.sha3_256" $ do
   let msg = toBitString $(listToVecTH "abc")
   let expected = SHA3.sha3_256 msg
   stateful0 msg `shouldBe` expected

 What this establishes:
 - Baseline working implementation
 - Test infrastructure in place
 - Any future change that breaks this test shows what went wrong

 Step 1: Make it Registered (Add register, keep pure function)

 Goal: Add clock domain but keep same pure computation

 Implementation:
 -- src/Hash/Stateful.hs
 stateful1 ::
   HiddenClockResetEnable dom =>
   Signal dom (Vec 64 Bit) ->
   Signal dom (Vec 256 Bit)
 stateful1 msgSig = register (repeat 0) digestSig
   where
     digestSig = fmap (Hash.Combinational.truncated @256 @64) msgSig

 Test:
 it "stateful1 (registered) = truncated = SHA3.sha3_256" $ do
   let msg = toBitString $(listToVecTH "abc")
   let msgSig = pure msg :: Signal System (Vec 64 Bit)
   let digestSig = stateful1 msgSig
   let digest = sampleN 2 digestSig !! 1  -- Take 2nd sample (after register)
   digest `shouldBe` SHA3.sha3_256 msg

 What this tests:
 - Signal interface works
 - Register adds 1-cycle delay
 - Logic is still correct

 Step 2: Split into Mealy Machine (Add state, but trivial state)

 Goal: Convert to Mealy machine form, but with trivial empty state

 Implementation:
 stateful2 ::
   HiddenClockResetEnable dom =>
   Signal dom (Vec 64 Bit) ->
   Signal dom (Vec 256 Bit)
 stateful2 = mealy step ()
   where
     step :: () -> Vec 64 Bit -> ((), Vec 256 Bit)
     step () msg = ((), Hash.Combinational.truncated @256 @64 msg)

 Test:
 it "stateful2 (Mealy with trivial state) = SHA3.sha3_256" $ do
   -- Same test as Step 1
   -- Verify output still matches

 What this tests:
 - Mealy machine form works
 - State threading (even though state is empty)
 - Output matches reference

 Step 3: Add Real State (Message accumulator)

 Goal: Instead of taking full message at once, accumulate beats

 Implementation:
 type State3 = (Index 2, Vec 64 Bit)  -- (beatCount, accumulated message)

 stateful3 ::
   HiddenClockResetEnable dom =>
   Signal dom (BitVector 64) ->      -- Input: one beat at a time
   Signal dom Bool ->                 -- done signal
   Signal dom (Vec 256 Bit)          -- Output: digest
 stateful3 = mealy step (0, repeat 0)
   where
     step :: State3 -> BitVector 64 -> (State3, (Bool, Vec 256 Bit))
     step (cnt, acc) beat =
       let acc' = writeBeats acc cnt beat
           cnt' = cnt + 1
           done = cnt' == 2  -- For 64-bit msg = 1 beat? Or 2 beats?
           digest = if done
                     then Hash.Combinational.truncated @256 @64 acc'
                     else repeat 0
        in ((if done then 0 else cnt', if done then repeat 0 else acc'),
            (done, digest))

 Test:
 it "stateful3 accumulates 64-bit message from beats" $ do
   -- Send message split into beats
   -- Verify digest after accumulation

 What this tests:
 - Beat accumulation logic
 - State transitions
 - Done signal timing

 Step 4-6: Continue incrementally...

 (Similar pattern: add complexity one piece at a time, test after each change)

 Implementation Files

 New Files to Create

 src/Hash/Stateful.hs - Incremental stateful implementations
 - stateful0 - Copy of truncated (baseline)
 - stateful1 - Registered version
 - stateful2 - Mealy machine with trivial state
 - stateful3 - Beat accumulation
 - (Continue adding steps incrementally)

 tests/Test/Stateful.hs - Incremental tests
 - Step 0 test: Verify baseline works
 - Step 1 test: Registered version
 - Step 2 test: Mealy machine
 - Step 3 test: Beat accumulation
 - (Continue testing each step)

 Files to Modify

 src/Sponge.hs - May need refactoring as we discover issues
 - Currently has spongeAxi with all 4 states
 - Will compare against stateful reference implementations
 - Fix bugs found during incremental testing

 tests/Test/Complete.hs - Should pass after all incremental tests pass
 - No changes needed to test itself
 - Just verify it passes at the end

 Key Insights

 From Combinational Success

 - Incremental testing caught issues early
 - Comparing two implementations found bugs
 - Type-level constraints prevented errors
 - Bottom-up is easier to debug than top-down

 From AXI Exploration

 - 4-state FSM is complex: hard to debug without tests
 - Permutation timing (24 cycles) adds latency
 - AXI protocol adds another layer of complexity
 - Separating concerns helps: stateful logic first, then protocol

 Design Decisions (From User Input)

 - State-by-state testing: Most granular, easiest to debug
 - Start without AXI: Simpler state machine first
 - Mock permutation: Test FSM logic in isolation
 - Reference implementation: Like combinational approach

 Testing Philosophy

 Each test should:
 1. Test ONE thing: One state or transition
 2. Use small inputs: "abc" (3 beats) is perfect
 3. Compare against reference: Hash.Combinational functions
 4. Check intermediate state: Not just final output
 5. Be deterministic: No timing-dependent behavior

 Benefits of This Approach

 1. Incremental verification: Catch bugs at each layer
 2. Clear failure modes: Know exactly which component failed
 3. Easier debugging: Small scope for each test
 4. Confidence building: Each passing test adds confidence
 5. Educational: Understand state machine design

 Critical Files

 Reference Implementations:
 - src/Hash/Combinational.hs - Working combinational version (pad, absorbed, squeezed, truncated)
 - src/Reference/Combinational.hs - Reference side for comparison
 - tests/Test/Combinational.hs - Tests showing incremental approach works

 Current AXI Implementation:
 - src/Sponge.hs - Contains spongeAxi FSM (lines ~50-400)
 - src/Hash/KeccakF1600.hs - Top entity wrapping spongeAxi
 - tests/Test/Complete.hs - End-to-end test (currently failing)

 Permutation:
 - src/Permutation/KeccakF1600.hs - Hardware permutation (24 cycles)
 - src/Reference/SHA3.hs - Contains SHA3.keccakf for mocking

 New Files (To Be Created):
 - src/Hash/Stateful.hs - Reference stateful implementations
 - tests/Test/Stateful.hs - Incremental tests for state machines

 Next Steps

 1. Create src/Hash/Stateful.hs with stateful0 (baseline copy of truncated)
 2. Create tests/Test/Stateful.hs with Step 0 test
 3. Verify Step 0 passes (should pass immediately - it's the working code)
 4. Add stateful1 (registered version) to Hash.Stateful.hs
 5. Add Step 1 test, verify passes
 6. Add stateful2 (Mealy with trivial state)
 7. Add Step 2 test, verify passes
 8. Continue incrementally, adding one small change at a time
 9. Eventually reach full AXI implementation
 10. Verify Test.Complete passes at the end

 Key principle: Each step should be a TINY change from the previous step. If a test fails, we know exactly what broke.
