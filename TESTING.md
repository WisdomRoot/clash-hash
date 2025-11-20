# Testing KeccakF1600 SHA3-256 Core

## Overview

This document explains how to test the SHA3-256 hardware core using Clash's behavioral simulation with Tasty+Hspec, **NOT** by synthesizing to HDL and running external simulators.

## The Proper Testing Pattern

### 1. Top Entity Stays Pure Hardware

`KeccakF1600.topEntity` is your real hardware design:
- Takes clock, reset, enable signals
- Exposes AXI4-Stream interface (TVALID, TDATA, TLAST, TREADY)
- Contains no test logic

### 2. Testbench is a Separate Clash Module

`KeccakF1600Testbench.testBench` is a pure Clash function that:
- Lives in `src/KeccakF1600Testbench.hs`
- Uses `tbSystemClockGen` and `systemResetGen` for clock/reset generation
- Uses `stimuliGenerator` to produce input AXI streams
- Uses SHA3 reference implementation for golden digest comparison
- Returns `Signal System Bool` indicating test completion

### 3. Testing with Tasty+Hspec

**The proper way to run tests:**

```bash
# Run all tests
stack test

# Run only the Clash hardware testbench suite
stack test --test-arguments "--pattern KeccakF1600"

# Run specific test case
stack test --test-arguments "-p '/eventually asserts done/'"
```

The test suite is defined in `tests/Main.hs` using Tasty+Hspec:
- Uses `sampleN` to simulate the Clash testbench in Haskell
- No HDL generation or external simulator needed
- Fast feedback loop for development

### 4. Interactive Testing in clashi (Optional)

You can also test interactively:

```bash
# Start the Clash REPL
stack exec clashi

# Load the testbench module
:m +KeccakF1600Testbench

# Run the simulation for N cycles
sampleN 100 testBench
```

### 5. What NOT to Do

**DO NOT use synthesis tools for functional verification:**
- ❌ Don't run `nix run .#synth` to check SHA3 correctness
- ❌ Don't run Yosys, Verilator, or Icarus for basic testing
- ❌ Don't try to synthesize the testbench itself
- ❌ Don't put test logic inside `topEntity`

**Only use HDL generation later for:**
- Gate-level timing analysis
- Power estimation
- Integration with external tools
- Final hardware validation

## Test Suite Structure

```
tests/
└── Main.hs                  # Tasty+Hspec tests for KeccakF1600Testbench

src/
├── KeccakF1600.hs          # Pure hardware (topEntity)
└── KeccakF1600Testbench.hs # Testbench (testBench)
```

The `clash-hash-test` suite in `clash-hash.cabal`:
```cabal
test-suite clash-hash-test
  type:           exitcode-stdio-1.0
  main-is:        Main.hs
  build-depends:
      base
    , clash-prelude
    , clash-hash
    , tasty
    , tasty-hspec
    , hspec
```

## Current Status

✅ **Test infrastructure complete:**
- Testbench module compiles and loads
- Tasty+Hspec test suite runs successfully
- Tests verify testBench type and basic behavior
- No HDL synthesis required for testing

⚠️ **Testbench functionality incomplete:**
- Multi-cycle AXI output collection not implemented
- Digest comparison logic not implemented
- Always returns `done = pure False`

### Next Steps

To complete the testbench functionality:

1. **Implement multi-cycle AXI collection**
   - Collect 4 beats of 64-bit M_AXIS_TDATA when M_AXIS_TVALID is high
   - Concatenate into a `BitVector 256`

2. **Add output verification**
   - Compare collected digest with `expectedDigest`
   - Use `outputVerifier'` or manual comparison

3. **Update test expectations**
   - Change test to expect `done = True` when digest matches
   - Add test case with known test vector (e.g., "abc")

## Running Tests

```bash
# Run all test suites (including clash-hash-test)
stack test

# See test output
stack test --test-arguments "--verbose"

# Run only Clash hardware tests
stack test clash-hash-test

# Run specific test pattern
stack test clash-hash-test --test-arguments "-p '/eventually/'"
```

## Design Philosophy

The key principle is **separation of concerns**:

- **Hardware (topEntity)**: Pure, synthesizable design with AXI interface
- **Testbench (testBench)**: Behavioral verification using Clash simulation
- **Reference (SHA3.sha3_256)**: Golden model for comparison (software only, never synthesized)
- **Test Suite (tests/Main.hs)**: Tasty+Hspec framework driving Clash simulation

This allows fast iteration during development without waiting for synthesis/P&R.
