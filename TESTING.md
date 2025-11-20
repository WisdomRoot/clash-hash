# Testing KeccakF1600 SHA3-256 Core

## Overview

This document explains how to test the SHA3-256 hardware core using Clash's behavioral simulation, **NOT** by synthesizing to HDL and running external simulators.

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

### 3. Behavioral Simulation in Clash

**Run tests using clashi/stack repl, NOT Yosys/Verilator/Icarus:**

```bash
# Start the Clash REPL
stack exec clashi

# Load the testbench module
:m +KeccakF1600Testbench

# Run the simulation for N cycles
sampleN 100 testBench

# Or load with automatic import
stack exec clashi -- -e ":m +KeccakF1600Testbench" -e "sampleN 100 testBench"
```

### 4. What NOT to Do

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

## Current Status

The testbench in `KeccakF1600Testbench.hs` is a **work in progress**:

- ✅ Compiles and loads in clashi
- ✅ Uses SHA3 reference for golden digest
- ✅ Drives empty-string test case via AXI
- ⚠️  Does NOT yet collect multi-cycle AXI output
- ⚠️  Does NOT yet compare output with expected digest
- ⚠️  Always returns `done = pure False`

### Next Steps

To complete the testbench:

1. **Implement multi-cycle AXI collection**
   - Collect 4 beats of 64-bit M_AXIS_TDATA when M_AXIS_TVALID is high
   - Concatenate into a `BitVector 256`

2. **Add output verification**
   - Compare collected digest with `expectedDigest`
   - Use `outputVerifier'` or manual comparison

3. **Implement done signal**
   - Return `True` when digest matches (pass)
   - Return `False` if mismatch or timeout (fail)

## Example: Running in clashi

```haskell
-- Load the testbench
$ stack exec clashi
Clashi> :m +KeccakF1600Testbench

-- Sample 50 cycles to see behavior
Clashi> take 50 $ sample testBench
[False,False,False,...]

-- Inspect the signals (once we implement proper verification)
Clashi> let sig = testBench
Clashi> sampleN 100 sig
```

## Design Philosophy

The key principle is **separation of concerns**:

- **Hardware (topEntity)**: Pure, synthesizable design with AXI interface
- **Testbench (testBench)**: Behavioral verification using Clash simulation
- **Reference (SHA3.sha3_256)**: Golden model for comparison (software only, never synthesized)

This allows fast iteration during development without waiting for synthesis/P&R.
