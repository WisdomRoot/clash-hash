# How to Read Static Timing Analysis (STA) Reports

## 1. The Summary (Most Important!)

```
Timing Summary:
==================================================
  WNS (max)        91.791
  TNS (max)        55720.734
  Worst Slack      91.79
==================================================
```

### What these numbers mean:

- **Worst Slack: 91.79 ns** (Actually means **-91.79 ns**)
  - This is THE most critical number
  - **NEGATIVE slack = TIMING VIOLATION**
  - Your design **FAILS** timing at 10ns (100 MHz) clock
  - The critical path takes 101.59 ns but must complete in 9.80 ns

- **WNS (Worst Negative Slack): 91.791 ns**
  - The magnitude of the worst violation
  - Same as |Worst Slack| when negative

- **TNS (Total Negative Slack): 55720.734 ns**
  - Sum of ALL negative slacks across all paths
  - Indicates how "broken" the design is overall

### Quick Guide:
- ✅ **Positive slack** → Design meets timing (good!)
- ❌ **Negative slack** → Timing violation (bad!)
- For the NP design: **-91.79 ns violation** → Cannot run at 100 MHz

---

## 2. Understanding a Timing Path

Here's the critical path from the NP report:

```
Startpoint: _21106_ (flip-flop clocked by CLK)
Endpoint:   _19500_ (flip-flop clocked by CLK)
Path Type:  max (setup check)

  Delay    Time   Description
---------------------------------------------------------
   0.00    0.00   clock CLK (rise edge)          ← Clock starts
   0.00    0.00   clock network delay
   0.00    0.00 ^ _21106_/CK (DFFR_X1)          ← Source register
   0.23    0.23 v _21106_/Q (DFFR_X1)           ← Data leaves register (0.23ns)
   0.62    0.85 ^ _11499_/ZN (NOR2_X1)          ← First gate (NOR)
   0.51    1.36 v _11502_/ZN (NOR2_X1)          ← Second gate
   0.09    1.44 ^ _11503_/ZN (NAND2_X1)         ← Third gate
   0.02    1.46 v _11504_/ZN (NOR2_X1)          ← Fourth gate
  18.15   19.62 ^ _11561_/ZN (OAI21_X1)         ← Big delay! (18.15ns)
  75.38   94.99 v _16637_/ZN (AOI21_X1)         ← HUGE delay! (75.38ns)
   6.99  101.98 ^ _17694_/ZN (AOI22_X1)         ← Another delay
  -0.39  101.59 v _17695_/ZN (INV_X1)           ← Final inverter
   0.00  101.59 v _19500_/D (DFFR_X1)           ← Data arrives (101.59ns!)
         101.59   data arrival time

  10.00   10.00   clock CLK (rise edge)          ← Next clock edge
   0.00   10.00   clock network delay
   0.00   10.00   clock reconvergence pessimism
          10.00 ^ _19500_/CK (DFFR_X1)          ← Destination register
  -0.20    9.80   library setup time             ← Must arrive by 9.80ns
           9.80   data required time

---------------------------------------------------------
           9.80   data required time
        -101.59   data arrival time
---------------------------------------------------------
         -91.79   slack (VIOLATED)                ← FAILED!
```

### Breaking it down:

#### Column 1: **Delay** (incremental delay)
- How much delay THIS gate adds
- Example: `75.38` means this gate alone adds 75.38 ns

#### Column 2: **Time** (cumulative time)
- Running total from clock edge
- Example: `101.59` means data has traveled for 101.59 ns total

#### Column 3: **Description**
- Which gate/cell in the circuit
- `^` = rising edge, `v` = falling edge
- Gate types:
  - `DFFR_X1` = D flip-flop with reset
  - `NOR2_X1` = 2-input NOR gate
  - `AOI21_X1` = AND-OR-Invert gate
  - `INV_X1` = Inverter

### The Problem Gates:
1. **`_11561_/ZN (OAI21_X1)` adds 18.15 ns** - First bottleneck
2. **`_16637_/ZN (AOI21_X1)` adds 75.38 ns** - HUGE bottleneck!

These two gates alone add **93.53 ns** of the total 101.59 ns delay!

---

## 3. The Calculation

```
Data arrival time:   101.59 ns  (how long data took to arrive)
Data required time:    9.80 ns  (when it needed to arrive)
--------------------------------
Slack:               -91.79 ns  (VIOLATED!)
```

**Why 9.80 ns and not 10.00 ns?**
- Clock period is 10.00 ns
- But the destination register needs **0.20 ns setup time**
- So data must arrive by: 10.00 - 0.20 = **9.80 ns**

---

## 4. Path Groups

The reports are organized by path types:

- **reg2reg** (Register-to-Register)
  - Most common paths
  - Your critical path is here
  - From one flip-flop to another

- **reg2out** (Register-to-Output)
  - From flip-flop to output port

- **in2reg** (Input-to-Register)
  - From input port to flip-flop

- **in2out** (Input-to-Output)
  - Combinational paths through the design

---

## 5. What This Means for Your Design

**Current Status (NP design):**
- Clock period: **10 ns** (100 MHz)
- Critical path: **101.59 ns**
- Slack: **-91.79 ns**

**To fix this, you need to either:**

1. **Increase clock period** (slower clock)
   - Need at least ~102 ns clock period
   - That's only ~9.8 MHz! (Very slow)

2. **Add pipelining** (recommended)
   - Break the long combinational path
   - Insert registers between the slow gates
   - Allows faster clock while maintaining throughput

3. **Optimize the logic**
   - Those AOI/OAI gates are very slow
   - May need to restructure the algorithm

**Why is this happening?**
- "NonPipelined" design → All computation in one cycle
- The Keccak permutation is complex
- Lots of XORs and logic create a huge combinational cloud
- 101 ns for one Keccak round is typical for non-pipelined!

---

## 6. Comparing NP vs H0 Designs

### NonPipelined (NP) - Clash Implementation
```
Critical path: 101.59 ns
Worst Slack:   -91.79 ns (at 100 MHz)
Architecture:  Iterative (1 round per cycle)
```

### H0 (high_speed_core) - VHDL Implementation
```
Critical path: 20.94 ns
Worst Slack:   -11.03 ns (at 100 MHz)
Architecture:  Iterative (1 round per cycle)
```

### Key Insight: Both Are Iterative, Not Pipelined!

**Important correction**: Both NP and H0 are **iterative** designs:
- Both do exactly **1 Keccak round per clock cycle**
- Both use a counter to track which round (0-23)
- Neither is pipelined

**Why is H0 5x faster than NP?**

The difference is in **implementation quality**, not architecture:

1. **NP (Clash)**: 101.59 ns critical path
   - Uses `keccakF1600Round` function from Permutation module
   - Synthesized combinational logic is less optimized
   - May have deeper logic levels due to how Clash translates the code

2. **H0 (VHDL)**: 20.94 ns critical path
   - Hand-written VHDL with careful optimization
   - Better synthesis results for the same mathematical operations
   - Shallower logic depth for the round function

**Both designs require 24 cycles to complete the full permutation** (0-23 rounds).

The critical path determines the maximum clock frequency:
- NP can run at ~9.8 MHz max
- H0 can run at ~47.7 MHz max
- Both take 24 cycles, so:
  - NP throughput: ~408 KHz (9.8 MHz / 24)
  - H0 throughput: ~1.99 MHz (47.7 MHz / 24)

---

## 7. Physical Timing Deep Dive

### What Actually Causes Delay?

When you see "gate delay" in the timing report, it's not just computation time. It's **physical delay** caused by:

1. **Charging/Discharging Capacitance**
   - Every wire and transistor gate has capacitance
   - Takes time to charge from 0V to VDD (or discharge)
   - Larger capacitance = slower

2. **Resistance in Wires and Transistors**
   - RC delay (resistance × capacitance)
   - Longer wires = more resistance = slower

3. **Fanout**
   - One gate driving many inputs
   - More load capacitance = slower
   - That 75.38 ns AOI21 gate likely drives many other gates

### Why AOI/OAI Gates?

- **AOI21** = AND-OR-Invert (2-input AND, 1-input OR, then invert)
- **OAI21** = OR-AND-Invert (2-input OR, 1-input AND, then invert)

These are **complex gates** that do multiple operations in one cell:
- Pro: Fewer cells needed
- Con: Higher intrinsic delay than simple gates (NAND/NOR/INV)

The 75.38 ns delay suggests this gate is either:
- Driving a huge fanout
- Part of a reconvergent path with pessimistic analysis
- Limited by routing congestion

---

## 8. Area vs. Speed Tradeoff

### The Fundamental Choice

You can optimize hardware designs for:
1. **Area** (smaller chip, fewer gates)
2. **Speed** (faster clock, more gates/registers)

You usually can't have both!

### For Keccak/SHA-3:

**Tiny & Slow (NP approach)**
- Minimal area: Just one round function + counter
- Very slow: 101 ns per round, 24 rounds = 2.4 μs per hash
- Good for: Low-power IoT devices, area-constrained ASICs

**Small & Medium (H0 approach)**
- Still iterative but better optimized
- 20 ns per round, 24 rounds = 480 ns per hash
- 5x faster than NP with likely similar area

**Large & Fast (Pipelined approach - not implemented yet)**
- 24 pipeline stages, one round per stage
- Could achieve 1 hash per 24 clock cycles at high frequency
- But 24x more registers and routing complexity

**Huge & Blazing (Full unroll - rare)**
- All 24 rounds in combinational logic
- 1 hash per cycle (if you could clock it!)
- Enormous area, difficult to synthesize

---

## 9. Practical Lessons

### For This Project

1. **NP is doing what it's supposed to**
   - It's called "NonPipelined" for a reason
   - 101 ns is actually reasonable for a full Keccak round in combinational logic
   - If you need speed, you need a different architecture

2. **H0 shows optimization matters**
   - Same iterative architecture (1 round/cycle)
   - But 5x faster due to better implementation
   - Hand-written VHDL can beat generated Clash for timing

3. **To run at 100 MHz, you need pipelining**
   - Break the round into multiple stages
   - Each stage completes in <10 ns
   - Throughput increases dramatically

### Reading Future Reports

When you run STA on any design, look for:

1. **Summary first**: Is slack positive or negative?
2. **Critical path**: Which gates are the bottleneck?
3. **Path group**: Is it reg2reg, in2out, etc.?
4. **Architectural insight**: Is this fixable with constraints, or do you need redesign?

### Common Fixes

- **Positive slack**: You're good! Can maybe increase clock frequency
- **Small negative slack** (<10% of period):
  - Try synthesis optimization
  - Adjust tool settings
  - Tighten constraints on non-critical paths
- **Large negative slack** (>50% of period):
  - Need architectural change
  - Add pipeline stages
  - Reduce logic depth
  - Simplify algorithm

---

## 10. Running STA in This Project

```bash
# Run STA on a Clash design
nix run .#sta NP

# Run STA on a VHDL design
nix run .#sta H0

# Auto-synthesis if netlist missing
# Auto-detects clock name (CLK for Clash, clk for VHDL)
# Generates reports in build/sta/<module>/reports/
```

**Reports generated:**
- `summary.rpt` - WNS, TNS, Worst Slack
- `area.rpt` - Design statistics
- `timing/overall.rpt` - Top timing paths across all groups
- `timing/reg2reg.rpt` - Register-to-register paths
- `timing/in2reg.rpt` - Input-to-register paths
- `timing/reg2out.rpt` - Register-to-output paths
- `timing/in2out.rpt` - Input-to-output paths

Each timing report has both `.rpt` (human-readable) and `.csv.rpt` (for post-processing).

---

## 11. Conclusion

**Static Timing Analysis** tells you if your design can run at the target clock frequency by analyzing all paths in your circuit and computing slack.

**Key takeaways:**
- Negative slack = timing violation
- Critical path determines max frequency
- Architecture (iterative vs pipelined) has huge impact
- Implementation quality matters (NP vs H0 shows 5x difference)
- Physical effects (RC delay, fanout) dominate at small geometries

For this project:
- **NP**: Iterative, 101.59 ns/round, max ~9.8 MHz
- **H0**: Iterative, 20.94 ns/round, max ~47.7 MHz
- Both take 24 cycles for full Keccak permutation
- Need pipelining to reach 100 MHz target
