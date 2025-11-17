## Failed Experiments

### Pipelined Keccak-f[200] permutation (4-stage round)

- **Date/Context**: After moving AXI sponge + permutation into separate modules, we tried reducing Yosys runtime by pipelining each Keccak-f[200] round into four registered stages (θ → ρπ → χ → ι).
- **Hypothesis**: Breaking deep combinational paths into shallow stages would make Yosys's `opt_clean` pass faster by simplifying the logic cones it needs to analyze.
- **Implementation**:
  - Introduced a `RoundStage` sum type with `StageTheta`, `StageRhoPi`, `StageChi`, and `StageIota`.
  - Added `pipelinedPermutation` and `pipelinedTopEntity` in `src/KeccakF200/Permutation.hs`, implementing a Mealy FSM that spends four cycles per round (18 rounds → 72 cycles total) and exposes a `busy` flag.
  - Generated separate Verilog (`verilog/KeccakF200.Permutation.pipelinedTopEntity/`) and ran `nix run .#synth -- KeccakF200.topEntity`.
- **Outcome**:
  - Cells grew from 1 306 to 3 344.
  - Sequential elements went from 0 to 1 101 (22.41 % of area).
  - Chip area jumped from 1 917 µm² to 4 913 µm².
  - Yosys CPU time *increased* slightly (22.38 s → 23.83 s), with `opt_clean` still ~55 % of runtime.
- **Trade-offs observed**:

  | Metric              | Single-Cycle Round | Pipelined (4 stages) |
  |---------------------|--------------------|----------------------|
  | Cells               | 1,306              | 3,344                |
  | Sequential Elements | 0 (0%)             | 1,101 (22.41%)       |
  | Chip Area           | 1,917 µm²          | 4,913 µm²            |
  | Yosys CPU Time      | 22.38s             | 23.83s               |
  | opt_clean Time      | 54% (13s)          | 55% (14s)            |

- **Conclusion**: The pipelined version was strictly worse for our goals—larger, slower to synthesise, and no faster in Yosys. It only makes sense if we ever need significantly higher fmax and can afford >2.5× area + 1.1k extra flops.

### Split sponge state into `PermState`/`ControlState`

- **Goal**: Separate the 200-bit permutation data from the FSM control flags so Clash/Yosys wouldn't have to optimise a 338-bit monolithic register every cycle.
- **Hypothesis**: Independent data path (200 bits) and control path (narrow flags) registers would allow Yosys to optimize each separately, reducing optimization time.
- **Implementation**: Replaced `SpongeState` with two records, updated the Mealy machine to carry `(PermState, ControlState)`, and rebuilt.
- **Synthesis comparison**:

  | Metric              | Before (Monolithic) | After (Split State) | Change          |
  |---------------------|---------------------|---------------------|-----------------|
  | Cells               | 3,014               | 3,108               | +94 (+3.1%)     |
  | Sequential Elements | 1,463 (31.00%)      | 1,463 (30.47%)      | 0 (same)        |
  | Chip Area           | 4,719.638 µm²       | 4,800.768 µm²       | +81 µm² (+1.7%) |
  | Yosys CPU Time      | 25.19s              | 25.72s              | +0.53s (+2.1%)  |
  | opt_clean Time      | 54% (15s)           | 54% (15s)           | 0 (same)        |

- **Conclusion**: Clash merged the tuple back into a single register, so we gained no hierarchy benefit and even paid extra area/logic for the tuple unpacking. `opt_clean` time stayed flat. This approach doesn't help our synthesis-time goal; keep the original monolithic state (or find another lever).

---

## Successful Experiment: Pre-Mapped Permutation Macro ✅

**Goal:** Eliminate redundant optimization of the permutation block by pre-synthesizing it once and reusing the mapped netlist.

**Hypothesis:** The 15-second `opt_clean` bottleneck is spent optimizing KeccakF200_Round's combinational logic. Pre-mapping it to liberty cells and marking it `keep_hierarchy` should let Yosys skip re-optimizing those internals.

**Implementation:**

1. **Pre-synthesize permutation:** `nix develop -c python3 scripts/synth_verilog.py KeccakF200.Permutation.topEntity`
   - Generates `build/synth/KeccakF200.Permutation.topEntity/netlist/KeccakF200_Round.mapped.v`
   - Contains 1,306 liberty cells (XNOR2_X1, XOR2_X1, NAND2_X1, etc.)

2. **Modified `scripts/synth_verilog.py`:**
   - Detects when synthesizing top entities containing "_SHA3" (e.g., KeccakF200_SHA3)
   - Derives permutation module name (KeccakF200_Round) and checks for pre-mapped netlist
   - If found:
     - `read_liberty -lib` (so Yosys knows about NanGate45 cells)
     - `read_verilog <mapped_round.v>` (load pre-mapped macro)
     - `setattr -set keep_hierarchy 1 -mod KeccakF200_Round` (prevent re-optimization)
     - Skip reading raw permutation Verilog from dependencies

3. **Run top synthesis:** `nix develop -c python3 scripts/synth_verilog.py KeccakF200.topEntity`

**Results:**

| Metric | Baseline (No Macro) | With Pre-Mapped Macro | Improvement |
|--------|--------------------|-----------------------|-------------|
| **Yosys CPU Time** | 25.19s | **0.39s** | **-98.5%** ⚡ |
| **opt_clean Time** | 15s (54%) | 0s (14%) | **~15s saved** |
| **Cells** | 3,014 | 3,033 | +19 (+0.6%) |
| **Sequential Elements** | 1,463 | 1,463 | 0 (same) |
| **Chip Area** | 4,719.638 µm² | 4,726.022 µm² | +6.38 µm² (+0.1%) |

**Analysis:**

✅ **Massive synthesis time reduction** - 64× faster! From 25.19s → 0.39s

✅ **opt_clean eliminated as bottleneck** - Went from 54% (15s) to 14% (0s) because Yosys no longer optimizes the permutation internals

✅ **QoR preserved** - Area increased by only 0.1%, well within acceptable margin

✅ **Hierarchy maintained** - Design still shows `KeccakF200_SHA3 → KeccakF200_Round` in hierarchy

✅ **Scalable** - Every subsequent synthesis of the top entity saves ~25 seconds

**Root Cause of Success:**

The permutation block (KeccakF200_Round) contains complex combinational logic (1,306 cells) that took Yosys 15 seconds to optimize via repeated `opt_clean` passes. By pre-mapping it to liberty cells and marking it as `keep_hierarchy`, Yosys treats it as a black box and only optimizes the top-level FSM glue logic, which is much faster.

**Conclusion:**

**First successful optimization!** This approach works because:
1. The permutation is large enough (1,306 cells) to be worth caching
2. The permutation is reused without changes across synthesis runs
3. The `keep_hierarchy` attribute prevents Yosys from flattening and re-optimizing

**Recommended for:**
- ✅ All Keccak variants (F200, F400, F800, F1600)
- ✅ Any design with large, stable sub-modules that don't change often
- ✅ Iterative synthesis workflows (changing top-level FSM but not the permutation)

**Files Modified:**
- `scripts/synth_verilog.py` - Auto-detects and uses pre-mapped macros for "_SHA3" top entities
