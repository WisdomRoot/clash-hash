## Versions

* Baseline: original monolithic design
* AXI-stream: replace the IO with AXI4-Stream
* Separate modules: top entity + permutation as separate modules
* Pre-synth permutation: pre-synthesise the permutation module and reuse mapped netlist
* Explicit pass list:
    1. select -module KeccakF200_SHA3 - Select only top module
    2. proc; opt; opt_clean; wreduce; share -aggressive; opt; opt_clean; techmap; opt; opt_clean; dfflibmap; abc; clean - All transforms run on selected module only
    3. write_verilog - Write output (still with top-only selection)
    4. select -clear - Clear selection to see all modules
    5. stat -top KeccakF200_SHA3 - Generate statistics for full hierarchy
    - The permutation macro KeccakF200_Round is never touched by optimization passes
    - Only the top module KeccakF200_SHA3 goes through the transformation pipeline
    - The final stat -top can traverse the full hierarchy to count all cells and calculate total area
* ADT-based FSM: cleanup the boolean soup in the FSM
* Fix writeback: write the permutation output back
* Rate-only XOR: only XOR the rate bits during absorb/squeeze
* Gather/scatter: decouple I/O bus width from rate/capacity
* Reintroduce `Vec`: reintroduce the `Vec` type permutation

Note: after `Pre-synth permutation`, results are shown as two values: first is for the pre-synth state (combinational permutation), second is for the whole design

How numbers are collected:
  1. Verilog generation:
    - stack run clash -- KeccakF200.Permutation.topEntity --verilog
    - stack run clash -- KeccakF200.topEntity --verilog
  2. Synthesis:
    - nix develop --command synth -- KeccakF200.Permutation.topEntity
    - nix develop --command synth -- KeccakF200.topEntity

Table format: <perm_vlog>s / <top_vlog>s | <total_synth>s (<perm_synth>s / <top_synth>s) | <perm_area> (0%) / <total_area> (<seq_pct>%)

### F200

| Version | Verilog Gen Time | Netlist Synth Time | Chip Area (µm²) (sequential percentage) |
|---------|------------------|--------------------|-----------------------------------------|
| **Baseline** | 2.254s | 28s | 6730.598 |
| **AXI-stream** | 2.393s | 28s | 4815.132 (30.38%) |
| **Separate modules** | 2.443s | 24.86s | 4719.638 (31.00%) |
| **Pre-synth permutation** | 1.760s / 2.430s | 22.56s (22.17s / 0.39s) | 1917.062 (0%) / 4726.022 (30.96%) |
| **Explicit pass list** | 1.760s / 2.430s | 19.02s (14.35s / 4.67s) | 1950.046 (0%) / 4531.310 (32.40%) |
| **ADT-based FSM** | 1.289s / 2.340s | 19.31s (14.12s / 5.19s) | 1948.450 (0%) / 4811.940 (30.18%) |
| **Fix writeback** | 1.289s / 2.360s | 19.49s (14.25s / 5.24s) | 1948.450 (0%) / 5318.138 (27.31%) |
| **Rate-only XOR** | 1.916s / 2.474s | 18.94s (13.85s / 5.09s) | 1948.450 (0%) / 5435.976 (26.72%) |
| **Gather/scatter** | 2.678s / 1.672s | 19.36s (14.19s / 5.17s) | 1948.450 (0%) / 5435.976 (26.72%) |
| **Reintroduce Vec** | 1.065s / 0.648s | 1.66s (1.15s / 0.51s) | 1948.45 (0%) / 5337.556 (27.21%) |

### F400

| Version | Verilog Gen Time | Netlist Synth Time | Chip Area (µm²) (sequential percentage) |
|---------|------------------|--------------------|-----------------------------------------|
| **Baseline** | 3.637s | 115s | 13371.288 |
| **AXI-stream** | 3.886s | 129s | 8253.980 (30.62%) |
| **Separate modules** | 4.140s | 112.05s | 8192.800 (30.84%) |
| **Pre-synth permutation** | 3.198s / 3.980s | 95.45s (94.84s / 0.61s) | 3828.006 (0%) / 8268.876 (30.56%) |
| **Explicit pass list** | 3.030s / 3.934s | 80.37s (61.19s / 19.18s) | 3884.132 (0%) / 8011.122 (31.61%) |
| **ADT-based FSM** | 2.471s / 3.810s | 73.12s (54.87s / 18.25s) | 3866.576 (0%) / 8211.686 (30.64%) |
| **Fix writeback** | 2.478s / 3.842s | 73.27s (54.82s / 18.45s) | 3866.576 (0%) / 9387.938 (26.80%) |
| **Rate-only XOR** | 3.319s / 3.997s | 71.47s (54.70s / 17.77s) | 3866.576 (0%) / 9553.656 (26.34%) |
| **Gather/scatter** | 2.616s / 0.981s | 155.81s (55.08s / 100.73s) | 3866.576 (0%) / 13435.660 (26.33%) |
| **Reintroduce Vec** | 2.162s / 0.922s | 4.07s (2.64s / 1.43s) | 3893.974 (0%) / 13845.832 (25.55%) |

### F800

| Version | Verilog Gen Time | Netlist Synth Time | Chip Area (µm²) (sequential percentage) |
|---------|------------------|--------------------|-----------------------------------------|
| **Baseline** | 6.722s | 739s | 24151.204 |
| **AXI-stream** | 6.726s | 647s | 15364.958 (30.30%) |
| **Separate modules** | 7.368s | 604.12s | 14909.034 (31.22%) |
| **Pre-synth permutation** | 5.926s / 7.515s | 546.75s (545.63s / 1.12s) | 7486.038 (0%) / 14989.100 (31.06%) |
| **Explicit pass list** | 5.984s / 7.341s | 424.96s (328.92s / 96.04s) | 7746.452 (0%) / 14952.392 (31.17%) |
| **ADT-based FSM** | 5.107s / 7.069s | 358.25s (269.55s / 88.70s) | 7591.374 (0%) / 14934.304 (31.10%) |
| **Fix writeback** | 5.244s / 7.256s | 362.24s (272.50s / 89.74s) | 7591.374 (0%) / 17293.192 (26.86%) |
| **Rate-only XOR** | 5.928s / 7.143s | 346.96s (261.44s / 84.52s) | 7591.374 (0%) / 17293.192 (26.86%) |
| **Gather/scatter** | 5.279s / 1.590s | 499.68s (271.89s / 227.79s) | 7591.374 (0%) / 27149.822 (25.88%) |
| **Reintroduce Vec** | 5.03s / 1.604s | 10.29s (7.0s / 3.29s) | 7789.278 (0%) / 26021.716 (27.01%) |

### F1600

| Version | Verilog Gen Time | Netlist Synth Time | Chip Area (µm²) (sequential percentage) |
|---------|------------------|--------------------|-----------------------------------------|
| **Rate-only XOR** | 12.252s / 14.329s | 1909.02s (1367.14s / 541.88s) | 14943.614 (0%) / 32848.606 (27.10%) |
| **Gather/scatter** | 11.040s / 2.810s | 2497.90s (1373.44s / 1124.46s) | 14943.614 (0%) / 51010.022 (28.13%) |
| **Reintroduce Vec** | 9.429s / 2.843s | 26.20s (16.18s / 10.02s) | 15214.136 (0%) / 49111.58 (29.22%) |

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

**Results (F200):**

| Metric | Baseline (Separate modules) | Top Entity Only | First-Time Total |
|--------|----------------------------|-----------------|------------------|
| **Verilog Gen** | 2.443s | 2.430s | 1.760s + 2.430s = 4.190s (+71%) |
| **Netlist Synth** | 24.86s | 0.39s | 22.17s + 0.39s = 22.56s (-9%) |
| **Chip Area** | 4,719.638 µm² | 4,726.022 µm² | +6.38 µm² (+0.1%) |

**Analysis:**

**Incremental build optimization** - Top entity synthesis drops from 24.86s → 0.39s (98.4% faster) when reusing cached permutation

**First-time build performance** - Modest improvement: 22.56s vs 24.86s baseline (9% faster netlist synthesis), but 71% slower Verilog generation due to separate module builds

**QoR preserved** - Area increased by only 0.1%, well within acceptable margin

**Hierarchy maintained** - Design still shows `KeccakF200_SHA3 → KeccakF200_Round` in hierarchy

**How it works:**

The permutation block (KeccakF200_Round) contains complex combinational logic (1,306 cells) that takes Yosys ~22 seconds to optimize. By pre-mapping it to liberty cells and marking it `keep_hierarchy`, Yosys treats it as a black box and only optimizes the top-level FSM glue logic, which is much faster.

**Conclusion:**

This is an **incremental build optimization**, not a first-build speedup. Benefits:
1. First-time synthesis: ~10% faster (mainly due to avoiding re-optimization)
2. Incremental builds: ~98% faster when only FSM changes
3. The permutation must be stable (not changing frequently)

**Use cases:**
- Iterative development on FSM logic while permutation stays fixed
- Large stable sub-modules that don't change often
- Works for all Keccak variants (F200, F400, F800, F1600)

**Files Modified:**
- `scripts/synth_verilog.py` - Auto-detects and uses pre-mapped macros for "_SHA3" top entities

---

## TODO

### Pre-synthesised permutation macro (reuse mapped netlist)

TODO:
- [ ] Auto-rebuild the permutation macro if the mapped netlist is missing or stale (mtime/hash check) before top synth.
- [ ] Replicate macro reuse for F400/F800/F1600 (verify round names and paths).
  - [ ] Verify top logs: no "Area for cell type … unknown" for the round; capture CPU time delta.

### Preserve round hierarchy (no flatten across permutation)

- Keep current pass list but insert:
  - `setattr -mod -name keep_hierarchy 1 KeccakF200_Round`
  - Use `synth -top KeccakF200_SHA3 -noflatten` (or equivalent hand-rolled flow without flattening).
- Hypothesis: Limits cross-boundary clean/expr work; reduces cumulative `opt_clean` time while keeping mapping quality.

TODO:
  - [ ] Use `-noflatten` for the top `synth` invocation and A/B measure CPU time/area.
  - [ ] Extend `keep_hierarchy` insertion to F400/F800/F1600 round modules too.

  ### Script integration (automatic macro reuse)

- Enhance `scripts/synth_verilog.py` to:
  - Detect and (re)build permutation mapped netlist if not present or stale.
  - Automatically `read_verilog -lib` the permutation module before the top.
  - Set `keep_hierarchy` on the permutation to preserve boundary.
- Hypothesis: Consistent synth time reduction without changing the optimisation flow for the top.

TODO:
  - [ ] Robustly derive permutation label/path from a top label (F200/F400/F800/F1600).
  - [ ] If the mapped netlist is missing or stale, synthesize permutation first.
  - [ ] Prepend `read_verilog` of the mapped macro and `setattr keep_hierarchy` before reading the top.
  - [ ] Verify logs (no "area unknown"), hierarchy lists exactly one round instance, and record CPU/time deltas in this doc.

### Clash-side RTL quality improvements (reduce Yosys cleanup work)

- Lane-level permutation
  - [ ] Refactor f[200] permutation to operate on `Vec 5 (Vec 5 (BitVector 8))` internally.
  - [ ] Implement θ/ρ/π/χ/ι as lane transforms; pack/unpack only at topEntity boundary.
  - [ ] Avoid per-bit `replaceBit`/`ifoldl` over 200 bits.

- Avoid wide intermediates
  - [ ] In absorb/squeeze, operate on `BitVector rate` (LSBs) or lane `BitVector 8` and only widen at the edge.
  - [ ] Remove zero-extending to 200 bits when only low `rate` bits are touched.

- Split FSM into explicit registers
  - [ ] Replace single mealy state record with explicit `register`/`regEn` regs for: permutation state, round counter, phase/active, pad flags/block, current block and remaining count.
  - [ ] Compute per-register enables; keep multiple small always blocks.

- Reduce boolean soup
  - [ ] Use compact sum type/case for phase logic; precompute guards; drive per-register enables to avoid large decoders.

- Control inlining where helpful
  - [ ] Consider `NOINLINE` on large lane helpers to keep nets named and avoid duplicating large expressions.
  - [ ] Optionally explore `-fclash-inline-*` flags during development; keep a single blessed config in CI.
