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
| **Reintroduce Vec** | 3.05s / 0.819s | 3.88s (2.45s / 1.43s) | 3917.116 (0%) / 13868.974 (25.51%) |

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
| **Reintroduce Vec** | 6.582s / 1.464s | 9.22s (5.92s / 3.3s) | 7799.918 (0%) / 26032.356 (27.0%) |

### F1600

| Version | Verilog Gen Time | Netlist Synth Time | Chip Area (µm²) (sequential percentage) |
|---------|------------------|--------------------|-----------------------------------------|
| **Rate-only XOR** | 12.252s / 14.329s | 1909.02s (1367.14s / 541.88s) | 14943.614 (0%) / 32848.606 (27.10%) |
| **Gather/scatter** | 11.040s / 2.810s | 2497.90s (1373.44s / 1124.46s) | 14943.614 (0%) / 51010.022 (28.13%) |
| **Reintroduce Vec** | 13.224s / 2.719s | 24.02s (13.94s / 10.08s) | 15246.854 (0%) / 49144.298 (29.2%) |

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
