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

- **Conclusion**: Clash merged the tuple back into a single register, so we gained no hierarchy benefit and even paid extra area/logic for the tuple unpacking. `opt_clean` time stayed flat. This approach doesn’t help our synthesis-time goal; keep the original monolithic state (or find another lever).
