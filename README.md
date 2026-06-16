# clash-hash

## Notes

- `CP (ns)`: critical path from STA report
- `TP@CP (Gbps) = (OutputBits / Cycles) / CP(ns)`
- `TP/A@CP (Gbps/mm²) = TP@CP(Gbps) * 1e6 / Area(um²)`
- `TP@5ns (Gbps) = (OutputBits / Cycles) / 5`
- `TP/A@5ns (Gbps/mm²) = TP@5ns(Gbps) * 1e6 / Area(um²)`
- `Cycles` is end-to-end cycles per operation
- `OutputBits` is the total output bits produced per operation for that component
- For variable-cycle streamers, each family section defines how `E[Cycles]` is modeled.

## ML-KEM Components

### G (General)

- Description: General ML-KEM, with `k` as a parameter.
- In: `32-byte d || 1-byte k`
- Out: `32-byte rho || 32-byte sigma`

| ID | Cycles | CP | Area | TP@CP | TP/A@CP | TP@5ns | TP/A@5ns | Notes |
|---|---:|---:|---:|---:|---:|---:|---:|---|
| [`G`](https://github.com/WisdomRoot/clash-hash/blob/main/systemverilog/Component.G.i272o512/dut.sv) | 25 | 0.65 | 24269.574 | 31.51 | 1298.24 | 4.10 | 168.77 | Baseline |
| [`G-X2`](https://github.com/WisdomRoot/clash-hash/blob/main/systemverilog/Component.GX2.i272o512/dut.sv) | 13 | 1.06 | 34245.106 | 37.16 | 1084.98 | 7.88 | 230.02 | 2 rounds/cycle |
| [`G-X3`](https://github.com/WisdomRoot/clash-hash/blob/main/systemverilog/Component.GX3.i272o512/dut.sv) | 9 | 1.49 | 43184.568 | 38.18 | 884.12 | 11.38 | 263.47 | 3 rounds/cycle |
| [`G-X4`](https://github.com/WisdomRoot/clash-hash/blob/main/systemverilog/Component.GX4.i272o512/dut.sv) | 7 | 1.92 | 52851.008 | 38.10 | 720.80 | 14.63 | 276.79 | 4 rounds/cycle |
| [`G-X6`](https://github.com/WisdomRoot/clash-hash/blob/main/systemverilog/Component.GX6.i272o512/dut.sv) | 5 | 2.77 | 71772.652 | 36.97 | 515.06 | 20.48 | 285.35 | 6 rounds/cycle |
| [`G-X8`](https://github.com/WisdomRoot/clash-hash/blob/main/systemverilog/Component.GX8.i272o512/dut.sv) | 4 | 3.61 | 90817.188 | 35.46 | 390.42 | 25.60 | 281.88 | 8 rounds/cycle |
  
### G (k = 2)

- ID: [`G2`](https://github.com/WisdomRoot/clash-hash/blob/main/systemverilog/Component.G2.i256o512/dut.sv)
- Description: Specialization of `G` with `k = 2` for ML-KEM-512.
- In: `32-byte d`
- Out: `32-byte rho || 32-byte sigma`
- Cycles: `25`
- CP: `0.64 ns`
- Area: `24258.402 um²`
- TP: `32.00 Gbps`
- TP/A: `1319.13 Gbps/mm²`

### G (k = 3)

- ID: [`G3`](https://github.com/WisdomRoot/clash-hash/blob/main/systemverilog/Component.G3.i256o512/dut.sv)
- Description: Specialization of `G` with `k = 3` for ML-KEM-768.
- In: `32-byte d`
- Out: `32-byte rho || 32-byte sigma`
- Cycles: `25`
- CP: `0.64 ns`
- Area: `24259.466 um²`
- TP: `32.00 Gbps`
- TP/A: `1319.07 Gbps/mm²`

### G (k = 4)

- ID: [`G4`](https://github.com/WisdomRoot/clash-hash/blob/main/systemverilog/Component.G4.i256o512/dut.sv)
- Description: Specialization of `G` with `k = 4` for ML-KEM-1024.
- In: `32-byte d`
- Out: `32-byte rho || 32-byte sigma`
- Cycles: `25`
- CP: `0.64 ns`
- Area: `24258.402 um²`
- TP: `32.00 Gbps`
- TP/A: `1319.13 Gbps/mm²`

### SampleNTT (lookahead = 2)

- ID: `SN-O24-L2`
- Description: ML-KEM `SampleNTT`, samples 4 coefficients to outputs 2 coefficients per cycle.
- In: `32-byte rho || 1-byte i || 1-byte j`
- Out: `12-bit coeff0 || 12-bit coeff1`
- Module: `systemverilog/Component.SampleNTT.i272o24l2/dut.sv`
- Area: 26203.394

Note: The probability of successfully emitting 2 valid coefficients per cycle is 0.99273073.

### SampleNTT (lookahead = 4)

- ID: `SN-O24-L4`
- Description: ML-KEM `SampleNTT`, samples 6 coefficients to output 2 coefficients per cycle.
- In: `32-byte rho || 1-byte i || 1-byte j`
- Out: `12-bit coeff0 || 12-bit coeff1`
- Module: `systemverilog/Component.SampleNTT4.i272o24l4/dut.sv`
- Area: 27400.660

Note: The probability of successfully emitting 2 valid coefficients per cycle is 0.99975214116.

### SampleNTT (lookahead = 6)

- Description: ML-KEM `SampleNTT`, samples 8 coefficients to output 2 coefficients per cycle.
- In: `32-byte rho || 1-byte i || 1-byte j`
- Out: `12-bit coeff0 || 12-bit coeff1`

| ID | E[Cycles] | CP | Area | E[TP@CP] | E[TP/A@CP] | E[TP@5ns] | E[TP/A@5ns] | Notes |
|---|---:|---:|---:|---:|---:|---:|---:|---|
| [`SN-O24-L6`](https://github.com/WisdomRoot/clash-hash/blob/main/systemverilog/Component.SampleNTT6.i272o24l6/dut.sv) | 201.83 | 1.58 | 26271.490 | 9.63 | 366.69 | 3.04 | 115.87 | baseline |
| [`SN-O48-L6`](https://github.com/WisdomRoot/clash-hash/blob/main/systemverilog/Component.SampleNTT6O48.i272o48l6/dut.sv) | 137.53 | 2.33 | 28037.464 | 9.59 | 341.91 | 4.47 | 159.33 | 4 coefficients/cycle |
| [`SN-O24-L6-X2`](https://github.com/WisdomRoot/clash-hash/blob/main/systemverilog/Component.SampleNTT6.i272o24l6x2/dut.sv) | 165.41 | 1.81 | 37312.352 | 10.26 | 274.99 | 3.71 | 99.55 | 2 rounds/cycle |
| [`SN-O24-L6-X3`](https://github.com/WisdomRoot/clash-hash/blob/main/systemverilog/Component.SampleNTT6.i272o24l6x3/dut.sv) | 153.28 | 1.97 | 46895.268 | 10.17 | 216.95 | 4.01 | 85.48 | 3 rounds/cycle |
| [`SN-O24-L6-X4`](https://github.com/WisdomRoot/clash-hash/blob/main/systemverilog/Component.SampleNTT6.i272o24l6x4/dut.sv) | 147.21 | 1.92 | 56398.916 | 10.87 | 192.72 | 4.17 | 74.00 | 4 rounds/cycle |
| [`SN-O24-L6-X6`](https://github.com/WisdomRoot/clash-hash/blob/main/systemverilog/Component.SampleNTT6.i272o24l6x6/dut.sv) | 141.14 | 2.77 | 75471.382 | 7.86 | 104.12 | 4.35 | 57.68 | 6 rounds/cycle |
| [`SN-O24-L6-X8`](https://github.com/WisdomRoot/clash-hash/blob/main/systemverilog/Component.SampleNTT6.i272o24l6x8/dut.sv) | 138.10 | 3.90 | 94512.194 | 5.70 | 60.35 | 4.45 | 47.07 | 8 rounds/cycle |

Notes:
- These are full-job metrics for producing `256` coefficients (`OutputBits = 256 * 12 = 3072`).
- O24 variants use `E[Cycles] = 1 + C_perm * E[PermCount] + 128 / r`
  where `E[PermCount] ≈ 3.0344` and `r = 0.99999146181`.
- `SN-O48-L6` uses the same lookahead-6 candidate screening but emits `12-bit coeff0 || coeff1 || coeff2 || coeff3`; its expected cycles use the quad-output model from `python3 scripts/markov_emit_rate.py --n 8 --emit-width 4 --output-count 64 --perm-cycles 24`.
- `C_perm` is permute cycles per permutation block: `24, 12, 8, 6, 4, 3` for `L6, X2, X3, X4, X6, X8`.
- `r` is computed from the Markov model via `python3 scripts/markov_emit_rate.py --n 8`.

### SamplePolyCBD+PRF (General)

- ID: `CBD-O24`
- Description: Composition of `PRF` and `SamplePolyCBD`, 2 coefficients per cycle, `η₁` carried in-band.
- In: `32-byte seed || 1-byte nonce || 1-byte η₁`
- Out: `12-bit coeff0 || 12-bit coeff1`
- Module: `systemverilog/Component.SamplePolyCBD.i272o24/dut.sv`
- Area: 35512.330

### SamplePolyCBD+PRF (η₁ = 2)

- ID: `CBD2-O24`
- Description: Composition of `PRF` and `SamplePolyCBD`, specialized for `η₁ = 2`, 2 coefficients per cycle.
- In: `32-byte seed || 1-byte nonce`
- Out: `12-bit coeff0 || 12-bit coeff1`
- Module: `systemverilog/Component.SamplePolyCBD2.i264o24/dut.sv`
- Area: 35314.692

### SamplePolyCBD+PRF (η₁ = 3)

- ID: `CBD3-O24`
- Description: Composition of `PRF` and `SamplePolyCBD`, specialized for `η₁ = 3`, 2 coefficients per cycle.
- In: `32-byte seed || 1-byte nonce`
- Out: `12-bit coeff0 || 12-bit coeff1`
- Module: `systemverilog/Component.SamplePolyCBD3.i264o24/dut.sv`
- Area: 35253.512

## Scripts / Commands

```
nix develop
synth N256 -- convert Clash to Verilog & SystemVerilog and run Yosys synthesis
bench N256 -- run benchmark for N256 target
stack test -- run all tests
```
<!-- 
### Targets

* N256: Non-pipelined SHA3-256 at `Hash.NonPipelined.SHA3256` (Clash)
* N256N: Non-pipelined SHA3-256 (Normal) at `Hash.NonPipelined.SHA3256Normal` (Clash)
* SHAKE3-256: Non-pipelined SHAKE-256 (normal order, Clash)
* N128X: Non-pipelined SHAKE-128 (Clash)
* H256: Pipelined *high_speed_core* SHA3-256 by *Team Keccak*

These targets can be used with the `synth` and `bench` commands. They are defined in `clash.json` and `vhdl.json`.
 -->

## Clash Pitfalls

- TH-generated helper functions (e.g. `mkRead`-produced `squeezeSlice`) may fail to inline when passed as higher-order arguments, which can force Clash to emit a separate SV module and increase area.
- Adding `{-# INLINE <name> #-}` to the generated helper (or replacing it with an equivalent non-TH definition) restores inlining and removes the extra module in practice.
