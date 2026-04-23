# clash-hash

## Notes

- `CP (ns)`: critical path from STA report
- `TP (Gbps) = (OutputBits / Cycles) / CP(ns)`
- `TP/A (Gbps/mm²) = TP(Gbps) * 1e6 / Area(um²)`
- `Cycles` is end-to-end cycles per operation
- `OutputBits` is the total output bits produced per operation for that component

## ML-KEM Components

### G (General)

| ID | Cycles | CP (ns) | Area (um²) | TP (Gbps) | TP/A (Gbps/mm²) | Notes |
|---|---:|---:|---:|---:|---:|---|
| [`G`](https://github.com/WisdomRoot/clash-hash/blob/main/systemverilog/Component.G.i272o512/dut.sv) | 25 | 0.65 | 24269.574 | 31.51 | 1298.33 | Baseline |
| [`G-X2`](https://github.com/WisdomRoot/clash-hash/blob/main/systemverilog/Component.GX2.i272o512/dut.sv) | 13 | 1.06 | 34245.106 | 37.16 | 1085.12 | 2 rounds/cycle |
| [`G-X3`](https://github.com/WisdomRoot/clash-hash/blob/main/systemverilog/Component.GX3.i272o512/dut.sv) | 9 | 1.49 | 43184.568 | 38.18 | 884.11 | 3 rounds/cycle |
| [`G-X4`](https://github.com/WisdomRoot/clash-hash/blob/main/systemverilog/Component.GX4.i272o512/dut.sv) | 7 | 1.92 | 52851.008 | 38.10 | 720.89 | 4 rounds/cycle |
| [`G-X6`](https://github.com/WisdomRoot/clash-hash/blob/main/systemverilog/Component.GX6.i272o512/dut.sv) | 5 | 2.77 | 71772.652 | 36.97 | 515.10 | 6 rounds/cycle |
| [`G-X8`](https://github.com/WisdomRoot/clash-hash/blob/main/systemverilog/Component.GX8.i272o512/dut.sv) | 4 | 3.61 | 90817.188 | 35.46 | 390.45 | 8 rounds/cycle |
  
- Description: General ML-KEM, with `k` as a parameter.
- In: `32-byte d || 1-byte k`
- Out: `32-byte rho || 32-byte sigma`

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

| ID | Cycles | CP (ns) | Area (um²) | TP (Gbps) | TP/A (Gbps/mm²) | Notes |
|---|---:|---:|---:|---:|---:|---|
| [`SN-O24-L6`](https://github.com/WisdomRoot/clash-hash/blob/main/systemverilog/Component.SampleNTT6.i272o24l6/dut.sv) | variable | 1.74 | 27853.126 | — | — | baseline |
| [`SN-O24-L6-X2`](https://github.com/WisdomRoot/clash-hash/blob/main/systemverilog/Component.SampleNTT6.i272o24l6x2/dut.sv) | variable | 1.81 | 37312.352 | — | — | 2 rounds/cycle |
| [`SN-O24-L6-X3`](https://github.com/WisdomRoot/clash-hash/blob/main/systemverilog/Component.SampleNTT6.i272o24l6x3/dut.sv) | variable | 1.97 | 46895.268 | — | — | 3 rounds/cycle |
| [`SN-O24-L6-X4`](https://github.com/WisdomRoot/clash-hash/blob/main/systemverilog/Component.SampleNTT6.i272o24l6x4/dut.sv) | variable | 1.92 | 56398.916 | — | — | 4 rounds/cycle |
| [`SN-O24-L6-X6`](https://github.com/WisdomRoot/clash-hash/blob/main/systemverilog/Component.SampleNTT6.i272o24l6x6/dut.sv) | variable | 2.77 | 75471.382 | — | — | 6 rounds/cycle |
| [`SN-O24-L6-X8`](https://github.com/WisdomRoot/clash-hash/blob/main/systemverilog/Component.SampleNTT6.i272o24l6x8/dut.sv) | variable | 3.90 | 94512.194 | — | — | 8 rounds/cycle |

Note: The probability of successfully emitting 2 valid coefficients per cycle is 0.99999146181.

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
