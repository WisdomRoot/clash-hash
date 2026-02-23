# clash-hash

## ML-KEM-512 Components

### G

|  In  |  Out  |  Area  |  Module  |
|------|-------|--------|----------|
| 256  | 256   | 24536  | systemverilog/Component.G512.i256o256/G512_I256_O256.sv |

### SampleNTT

|  In  |  Out  |  Area  |  Module  |
|------|-------|--------|----------|
| 272  | 24    | 28209  | systemverilog/Component.SampleNTT512.i272o24/SampleNTT512_I272_O24.sv |

#### Lookahead Analysis

* Acceptance per candidate: p = 3329/4096 ≈ 0.81274414
* Rejection per candidate: q = 767/4096 ≈ 0.18725586
* If we inspect `n = 2 + L` candidates:
    * The probability of having 0 valid candidates is `q^n`
    * The probability of having 1 valid candidate is `p * q^(n-1) * n`
    * Failure rate is `q^n + p * q^(n-1) * n`

| Lookahead (L) | Failure rate  | Expected cycles (128 pairs) |
|---:|---:|---:|
| 0 | 33.9447% | 193.78 |
| 1 | 9.2062% | 140.98 |
| 2 | 2.2576% | 130.96 |
| 3 | 0.5227% | 128.67 |
| 4 | 0.1166% | 128.15 |
| 5 | 0.0253% | 128.03 |
| 6 | 0.0054% | 128.01 |

### SamplePolyCBD+PRF

|  In  |  Out  |  Area  |  Module  |
|------|-------|--------|----------|
| 264  | 24    | 28004  | systemverilog/Component.SamplePolyCBD512.i264o24/SamplePolyCBD512_I264_O24.sv |

Timing: Permute 25, Output 90 pairs, Permute 25, Output 38 pairs (128 handshakes total).

## Scripts / Commands

```
nix develop
synth N256 -- convert Clash to Verilog & SystemVerilog and run Yosys synthesis
bench N256 -- run benchmark for N256 target
stack test -- run all tests
```

### Targets

* SampleNTT (Clash)
* N256: Non-pipelined SHA3-256 at `Hash.NonPipelined.SHA3256` (Clash)
* N256N: Non-pipelined SHA3-256 (Normal) at `Hash.NonPipelined.SHA3256Normal` (Clash)
* N256X: Non-pipelined SHAKE-256 (Clash)
* N128X: Non-pipelined SHAKE-128 (Clash)
* N128XB: Non-pipelined SHAKE-128 Byte-stream (8-bit) (Clash)
* H256: Pipelined *high_speed_core* SHA3-256 by *Team Keccak*

These targets can be used with the `synth` and `bench` commands. They are defined in `clash.json` and `vhdl.json`.

## Clash Pitfalls

- TH-generated helper functions (e.g. `mkRead`-produced `squeezeSlice`) may fail to inline when passed as higher-order arguments, which can force Clash to emit a separate SV module and increase area.
- Adding `{-# INLINE <name> #-}` to the generated helper (or replacing it with an equivalent non-TH definition) restores inlining and removes the extra module in practice.
