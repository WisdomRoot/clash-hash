# clash-hash

## ML-KEM Components

### G (General)

- ID: `G`
- Description: General ML-KEM `G`, with `k` carried in-band.
- In: `32-byte d || 1-byte k`
- Out: `32-byte rho || 32-byte sigma`
- Module: `systemverilog/Component.G.i272o512/dut.sv`
- Area: 24527.594

### G (k = 2)

- ID: `G2`
- Description: Specialization of `G` with `k = 2` for ML-KEM-512.
- In: `32-byte d`
- Out: `32-byte rho || 32-byte sigma`
- Module: `systemverilog/Component.G2.i256o512/dut.sv`
- Area: 24517.220

### G (k = 3)

- ID: `G3`
- Description: Specialization of `G` with `k = 3` for ML-KEM-768.
- In: `32-byte d`
- Out: `32-byte rho || 32-byte sigma`
- Module: `systemverilog/Component.G3.i256o512/dut.sv`
- Area: 24517.486

### G (k = 4)

- ID: `G4`
- Description: Specialization of `G` with `k = 4` for ML-KEM-1024.
- In: `32-byte d`
- Out: `32-byte rho || 32-byte sigma`
- Module: `systemverilog/Component.G4.i256o512/dut.sv`
- Area: 24517.220

### SampleNTT

- ID: `SN-O24-L2`
- Description: ML-KEM `SampleNTT`, samples 4 coefficients to outputs 2 coefficients per cycle.
- In: `32-byte rho || 1-byte i || 1-byte j`
- Out: `12-bit coeff0 || 12-bit coeff1`
- Module: `systemverilog/Component.SampleNTT.i272o24l2/dut.sv`
- Area: 26203.394

Note: The probability of successfully emitting 2 valid coefficients per cycle is 99.2732% in this implementation.

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
