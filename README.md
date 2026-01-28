# clash-hash

## Components

### SampleNTT2

Implements FIPS 203 Algorithm 6 (SampleNTT) - rejection sampling for ML-KEM polynomial coefficients.

Module location: `systemverilog/Component.SampleNTT2.topEntity/Component_SampleNTT2.sv`

#### Interface

| Port | Direction | Width | Description |
|------|-----------|-------|-------------|
| CLK | in | 1 | Clock |
| RST | in | 1 | Reset |
| EN | in | 1 | Enable |
| SEED_TDATA | in | 272 | 32-byte ρ + i + j indices (bit-reversed bytes) |
| SEED_TVALID | in | 1 | Input valid |
| SEED_TLAST | in | 1 | Input last (unused) |
| COEFF_TREADY | in | 1 | Downstream ready |
| COEFF_TDATA | out | 24 | Coefficient pair (2 × 12-bit, bit-reversed) |
| COEFF_TVALID | out | 1 | Valid when coefficient < 3329 |
| COEFF_TLAST | out | 1 | Last signal (unused, always 0) |
| SEED_TREADY | out | 1 | Ready to accept input |

#### Input

AXI4-Stream interface with 272-bit data.

| Field | Bits | Description |
|-------|------|-------------|
| ρ     | 256  | 32-byte seed |
| i     | 8    | Row index |
| j     | 8    | Column index |

**Bit ordering (REVERSED)** `SEED_TDATA[271:0]`:

```
index: 271    ...  264    263   ... 23     ...  16     15   ...  8     7    ...  0
data:  ρ₀[0]  ...  ρ₀[7]  ρ₁[0] ... ρ₃₁[0] ...  ρ₃₁[7] i[0] ...  i[7]  j[0] ...  j[7]
```

> `ρₙ[0]` = LSB of byte ρₙ, `ρₙ[7]` = MSB of byte ρₙ

#### Output

AXI4-Stream interface with 24-bit data (2 × 12-bit coefficient pairs). Produces infinite coefficient pairs via rejection sampling.

| Coefficient | `COEFF_TVALID` | Action |
|-------------|----------------|--------|
| 0 – 3328    | 1              | Valid, consume |
| 3329 – 4095 | 0              | Rejected, ignore |

> **Note**: `COEFF_TLAST` is always 0. It is the downstream module's responsibility to count and stop after receiving 256 valid coefficients.

**Bit ordering (REVERSED)** `COEFF_TDATA[23:0]`:

```
index:  23-12 (newer)                                         11-0 (older)
data:   c_new[0] c_new[1] ... c_new[10] c_new[11]             c_old[0] c_old[1] ... c_old[10] c_old[11]
```

> bits 23:12 = newer valid coefficient, bits 11:0 = older valid coefficient (buffered or first of pair)
> `c[0]` = LSB of coefficient, `c[11]` = MSB of coefficient

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
* N256X: Non-pipelined SHAKE-256 (Clash)
* N128X: Non-pipelined SHAKE-128 (Clash)
* N128XB: Non-pipelined SHAKE-128 Byte-stream (8-bit) (Clash)
* H256: Pipelined *high_speed_core* SHA3-256 by *Team Keccak*

These targets can be used with the `synth` and `bench` commands. They are defined in `clash.json` and `vhdl.json`.
