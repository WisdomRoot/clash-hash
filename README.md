# clash-hash

## Components

### SampleNTT

Implements FIPS 203 Algorithm 6 (SampleNTT) - rejection sampling for ML-KEM polynomial coefficients.

Module location: `systemverilog/Component.SampleNTT.topEntity/Component_SampleNTT.sv`

#### Interface

| Port | Direction | Width | Description |
|------|-----------|-------|-------------|
| CLK | in | 1 | Clock |
| RST | in | 1 | Reset |
| EN | in | 1 | Enable |
| MSG_34B | in | 272 | Input: 32-byte ρ + i + j indices (bit-reversed bytes) |
| DIGEST_TREADY | in | 1 | AXI4-Stream ready (downstream) |
| DIGEST_TDATA | out | 12 | Coefficient value (bit-reversed) |
| DIGEST_TVALID | out | 1 | Valid when coefficient < 3329 |
| DIGEST_TLAST | out | 1 | Last signal (unused, always 0) |

#### Input

Fixed-size 272-bit input. No AXI4-Stream interface.

| Field | Bits | Description |
|-------|------|-------------|
| ρ     | 256  | 32-byte seed |
| i     | 8    | Row index |
| j     | 8    | Column index |

**Bit ordering (REVERSED)** `MSG_34B[271:0]`:

```
index: 271    ...  264    263   ... 23     ...  16     15   ...  8     7    ...  0
data:  ρ₀[0]  ...  ρ₀[7]  ρ₁[0] ... ρ₃₁[0] ...  ρ₃₁[7] i[0] ...  i[7]  j[0] ...  j[7]
```

> `ρₙ[0]` = LSB of byte ρₙ, `ρₙ[7]` = MSB of byte ρₙ

#### Output

AXI4-Stream interface with 12-bit data. Produces infinite coefficients via rejection sampling.

| Coefficient | `TVALID` | Action |
|-------------|----------|--------|
| 0 – 3328    | 1        | Valid, consume |
| 3329 – 4095 | 0        | Rejected, ignore |

> **Note**: `TLAST` is always 0. It is the downstream module's responsibility to count and stop after receiving 256 valid coefficients.

**Bit ordering (REVERSED)** `DIGEST_TDATA[11:0]`:

```
index:  11     10     9      8      7      6      5      4      3      2      1      0
data:   c[0]   c[1]   c[2]   c[3]   c[4]   c[5]   c[6]   c[7]   c[8]   c[9]   c[10]  c[11]
```

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
