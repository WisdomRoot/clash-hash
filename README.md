# ML-DSA in Haskell and Clash

This project implements parts of **ML-DSA (FIPS 204)** in Haskell and explores a synthesizable hardware implementation of the Number Theoretic Transform (NTT) using **Clash**.

The repository currently contains:

- a software 256-point NTT and inverse NTT in Haskell;
- key-generation and polynomial-support modules;
- a full 256-point combinational NTT in Clash;
- Montgomery modular multiplication for the hardware NTT;
- Hspec and QuickCheck tests that compare the hardware output with the software reference;
- Nix, Stack, Yosys, synthesis, benchmarking, and static-timing-analysis tooling.

## Project status

### Implemented

- Software forward NTT for 256 coefficients
- Software inverse NTT for 256 coefficients
- ML-DSA key-generation support
- Polynomial and auxiliary operations
- Full length-256 NTT in Clash
- Modular addition and subtraction
- Montgomery reduction and multiplication
- Hardware-to-software NTT comparison tests
- Nix development environment
- Clash/Yosys synthesis and analysis scripts

### In progress

- Remaining ML-DSA signing and verification operations
- Iterative NTT architecture with reusable butterfly units
- Pipelined Montgomery multiplication
- Coefficient-memory banking
- FPGA timing and resource optimization

## Main modules

### Software implementation

| Module | Purpose |
|---|---|
| `MLDSA` | Main ML-DSA module |
| `MLDSA.KeyGen` | Key generation and key encoding support |
| `MLDSA.NTT` | Software forward and inverse NTT reference |
| `MLDSA.Polynomial` | Polynomial types and operations |
| `MLDSA.Auxiliary` | Supporting ML-DSA functions |

### Clash hardware implementation

| Function | Purpose | Visibility |
|---|---|---|
| `topEntity` | Synthesizable 256-point NTT interface | Exported |
| `ntt256` | Eight-stage, length-256 NTT | Exported |
| `butterfly` | Modular NTT butterfly | Exported |
| `montgomeryMul` | Montgomery modular multiplication | Exported |
| `nttStage` | Generates one NTT stage | Internal |
| `montgomeryReduce` | Montgomery reduction | Internal |
| `addModQ` | Modular addition | Internal |
| `subModQ` | Modular subtraction | Internal |

## NTT parameters

The implementation uses the ML-DSA modulus:

```text
q = 8380417
```

The transform has:

```text
256 coefficients
8 radix-2 stages
128 butterflies per stage
1024 butterfly operations in total
```

The forward transform processes stage lengths in this order:

```text
128, 64, 32, 16, 8, 4, 2, 1
```

The zeta table is consumed as follows:

| Stage length | Zeta indices |
|---:|---:|
| 128 | 1 |
| 64 | 2-3 |
| 32 | 4-7 |
| 16 | 8-15 |
| 8 | 16-31 |
| 4 | 32-63 |
| 2 | 64-127 |
| 1 | 128-255 |

`zetas[0]` is not used by the forward NTT.

## Montgomery representation

The Clash butterfly uses Montgomery multiplication with:

```text
R = 2^24
R mod q = 16382
-q^(-1) mod R = 8380415
R^2 mod q = 196580
```

The hardware representation is:

```text
Polynomial input coefficients: ordinary values modulo q
Twiddle factors supplied to hardware: zeta * R mod q
NTT output coefficients: ordinary values modulo q
```

The butterfly computes:

```text
t    = MontgomeryReduce(zetaMont * b)
outA = a + t mod q
outB = a - t mod q
```

Because `zetaMont = zeta * R mod q`, Montgomery reduction returns the ordinary-domain product `zeta * b mod q`.

Do not pass ordinary zeta values directly to `Component.NTT`. Convert them first:

```haskell
toMontgomeryInteger :: Integer -> Integer
toMontgomeryInteger x =
  ((x `mod` 8380417) * 16382) `mod` 8380417
```

## Current hardware architecture

`Component.NTT.topEntity` currently describes the entire transform as a **combinational circuit**:

```text
input
  -> stage 128
  -> stage 64
  -> stage 32
  -> stage 16
  -> stage 8
  -> stage 4
  -> stage 2
  -> stage 1
  -> result
```

The complete zeta vector and polynomial are supplied in one input value, and the complete result is produced combinationally.

The `Clock`, `Reset`, and `Enable` ports are present in the top-level interface but are not currently used by registers or a controller.

This version is suitable for:

- functional validation;
- comparison with the software reference;
- initial synthesis experiments;
- evaluating Montgomery arithmetic.

A practical high-performance FPGA implementation will reuse one or more butterfly units over multiple cycles and add pipelining, coefficient memory, address generation, and control logic.

## Development environment

The recommended workflow uses Nix and Stack.

Enter the development shell from the repository root:

```bash
nix develop
```

The shell provides:

- GHC 9.6.6
- Stack
- Cabal
- Clash
- Yosys
- Python
- synthesis, benchmark, and STA helper commands

## Build

Inside `nix develop`:

```bash
stack build
```

## Testing

The main full-transform test module is:

```text
tests/Test/NTT256.hs
```

It verifies:

- Montgomery multiplication against ordinary modular multiplication;
- Montgomery boundary cases;
- zero-polynomial behavior;
- full-transform agreement with `MLDSA.NTT.ntt`;
- coefficient outputs remaining in `[0, q)`;
- correct omission of `zetas[0]`;
- randomized reduced-input cases.

Run all tests:

```bash
stack test
```

Run only Montgomery tests:

```bash
stack test --test-arguments="--match Montgomery"
```

Run only the full 256-point transform tests:

```bash
stack test --test-arguments="--match full 256-point"
```

Run all tests under the outer NTT description:

```bash
stack test --test-arguments="--match Component.NTT"
```

Hspec matches the text in `describe` and `it`, not the Haskell module name. Therefore, this may run zero tests unless a description contains `NTT256`:

```bash
stack test --test-arguments="--match NTT256"
```

A successful build with zero selected examples looks like:

```text
0 examples, 0 failures
```

That means compilation succeeded, but the filter did not match any test description.

## Software reference

`MLDSA.NTT` contains the software reference:

```haskell
ntt
  :: Integer
  -> Data.Vector.Vector Integer
  -> Data.Vector.Vector Integer
  -> Data.Vector.Vector Integer
```

Its arguments are:

```text
modulus
ordinary-domain zeta table
ordinary-domain input polynomial
```

The hardware tests convert zetas to Montgomery representation only for the DUT. The reference continues to receive ordinary zetas.

## Key-generation support

The key-generation work includes functions and operations for:

- seed generation and expansion;
- matrix generation;
- uniform polynomial sampling;
- public-key construction and encoding;
- private-key construction and encoding;
- polynomial and NTT support used by key generation.

Relevant modules are:

```text
MLDSA/KeyGen.hs
MLDSA/Polynomial.hs
MLDSA/Auxiliary.hs
MLDSA/NTT.hs
```

## Limitations

- The Clash NTT is fully combinational and may use substantial logic and routing resources.
- Each stage is expanded rather than executed by a reusable multi-cycle butterfly engine.
- The clock, reset, and enable inputs are currently unused.
- The design does not yet include coefficient RAM or a start/busy/done interface.
- Timing and resource usage must be checked after synthesis.
- Signing and verification are not yet complete.

  


  


