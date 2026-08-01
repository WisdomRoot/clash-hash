# MLDSA

## In progress

- CLASH implementation of ML-DSA
- Software of NIST ML-DSA (Haskell)

  | `Module` | `Description` |
  |---|---|
  | `Sign`   | Sign Generation for ML-DSA |
  | `Verify` | Verify Sign Generation for ML-DSA |
  | `MLDSA` | Main MLDSA module for ML-DSA |

## Finished

- Full length-256 NTT (CLASH)

  | `Component` | `Description` |
  |---|---|
  | `topEntity` | NTT for ML-DSA |
  | `butterfly` | Butterfly for ML-DSA |
  | `ntt256` | NTT for ML-DSA |
  | `nttStage` | NTT for ML-DSA |
  | `montgomeryMul` | Montgomery multiplication for ML-DSA |
  | `addModQ` | Modular addition for ML-DSA |
  | `subModQ` | Modular subtraction for ML-DSA |
  | `montgomeryReduce` | Montgomery reduction for ML-DSA |
  | `NTT.hs` | NTT and inverse NTT for ML-DSA |

- Software of NIST ML-DSA (Haskell)

  | `Module` | `Description` |
  |---|---|
  | `NTT`    | Number Theoretic Transform for ML-DSA |
  | `KeyGen` | Key Generation for ML-DSA |

### Key Generation for ML-DSA

- Include generate_keypair, public_key_encoding, private_key_encoding, seed_expand, matrix_generation, matrix_uniform, public_key_pack, private_key_pack
- Used files:

  | `Module` | `Description` |
  |---|---|
  | `MLDSA.KeyGen.hs` | Key Generation for ML-DSA |
  | `MLDSA.Polynomial.hs` | Polynomial operations for ML-DSA |
  | `MLDSA.Auxiliary.hs` | Auxiliary functions for ML-DSA |
  | `MLDSA.NTT.hs` | NTT and inverse NTT for ML-DSA |
  


  


