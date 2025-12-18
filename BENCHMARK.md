## Sponge

* Baseline: stateful sponge, no streaming interface, fixed 1084-bit input / 256-bit output

```
[bench] Module areas (from stat):
  module                                            area (µm²)   seq area (µm²)    seq %
  --------------------------------------------------------------------------------------
  Hash_Stateful4_topEntity_keccakF1600Round          15558.340            0.000     0.00%
  Hash_Stateful4_topEntity_spongeFSM                 14317.450         8543.920    59.67%
  Stateful4_SHA3                                     29875.790         8543.920    28.60%

[bench] Time/Mem: load 4.12s | compile 9.46s | synth 13.39s | mem 3295.20 MB
```

* S5: stateful sponge, 17 beats of 64-bit input / 256-bit output

```
[bench] Module areas (from stat):
  module                                            area (µm²)   seq area (µm²)    seq %
  --------------------------------------------------------------------------------------
  Hash_Stateful5_topEntity_keccakF1600Round          15047.886            0.000     0.00%
  Hash_Stateful5_topEntity_spongeFSM                 16096.724         8543.920    53.08%
  Stateful5_SHA3                                     31144.610         8543.920    27.43%

[bench] Time/Mem: load 4.41s | compile 9.50s | synth 17.68s | mem 3271.12 MB
```

* S6: stateful sponge, 17 beats of 64-bit input / 64-bit AXI4-Stream output

[bench] Module areas (from stat):
  module                                            area (µm²)   seq area (µm²)    seq %
  --------------------------------------------------------------------------------------
  Hash_Stateful6_topEntity_keccakF1600Round          15558.340            0.000     0.00%
  Hash_Stateful6_topEntity_spongeFSM                 17058.846         8549.240    50.12%
  Stateful6_SHA3                                     32617.186         8549.240    26.21%

[bench] Time/Mem: load 4.49s | compile 9.46s | synth 16.97s | mem 3419.91 MB

* S7: stateful sponge, 64-bit AXI4-Stream input/output

 [bench] Module areas (from stat):
  module                                            area (µm²)   seq area (µm²)    seq %
  --------------------------------------------------------------------------------------
  Hash_Stateful7_topEntity_keccakF1600Round          15559.138            0.000     0.00%
  Hash_Stateful7_topEntity_spongeFSM                 17378.578         8559.880    49.26%
  Stateful7_SHA3                                     32937.716         8559.880    25.99%

[bench] Time/Mem: load 4.50s | compile 9.45s | synth 26.08s | mem 3312.36 MB

* S7 with P3: stateful sponge, 64-bit AXI4-Stream input/output

[bench] Module areas (from stat):
  module                                            area (µm²)   seq area (µm²)    seq %
  --------------------------------------------------------------------------------------
  Hash_Stateful7_topEntity_keccakF1600Round           9516.150            0.000     0.00%
  Hash_Stateful7_topEntity_spongeFSM                 17410.498         8559.880    49.17%
  Stateful7_SHA3                                     26926.648         8559.880    31.79%

[bench] Time/Mem: load 2.97s | compile 3.38s | synth 18.00s | mem 2331.52 MB
## Permutation

* P0: baseline

[bench] Module areas (from stat):
  module                                            area (µm²)   seq area (µm²)    seq %
  --------------------------------------------------------------------------------------
  KeccakF1600_Round                                  15558.340            0.000     0.00%
  Permutation_KeccakF1600_topEntity_keccakF1600Round 15558.340            0.000     0.00%

[bench] Time/Mem: load 4.20s | compile 9.39s | synth 12.54s | mem 3292.19 MB

* P1: remove SOME index reversals

[bench] Module areas (from stat):
  module                                            area (µm²)   seq area (µm²)    seq %
  --------------------------------------------------------------------------------------
  KeccakF1600_P1                                     15124.228            0.000     0.00%
  Permutation_P1_topEntity_keccakF1600Round          15124.228            0.000     0.00%

[bench] Time/Mem: load 4.28s | compile 9.49s | synth 12.79s | mem 3238.58 MB

* P2: remove ALL index reversals

[bench] Module areas (from stat):
  module                                            area (µm²)   seq area (µm²)    seq %
  --------------------------------------------------------------------------------------
  KeccakF1600_P2                                     15630.958            0.000     0.00%
  Permutation_P2_topEntity_keccakF1600Round          15630.958            0.000     0.00%

[bench] Time/Mem: load 4.18s | compile 9.08s | synth 11.81s | mem 3253.62 MB

* Theta0: baseline theta implementation

[bench] Module areas (from stat):
  module                                            area (µm²)   seq area (µm²)    seq %
  --------------------------------------------------------------------------------------
  Theta0                                             11194.344            0.000     0.00%

[bench] Time/Mem: load 2.27s | compile 0.36s | synth 7.21s | mem 2889.66 MB


* Theta1: 2-stage theta implementation

[bench] Module areas (from stat):
  module                                            area (µm²)   seq area (µm²)    seq %
  --------------------------------------------------------------------------------------
  Theta1                                              5107.200            0.000     0.00%

[bench] Time/Mem: load 0.77s | compile 0.01s | synth 0.72s | mem 137.53 MB

* HTheta0: high-speed baseline theta implementation
[bench] Module areas (from stat):
  module                                            area (µm²)   seq area (µm²)    seq %
  --------------------------------------------------------------------------------------
  keccak_round_theta                                  7150.080            0.000     0.00%

[bench] Time/Mem: load N/A | compile N/A | synth 0.35s | mem 94.77 MB

* HTheta1: high-speed baseline theta implementation with precomputed rotates

[bench] Module areas (from stat):
  module                                            area (µm²)   seq area (µm²)    seq %
  --------------------------------------------------------------------------------------
  keccak_round_theta1                                 5107.200            0.000     0.00%

[bench] Time/Mem: load N/A | compile N/A | synth 0.26s | mem 79.00 MB
