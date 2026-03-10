## G512-I256-O256

  module                                            area (µm²)   seq area (µm²)    seq %
  --------------------------------------------------------------------------------------
  Component_G512_topEntity_pad                        9743.580            0.000     0.00%
  Component_G512_topEntity_spongeFSM                 14849.716         8559.880    57.64%
  Component_G_512_I256_O256                          24593.296         8559.880    34.81%

[bench] Time/Mem: load 2.41s | compile 2.68s | synth 18.59s | mem 2996.59 MB

## SN-O24-L2

  module                                            area (µm²)   seq area (µm²)    seq %
  --------------------------------------------------------------------------------------
  Component_SampleNTT512N_i272o24l2_keccakF1600       9518.810            0.000     0.00%
  SN512_I272_O24_L2                                  26241.166         8884.400    33.86%

[bench] Time/Mem: load 2.76s | compile 3.10s | synth 15.38s | mem 2069.53 MB

## SamplePolyCBD512-I256-O12

  module                                            area (µm²)   seq area (µm²)    seq %
  --------------------------------------------------------------------------------------
  Component_SamplePolyCBD512                         27532.330         8607.760    31.26%
  Component_SamplePolyCBD512_i264o12_keccakF1600       9518.810            0.000     0.00%

[bench] Time/Mem: load 2.71s | compile 5.23s | synth 12.92s | mem 1967.38 MB

## SamplePolyCBD512-I256-O24

  module                                            area (µm²)   seq area (µm²)    seq %
  --------------------------------------------------------------------------------------
  Component_SamplePolyCBD512_i264o24_keccakF1600       9518.810            0.000     0.00%
  SamplePolyCBD512_I264_O24                          28004.214         8756.720    31.27%

[bench] Time/Mem: load 2.83s | compile 5.35s | synth 13.10s | mem 2022.78 MB

## SHA3-256

* 20260105: baseline

  module                                            area (µm²)   seq area (µm²)    seq %
  --------------------------------------------------------------------------------------
  Hash_SHA3256_topEntity_keccakF1600Round             9516.682            0.000     0.00%
  Hash_SHA3256_topEntity_spongeFSM                   19480.776         8559.880    43.94%
  SHA3_256_NonPipelined                              28997.458         8559.880    29.52%

[bench] Time/Mem: load 2.77s | compile 3.70s | synth 17.36s | mem 2400.94 MB

* 20260105: refactored squeeze

  module                                            area (µm²)   seq area (µm²)    seq %
  --------------------------------------------------------------------------------------
  Hash_SHA3256_topEntity_keccakF1600Round             9516.682            0.000     0.00%
  Hash_SHA3256_topEntity_spongeFSM                   19612.446         8559.880    43.65%
  SHA3_256_NonPipelined                              29129.128         8559.880    29.39%

[bench] Time/Mem: load 2.75s | compile 3.24s | synth 12.91s | mem 2208.92 MB

* 20260105: standalone, decoupled from SHAKE256

  module                                            area (µm²)   seq area (µm²)    seq %
  --------------------------------------------------------------------------------------
  Hash_NonPipelined_SHA3256_topEntity_keccakF1600Round       9516.682            0.000     0.00%
  Hash_NonPipelined_SHA3256_topEntity_spongeFSM      16672.880         8559.880    51.34%
  SHA3_256_NonPipelined                              26189.562         8559.880    32.68%

[bench] Time/Mem: load 2.90s | compile 3.37s | synth 13.12s | mem 2400.42 MB

## SHA3-512

* 20260126: baseline

  module                                            area (µm²)   seq area (µm²)    seq %
  --------------------------------------------------------------------------------------
  Hash_NonPipelined_SHA3512_topEntity_pad             9527.056            0.000     0.00%
  Hash_NonPipelined_SHA3512_topEntity_spongeFSM      15366.554         8559.880    55.70%
  SHA3_512_NonPipelined                              24893.610         8559.880    34.39%

[bench] Time/Mem: load 2.58s | compile 3.36s | synth 10.99s | mem 2064.12 MB

## SHAKE256

* 20260105: baseline

  module                                            area (µm²)   seq area (µm²)    seq %
  --------------------------------------------------------------------------------------
  Hash_SHAKE256_topEntity_keccakF1600Round            9516.682            0.000     0.00%
  Hash_SHAKE256_topEntity_spongeFSM                  19479.180         8559.880    43.94%
  SHAKE_256_NonPipelined                             28995.862         8559.880    29.52%

[bench] Time/Mem: load 2.69s | compile 3.53s | synth 21.99s | mem 3241.19 MB

* 20260105: refactored squeeze

  module                                            area (µm²)   seq area (µm²)    seq %
  --------------------------------------------------------------------------------------
  Hash_SHA3256_topEntity_keccakF1600Round             9516.682            0.000     0.00%
  Hash_SHA3256_topEntity_spongeFSM                   19133.114         8559.880    44.74%
  SHA3_256_NonPipelined                              28649.796         8559.880    29.88%

[bench] Time/Mem: load 2.60s | compile 3.42s | synth 12.73s | mem 2149.11 MB

* 20260105: standalone, decoupled from SHA3-256

  module                                            area (µm²)   seq area (µm²)    seq %
  --------------------------------------------------------------------------------------
  Hash_NonPipelined_SHAKE256_topEntity_pad            9517.480            0.000     0.00%
  Hash_NonPipelined_SHAKE256_topEntity_spongeFSM      18445.770         8559.880    46.41%
  SHAKE_256_NonPipelined                             27963.250         8559.880    30.61%

[bench] Time/Mem: load 2.69s | compile 3.63s | synth 16.89s | mem 3222.78 MB

## SHAKE128

* 20260106: baseline

  module                                            area (µm²)   seq area (µm²)    seq %
  --------------------------------------------------------------------------------------
  Hash_NonPipelined_SHAKE128_topEntity_pad            9517.480            0.000     0.00%
  Hash_NonPipelined_SHAKE128_topEntity_spongeFSM      19273.828         8559.880    44.41%
  SHAKE_128_NonPipelined                             28791.308         8559.880    29.73%

[bench] Time/Mem: load 2.79s | compile 3.57s | synth 16.79s | mem 3218.16 MB

## SHAKE128B

* 20260119: baseline
  module                                            area (µm²)   seq area (µm²)    seq %
  --------------------------------------------------------------------------------------
  Hash_NonPipelined_SHAKE128B_topEntity_keccakF1600Round       9516.682            0.000     0.00%
  Hash_NonPipelined_SHAKE128B_topEntity_spongeFSM      42142.646         8565.200    20.32%
  SHAKE_128B_NonPipelined                            51659.328         8565.200    16.58%

[bench] Time/Mem: load 2.81s | compile 3.24s | synth 20.37s | mem 3391.34 MB

  module                                            area (µm²)   seq area (µm²)    seq %
  --------------------------------------------------------------------------------------
  Hash_NonPipelined_SHAKE128B_topEntity_keccakF1600Round       9516.682            0.000     0.00%
  Hash_NonPipelined_SHAKE128B_topEntity_spongeFSM      37037.042         8565.200    23.13%
  SHAKE_128B_NonPipelined                            46553.724         8565.200    18.40%

[bench] Time/Mem: load 2.81s | compile 7.03s | synth 24.78s | mem 3844.94 MB

  module                                            area (µm²)   seq area (µm²)    seq %
  --------------------------------------------------------------------------------------
  Hash_NonPipelined_SHAKE128B_topEntity_keccakF1600Round       9516.682            0.000     0.00%
  Hash_NonPipelined_SHAKE128B_topEntity_spongeFSM      35788.704         8565.200    23.93%
  SHAKE_128B_NonPipelined                            45305.386         8565.200    18.91%

[bench] Time/Mem: load 2.97s | compile 8.26s | synth 24.61s | mem 3954.42 MB

  module                                            area (µm²)   seq area (µm²)    seq %
  --------------------------------------------------------------------------------------
  Hash_NonPipelined_SHAKE128B_topEntity_keccakF1600Round       9516.682            0.000     0.00%
  Hash_NonPipelined_SHAKE128B_topEntity_spongeFSM      28860.468         8565.200    29.68%
  SHAKE_128B_NonPipelined                            38377.150         8565.200    22.32%

[bench] Time/Mem: load 3.16s | compile 10.47s | synth 33.31s | mem 4304.33 MB

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

* baseline

[bench] Module areas (from stat):
  module                                            area (µm²)   seq area (µm²)    seq %
  --------------------------------------------------------------------------------------
  KeccakF1600_Round                                  15558.340            0.000     0.00%
  Permutation_KeccakF1600_topEntity_keccakF1600Round 15558.340            0.000     0.00%

[bench] Time/Mem: load 4.20s | compile 9.39s | synth 12.54s | mem 3292.19 MB

* remove SOME index reversals

[bench] Module areas (from stat):
  module                                            area (µm²)   seq area (µm²)    seq %
  --------------------------------------------------------------------------------------
  KeccakF1600_P1                                     15124.228            0.000     0.00%
  Permutation_P1_topEntity_keccakF1600Round          15124.228            0.000     0.00%

[bench] Time/Mem: load 4.28s | compile 9.49s | synth 12.79s | mem 3238.58 MB

* remove ALL index reversals

[bench] Module areas (from stat):
  module                                            area (µm²)   seq area (µm²)    seq %
  --------------------------------------------------------------------------------------
  KeccakF1600_P2                                     15630.958            0.000     0.00%
  Permutation_P2_topEntity_keccakF1600Round          15630.958            0.000     0.00%

[bench] Time/Mem: load 4.18s | compile 9.08s | synth 11.81s | mem 3253.62 MB

### Theta

* Baseline theta implementation

[bench] Module areas (from stat):
  module                                            area (µm²)   seq area (µm²)    seq %
  --------------------------------------------------------------------------------------
  Theta                                              11194.344            0.000     0.00%

[bench] Time/Mem: load 2.27s | compile 0.36s | synth 7.21s | mem 2889.66 MB


* 2-stage theta implementation

[bench] Module areas (from stat):
  module                                            area (µm²)   seq area (µm²)    seq %
  --------------------------------------------------------------------------------------
  Theta                                              5107.200            0.000     0.00%

[bench] Time/Mem: load 0.77s | compile 0.01s | synth 0.72s | mem 137.53 MB

* high-speed baseline theta implementation
[bench] Module areas (from stat):
  module                                            area (µm²)   seq area (µm²)    seq %
  --------------------------------------------------------------------------------------
  keccak_round_theta                                  7150.080            0.000     0.00%

[bench] Time/Mem: load N/A | compile N/A | synth 0.35s | mem 94.77 MB
