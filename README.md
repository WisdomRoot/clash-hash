# clash-hash

## Scripts / Commands

```
nix develop
synth N256 -- convert Clash to Verilog and run Yosys synthesis
bench N256 -- run benchmark for N256 target
stack test -- run all tests
```

## Targets

* N256: Non-pipelined SHA3-256 at `Hash.NonPipelined.SHA3256` (Clash)
* N256X: Non-pipelined SHAKE-256 (Clash)
* N128X: Non-pipelined SHAKE-128 (Clash)
* H256: Pipelined *high_speed_core* SHA3-256 by *Team Keccak*

These targets can be used with the `synth` and `bench` commands. They are defined in `clash.json` and `vhdl.json`.
