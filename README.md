# clash-hash

## Synthesis

```
nix develop          # enter shell exposing yosys + synth
synth                # synthesise all targets under ./verilog
synth verilog/SHA2.topEntity
```

Outputs land in `build/synth/<target>/netlist`, with Yosys logs in `build/synth/<target>/reports`.
