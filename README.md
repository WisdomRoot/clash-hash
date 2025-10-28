# clash-hash

## Synthesis

```
nix develop          # enter shell exposing yosys + synth
synth                # synthesise every target found under ./verilog
synth SHA2.topEntity # target names resolve relative to ./verilog/
```

The tool expects each Clash export under `verilog/<target>/`, typically containing `clash-manifest.json` and the emitted `.v` file. Outputs land in `build/synth/<target>/netlist`, with Yosys logs in `build/synth/<target>/reports`.
