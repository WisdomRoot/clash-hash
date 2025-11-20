# clash-hash

## Synthesis

```
nix develop          # enter shell exposing yosys + synth
synth                # synthesise every target found under ./verilog
synth SHA2.topEntity # target names resolve relative to ./verilog/
```

The tool expects each Clash export under `verilog/<target>/`, typically containing `clash-manifest.json` and the emitted `.v` file. Outputs land in `build/synth/<target>/netlist`, with Yosys logs in `build/synth/<target>/reports`.

## Clash coding guidelines (to keep Yosys happy)

When writing permutation and sponge logic in Clash we found the following patterns generate much better RTL:

- Keep the state and ports as `BitVector` (or `BV n` as a type alias) so they are simple packed bit-vectors in the emitted HDL.
- Use `Vec` locally, just where you want to avoid `replaceBit` and bit-level mutation:
  - Build `Vec n Bit` with `map` from a permutation table (e.g. `Vec (Index n)` constants).
  - Use `bitCoerce` at the boundaries to go between `BitVector n` and `Vec n Bit`.
  - Avoid using large `Vec` as long-lived state or for huge TH-generated tables; that tends to go through `vecArray` and produce big mux/decoder structures in Yosys.

In short: construct new vectors in one pass from their source bits instead of "copy + overwrite one bit at a time", and keep `Vec` confined to those construction sites.
