# NTT Component Guide

`Component.NTT` is a placeholder for the ML-DSA (Dilithium) NTT. It just adds two
coefficients mod `q = 8380417` (`SUM = (A + B) mod q`), so the tooling has a real
module to run against. You'll replace the math over time.

## Install

- **Nix** (flakes enabled) — provides the toolchain (Yosys, Python, `synth`,
  `bench`, `stack`). Enable flakes: `experimental-features = nix-command flakes`
  in `~/.config/nix/nix.conf`.
- **Docker** — only for the timing stage of `bench`.

## Commands

Enter the shell first: `nix develop`. Target name is `NTT`.

```bash
stack build                                   # compile
stack test --test-arguments "--pattern NTT"   # test NTT
synth NTT                                      # Clash -> HDL -> Yosys, reports area
bench NTT                                       # build + synth + timing (needs Docker)
```

Placeholder: area **219.45 µm²**, critical path **1.25 ns**. Slack is `N/A`
(combinational, no clocked paths).

## Files

| Path | What |
|---|---|
| `src/Component/NTT.hs` | The component. |
| `tests/Test/NTT.hs` | Its tests. |
| `tests/Main.hs` | Test registry. |
| `clash.json` | Maps `NTT` to the module. |

`clash-hash.cabal` is generated and git-ignored — new modules are picked up
automatically.

## Growth path

1. Replace `addModQ` with a single butterfly: multiply-mod-`q` plus a modular add
   and subtract, over a coefficient pair and a twiddle. Test against a reference.
2. Scale up to a full length-256 NTT — one pure function, wider ports.
3. We will replace the modular arithmetic with something fancy like Barrett or Montgomery multiplication? 
4. We can make this NTT clocked and pipelined if need be (in case that the combinational path is too long)