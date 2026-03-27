# Bench Caching Plan

## Goal

Improve `bench` so it does not rerun every expensive stage on every invocation.

The key constraint is:

- use `stack` for Haskell rebuild invalidation
- add our own caching only for post-`stack` stages

This avoids reimplementing Haskell dependency tracking poorly.

## Principle

Do not try to replace what `stack build clash-hash:lib` already does well.

`stack` already handles:

- Haskell module dependency tracking
- package rebuild invalidation
- interface/object caching

So the caching work for `bench` should begin only after the library is built.

## Stage Model

Keep `stack build clash-hash:lib` as the first step, then cache these stages:

1. `hdl`
2. `synth`
3. `sta`

### `hdl`

Generates Clash HDL artifacts, such as:

- `systemverilog/<target>/clash-manifest.json`
- emitted HDL files

### `synth`

Runs Yosys synthesis and produces:

- `build/synth/<target>/netlist/...`
- `build/synth/<target>/reports/yosys.log`

### `sta`

Runs OpenSTA and produces:

- `build/sta/<top>/reports/summary.rpt`
- timing reports

## Cache Scope

Use a per-target cache file:

- `build/cache/<target>.json`

This file should store, per stage:

- stage key
- artifact paths
- success flag

## Stage Keys

Each cached stage should be keyed by the actual inputs that affect it.

### `hdl` key

Depends on:

- requested target label
- resolved target/module
- Clash backend / invocation shape
- existence of expected HDL artifacts
- optionally Clash version stamp

Note:

- do not try to hash all Haskell source files here
- rely on `stack build clash-hash:lib` for Haskell invalidation

### `synth` key

Depends on:

- HDL file hashes
- `scripts/synth.py`
- liberty file hash
- yosys version

### `sta` key

Depends on:

- mapped netlist hash
- SDC hash
- `scripts/sta.py`
- `scripts/tcl/*.tcl`
- liberty file hash
- OpenSTA version

## Reuse Rule

A stage is reusable only if:

1. the cached key matches
2. all expected artifacts exist
3. the previous run succeeded

If any of these fails, rerun the stage.

## Invalidation Rules

Stage invalidation follows the pipeline DAG:

- rerun `hdl` -> invalidate `synth` and `sta`
- rerun `synth` -> invalidate `sta`

Do not try to be more clever than the stage graph.

## Output Behavior

Caching must be visible.

Suggested status lines:

- `hdl cached`
- `synth cached`
- `sta cached`

This is important so cache behavior is inspectable and trustworthy.

## First Implementation

The first implementation should be simple and correct:

- always run `stack build clash-hash:lib`
- cache only `hdl`, `synth`, and `sta`
- use artifact hashing for `synth` and `sta`
- use stage metadata + artifact existence checks

Do not start with:

- target-specific Haskell source closure hashing
- custom Haskell dependency tracking
- timestamp-only invalidation

## Why This Plan

This splits responsibilities cleanly:

- `stack` owns Haskell rebuild correctness
- `bench` owns expensive downstream artifact reuse

That is the right boundary for this repository.

## Later Refinements

Possible later improvements:

- tighter `hdl` keys
- storing tool version fingerprints explicitly
- better cache introspection/debugging
- optional machine-readable cache status output

But those should come after the first working stage cache.
