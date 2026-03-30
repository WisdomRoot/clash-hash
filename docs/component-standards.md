# Component Implementation Standards

This document defines the expected implementation style for reusable components in this repository.

## 1) Interface and Top-Level Shape

- Internal component implementation should use `Pipe dom inBits outBits`.
- If an input control sideband is required (for example `flush`), use `PipeCtrl dom ctrl inBits outBits`.
- Top-level synthesis-facing entity should be first-order and exposed via `toDUT` (or `toDUTCtrl` for `PipeCtrl`).
- Follow the `G`-style split:
  - `<name>Core` for the internal `Pipe` implementation (for example `i272o256Core`)
  - `<name>` for the synthesized DUT wrapper (for example `i272o256`)
- Synthesis annotation should use `t_name = "dut"` for consistent generated module naming.
- AXI4-Stream handshake semantics (`TVALID`/`TREADY`/`TLAST`) must be preserved.

## 2) Naming

- Use explicit width-oriented function names for concrete designs: `i<inBits>o<outBits>`, for example `i272o24`.
- Use concise and explicit bench target names in `clash.json`, for example:
  - `CBD-O12`
  - `CBD-O24`
  - `SN512-I272-O24-L2`
- Avoid one-off naming variants unless they provide clear reusable value.

## 3) Behavioral Requirements

- Backpressure must propagate correctly through the data path.
- `inReady`/`outValid` behavior must remain stable under stalls.
- Component output ordering must match the reference model exactly.
- Any lookahead/buffer behavior must be tested under normal and stalled traffic.

## 4) Testing Requirements

- Use `Stream`-module based tests for component IO behavior.
- Include both:
  - directed tests (fixed vectors, known timings)
  - randomized property tests (QuickCheck)
- Randomized tests must include realistic ready/backpressure patterns.
- Tests should check both:
  - functional equivalence to reference
  - timing/handshake expectations when applicable

## 5) Quality Gates

- No compiler warnings in component/test changes.
- Relevant tests must pass before considering the change complete.
- Refactors must be checked for synthesis impact (area/timing) when behavior is intended to be unchanged.

## 6) Structure and Maintainability

- Prefer shared/common modules for real reuse; keep modules focused.
- Avoid helper definitions that are only thin one-off wrappers.
- Keep interfaces and naming consistent across general and specialized variants.
