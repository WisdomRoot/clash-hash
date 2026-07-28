# NTT Testing Guide

How to test `Component.NTT`. Companion to `NTT.md`.

You are testing `topEntity` — the module `synth NTT` and `bench NTT` turn into
gates. Everything below drives it through its real ports.

The circuit is combinational: no registers, no clock, no handshake.

The snippets assume a butterfly: inputs `(a, b, zeta)`, outputs `(a', b')`.
Adjust them to your actual port shape — they will not typecheck against the
`a + b mod q` stub still in `src/Component/NTT.hs`.

## Files

| Path | What |
|---|---|
| `tests/Test/NTT.hs` | Your tests. |
| `tests/Test/Reference/NTT.hs` | Create this: your software NTT. Answer key only — never under test. |
| `tests/Main.hs` | Spec registry. `Test.NTT.spec` is already registered. |

`tests/Test/Reference/` is where the other reference models live (`SHA3.hs`,
`SHAKE256.hs`, `SamplePolyCBD.hs`). Reference code is test-only and never
synthesised — write it in `Integer`.

Adding a new test module warns that it is missing from `other-modules`. The
generated `clash-hash.cabal` was built by a newer hpack than the one stack
bundles, so stack ignores `package.yaml` and reuses the stale file. Delete it and
rebuild:

```bash
rm clash-hash.cabal && stack test
```

```bash
stack test --test-arguments "--pattern NTT"   # your tests
stack test                                    # all tests
```

## Driving the circuit

`topEntity` takes and returns `Signal`s, so you feed it a list of input values
and read back a list of output values. Write this helper once and use it in
every test:

```haskell
-- One (a, b, zeta) triple per cycle; collect the matching outputs.
runDUT :: [(P.Integer, P.Integer, P.Integer)] -> [(BitVector 23, BitVector 23)]
runDUT inputs =
  sampleN
    (P.length inputs)
    (NTT.topEntity clockGen resetGen enableGen (fromList beats))
  where
    beats =
      P.map (\(a, b, z) -> (P.fromInteger a, P.fromInteger b, P.fromInteger z)) inputs
        P.++ P.repeat (0, 0, 0)
```

- `clockGen` / `resetGen` / `enableGen` are simulation stand-ins for the clock,
  reset and enable ports. The design ignores them; the type requires them.
- `fromList` turns a list into an input `Signal`, one element per cycle.
- The `P.repeat` padding matters: `sampleN n` forces `n` samples, so the input
  list must be infinite or simulation runs off the end and crashes.
- `sampleN` aligns input *n* with output *n* because the design is
  combinational. A value applied on a cycle appears on the output that cycle.

Driving `topEntity` rather than the internal function is what catches wiring
bugs: operands swapped in the `PortProduct` annotation, a `resize` that
truncates, outputs in the wrong order, a port at the wrong width.

## Step 1: directed tests

Fixed inputs, hand-computed expected outputs.

```haskell
it "computes a known butterfly" P.$
  runDUT [(1, 1, 1)] `shouldBe` [(2, 0)]
```

Pick cases that matter for your butterfly: zero inputs, a twiddle value that
simplifies the operation, the boundaries of your reduction.

Assert exact equality with `shouldBe` against the full expected value. Do not use
`shouldSatisfy` or bound checks.

## Step 2: random inputs, expected values from the software NTT

Step 1 does not scale — you cannot hand-compute enough vectors to cover the
circuit. So let QuickCheck generate the inputs and let the software NTT supply
the expected values.

The circuit is still the only thing under test. The software NTT is the answer
key: it is assumed correct, it is never checked, and if a test fails the bug is
in the hardware.

```haskell
q :: P.Integer
q = 8380417

genCoeff :: Gen P.Integer
genCoeff = chooseInteger (0, q P.- 1)

genCase :: Gen [(P.Integer, P.Integer, P.Integer)]
genCase = vectorOf 20 ((,,) P.<$> genCoeff P.<*> genCoeff P.<*> genCoeff)

spec :: Spec
spec = describe "NTT" P.$
  it "matches the software model" P.$
    forAll genCase P.$ \inputs ->
      runDUT inputs
        `shouldBe` P.map (\(a, b, z) -> bimapBV (Ref.butterfly a b z)) inputs
```

Two requirements:

- **Generate reduced coefficients.** `arbitrary :: Gen (BitVector 23)` produces
  values above `q`. The butterfly assumes inputs in `[0, q)`, so unreduced inputs
  fail on cases the hardware never sees.
- **Compute the reference in `Integer`.** A reference using `BitVector` or
  `Unsigned` wraps at the same width as the circuit, so both sides go wrong the
  same way and the test passes.

If the software model works in the Montgomery domain and the circuit does not
(or the reverse), every comparison is off by a factor of `R`. Pick one domain,
convert on one side, comment it.

## Step 3: property tests

Same `runDUT`, but checking relationships that hold for all inputs instead of
comparing against the reference.

| Property | Catches |
|---|---|
| both outputs always `< q` | missing conditional subtraction |
| `zeta = 1` gives `(a + b, a - b)` | twiddle applied to the wrong operand |
| linearity in `a` and `b` | reduction in the wrong place |
| `intt . ntt == id` | most end-to-end errors |
| `ntt (f * g) == ntt f .* ntt g` | wrong twiddle order |

Write the last one when you scale up to the full transform. A permuted zeta table
still passes `intt . ntt == id`, because the inverse permutes the same way. Only
the convolution property checks the ordering.
