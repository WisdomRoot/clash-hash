# Template Haskell: a tutorial

Template Haskell (TH) runs Haskell during compilation to write more Haskell.
This guide teaches it through `mkRead` in `src/TH.hs`. No prior TH needed.

## 1. The problem

Clash makes hardware from Haskell. A `case` becomes a multiplexer. Each branch
is a circuit. Hardware has no loop to roll up.

So reading a lane out of the 1600-bit Keccak state needs one clause per index:

```haskell
read4Block0 :: BitVector 1600 -> Index 256 -> BitVector 4
read4Block0 state 0 = slice (SNat @3)  (SNat @0)  state
read4Block0 state 1 = slice (SNat @7)  (SNat @4)  state
-- ... 254 more ...
```

Every clause follows one rule: clause `i` slices bits `[4i, 4i+3]`. State the
rule once. Let the machine expand it.

## 2. The solution

```haskell
$(mkRead "read4Block0" 1600 [(i, i * 4, 4) | i <- [0 .. 255]])
```

One line in `Component.SamplePolyCBD`. It becomes all 256 clauses.

Two things are going on. `mkRead` is an ordinary Haskell function; section 3
shows what it returns. The `$( )` around it is not ordinary Haskell at all;
section 4 explains that.

## 3. Programs as data

`mkRead` returns declarations, as data:

```haskell
mkRead :: String -> Integer -> [(Integer, Integer, Integer)] -> Q [Dec]
```

`Dec` is a declaration. TH has one type per part of the language:

| Type   | Represents    |
|--------|---------------|
| `Exp`  | an expression |
| `Pat`  | a pattern     |
| `Type` | a type        |
| `Dec`  | a declaration |

They are ordinary algebraic data types. So `mkRead` is an ordinary function
building ordinary values. This is how it makes the type
`BitVector 1600 -> Index 256 -> BitVector 4`:

```haskell
funcTy = foldr1 (AppT . AppT ArrowT) [bvState, idxCases, bvLane]
```

and this is how it makes the body `slice (SNat @3) (SNat @0) state`:

```haskell
body = foldl AppE (VarE sliceName) [snatUpper, snatLower, VarE stateName]
```

`AppT` applies a type to a type. `AppE` applies an expression. `VarE` is a
variable. Nothing is special. You write a program that builds a program.

Read the rest in `src/TH.hs`. It is 40 lines.

One thing to notice: `stateSize` arrives as an `Integer` and leaves as a
*type*. No ordinary function can do that. That is why section 1 needs a
generator.

## 4. Unquote

`$(e)` runs `e` during compilation and puts the result in as source code. Call
it **unquote**.

```haskell
-- 1. what you write
$(mkRead "read4Block0" 1600 [(i, i * 4, 4) | i <- [0 .. 255]])

-- 2. mkRead runs, and returns a value                    <- data
[ SigD "read4Block0" (BitVector 1600 -> Index 256 -> BitVector 4)
, FunD "read4Block0" [ Clause 0 ..., Clause 1 ..., ...254 more ]
]

-- 3. $( ) unquotes it, and GHC compiles this             <- program
read4Block0 :: BitVector 1600 -> Index 256 -> BitVector 4
read4Block0 state 0 = slice (SNat @3) (SNat @0) state
read4Block0 state 1 = slice (SNat @7) (SNat @4) state
-- ...254 more...
```

A splice takes the shape of what it produces:

```haskell
xs = map $(f)                -- f :: Q Exp,   expression position
$(mkRead "read4Block0" ...)  -- mkRead :: Q [Dec], top level
```

## 5. Quote

Section 3 wrote the AST by hand. That does not scale to reading:

```haskell
-- the AST of  1 + 1
InfixE (Just (LitE (IntegerL 1))) (VarE '(+)) (Just (LitE (IntegerL 1)))

-- the same value, quoted
[| 1 + 1 |]
```

A **quote** turns code into the data that describes it. `mkRead`'s two lines
from section 3, written both ways:

```haskell
-- by hand
funcTy = foldr1 (AppT . AppT ArrowT) [bvState, idxCases, bvLane]
body   = foldl AppE (VarE sliceName) [snatUpper, snatLower, VarE stateName]

-- quoted
funcTy = [t| BitVector 1600 -> Index 256 -> BitVector 4 |]
body   = [| slice (SNat @3) (SNat @0) state |]
```

The quoted version says what it generates. The hand-written version makes you
decode it.

One quote per part of the language:

```haskell
[|  1 + 1       |]  ::  Q Exp      -- an expression
[t| Int -> Bool |]  ::  Q Type     -- a type
[d| f x = x     |]  ::  Q [Dec]    -- declarations
[p| (a, b)      |]  ::  Q Pat      -- a pattern
[|| 1 + 1      ||]  ::  Code Q a   -- a typed expression
```

`[|| ||]` is the typed form. Learn the untyped forms first.

GHC calls these quotation brackets, and most people say "bracket" for short.
This guide says quote, to keep it paired with unquote.

GHC does not typecheck an untyped quote. It checks only that the names are in
scope. So `[| slice a b c |]` compiles although `slice` carries constraints GHC
cannot solve yet.

Use `runQ` in a REPL to see what any quote produces. This is the fastest way to
learn TH:

```
$ stack repl
> :set -XTemplateHaskell
> import Language.Haskell.TH
> runQ [| 1 + 1 |]
InfixE (Just (LitE (IntegerL 1))) (VarE GHC.Num.+) (Just (LitE (IntegerL 1)))
```

### Splices inside quotes

The quotes above are frozen. `[| slice (SNat @3) (SNat @0) state |]` gives you
clause 0 and nothing else. A splice inside a quote is a hole you fill by
computation:

```haskell
-- frozen: clause 0 only
[| slice (SNat @3) (SNat @0) state |]

-- a hole per size: any clause
[| slice $(snatE upper) $(snatE start) $(varE state) |]
```

Type quotes take splices too. This is section 3's `Integer`-to-type step:

```haskell
-- frozen
[t| BitVector 1600 -> Index 256 -> BitVector 4 |]

-- computed from the arguments
[t| BitVector $(nat stateSize) -> Index $(nat numCases) -> BitVector $(nat laneSize) |]
```

## 6. Three ways to build the same thing

```haskell
AppE (VarE (mkName "f")) (LitE (IntegerL 1))   -- constructors
appE (varE (mkName "f")) (litE (integerL 1))   -- combinators, in Q
[| f 1 |]                                      -- quote
```

`mkName "f"` makes a `Name` from a string.

A quote reads best, but its **shape is fixed** when you write it.

Look back at `mkRead`. Its type is one type, so a quote fits. Each clause body
is one expression, so a quote fits. The clause **list** could not be, because
its length is `length slices`, known only when the generator runs.

So real generators mix both: quotes for the fixed parts, code for the rest.

## 7. Stage restriction

You cannot splice a function defined in the same module. GHC must compile it
first. That is why generators live in `src/TH.hs`.

## 8. Seeing what you produced

```
stack build --ghc-options=-ddump-splices
```

GHC prints each splice and its expansion. Start with a one-clause site.
`Component.G.Common` line 76 is a good one. Not the 256-clause site.

## 9. Exercises

### 1. Rewrite `mkRead` with quotes

Add `mkRead2` beside `mkRead`. Do not edit `mkRead`: it has about 20 splice
sites, so one mistake fails in twenty places at once. Switch a single site to
`mkRead2` and compare.

Three obstacles, in the order you will meet them:

1. `Variable not in scope: slice`, inside `TH.hs`. See the hint below.
2. `SNat @3`, where the `3` is an `Integer`. A quote cannot do this alone. The
   answer is in `Language.Haskell.TH.Lib`.
3. `newName` is an effect in `Q`. Clause building becomes monadic, and `map`
   becomes `fmap`.

<details>
<summary>Hint for obstacle 1: names and hygiene</summary>

A `Name` can come from four places:

```haskell
'foo           -- the value foo, resolved where you write it
''Foo          -- the type Foo, resolved where you write it
mkName  "foo"  -- whatever foo means at the SPLICE SITE
newName "foo"  -- a fresh name that nothing can capture
```

`mkRead` uses `mkName "slice"`. The generated code says `slice`, and that name
resolves at the splice site. So `SamplePolyCBD.hs` must import `Clash.Prelude`,
or GHC reports `Variable not in scope: slice` in code its author never wrote.

A quote resolves `slice` in the other place: here, in `TH.hs`. The splice site
then needs no import, but `TH.hs` must import `Clash.Prelude` itself. That is
your error. Fix it with:

```haskell
import Clash.Prelude (BitVector, Index, SNat (..), slice)
```

Resolving names in the generator rather than at the splice site is called
**hygiene**. It also stops capture:

```haskell
mkName  "state"  -- generated: state         -- collides with a local `state`
newName "state"  -- generated: state_a5Mw    -- cannot collide
```

`src/TH.hs` chose `mkName` throughout, to keep `Clash.Prelude` out of it. You
have just paid the other price. That is the last question of this exercise.

</details>

You are done at three levels:

```
1. it compiles
2. -ddump-splices matches mkRead, except  state  becomes  state_a5Mw
3. synth SN-O24-L6 still reports 26271.490 um2
```

Level 3 is the one that matters. The gates are the product.

Then answer this: `mkRead2` reads better, and it costs `src/TH.hs` a dependency
on `Clash.Prelude`. Do you merge it? Write down why.

### 2. Generate `popPair`

`src/Component/SampleNTT6.hs` defines `popPair`. Eight clauses, one rule:

```haskell
popPair :: Buffer -> (BitVector 24, Buffer)
popPair (Buffer2 a b)                 = (b ++# a, Buffer0)
popPair (Buffer3 a b c)               = (b ++# a, Buffer1 c)
popPair (Buffer4 a b c d)             = (b ++# a, Buffer2 c d)
-- ...through Buffer9...
popPair _ = error "Component.SampleNTT6.popPair: buffer underflow"
```

The rule: clause `n` matches `Buffer<n>` with `n` fields, returns the second
and first joined, and repacks the rest as `Buffer<n-2>`.

Write `mkPopPair :: Integer -> Integer -> Q [Dec]`, taking the lowest and
highest buffer size. Splice it in place of the hand-written clauses.

This needs three things `mkRead` did not:

1. `ConP` for a constructor pattern, not `LitP` for a literal.
2. `newName` per field, because the count varies per clause.
3. A constructor application on the right, built with `ConE` and `AppE`.

Checks, in order:

```
1. -ddump-splices reproduces all 8 clauses and the error fallback
2. stack build
3. synth SN-O24-L6 still reports 26271.490 um2
```

Then answer this. Eight clauses became a generator plus a call. Did that pay?
`popPair` is small enough that the answer may be no, and knowing where the line
sits matters more than the generator does.

## Reference

- `Language.Haskell.TH.Syntax` — AST types and `Q`
- `Language.Haskell.TH.Lib` — the lowercase combinators
