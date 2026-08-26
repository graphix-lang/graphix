# P2b: intrinsics vs Graphix bodies (design/recursive_activations.md)

The measurement phase of the recursive-activations arc: for each
collection operation, the INTRINSIC (the compiler-owned MapQ/FoldQ node
with its scaffold fast path) against the same operation reached or
written another way:

- **`*_intr`** — the intrinsic called directly (`array::fold`, …); the
  baseline, and for the 100k rows the same shape as the main `bench/`
  corpus (`fold_sum`, `map_fold`, …).
- **`*_trait`** — the same intrinsic reached through `Collection::…`
  trait dispatch. Same body; measures dispatch + the trait-call fusion
  gap (closed — the P3 premat fall-through).
- **`*_fmshape` / `find_fold` / `flatmap_fold` / `map_init`** — the
  trait DEFAULT's body shape written out directly (`map` as
  `filter_map` with a total callback, `find` as a fold carrying an
  Option, `flat_map` as fold + concat, `map` as `init(len, a[i])`):
  the Graphix derivation over a smaller intrinsic vocabulary, with no
  trait machinery in the way.
- **`*_rec` / `map_push`** — the operation written in Graphix as a
  tail recursion (index loop + `push` for Array, natural `` `Cons ``/
  `` `Nil `` recursion for List): what a user-defined `Collection`
  impl would write today.

Self-timing, seeding, and the runner are `bench/`'s conventions
verbatim (see `bench/README.md`); `run.sh` here is a copy. The map
variants thread `seed` through the callback so the timed region
provably contains the map stage in both modes.

Sizes: 100k for the linear Array/List/Map rows, 500k for the find
pair, 10k for the O(n²)-by-representation rows (`map_push`,
`flatmap_*` — each step copies the array), 4k for the list-recursion
pair (sized under the pre-discriminator quadratic; the row is post-fix).

## The semantic face

`collection_bodies_*` in `stdlib/graphix-tests/src/lang/collection.rs`
pins VALUE agreement across all the implementation routes of each
family at small n, and `run!` adds the interp-vs-JIT axis. Its first
run found the `is_a_int` unguarded value-depth recursion (a 200-element
`lfold_rec` aborted the debug test process) and led to the four
"Found during P2b" findings in `design/recursive_activations.md`.

## Results

Release build, best-of-3 per mode, 2026-08-25 (full re-sweep after
the map-default widening fix).
Grouped by family; `1x` = the shape does not fuse (jit == node-walk),
and the honest differential is that row's jit time against the
intrinsic row's jit time.

| bench            | n    | jit       | node-walk | speedup |
|------------------|------|-----------|-----------|---------|
| `fold_intr`      | 100k | 0.31 ms   | 2.17 s    | 7070x   |
| `fold_trait`     | 100k | 0.27 ms   | 2.17 s    | 8046x   |
| `fold_rec`       | 100k | 13.7 ms   | 0.27 s    | 19x     |
| `map_intr`       | 100k | 2.4 ms    | 4.24 s    | 1781x   |
| `map_fmshape`    | 100k | 2.3 ms    | 4.31 s    | 1835x   |
| `map_init`       | 100k | 2.4 ms    | 4.72 s    | 1963x   |
| `map_intr_10k`   | 10k  | 0.28 ms   | 0.31 s    | 1117x   |
| `map_push`       | 10k  | 0.36 s    | 0.54 s    | 1.5x    |
| `filter_intr`    | 100k | 1.2 ms    | 2.83 s    | 2319x   |
| `filter_fmshape` | 100k | 3.3 ms    | 3.54 s    | 1060x   |
| `find_intr`      | 500k | 1.7 ms    | 7.15 s    | 4184x   |
| `find_fold`      | 500k | 29.7 s    | 31.5 s    | 1x      |
| `flatmap_intr`   | 10k  | 1.5 ms    | 0.56 s    | 381x    |
| `flatmap_fold`   | 10k  | 1.53 s    | 2.25 s    | 1x      |
| `lfold_intr`     | 100k | 4.1 ms    | 2.15 s    | 525x    |
| `lfold_intr_4k`  | 4k   | 0.15 ms   | 60 ms     | 404x    |
| `lfold_rec`      | 4k   | 0.30 s    | 0.29 s    | 1x      |
| `mfold_intr`     | 100k | 26 ms     | 4.05 s    | 155x    |
| `mfold_trait`    | 100k | 4.63 s    | 5.10 s    | 1x      |

## Reading the table

- **Trait dispatch fuses (FIXED)**: the resolved trait site now
  falls through to HOF pre-materialization exactly like a direct
  call (`CallSite::premat_fn_args`), and `fold_trait` went
  2.21 s -> 0.31 ms — parity with the intrinsic; the generic
  `|c: Collection|` path fuses too. The residue is `mfold_trait`:
  Map's impl fold is a Graphix WRAPPER body (`fold_pairs` with a
  derived callback closing over `f`), and the nested callback's call
  to `f` doesn't statically resolve through the inner collection
  site — the named next target, since every trait DEFAULT body is
  this same wrapper shape.
- **The hand-written recursion rows collapse (finding 3 FIXED,
  2026-08-25)**: a formal every self-call passes through unchanged is
  LOOP-INVARIANT — never rebound — so its kind no longer gates
  `structural_tail_loop`, and an invariant fn-typed formal drops out
  of the kernel signature entirely (its body uses are statically-
  resolved calls; the kernel cache key gained a resolution
  fingerprint so two sites with different callbacks key two
  kernels). With the premat wiring's synthetic Refs resolving by id,
  `fold_rec` went 8.40 s -> 16.3 ms JIT and 9.34 s -> 0.26 s
  node-walk (the interp tail loop applies too); `map_push` collapses
  on both engines and is copy-bound (each push copies the array,
  hence 1.5x). The remaining ~60x to `fold_intr` is per-iteration
  call overhead (`array::len` + `a[i]$` DynCalls, the cross-kernel
  `f` call), not activation cost. `lfold_rec` still native-recurses
  per level: its List formal is loop-CARRIED and Value-shaped, and
  the rebind carries Prim/Array/Tuple/Struct only.
- **`lfold_rec` was quadratic on top** (finding 2's cost face): each
  arm consult walked the remaining chain — 9.58 s at 4k, curve
  0.55/1.97/9.6/49.4 s at 1k/2k/4k/8k, clean O(n^2). FIXED same day
  by the shallow arm discriminators (`Type::shallow_discriminant`):
  the curve is linear (0.050/0.096/0.201/0.414 s — 119x at 8k) and
  the row above is post-fix. The residual ~1900x to `lfold_intr_4k`
  is finding 3's per-level activation, no longer the type test.
- **`map_fmshape` is at parity (widening FIXED, 2026-08-25)**: the
  map default's callback returns bare `'b` in `Option<'b>` position
  (the trait default body, `|c, f| filter_map(c, |x| f(x))`). A TOTAL
  callback — return type provably null-free — can never produce the
  `Null` filter_map drops, so the emitter routes it to the MAP loop:
  2.3 ms vs `map_intr`'s 2.4 ms. A may-be-null or unknown-shaped
  return keeps the interpreted path (`frozen_may_be_null`,
  conservative).
- **`map_init` matches the intrinsic** (2.4 ms vs 2.4 ms): `map`
  derived as `init(len(a), |i| f(a[i]))` is free under the JIT. With
  the widening fixed, Array `map` now has TWO parity derivations
  (via `filter_map`-with-total-callback and via `init`) — the first
  deletable-candidate operation.
- **`find_fold` is the worst derivation**: unfused plus a per-element
  double select over the Option accumulator — 31.5 s vs 1.7 ms.
- **`flatmap_fold`** pays O(n^2) concat copies AND doesn't fuse.
- `lfold_intr` at 4.3 ms/100k is ~9x faster than the 2026-07
  `list_fold_sum` row (38 ms) — the flatten boundary improved since.

**Per-operation verdict (the P2b question)**: every intrinsic stays
for now, but Array `map` is the first deletable CANDIDATE — two
parity derivations (`filter_map`-with-total-callback at 2.3 ms and
`init` at 2.4 ms vs the intrinsic's 2.4 ms). Fixed this round: trait
dispatch to a marker-bodied impl (`fold_trait` at parity), finding
2's quadratic arm consults (shallow discriminators — `lfold_rec`
9.58 s -> 0.29 s), finding 3's formal-kind gate for loop-invariant
formals (`fold_rec` 8.40 s -> 13.7 ms), and the map-default widening
(`map_fmshape` 4.21 s -> 2.3 ms). Still open: WRAPPER impl bodies
(`mfold_trait` — the trait-default shape, which also gates fn-formal
FORWARDING) and Value-kind loop-CARRIED rebinds (`lfold_rec`'s
per-level activation). Re-run this corpus after each of those
lands — the verdicts are per-operation and may flip.
