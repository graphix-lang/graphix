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

Release build, best-of-3 per mode, **2026-08-30** (re-sweep after
`b386f97d` closed wrapper premat; the 2026-08-25 table it replaces had
`mfold_trait` 4.63 s, `filter_fmshape` 3.3 ms, `find_fold` 29.7 s —
all three were the same unresolved-callback bug).
Grouped by family; `1x` = the shape does not fuse (jit == node-walk),
and the honest differential is that row's jit time against the
intrinsic row's jit time.

| bench            | n    | jit       | node-walk | speedup |
|------------------|------|-----------|-----------|---------|
| `fold_intr`      | 100k | 0.25 ms   | 2.04 s    | 8047x   |
| `fold_trait`     | 100k | 0.30 ms   | 2.01 s    | 6712x   |
| `fold_rec`       | 100k | 12.3 ms   | 0.22 s    | 18x     |
| `map_intr`       | 100k | 2.33 ms   | 3.90 s    | 1671x   |
| `map_fmshape`    | 100k | 2.35 ms   | 3.97 s    | 1691x   |
| `map_init`       | 100k | 2.35 ms   | 4.42 s    | 1880x   |
| `map_intr_10k`   | 10k  | 0.24 ms   | 0.27 s    | 1154x   |
| `map_push`       | 10k  | 0.26 s    | 0.42 s    | 2x      |
| `filter_intr`    | 100k | 1.06 ms   | 2.53 s    | 2381x   |
| `filter_fmshape` | 100k | 1.37 ms   | 3.27 s    | 2387x   |
| `find_intr`      | 500k | 1.90 ms   | 6.72 s    | 3533x   |
| `find_fold`      | 500k | 5.19 ms   | 28.6 s    | 5510x   |
| `flatmap_intr`   | 10k  | 1.63 ms   | 0.51 s    | 315x    |
| `flatmap_fold`   | 10k  | 1.42 s    | 1.93 s    | 1x      |
| `lfold_intr`     | 100k | 4.13 ms   | 2.04 s    | 494x    |
| `lfold_intr_4k`  | 4k   | 0.14 ms   | 57 ms     | 400x    |
| `lfold_rec`      | 4k   | 0.28 s    | 0.27 s    | 1x      |
| `mfold_intr`     | 100k | 23.0 ms   | 3.75 s    | 163x    |
| `mfold_trait`    | 100k | 23.6 ms   | 4.58 s    | 194x    |

## Reading the table

- **Trait dispatch fuses (FIXED)**: the resolved trait site now
  falls through to HOF pre-materialization exactly like a direct
  call (`CallSite::premat_fn_args`), and `fold_trait` went
  2.21 s -> 0.31 ms — parity with the intrinsic; the generic
  `|c: Collection|` path fuses too. The 08-25 residue was
  `mfold_trait`: Map's impl fold is a Graphix WRAPPER body
  (`fold_pairs` with a derived callback closing over `f`), and the
  nested callback's call to `f` didn't statically resolve — the
  fn-params were registered only after the instance body typecheck.
  FIXED 2026-08-27 (`b386f97d`, registered before the check):
  `mfold_trait` 4.63 s -> 24 ms, parity with `mfold_intr`; the same
  fix is why `filter_fmshape` and `find_fold` fuse in the 08-30 table.
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
- **`find_fold` fuses since `b386f97d`** (was the worst row at
  31.5 s, unfused): 5.2 ms vs 1.9 ms — the remaining 2.4x is the
  Option-carrying fold visiting every element; a fold cannot
  early-exit, so the derivation is inherently a full scan.
- **`flatmap_fold`** pays O(n^2) concat copies AND doesn't fuse.
- `lfold_intr` at 4.3 ms/100k is ~9x faster than the 2026-07
  `list_fold_sum` row (38 ms) — the flatten boundary improved since.

**Per-operation verdict (the P2b question, re-judged 2026-08-30)**:
at parity and deletable today — Array `map` (`filter_map`-with-total-
callback 2.35 ms and `init` 2.35 ms vs 2.33 ms), Array `filter`
(`filter_fmshape` 1.37 ms vs 1.06 ms), and Map's whole impl (already
Graphix wrappers, `mfold_trait` 23.6 ms vs 23.0 ms). Not yet: `fold`
itself — a hand-written loop pays ~50x in per-element call overhead
(`fold_rec` 12.3 ms vs 0.25 ms: `array::len` + `a[i]$` DynCalls and
the cross-kernel `f` call), which is the cost any user-written
`Collection` impl runs at; `find`/`find_map` — the Option-carrying fold
FUSES now (5.2 ms vs 1.9 ms) but cannot early-exit; `flat_map` — the
fold+concat derivation is O(n²) by construction and doesn't fuse;
List — the loop-CARRIED Value formal keeps an activation per level
(`lfold_rec` 0.28 s vs 0.14 ms). Fixed since 08-25: wrapper premat /
fn-formal forwarding and capture (`b386f97d`). Re-run this corpus
after each remaining cut lands — the verdicts are per-operation.
