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
  gap (trait-dispatched intrinsics interpret — the P3 item).
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

Release build, best-of-3 per mode, 2026-08-25 (post `is_a_int` guard).
Grouped by family; `1x` = the shape does not fuse (jit == node-walk),
and the honest differential is that row's jit time against the
intrinsic row's jit time.

| bench            | n    | jit       | node-walk | speedup |
|------------------|------|-----------|-----------|---------|
| `fold_intr`      | 100k | 0.28 ms   | 2.28 s    | 8082x   |
| `fold_trait`     | 100k | 0.31 ms   | 2.19 s    | 7150x   |
| `fold_rec`       | 100k | 8.40 s    | 9.34 s    | 1x      |
| `map_intr`       | 100k | 2.5 ms    | 4.60 s    | 1876x   |
| `map_fmshape`    | 100k | 4.21 s    | 4.39 s    | 1x      |
| `map_init`       | 100k | 2.6 ms    | 4.99 s    | 1936x   |
| `map_intr_10k`   | 10k  | 0.24 ms   | 0.32 s    | 1338x   |
| `map_push`       | 10k  | 1.80 s    | 2.10 s    | 1x      |
| `filter_intr`    | 100k | 1.3 ms    | 2.86 s    | 2285x   |
| `filter_fmshape` | 100k | 2.3 ms    | 3.60 s    | 1534x   |
| `find_intr`      | 500k | 1.7 ms    | 7.55 s    | 4421x   |
| `find_fold`      | 500k | 31.5 s    | 32.9 s    | 1x      |
| `flatmap_intr`   | 10k  | 2.1 ms    | 0.60 s    | 294x    |
| `flatmap_fold`   | 10k  | 1.59 s    | 2.29 s    | 1x      |
| `lfold_intr`     | 100k | 4.3 ms    | 2.27 s    | 526x    |
| `lfold_intr_4k`  | 4k   | 0.15 ms   | 62 ms     | 407x    |
| `lfold_rec`      | 4k   | 0.29 s    | 0.29 s    | 1x      |
| `mfold_intr`     | 100k | 26 ms     | 4.16 s    | 158x    |
| `mfold_trait`    | 100k | 4.77 s    | 5.25 s    | 1x      |

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
- **The hand-written recursion rows never collapse** (`fold_rec`,
  `map_push`, `lfold_rec`): the callback is a FORMAL, and a fn-typed
  formal fails `structural_tail_loop`'s kind gate — finding 3's widest
  face, since every stdlib-shaped Graphix body threads `f` as a
  parameter. They native-recurse per level on both engines (correct
  and memory-bounded since the `is_a_int` guard, but no loop).
- **`lfold_rec` was quadratic on top** (finding 2's cost face): each
  arm consult walked the remaining chain — 9.58 s at 4k, curve
  0.55/1.97/9.6/49.4 s at 1k/2k/4k/8k, clean O(n^2). FIXED same day
  by the shallow arm discriminators (`Type::shallow_discriminant`):
  the curve is linear (0.050/0.096/0.201/0.414 s — 119x at 8k) and
  the row above is post-fix. The residual ~1900x to `lfold_intr_4k`
  is finding 3's per-level activation, no longer the type test.
- **`map_fmshape` loses fusion where `filter_fmshape` keeps it**: the
  map default's callback returns bare `'b` in `Option<'b>` position
  (the trait default body), and that widening de-fuses the loop;
  filter's callback produces the union itself and fuses at 1.9x the
  intrinsic. A fusion-coverage bug worth its own fix — with it, the
  map DEFAULT would be a 2-ms row.
- **`map_init` matches the intrinsic** (2.6 ms vs 2.5 ms): `map`
  derived as `init(len(a), |i| f(a[i]))` is free under the JIT — the
  one place the measurement found a deletable-candidate path, blocked
  only by the default-body fusion bug above (and Array-only: `init`
  is index-based).
- **`find_fold` is the worst derivation**: unfused plus a per-element
  double select over the Option accumulator — 31.5 s vs 1.7 ms.
- **`flatmap_fold`** pays O(n^2) concat copies AND doesn't fuse.
- `lfold_intr` at 4.3 ms/100k is ~9x faster than the 2026-07
  `list_fold_sum` row (38 ms) — the flatten boundary improved since.

**Per-operation verdict (the P2b question)**: every intrinsic stays.
Trait dispatch to a marker-bodied impl is FIXED (`fold_trait` at
parity) and finding 2's quadratic arm consults are FIXED (shallow
discriminators — `lfold_rec` 9.58 s -> 0.29 s). Still open: WRAPPER
impl bodies (`mfold_trait` — the trait-default shape), fn-typed
formals blocking the tail loop (finding 3), and the map-default
widening. The only
near-parity derivation (`map` via `init`) is gated on the
default-body widening fix, and only for Array. Re-run this corpus
after each of those lands — the verdicts are per-operation and may
flip.
