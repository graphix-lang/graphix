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
rows, 10k for the O(n²)-by-representation rows (`map_push`,
`flatmap_*` — each step copies the array), 4k for the list-recursion
pair's original size (kept for continuity; the pair fuses since
2026-08-30 and `lfold_rec_100k` is the linear-row twin).

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
| `fold_rec`       | 100k | 1.5 ms    | 0.22 s    | 143x    |
| `map_intr`       | 100k | 2.33 ms   | 3.90 s    | 1671x   |
| `map_fmshape`    | 100k | 2.35 ms   | 3.97 s    | 1691x   |
| `map_init`       | 100k | 2.35 ms   | 4.42 s    | 1880x   |
| `map_intr_10k`   | 10k  | 0.24 ms   | 0.27 s    | 1154x   |
| `map_push`       | 10k  | 0.26 s    | 0.42 s    | 2x      |
| `filter_intr`    | 100k | 1.06 ms   | 2.53 s    | 2381x   |
| `filter_fmshape` | 100k | 1.37 ms   | 3.27 s    | 2387x   |
| `find_intr`      | 500k | 1.90 ms   | 6.72 s    | 3533x   |
| `find_fold`      | 500k | 5.19 ms   | 28.6 s    | 5510x   |
| `find_rec`       | 500k | 8.0 ms    | 0.70 s    | 88x     |
| `flatmap_intr`   | 10k  | 1.4 ms    | 0.51 s    | 364x    |
| `flatmap_fold`   | 10k  | 1.42 s    | 1.93 s    | 1x      |
| `flatmap_cons`   | 10k  | 3.8 ms    | 0.56 s    | 147x    |
| `flatmap_list`   | 10k  | 5.1 ms    | 1.78 s    | 349x    |
| `lfold_intr`     | 100k | 4.3 ms    | 2.04 s    | 474x    |
| `lfold_intr_4k`  | 4k   | 0.14 ms   | 57 ms     | 400x    |
| `lfold_rec`      | 4k   | 5.6 ms    | —         | 1x      |
| `lfold_rec_100k` | 100k | 110 ms    | —         | 1x      |
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
- **`flatmap_cons` is the right derivation (Eric, 2026-08-30)**: cons
  every produced element onto a List (O(1)) and finish with one
  `list::to_array_rev` — linear, fuses, 5.8 ms vs the intrinsic's
  1.6 ms (3.5x, the per-element `` `Cons `` construction). Two hand
  conses per element; the general form folds the chunk.
- **`flatmap_list` — the general form (the chunk consed through a
  NESTED `array::fold` inside the outer callback) fuses since
  2026-08-30**: 7.2 ms vs the intrinsic's 1.6 ms, one kernel with nested
  loops. It was 18.7 s and QUADRATIC (1.09 / 4.26 / 18.7 s at 2.5k / 5k /
  10k) because the shape did not fuse at all: `CallSite::resolve_static`'s
  recursion knot was keyed on the def alone, so the inner `fold` site —
  reached while the outer `fold` resolved, because the callback premats
  in that window — was stamped with the OUTER instance, the static graph
  read `fold -> callback -> fold` as a cycle, and the emitter refused the
  region ("mutually recursive static call edge"). A user HOF nested
  under its own callback (`apply(|y| apply(g, y), x)`) hit the same
  refusal. The knot now keys on INSTANTIATION IDENTITY (def + the source
  lambda each fn arg resolves to — `FnArgIdentity`); pins in
  `lang/collection.rs` (`nested_same_intrinsic`, `nested_map_in_map`,
  `user_hof_nested`, `nested_mixed_types`) and `lang/functions.rs`
  (`cps_wrapper_recursion`, the termination case).
- **The node-walk was quadratic on that shape too — FIXED 2026-08-30**
  (19.0 s → 1.78 s at 10k; the `i64`-accumulator twin 1.09 / 4.26 / 18.6 s
  → 0.31 / 0.66 / 1.42 s at 2.5k / 5k / 10k, linear). Each outer slot's
  callback body is lazily compiled and its nested `array::fold` is a
  fresh per-callsite instance; instance signatures SHARED the def's
  `LambdaIds` nodes (`FnType::cow_walk` cloned the `SArc`), so the `f`
  param cell was one hub every retained instance's callback linked into
  and `typecheck1_resolve` walked `ids()` over it per site — 57.8% of
  the run, per-bind cost doubling with n. An instantiation now SNAPSHOTS
  its def's node (`LambdaIds::instantiate`: same `own`, a one-way copy
  of the links) so def-body facts carry but a site's inflows land on its
  own copy. The same hub was why `hof_nested_map_json_read` could not
  type (its arg cell held every instance's callback, so the site never
  resolved statically). What remains is the per-activation constant:
  ~140 µs per outer slot for the four lazy binds a nested loop costs.
Only the interp fallback pays this now that the shape fuses.
- `lfold_intr` at 4.3 ms/100k is ~9x faster than the 2026-07
  `list_fold_sum` row (38 ms) — the flatten boundary improved since.

**Per-operation verdict (the P2b question, re-judged 2026-08-30)**:
at parity and deletable today — Array `map` (`filter_map`-with-total-
callback 2.35 ms and `init` 2.35 ms vs 2.33 ms), Array `filter`
(`filter_fmshape` 1.37 ms vs 1.06 ms), and Map's whole impl (already
Graphix wrappers, `mfold_trait` 23.6 ms vs 23.0 ms). Not yet: `fold`
itself — the hand-written loop's cost was ONE DynCall per iteration,
`i < array::len(a)` (14 of its 15 ms; the callback call is ~2.6 ns and
the rebind/index/qop/select ~6 ns): with `len` hoisted to a
loop-invariant parameter the loop runs in 1.13 ms, 3.8x the intrinsic.
`BuiltIn::FASTCALL` (2026-08-30 — `array::len`, `str::len`, `map::len`
opted in) replaces the dispatch with a direct call over a zero-copy
stack buffer of (disc, payload) pairs viewed as `&[Value]`: 15.2 ms ->
1.47 ms, i.e. ~3 ns per call (the DynCall was ~140 ns; a first cut that
kept DynCall's pooled-Vec marshal was ~60 ns). The hand-written fold is
now 5.8x the intrinsic and the remainder is the loop's own bounds
check, qop and select — the honest price of writing the loop yourself.
`find`/`find_map` — the
Option-carrying fold FUSES now (5.2 ms vs 1.9 ms) but cannot
early-exit; a hand-written tail loop CAN (`find_rec`, 2026-08-30): it
stops at the match, fuses, and runs the worst-case full scan at 8.0 ms
(16 ns/element — `fold_rec`'s rate, ~2x the intrinsic's slot walk),
which means on a uniform target it BEATS the intrinsic on average
(measured 1.6 ms at a 24%-position hit; the intrinsic scans all slots
by rule). So early exit blocks only the fold-derived trait DEFAULT —
a per-type loop impl has it today; what a deletion needs is `find`'s
default written as a loop per representation, not new machinery;
`flat_map` — the fold+concat derivation is O(n²) by construction (the
cons + `to_array_rev` derivation, `flatmap_cons`, is 5.8 ms vs 1.6 ms);
List — the native-List phase A rep swap (2026-08-31,
`design/list_native.md`) improved every INTRINSIC row (slim 2-slot
cells, no tag, no per-consult strcmp: `lfold_intr` 6.5 -> 4.3 ms,
`flatmap_cons` 5.8 -> 3.8 ms, `flatmap_list` 7.2 -> 5.1 ms) but
REGRESSED the hand-written recursion rows to interp speed
(`lfold_rec_100k` 4.5 -> 110 ms): the type is opaque until phase B's
list patterns, so the interim spelling selects over `list::uncons`,
and `(x, rest)` destructures a nullable TUPLE — the 2026-08-30
non-scalar bind machinery covers VARIANT payloads only, so the select
de-fuses and the loop node-walks (~1.1 µs/elem). Phase B's `[<h,
rest..>]` patterns re-target that machinery at the list rep (nil =
len-0 test, head/tail from the spine cell) and are expected to beat
the 2026-08-30 numbers (which were: hand fold 0.35 ms at 4k, BEATING
the intrinsic at 100k — 4.5 vs then-6.5 ms — via the variant-payload
bind + Value-carried tail rebind, both of which survive and carry
over to the pattern lowering).
Fixed since 08-25:
wrapper premat / fn-formal forwarding and capture (`b386f97d`),
same-HOF nesting (`3bd9a9a9`), the interp's nested-HOF quadratic
(`ef153027`). Re-run this corpus after each remaining cut lands — the
verdicts are per-operation.
