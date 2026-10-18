# Collection operations: intrinsics vs Graphix bodies

For each collection operation, the INTRINSIC (the compiler-owned
MapQ/FoldQ node, `design/collection_intrinsics.md`) against the same
operation reached or written another way:

- **`*_intr`**: the intrinsic called directly (`array::fold`, …); the
  baseline, and for the 100k rows the same shape as the main `bench/`
  corpus (`fold_sum`, `map_fold`, …).
- **`*_trait`**: the same intrinsic reached through `Collection::…`
  trait dispatch.
- **`*_fmshape` / `find_fold` / `flatmap_fold` / `map_init`**: the
  trait DEFAULT's body shape written out directly (`map` as
  `filter_map` with a total callback, `find` as a fold carrying the
  default's `(done, Option)` tuple, `flat_map` as fold + concat, `map`
  as `init(len, a[i])`).
- **`*_rec` / `map_push`**: the operation written in Graphix as a tail
  recursion (an index loop, `push` for Array, `[<>]` / `[<x, rest..>]`
  patterns for List): what a user-defined `Collection` impl writes.
- **`flatmap_cons` / `flatmap_list`**: `flat_map` built by consing each
  produced element onto a List and finishing with one
  `list::to_array_rev`, by hand or through a nested fold.

Self-timing and seeding are `bench/`'s conventions (see
`bench/README.md`); `run.sh` here runs `../run.sh` over this directory.
The map variants thread `seed` through the callback so the timed region
provably contains the map stage in both modes.

Sizes: 100k for the linear Array/List/Map rows, 500k for the find
rows, 10k for the rows quadratic by representation (`map_push`,
`flatmap_*`: each step copies the array), 4k for `lfold_intr_4k` and
`lfold_rec`.

`collection_bodies_*` in `stdlib/graphix-tests/src/lang/collection.rs`
pins VALUE agreement across all the implementation routes of each
family at small n, and `run!` adds the engine axis.

## Results

Quick build, `bench` CPU mode, best of 3 per mode (2026-10-13), under
the default fork mode, `auto`. Grouped by family. `timeout` means the
node-walk did not finish within run.sh's 120 s limit.

| bench            | n    | jit       | node-walk | speedup |
|------------------|------|-----------|-----------|---------|
| `fold_intr`      | 100k | 0.43 ms   | 1.52 s    | 3534x   |
| `fold_trait`     | 100k | 0.45 ms   | 1.55 s    | 3459x   |
| `fold_rec`       | 100k | 1.73 ms   | 7.08 s*   | 4092x   |
| `map_intr`       | 100k | 1.87 ms   | 3.05 s    | 1629x   |
| `map_fmshape`    | 100k | 2.09 ms   | 2.84 s    | 1355x   |
| `map_init`       | 100k | 1.88 ms   | 3.60 s    | 1916x   |
| `map_intr_10k`   | 10k  | 0.24 ms   | 0.20 s    | 810x    |
| `map_push`       | 10k  | 0.14 s    | 5.59 s    | 40x     |
| `filter_intr`    | 100k | 1.30 ms   | 2.50 s    | 1923x   |
| `filter_fmshape` | 100k | 1.24 ms   | 3.16 s    | 2549x   |
| `find_intr`      | 500k | 0.49 ms   | 7.21 s    | 14773x  |
| `find_fold`      | 500k | 46.1 ms   | 59.5 s    | 1291x   |
| `find_rec`       | 500k | 5.86 ms   | 38.6 s*   | 6587x   |
| `flatmap_intr`   | 10k  | 1.31 ms   | 0.38 s    | 289x    |
| `flatmap_fold`   | 10k  | 0.47 s    | 1.22 s    | 2.6x    |
| `flatmap_cons`   | 10k  | 3.80 ms   | 0.42 s    | 112x    |
| `flatmap_list`   | 10k  | 5.28 ms   | 1.40 s    | 264x    |
| `lfold_intr`     | 100k | 4.96 ms   | 1.56 s    | 315x    |
| `lfold_intr_4k`  | 4k   | 0.20 ms   | 35 ms     | 179x    |
| `lfold_rec`      | 4k   | 0.15 ms   | 0.20 s*   | 1302x   |
| `lfold_rec_100k` | 100k | 3.10 ms   | 4.59 s*   | 1481x   |
| `mfold_intr`     | 100k | 22.9 ms   | 2.56 s    | 112x    |
| `mfold_trait`    | 100k | 22.7 ms   | 6.16 s    | 272x    |

`*` node-walk re-measured 2026-10-18 (quick build, one unpinned run)
once a select's list test stopped walking the spine.

With `GRAPHIX_PAR=off` the JIT takes 1.91 ms for `find_intr` (its
kernel loop forks under `auto`) and 1.08 ms for `fold_rec`; the other
rows move little.

## Reading the table

- **Trait dispatch costs nothing** once resolved: `fold_trait` and
  `mfold_trait` match their intrinsics under the JIT.
- **`map` derives at parity**: `map_fmshape` (`filter_map` with a total
  callback, which fusion routes to the map loop) and `map_init` run
  with `map_intr`; `filter_fmshape` runs with `filter_intr`.
- **The hand-written fold** (`fold_rec`) runs a few times the
  intrinsic: the loop pays its own bounds check, `$` and select per
  element. Its node-walk is linear; each activation costs tens of
  microseconds there, which is what fusion is for.
- **`find`**: the default's tuple fold (`find_fold`) visits every
  element and carries a tuple, about 25x the serial intrinsic. A
  hand-written loop (`find_rec`) stops at the match; it wins against
  Array's intrinsic only on a hit near the front, and never against the
  forked one.
- **`flat_map`**: the fold + concat derivation (`flatmap_fold`) is
  O(n²) by construction; consing onto a List and reversing once
  (`flatmap_cons`, `flatmap_list`) is linear and fuses, a few times the
  intrinsic.
- **List**: the hand-written pattern fold (`lfold_rec`) runs with the
  intrinsic at 4k, and `lfold_rec_100k` beats `lfold_intr`, which pays
  the flatten boundary.
- **`map_push`** copies the array at each push, so it is quadratic on
  both engines.
