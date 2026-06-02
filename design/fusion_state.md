# Fusion-state metric & end-state gap map

Last updated 2026-05-28.

This document tracks how close the test corpus is to the fusion/JIT
end-state, using the bidirectional `run!` harness as the source of
truth.

## The mechanism

Every fixture runs through `run!` (in `graphix-package-core/src/testing.rs`)
which expands to three `#[tokio::test]` modes — `interp`
(`FusionDisabled`, ground truth), `fused` (`JitDisabled`, kernels run
on the interpreter), and `jit` (full fusion + JIT). Each fixture
declares an expected fusion outcome, asserted **bidirectionally** — a
fixture that fuses when it shouldn't, or fails to fuse/JIT when it
should, fails the test:

- `run!(name, code, pred)` — default; asserts **`Jit`** (fuses AND the
  JIT compiles + runs it: `JIT_INVOCATIONS > 0`).
- `run!(name, code, pred; graphix_package_core::testing::FuseExpect::Interp)`
  — fuses but runs on the interpreter (JIT can't lower the shape yet):
  `FUSION_INVOCATIONS > 0` in fused mode, `JIT_INVOCATIONS == 0` in jit
  mode.
- `... ; FuseExpect::None` — produces no fused kernel at all (async/IO
  edge, error/typecheck test, or no sync subgraph).

Two `cfg(debug_assertions)` thread-local counters back this, reset
after runtime init so they reflect only the fixture's own program:
- `JIT_INVOCATIONS` — bumped at every JIT'd wrapper entry.
- `FUSION_INVOCATIONS` — bumped at `GirNode::update`'s commit point
  (every fused-kernel execution, JIT or interp).

`run_no_jit!` is gone — its 466 call sites were converted to `run!`
with measured levels. The bidirectional check means the suite is a
live, drift-detecting map of the fusion frontier: improve fusion and
the now-fusing `None`/`Interp` fixtures fail until their annotation is
upgraded; regress and a `Jit` fixture fails.

Measure the current map any time with:
`GRAPHIX_FUSION_DISCOVERY=1 <test-bin> --nocapture --test-threads=1 jit`
(prints `FUSEMAPF <path> Fuses|None` from fused mode + `FUSEMAPJ <path>
Jit|NoJit` from jit mode per fixture).

## Current metric (graphix-tests, 543 `run!` fixtures)

| state | count | share | meaning |
|---|---:|---:|---|
| **Jit** | 385 | 71% | fuses + JIT-compiles + runs native |
| **Interp** | 1 | 0.2% | fuses, runs on interp (JIT can't lower yet) |
| **None** | 157 | 29% | no fused kernel |

Of the 157 `None`:
- **63 are aspiration GAPS** — pure sync computation that *should* JIT
  in the end-state but hits a current cliff. Each is annotated in-file
  with `// ASPIRE: Jit (currently None) — blocked on: <cliff>`.
- **94 are genuinely-None** — async/IO edges (timers, net, tcp, tls,
  http, fs, sqlite), serialization round-trips, or type-error /
  compile-error / module-structure tests with no kernel. These
  correctly never fuse.

**Distance to end-state:** of the ~449 fixtures that *should* fuse
(385 Jit + 1 Interp + 63 gaps), 386 do today — **86%**. Closing the 63
gaps (+ the 1 Interp) is the remaining work.

## Gaps by blocker (the work queue)

Sorted by fixture count — the biggest clusters are the highest-leverage
fixes:

| gap fixtures | blocker | notes |
|---:|---|---|
| ~12 | composite / value-shape cross-kernel `GirOp::Call` args (**task #131**) | lambdas whose captures/args/return are tuple/struct/array/variant/nullable, called via `GirOp::Call`. Also the coupled nested-closure + unique-kernel-naming work. |
| 10 | map literal / `Map` ops not lowered | `{k => v}` literals and `map::*` builtins have no GIR lowering. |
| 8 | datetime / duration not a `GirType` | datetime/duration arithmetic — they're prims at the language level but `GirType::from_type` doesn't accept them. |
| 6 | nested variant / composite payloads | `select` / `match` on variants whose payloads are themselves composite; `rectypes`. |
| 5 | labeled-arg lambda call | a lambda called with `#labeled` args / defaults doesn't lower to a fused call. |
| ~8 | assorted sync builtins not yet fused | `sum`, `product`, `all`, `divide`, `filter_err`, `buffer::decode`, some `sys::dirs::*`. Sync builtins that should DynCall-fuse. |
| ~5 | misc | struct-with spread (`{s with …}`), unannotated recursive lambdas, fn-typed value as dynamic dispatch, HOF type-propagation for json deserializers. |

(`grep -rn "// ASPIRE:" stdlib/graphix-tests/src` for the per-fixture list.)

## How to close a gap

1. Pick a cluster (e.g. datetime, or map ops).
2. Implement the lowering / lift the cliff in `fusion/lowering.rs` +
   `gir`/`gir_interp`/`gir_jit`.
3. The affected fixtures' `jit` (or `fused`) arm will start passing —
   but the bidirectional check then **fails** them ("this now JITs,
   upgrade to `fuse: Jit`"). Remove the `; FuseExpect::None` (back to
   default `Jit`) and delete the `// ASPIRE:` comment.
4. Re-run; green again at a higher fusion percentage.

The metric (Jit % and gap count) moves monotonically toward 100% as
cliffs are closed, and the harness guarantees no silent regressions
along the way.
