# Fusion-state metric & end-state gap map

Last updated 2026-06-04.

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

- `run!(name, code, pred)` — default; asserts **`Jit`** (`JIT_INVOCATIONS > 0`).
- `... ; FuseExpect::Interp` — fuses but runs on the interpreter
  (`FUSION > 0 && JIT == 0`).
- `... ; FuseExpect::None` — no fused kernel (`FUSION == 0`).

Measure the current map any time with:
`GRAPHIX_FUSION_DISCOVERY=1 <test-bin> --nocapture --test-threads=1 jit`
(prints `FUSEMAPF <path> Fuses|None`, `FUSEMAPJ <path> Jit|NoJit`, and
— under a debug-only instrumentation — `FUSEBAIL <path> <reason-tags>`
listing *why* each non-fusing fixture bailed).

### The FUSEBAIL instrumentation

`graphix-compiler` carries a debug-only thread-local fusion-bail log
(`gir_jit_helpers::{record_fuse_bail, take_fuse_bails, reset_fuse_bails}`).
Lowering records a short tag at each give-up site:
- `node:<Variant>` — `emit_node`'s catch-all hit an un-lowerable node
  (`Sample`, `Map`, `ByRef`, `StructWith`, `Connect`, `Lambda`, …).
- `dostmt:<Variant>` — a block statement that isn't a `Bind`/`Nop`/
  `TypeDef`/`Use`.
- `emit:<Helper>` — a producer/accessor sub-emitter bailed
  (`emit:ArrayRef`, `emit:Select`, `emit:Array`, …).
- `call:<name>` / `callqual:<path>` — a call to a function the kernel
  builder doesn't know how to lower (bare or module-qualified).

The `run!` discovery branch harvests these per fixture. This is the
tool that turns "this fixture is None" into "this fixture is None
*because* X", which is what makes the gap map actionable.

## Current metric (graphix-tests, 547 `run!` fixtures)

| state | count | share | meaning |
|---|---:|---:|---|
| **Jit** | 119 | 22% | fuses + JIT-compiles + runs native |
| **Interp** | 2 | 0.4% | fuses, runs on interp (JIT can't lower yet) |
| **None** | 426 | 78% | no fused kernel |

**This replaces the previous (badly inflated) figure of 385 Jit.**
That number counted the hollow `let result = {code}` *wrapper*
identity kernel as fusion; #139's identity-kernel suppression removed
it, dropping the honest count to ~115. The 547-fixture corpus is the
real fusion frontier.

## None breakdown (the work queue)

From a FUSEBAIL harvest, by dominant blocker:

| count | category | tractability |
|---:|---|---|
| ~132 | **qualified user-fn calls, resolved but type-rep-blocked** | the big one |
| 82 | EMPTY (destructure-let / select-internal / **typecheck-error**) | mixed: some correct-None |
| 78 | async-None (sys/json/pack/toml/sqlite/tcp/net/tls/http/fs) | **correct None** |
| 32 | lambda-bind-in-body (`node:Lambda`) | needs registry coordination |
| 28 | map literal / map ops (`node:Map`) | needs `GirType::Map` |
| 13 | reactive `never()` | **correct None** |
| ~20 | structural: `emit:Select`, `emit:Array`, array slice, ByRef/Deref, try/catch, Sample, StructWith, checked-arith, Connect | per-construct lowering |
| ~12 | misc builtin calls (`filter`, `sum`, `divide`, `all`, …) | several are loose-`Number` **correct-None** |

### The qualified-user-fn-call cluster (~132 — the biggest opportunity)

Calls to **graphix-implemented** functions (`list::*`, `array::*`
graphix HOFs, `buffer::*`, interface-module functions) used to bail
silently. Two foundational fixes (2026-06-04) made them *resolve*:

1. **Interface-proxy resolution.** A signed module re-exports each impl
   binding's value to its public `val` signature binding under a
   *different* BindId; the caller references the signature BindId.
   `static_resolve::collect_lambda_binds` now follows the Module's
   `proxy()` map (impl_id → sig_id) so a caller's interface `Ref`
   resolves to the impl `LambdaDef`. Without this, `resolved_apply()`
   was `None` for every cross-module-through-interface call.
2. **Qualified source-name.** `emit_node`'s CallSite Lambda arm now
   derives the kernel registry / `GirOp::Call` key from the full path
   when `ident_of` rejects a module-qualified name.

After resolution the kernel build still bails on **type
representability** — the remaining, per-cluster blocker:
- `/list` (44): **recursive variant types** (`List<'a> = [\`Cons('a,
  List<'a>), \`Nil]`) — `GirType::from_type` can't represent an
  infinite type.
- `/array` (33): graphix HOFs over **composite element types**
  (`array::map`/`fold`/`init` are scalar-element on both backends).
- `/buffer` (34): **bytes + mutable-ref** buffer ops (ByRef/Deref).
- `/inner`, `/mod_a`, `/outer` (~21): **abstract (opaque) types** —
  `from_type(T)` can't know an abstract type's concrete runtime rep
  (i64 vs struct vs string) without the impl's private typedef.

So the ~132 split further by *type-system feature needed*, not by a
single fix.

## Distance to end-state

Of the ~426 None, a large fraction are **correct-None**: 78 async/IO,
13 `never()`, a chunk of the 82 EMPTY (typecheck-error / error-
expecting / `Err`-asserting fixtures), and several loose-`Number`
builtins. The genuinely-should-fuse remainder is dominated by the
type-system features above (recursive variants, composite-element
HOFs, abstract-type rep, `GirType::Map`) plus the structural cliffs
(lambda-binds-in-bodies, Sample/Connect/ByRef reactivity, array
slices, StructWith).

## How to close a gap

1. Pick a blocker from the FUSEBAIL harvest.
2. Implement the lowering / type-rep in `fusion/lowering.rs` +
   `gir`/`gir_interp`/`gir_jit`.
3. The bidirectional check then **fails** the now-fusing fixtures
   ("this now JITs — upgrade to `Jit`"); remove their `; FuseExpect::None`
   and the `// ASPIRE:` comment.
4. Re-harvest; the Jit % moves monotonically up, with no silent
   regressions.

## Session log

- **2026-06-04**: interface-proxy resolution + qualified source-name +
  TypeDef/Use-skip-in-blocks. 115 → 119 Jit (`rectypes0`, `rectypes1`,
  `typedef_tvar_ok`, `interface_no_abstract_types`). Established the
  FUSEBAIL instrumentation and this accurate map.
