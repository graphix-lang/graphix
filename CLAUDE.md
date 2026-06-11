# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository. You should keep this file up to date! Whenever you have a significant conversation with the user about the project you should summarize it in this file as part of completing the assigned task.

## What is Graphix?

Graphix is a dataflow programming language particularly well suited for building UIs and network programming with netidx. Programs are compiled to directed graphs where operations are nodes and edges represent data flow paths. The language is reactive at the language level - when dependent values change, the entire graph updates automatically.

Key language features: lexically scoped, expression-oriented, strongly statically typed with type inference, structural type discipline, parametric polymorphism, algebraic data types, pattern matching, first-class functions and closures.

## Project Structure

This is a Rust workspace with these main crates:

- **graphix-compiler**: The compiler that parses and compiles Graphix expressions into node graphs. Entry point is `compile()` in `lib.rs` which calls `compiler::compile()` then typechecks the resulting node.
- **graphix-rt**: A general-purpose runtime that executes the compiled node graphs. The runtime runs in a background task and is interacted with via `GXHandle`. Supports custom extensions via the `GXExt` trait.
- **graphix-package**: Package system for graphix. Handles package loading, vendoring, and standalone builds.
- **graphix-derive**: Proc macros (e.g. `defpackage!`) used by packages.
- **graphix-shell**: REPL and CLI tool. The binary is named `graphix`.

The standard library is split into individual packages under `stdlib/`:
- **graphix-package-core**: Core builtins and types
- **graphix-package-array**, **-map**, **-str**, **-re**, **-rand**: Data structure and utility packages
- **graphix-package-sys**: System-level I/O (unified streams, filesystem, TCP, TLS, netidx, timers)
- **graphix-package-http**: HTTP client/server and REST helpers
- **graphix-package-toml**: TOML serialization/deserialization
- **graphix-package-xls**: Spreadsheet reading (xlsx, xls, ods, xlsb via calamine)
- **graphix-package-pack**: Native binary serialization via netidx Pack format
- **graphix-package-tui**: Terminal UI widgets (ratatui-based)
- **graphix-package-gui**: Graphical UI widgets (iced-based)
- **graphix-tests**: Language feature and stdlib integration tests (separate crate to avoid circular dev-deps)

Each stdlib package has Rust implementations in `src/` and Graphix source in `src/graphix/*.gx`.

Additional directories:
- **book/**: mdbook documentation source
- **book/src/examples/**: All graphix example programs (`tui/`, `gui/`, `net/` subdirs)
- **examples/**: Symlink to `book/src/examples/` for convenience
- **docs/**: Compiled HTML documentation

The compiler depends on netidx (a networked publish-subscribe system) which is expected to be at `../netidx/` (sibling directory).

The project uses workspace-level dependencies where possible.

The project uses poolshark where possible to avoid allocations. If it isn't
possible to avoid allocation using poolshark, then smallvec should be
considered.

## Building and Testing

Build the workspace:
```bash
cargo build                          # Debug build
cargo build --release                # Release build (optimized, LTO enabled)
```

Build specific crate:
```bash
cargo build -p graphix-shell         # Build shell
cargo build -p graphix-compiler      # Build compiler
```

Run tests:
```bash
cargo test                           # Run all tests in workspace
cargo test -p graphix-tests          # Test specific crate
cargo test pattern                   # Run tests matching name
```

Note, the compiler is designed to support multiple instances in a process,
therefore tests should be designed to run in parallel, running with
test-threads=1 should be avoided.

Run the Graphix shell:
```bash
cargo run --bin graphix                    # Start REPL
cargo run --bin graphix file.gx         # Execute file
cargo run --bin graphix --check file.gx # check that a file compiles and type checks
cargo run --bin graphix --help          # See all options
```

Build documentation:
from the graphix/book directory
```bash
mdbook build -d ../docs/book         # Build language docs to docs/book/
mdbook serve ../docs/book            # Serve docs locally
```

## Architecture

### Compilation Pipeline

1. **Parsing** (`graphix-compiler/src/expr/parser/`): Text → `Expr` AST with position info
2. **Compilation** (`graphix-compiler/src/node/compiler.rs`): `Expr` → `Node<R, E>` graph
3. **Type Checking**: Each node implements `typecheck()` to verify type correctness

Key types:
- `Expr`: Immutable AST representation with `ExprKind` variants
- `Node<R, E>`: `Box<dyn Update<R, E>>` - compiled graph node
- `ExecCtx<R, E>`: Execution context holding builtins, environment, runtime
- `Scope`: Lexical and dynamic module path information

### Node Graph Execution

Nodes implement either:
- `Update` trait: Regular graph nodes (most built-in nodes)
- `Apply` trait: Function applications (called by `CallSite` nodes)

The `Update` trait requires:
- `update()`: Process events and return output value
- `delete()`: Clean up node and children
- `typecheck()`: Verify types
- `refs()`: Populate referenced bind IDs
- `sleep()`: Put node to sleep (for unselected branches)

### Runtime System

The runtime (`graphix-rt`) implements the `Rt` trait which handles:
- Netidx subscriptions and publications
- Variable references and updates
- RPC calls
- Timer events

Event processing is batch-based: the runtime collects all simultaneous events into an `Event` struct and delivers them to the graph in one cycle. Multiple updates to the same variable in one cycle must be queued for the next cycle.

### Type System

Located in `graphix-compiler/src/typ/`:
- `Type`: Structural types including primitives, tuples, structs, variants, functions, refs
- `TVar`: Type variables for inference (bound via `TVal`)
- `FnType`: Function signature (args, return type, throws, constraints)

Types are structural - compatibility is based on structure, not names. Type inference uses constraint solving with type variables.

### Built-in Functions

Built-ins implement the `BuiltIn<R, E>` trait:
- `NAME`: Function name constant
- `init()`: Returns initialization function

The function's type is declared in the `.gx` file where the builtin is
bound — all arguments and the return type must have type annotations.

Register built-ins with `ExecCtx::register_builtin::<T>()`.

## Coding Style

- Rust code is formatted with `rustfmt` (`rustfmt.toml` in repo). Run `cargo fmt` before submitting.
- Rust conventions: `snake_case` for modules/functions, `CamelCase` for types/traits, `SCREAMING_SNAKE_CASE` for constants.
- Graphix source files use the `.gx` extension; keep examples small and focused.

## Code Review Process

When doing code review, follow the CR/XCR comment system:

1. Add comments as: `// CR <your-name> for <addressee>: comment text` to the relevant file near the relevant code
2. When issues are addressed, the comment becomes: `// XCR ...`
3. Review XCRs - delete if resolved, convert back to CR with explanation if not

This project maintains very high code quality standards - no shortcuts, careful consideration of all implications.

## Commits and Pull Requests

- PRs should include a concise summary, testing notes, and links to related issues.
- Treat `docs/` as build output — edit sources in `book/` and regenerate with `mdbook`. If you update docs or examples, rebuild the book.

## Common Patterns

### Working with Types

Use `format_with_flags()` to control type variable formatting:
```rust
format_with_flags(PrintFlag::DerefTVars, || {
    // Type printing code here
})
```

### Error Handling

Use the `wrap!` macro to add expression context to errors:
```rust
wrap!(node, some_result())
```

For creating error values:
```rust
err!(tag, "error message")           // Static message
errf!(tag, "format {}", args)        // Formatted message
```

### Node Implementation

When implementing nodes:
1. Store spec (`Arc<Expr>`) for error reporting
2. Implement all trait methods (update, delete, typecheck, refs, sleep)
3. Use `Refs` to track bound and referenced BindIds
4. Call `ctx.set_var()` when setting variables (handles caching)

## Testing

The purpose of writing tests is not for them to pass, it's to find
bugs in the main code. Never work around a problem with a test that
you think should work. Even if it isn't related to the purpose of the
test you are writing, every failure is an opportunity to learn about a
bug and fix it. If you find such an "off topic" bug, discuss it with
the user before trying to fix it yourself.

The parser includes it's own dedicated tests:
- `graphix-compiler/src/expr/test.rs`: The round trip test of the
  parser pretty printer with random expressions generated by
  proptest. Whenever we change the syntax we must update this test and
  it must run successfully (preferably overnight)
- `graphix-compiler/src/expr/parser/test.rs`: A selection of specific
  tests for the parser.

## Examples

All graphix example programs live in `book/src/examples/` (symlinked as `examples/` from the project root), organized by UI backend:
- `tui/` — Terminal UI examples
- `gui/` — Graphical UI examples (iced-based)
- `net/` — Network examples

The book includes these via mdbook's `{{#include ...}}` syntax, so they serve double duty as documentation and testable code.

TUI and GUI examples are visual and must be tested manually:
```bash
cargo run --bin graphix -- examples/tui/barchart_basic.gx
cargo run --bin graphix -- examples/gui/hello.gx
```

Some examples are code snippets that reference undefined variables and are meant to illustrate concepts within a larger context. These should remain syntactically valid but may not run standalone. When updating the compiler, review these examples to ensure they still compile.

## Development Notes

- The compiler is optimized for dev builds (opt-level="s", lto="thin")
  to reduce compile times. If you need to debug something you can turn
  this optimization off, however the parser may overflow the
  stack without at least some optimization.
- Release builds use full optimization (opt-level=3, codegen-units=1, lto=true)
- Rust edition 2024 is used throughout
- The project uses `triomphe::Arc` instead of `std::sync::Arc` for better performance
- Pooling is used extensively (`poolshark`, `immutable-chunkmap`) to reduce allocations

## Debugging the Compiler

### Trace Facility

The compiler has a built-in trace facility gated by a global `AtomicBool` (`TRACE` in `lib.rs`). Key tools:

- `trace() -> bool`: check if tracing is active
- `set_trace(bool)`: toggle tracing
- `with_trace(enable, spec, f)`: enable tracing for the duration of `f`, prints the spec position and any errors
- `tdbg!(expr)`: like `dbg!()` but only fires when `trace()` is true

Usage in the compiler: `callsite.rs` has `if trace() { ... }` guards that print pre/post callsite FnTypes with deref'd TVars. Builtins like MapQ also print their resolved types via `format_with_flags(PrintFlag::DerefTVars, ...)`.

The trace facility solves a critical problem: the compiler typechecks the entire stdlib on every compilation, which produces gigabytes of debug output if you just add `eprintln!`. To debug a specific expression, use `with_trace` to enable tracing only during that expression's compilation/typecheck, so only the relevant output appears.

### Type Alias Expansion in Contains

When `contains` encounters a `Type::Ref` (e.g. `Result<T, E>`), the Ref case at `contains.rs:56` expands both sides via `lookup_ref(env)` before recursing. This means TVar bindings established during `contains` store the **expanded** form (e.g. `[T, Error<E>]` instead of `Result<T, E>`). Code that inspects resolved types must handle both the `Type::Ref` form and the expanded `Type::Set` form — see `extract_cast_type` in `graphix-package-core/src/lib.rs` for an example.

### Two-Phase Typecheck and Deferred Checks

Builtins that need type information from their call site (e.g. `json::read` for type-directed deserialization) use a two-phase typecheck: return `NeedsCallSite` from `TypecheckPhase::Lambda`, then extract concrete types during `TypecheckPhase::CallSite(resolved)`. The compiler collects deferred check closures during `CallSite::typecheck` and processes them in a `while let Some(check) = ctx.deferred_checks.pop()` loop after primary typechecking. This loop processes cascaded checks automatically — a deferred check that pushes new deferred checks will have those processed in subsequent iterations.

HOF builtins (e.g. `MapQ`, `FoldQ`) that take function-typed arguments must return `NeedsCallSite` and handle the `CallSite(resolved)` phase to update their stored predicate types (`mftyp`, `etyp`) from the resolved FnType. This enables the deferred check cascade to propagate concrete types to inner predicates like `json::read`.


## Fusion / JIT subsystem (current state)

> The fusion/JIT subsystem was heavily reworked May–Jun 2026. This section is
> the **durable current-state summary**; the per-change history is in `git log`,
> and the deep design rationale is in `design/` (indexed below). The old
> blow-by-blow changelog was removed — most of it described code that has since
> been deleted (`GirType`, the GIR interpreter, `RegValue`/`ConstVal`,
> `EvalResult`, the 3-mode test harness, the AOT backend).

**Two evaluators, one of them canonical:**

- **Node-walk** (`node/*.rs` — the `Box<dyn Update>` reactive graph) is the
  **canonical execution model.** It runs when fusion is off, and is the
  **universal fallback** for any subtree the JIT can't compile. It must ALWAYS
  be correct (see the global `node-walk-is-canonical` memory). A fusion/JIT bug
  can *lose fusion* (a perf regression) but can never produce a *wrong answer* —
  correctness is structural.
- **Fusion → cranelift JIT** (`fusion/`, `gir_jit.rs`) identifies sync (pure)
  subtrees and compiles them to native code. **JIT success → splice the native
  kernel + delete the originals; JIT failure → don't splice, the originals
  node-walk.** There is no third evaluator.

**Direction of travel:** the GIR interpreter (a slower second copy of the
node-walk) was deleted; the GIR IR itself (`GirKernel`/`GirOp`/`GirExpr`) is
being removed so the JIT walks the node graph directly via `NodeView` and emits
CLIF — the pipeline becomes `Expr → node graph → CLIF` with no intermediate IR
(`design/delete_gir_ir.md`, Stage 1 landed behind `CFlag::DirectNodeJit`). Until
that lands, fusion lowers a node subtree to GIR (`fusion/lowering.rs`) and
compiles GIR → CLIF (`gir_jit.rs`).

**Value & type representation — use the netidx types, no parallel copies:**

- **Values:** netidx `Value` everywhere (`#[repr(u64)]`, 16 bytes = (disc,
  payload)). No bespoke value types — `RegValue`/`ConstVal`/`EvalResult` are
  gone. `Value::copy_unchecked` is the branch-free copy for proven scalars.
- **Types:** netidx `Type` everywhere in the GIR. `GirType` is gone. Runtime
  shape comes from `abi_kind(&Type) -> Option<AbiKind>` + `freeze_concrete` (in
  `gir.rs`); `PrimType` is the closed register-scalar set (the one *good* small
  classifier enum — exhaustively matched in codegen).

**Semantics — node-walk and JIT must agree (the differential fuzzer enforces it):**

- `&&`/`||` are **STRICT** — both operands required, `false && ⊥ = ⊥`. NOT
  short-circuit (uniform with every other binary op; a dataflow value reflects
  all its inputs).
- Float comparison uses graphix's **TOTAL order** (`Value::partial_cmp`): `NaN
  == NaN`, `NaN` sorts below every non-NaN (so `Value` is map-key-able). NOT
  IEEE.
- Checked arith (`+?`/`-?`/`*?`) detects overflow via `Value::checked_*`;
  unchecked wraps; integer div0 / signed MIN-/-1 → bottom.
- `a[i]` / `a[i..j]` / `bytes[i]` / `m{key}` are bounds-checked through shared
  `node::array` / `node::map` helpers — one semantic seam, all backends agree
  bit-for-bit.
- **Bottom** ("no value this cycle" — div0, `?`-error, a never-fired input) is
  `None`-from-`update` in the node-walk (canonical). A JIT'd kernel that would
  produce an *eager* bottom it can't represent simply doesn't fuse → node-walks
  (correct). `design/representable_bottom.md`.
- An **infinite PURE tail recursion hangs** the JIT (a native loop can't yield
  to the scheduler) — accepted/correct; the reactive node-walk's per-cycle
  "continue" is the artifact. `design/final_jit_architecture.md` Part 2.

**Testing is differential:**

- `run!` (`graphix-package-core/src/testing.rs`): each fixture runs in 2 modes —
  `interp` (node-walk, `FusionDisabled`) and `jit` (fusion+JIT) — asserting equal
  values. `FuseExpect::{Jit, None}` asserts *whether* it fuses+JITs (a
  bidirectional drift check). Optional `; shape:` asserts the compiled graph via
  `NodeShape` (`node_shape.rs`).
- **graphix-fuzz** (`graphix-fuzz/`) — the differential model-checking fuzzer:
  node-walk (trusted) vs JIT (under test). `check`/`run`/`generate`/`fuzz`/
  `minimize`/`regress`; the committed `findings/` corpus is the regression gate.
  It found ~a dozen real bugs this cycle. `design/graphix_fuzz.md`.
- **`FusionStats`** (D0, `fusion/mod.rs`) — per-`ExecCtx` compile-time counters
  (`attempted`/`fused`/`failed: Vec<(ExprId, reason)>`), populated by `try_fuse`
  (+ the classic splice, fused-only), exposed via `GXHandle::fusion_stats()`
  (the `ToGX` exec pattern), `TestCtx::fusion_stats()`, and graphix-fuzz's
  `run_program_with_stats` (which subtracts the per-init stdlib-root baseline —
  9 stdlib regions fuse per init under DirectJit). The `direct_node_jit_*`
  probes assert `fused > 0` via `all_three_agree_fused` for every probe known
  to fuse — the direct path degrades silently, so value agreement alone can't
  catch a coverage regression. Landing it immediately exposed two probes that
  had NEVER compiled (CompileErr == CompileErr passes `all_three_agree`).
  Read `failed` as a blocker profile, not a gap count: the jit_node
  attempt-then-recurse protocol means a wholly-fused trivial program still
  logs ~3 Module/Bind misses.
- A divergence is **at least as likely a fused/JIT bug as a node-walk one** —
  verify the intended semantics against the node-walk before changing the
  canonical model.

**Per-slot HOF callbacks** (`array::map(a, cb)` etc.): the callback fuses into a
template kernel once, then `clone_rebind`s per array slot (each slot gets fresh
async state). An *impure* callback splits at the async boundary — the sync part
fuses + JITs per slot, the async residue node-walks. `design/impure_hof_fusion.md`,
`design/composite_hof_fusion.md`, `design/clone_rebind_testing.md`.

**Kernel ABI** (until GIR-IR removal lands): kind-grouped params — scalars,
then array/tuple/struct pointers, then string, then 2-word variant/nullable/
value — derived from a single source (`gir.rs` `abi_params`/`AbiParamKind`).

### Design documents (`design/`)

**Current / active:**
- `distributed_jit.md` — the GIR-IR removal as it's actually being executed:
  fusion/JIT distributed as `Update::emit_clif` + `Update::jit` trait methods
  (the delete/sleep/refs pattern), `Apply::emit_clif` for builtins,
  `fusion::try_fuse` as mechanics-only library, `KernelSig` in
  `kernel_abi.rs`. In progress (Stage A landed; Stage C complete through
  C6 — scalars, strings/values, BindId env, composites, qop/DynCall,
  `select` via `emit_select_node`, StringInterpolate via
  `emit_string_interpolate_node`, and checked arith via
  `emit_checked_arith_node` + the shared `node::op::wrap_arith_error`
  core, NEW coverage the GIR path never had — overflow/div0 is the
  catchable `ArithError` error VALUE, never bottom). C5 notes: the direct path
  tests an explicit-type-predicate `i64 as _` over a Nullable scrutinee
  as `disc != NULL` (the classic path's trivially-true shortcut was
  arm-order-UNSOUND — wrong-value divergence, #200, since fixed in
  the classic path too), and refuses to fuse a select whose final arm
  is conditional under a possibly-bottom scrutinee (the classic
  `compile_ifchain` trap was reachable there and SIGILL'd — #201,
  classic path now refuses identically; the stmt form was already
  immune via its pending-exit scrutinee gate). Both repros live in
  findings/select-jun2026. `gir::freeze_normalized` exists because
  typecheck leaves a select's result type as the un-flattened arm
  union (`Set([i64, TVar→i64])`), which `freeze_concrete` rejects.
  Stage D1 landed: the eight HOF loop scaffolds extracted from the
  GirOp arms into `gir_jit/scaffold` (`gir_jit_scaffold.rs`) —
  mechanics (loop/buf/element binding+drops/pending-cleanup) behind
  `emit_*_loop` fns taking body closures over `BodyCx`; verified
  CLIF-identical via `GRAPHIX_DUMP_CLIF` differential. D2 reuses the
  scaffolds from `Apply::emit_clif` HOF impls (pass `ArraySrc { owned:
  true }` for fresh producer inputs — dead config under the GIR arms).
  D2-map landed: `MapFn::emit_clif` + MapQ's `Apply::emit_clif`
  orchestration + `MapImpl` via `emit_map_loop` — whole-block maximal
  fusion on the direct path, EXCEEDING classic on composite-elem and
  qop-body maps. TRAP (cost a day): `BuiltInLambda` (node/lambda.rs,
  the builtin plumbing wrapper) must delegate EVERY `Apply` method —
  it delegated ten but not the new `emit_clif`, so the trait default's
  `Ok(None)` silently swallowed every builtin's hook while all probes
  "passed" (the array-literal region satisfied `fused > 0`
  vacuously). Diagnostics that catch this class: `graphix-fuzz run
  <f>` runs all THREE modes printing per-mode `attempted/fused` + the
  blocker list, and probe suites use a `Fuse::{No,Some,Clean}` ladder
  (`Clean` = fused>0 AND no non-ancestor-noise blocker). Nested HOFs
  don't inline on either path (#203 — resolve_static_calls doesn't
  descend lambda bodies). D2-filter landed: `FilterImpl::emit_clif`
  via `emit_filter_loop` — the orchestration is generic over `MapFn`,
  so each further HOF is one new method. Filter's contract: a
  may-bottom (Scalar2) PREDICATE Errs = build-time de-fuse (vs map's
  runtime bottom-abort seam) — there's no runtime keep-vs-drop answer
  for a bottom pred; canonically the pred slot never fires and the
  output BLOCKS. The strong de-fuse probe is statically-may-bottom /
  runtime-clean (div by element, no zero): de-fuses, then node-walks
  to a REAL value all modes agree on — Timeout==Timeout agreement is
  value-blind. Composite-elem filters inline on direct only
  (classic needs a register-scalar elem for single-name callbacks).
  D2-fold landed: `FoldFn::emit_clif` + FoldQ's `Apply::emit_clif`
  (the 2-arg `(acc, elem)` orchestration) + `FoldImpl` via
  `emit_fold_loop`; the scaffold's acc bind gained `Option<BindId>`
  (classic passes None — differential-proven invariant), so
  `fold(a, acc, |acc, x| ...)` resolves the init's outer `acc` vs
  the loop's acc BindId-first. BOTH init and body closures de-fuse
  at build on may-bottom (a bottom acc poisons all later iterations —
  no per-element runtime seam). Fold probes found #204 (pre-existing,
  both paths): a HOF callsite in OPERAND position (`k + fold(...)`)
  never statically resolved — static_resolve's visit_mut only
  descended Module/Block/Bind/CallSite. #204 FIXED (pulled ahead of
  the Stage-F flip): static resolution now descends every
  child-bearing node except Lambda bodies (#203) and FusedKernel.
  `collect_lambda_binds` is the canonical enumeration — an EXHAUSTIVE
  NodeView match (no `_` arm; a new node variant is a compile error
  there, not a silently-untraversed container); `visit_mut` mirrors
  it with per-type downcast arms. Position probes (operand /
  select-arm / array-element HOFs) pin it at `Clean`. Benefits both
  paths; zero fixture/CLIF fallout. The D2 HOF ladder then COMPLETED:
  flat_map, filter_map, find, find_map, array::init all inline on the
  direct path (one `emit_clif` each — the orchestrations made every
  rung a single method). find/find_map return Value-shape Nullable
  pairs; borrowed body sources clone via `ensure_owned_*_src` (now
  pub). OPERATIONAL RULE (bitten twice): `cargo test` does NOT
  rebuild `target/debug/graphix-fuzz` — always `cargo build -p
  graphix-fuzz` before CLI kernel inspection or differential capture.
  Owned-array-arg widening landed: fresh-producer inputs (literals,
  slices, inlined-HOF results) feed the loop scaffolds via
  `adopt_owned_src` + the ValArray-typed `owned_input_stack`
  (pending exits free them; normal path drops + pops after the loop
  — the buf stack has the wrong destructor; env-binding was rejected
  because JitEnv `truncate` emits no drops: select arms DO
  mark/truncate per arm, sound today only because arm binds are
  scalar-only, so an env-bound composite adoption inside an arm
  would leak on the normal path). With #204, HOF-of-HOF args now
  fuse as multi-loop single kernels (`filter(map(a,f),g)` = one
  kernel, two loops, intermediate dropped exactly once —
  kernel-verified). D3 landed, closing Stage D's functional scope:
  destructured `|(k,v)|` callbacks inline via `HofElem::leaves`
  (per-leaf BindId-bound scalar reads off the owned composite
  element; sparse `_` slots free via tuple_leaves; composite leaves
  node-walk). Remaining parity gaps: #150 (string/value elements,
  composite leaves, filter_map composite elems), #203 (nested HOFs
  in lambda BODIES — Stage E). Stage E1+E2 landed: cross-kernel
  lambda calls on the direct path — `discover_lambda_calls` (the
  canonical full `for_each_node` walker) builds callee kernels via
  the shared `build_lambda_kernel` (pub(crate); callees compile from
  GIR bodies during the parallel period), records
  `ExprId → LambdaCallInfo` (the apply_sites pattern), and
  `CallSite::emit_clif` + `emit_lambda_call_node` emit kind-grouped
  CLIF calls with closure-converted captures (kernel-verified: the
  capture rides the parent as an input, forwards as the trailing
  arg). Source-name keying mirrors classic (`ident_of`). New guards:
  `ExecCtx::fusion_building` (mutual-recursion re-entrancy —
  pre-existing infinite-build landmine, now a de-fuse) and two
  formerly-SILENT try_fuse Ok(None) paths now log (FusedKernel::new
  Err; duplicate-basename). Findings: #205 (pre-existing:
  GirStmt::Return mis-routes un-normalized Nullable select types —
  the first kernel build ever to reach it found it); recursive
  lambdas have NEVER fused on any path (`build_lambda_kernel` →
  region build hardcodes `self_info: None`; the SelfInfo/tail
  machinery exists unconnected) — E3 threads SelfInfo through,
  unlocking recursion + tail loops for both paths. NOTE: deep
  recursion overflows the NODE-WALK's native stack (~50k frames) —
  keep node-walk-compared recursion probes shallow.
- `delete_gir_ir.md` — superseded by `distributed_jit.md` (planned the same
  removal around a central walker); its scoping analysis and risks remain
  valid.
- `final_jit_architecture.md` — the end-state (node graph → CLIF; eager node-walk is a future note).
- `representable_bottom.md` — bottom semantics.
- `graphix_fuzz.md` — the differential fuzzer.
- `fusion_state.md` — the fusion-coverage map / gap metric.
- `impure_hof_fusion.md`, `composite_hof_fusion.md` — HOF fusion.
- `clone_rebind_testing.md` — the `clone_rebind` contract + its test campaign.
- `queue_fn.md` — `queue_fn` feature design.

**Historical** (earlier fusion stages — superseded, kept for archaeology):
`fusion_architecture.md`, `unified_fusion.md`, `whole_graph_fusion.md`,
`whole_graph_fusion_m7.md`.

### Major recent changes (newest first; `git log` for detail)

- **GIR IR removal** in progress — `compile_node` walks the node graph + emits
  CLIF directly (Stage 1 landed, gated by `CFlag::DirectNodeJit`).
- **GIR interpreter deleted** — fusion is JIT-only; node-walk is the universal
  fallback; correctness became structural.
- **`GirType` deleted** → `Type` + `abi_kind`; **`RegValue`/`ConstVal` deleted**
  → `Value` + `copy_unchecked`.
- Semantics fixes: `&&`/`||` made strict; NaN total-order; checked-overflow
  detection; representable bottom (interp), then subsumed by the node-walk
  fallback.
- **graphix-fuzz** differential fuzzer built (oracle, mutation/generation
  sources, minimizer, regression corpus) — found ~a dozen real bugs.
- Whole-graph maximal fusion; impure-HOF callback splitting; `clone_rebind`
  per-slot template + closure conversion; the `node-walk↔fused` capture/scope
  bug cascade (#162–#170).

## Stdlib package notes

- **GUI** (`graphix-package-gui`, iced 0.14): uses the iced sub-crates directly
  (`iced_core`/`iced_wgpu`/`iced_widget`/…) not the umbrella crate, for
  render-pipeline control. `iced_renderer` needs both `wgpu` and `wgpu-bare`
  features (the cfg checks key off `wgpu-bare`). GUI/TUI examples are visual —
  test manually (`cargo run --bin graphix -- examples/gui/hello.gx`).
- **Package manager** (`graphix-package`): `download_source` is testable by
  injecting a temp graphix data dir and downloading a fixed released
  `graphix-shell` from crates.io (e.g. `0.5.0`) — avoids mutating the user's
  `~/.local/share/graphix` and regression-tests archive-extraction layout.
- **GUI widget tests**: `GuiWidget` has a `#[cfg(test)] as_any`/`as_any_mut`
  (default `unimplemented!()`); widgets needing test-state inspection (e.g.
  `DataTableW`) override it, and `GuiTestHarness::dt()/dt_mut()` downcast. Tests
  fire per-column callbacks via `gx.call(callable_id, args)` (mirrors the
  widget's own dispatch). `InternalOnly` test ctx DOES spin up a real in-process
  resolver, so `sys::net` round-trips work — but publisher coalescing means
  rapid updates collapse; space them with one-shot timers for multi-point tests.
