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
3. **Type Checking & static resolution**: each node implements `typecheck0`/`typecheck1`
   (two passes). `typecheck0` also builds `ctx.bind_to_lambda` (the `BindId → LambdaDef`
   index, via `Bind::lambda_def_value`); `CallSite::typecheck1` then pre-binds every
   statically-resolvable call (`try_static_resolve`) and pre-materializes HOF callbacks.
   This is the former standalone `static_resolve` pass, folded in — 4 compile walks → 2.

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
- **Fusion → cranelift JIT** (`fusion/`, the emitter is `fusion/emit.rs`)
  identifies sync (pure) subtrees and compiles them to native code. **JIT
  success → splice the native kernel + delete the originals; JIT failure →
  don't splice, the originals node-walk.** There is no third evaluator.

**The GIR IR is GONE (F3, 2026-06-12; the gir-named files were renamed
into `fusion/` on 2026-06-16 — pure churn, behavior-neutral).** The
pipeline is `Expr → node graph → CLIF`, period: `fusion::fuse` is
`Update::fuse` recursion, each node's `emit_clif` emits its own CLIF
(`Apply::emit_clif` for builtins, `MapFn`/`FoldFn::emit_clif` + the
`fusion::emit::scaffold` loop scaffolds for HOFs), and kernel builds
are pure SIGNATURE derivation — `sig_from_inputs` is the single sig
builder, the `Arc<KernelSig>` is the compiled-callable handle, and "is
it fusable" IS the compile attempt. The kernel-ABI vocabulary
(`KernelSig`/`abi_kind`/`freeze_for_abi`/slots/`FnSource`/`BuiltinSlot`, plus
`KnownFusedFn`) lives in the single `fusion/kernel_abi.rs` (the old
`fusion/vocab.rs` that re-exported it was deleted — one module, one path); the
`BinOp`/`CmpOp`/`BoolOp` scalar operator enums (not ABI — shared by node-walk
and JIT) live in `node::op` with the node-walk, and `fusion::emit` imports them;
`GirExpr`/`GirOp`/`GirStmt`/`GirKernel`/`GirEmitter`/`FusedBuiltin`/
`FUSABLE`/`emit_gir` no longer exist.

> **GIR is historical — do NOT rebuild it.** GIR was a parallel typed
> IR (`GirExpr`/`GirOp`/`GirStmt`/`GirKernel`/`GirType`) plus a GIR
> *interpreter* — a redundant THIRD evaluator beside the node-walk and
> the JIT. It was removed deliberately because: (1) the interpreter
> forced every semantics fix to be written THREE times (node-walk +
> GIR-interp + JIT), a standing drift hazard, and its centralized
> jump-table was slower than the node-walk's distributed vtable
> dispatch; (2) the closed op-set was a *vocabulary tax* — every new
> op/shape had to be added to `GirOp`/`GirType` AND the node graph AND
> the emitter; (3) the op-list was a pass-through label anyway —
> emission keys off the netidx `Type` + `abi_kind`, never off `GirOp`
> structure, so **the node graph already IS the IR**. Lesson: keep the
> node graph as the single IR and distribute codegen as `emit_clif`
> per node; the only part of `GirKernel` worth keeping was the ABI
> contract, which survives as `KernelSig` / `abi_kind` in
> `fusion/kernel_abi.rs`.

The 2026-06-16 rename map: `gir_jit.rs → fusion/emit.rs`,
`gir_interp.rs → fusion/kernel.rs` (`GirNode → Kernel`), `gir.rs →
fusion/vocab.rs`, `gir_jit_helpers.rs → fusion/emit_helpers.rs`,
`gir_jit_intern.rs → fusion/intern.rs`, `gir_jit_scaffold.rs →
fusion/scaffold.rs` (still a submodule of `emit`); `Update::jit →
Update::fuse`, `fusion::jit_node → fusion::fuse` (the old
flag-checking `fuse` driver was deleted and its skip promoted into
`compile()` via `ctx.fusion.enabled`, so it's checked once
instead of every recursion); `GirMatcher → KernelMatcher`. See
`design/distributed_jit.md` — "F3 — the delete"
for what went and the two behavioral seams (fuse_callsite's
fall-through to the split path on a failed whole-body compile;
builtin-call discovery riding `for_each_node` with resolved
FnTypes), and "Semantic contracts for emit work" for the six
invariants the F2 flip taught us (replayability ≠ Sync, effects
de-fuse-never-skip, first-dispatch-is-init, `(BindId, top_id)`
wake-up keying, with_deref/registry lock discipline, dead-statement
elimination). Remaining: F4 (#213) EmitTags for node_shape, #219
missing-input bottom support.

**Value & type representation — use the netidx types, no parallel copies:**

- **Values:** netidx `Value` everywhere (`#[repr(u64)]`, 16 bytes = (disc,
  payload)). No bespoke value types — `RegValue`/`ConstVal`/`EvalResult` are
  gone. `Value::copy_unchecked` is the branch-free copy for proven scalars.
- **Types:** netidx `Type` everywhere. `GirType` is gone. Runtime shape
  comes from `abi_kind(&Type) -> Option<AbiKind>` + `freeze_for_abi` (in
  `fusion/kernel_abi.rs`); `PrimType` is the closed
  register-scalar set (the one *good* small classifier enum — exhaustively
  matched in codegen).

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
  Read `failed` as a blocker profile, not a gap count: the `fusion::fuse`
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

**Kernel ABI**: kind-grouped params — scalars, then array/tuple/struct
pointers, then string, then 2-word variant/nullable/value — derived from a
single source (`fusion/kernel_abi.rs`: `KernelSig::abi_params`/`AbiParamKind`).

### Design documents (`design/`)

**Current / active:**
- `distributed_jit.md` — the GIR-IR removal as it's actually being executed:
  fusion/JIT distributed as `Update::emit_clif` + `Update::fuse` trait methods
  (the delete/sleep/refs pattern), `Apply::emit_clif` for builtins,
  `fusion::try_fuse` as mechanics-only library, `KernelSig` in
  `fusion/kernel_abi.rs`. In progress (Stage A landed; Stage C complete through
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
  findings/select-jun2026. `fusion::kernel_abi::freeze_for_abi_normalized` exists because
  typecheck leaves a select's result type as the un-flattened arm
  union (`Set([i64, TVar→i64])`), which `freeze_for_abi` rejects.
  Stage D1 landed: the eight HOF loop scaffolds extracted from the
  GirOp arms into `fusion/scaffold.rs` —
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
  lambdas have NEVER fused on any path. NOTE: deep recursion
  overflows the NODE-WALK's native stack (~50k frames) — keep
  node-walk-compared recursion probes shallow. E3 landed: recursive
  lambdas fuse on the direct path. The REAL build-killer was the
  captures scan, not the `self_info: None` hardcode — a rec
  binding's env type is a TVar-wrapped `Fn` the shallow skip missed,
  so `freeze_for_abi` rejected the self-"capture".
  `build_lambda_kernel` gains `self_bind: Option<BindId>` (the call
  site's fnode Ref id, threaded from discovery /
  `ensure_lambda_kernel` / the per-slot path): the self-reference is
  excluded from captures, recursion = external-refs ∋ self_bind,
  non-tail self-calls lower to `GirOp::Call` native recursion
  (fib-verified), and `body_has_self_tail_call` (pure pre-scan of
  emit_tail's positions) + `SelfInfo { name, bind_id, source_args }`
  (params field deleted — zero consumers; matching is BindId-based)
  turn tail self-calls into the rebind-and-jump loop
  (kernel-verified: icmp/isub/iadd/jump, no call insn; 5M-deep
  fused-only probe — DirectJit ONLY, the node-walk AND classic both
  node-walk it and overflow). `tail_call_slots` must stay full (it
  doubles as the runtime arg layout) — the JIT TailCall arm rebinds
  only the leading formals (captures are loop-invariant; the
  clone-bump loop needed `.take(args.len())`, OOB otherwise).
  Tail-loop gate: all formals Prim/Array/Tuple/Struct. **#206 (live
  E1-reachable crasher, found+fixed)**: f2's body call to a shadowed
  same-name OUTER f name-resolved against f2's own known_fns self
  entry → infinite native self-call → stack overflow.
  `KnownFusedFn::self_bind` now guards every name-keyed resolution
  (fnode Ref BindId must match). Repro in findings/lambda-jun2026;
  when #203 resolves inner-body sites, known_fns must re-key by
  BindId. Rec FN-valued lets now emit the "function-valued let"
  message (Clean-compatible); 5 recursion seeds in the fuzz corpus.
  Stage F runs F0a→F0b→F0c→F1→F2→F3→F4 (GIR bodies are load-bearing
  in three places that must port to Node emission BEFORE the delete:
  lambda callees, per-slot HOF kernels, body-split kernels). Scope
  fact: lambda-calls-OTHER-lambda in a callee body never fused on any
  path (#203 — per-callee known_fns starts empty); a callee's only
  cross-kernel reference is SELF. F0a landed: non-rec callee bodies
  Node-emit (discovery returns kernel-identity → body-Node;
  per-callee NodeBodyEmitters with empty site maps; define loop
  routes by Arc::as_ptr; needed-FuncRefs per-emitter). The define
  loop now log::trace!s "Node-emitted body" vs "GIR body" per kernel
  — the standing diagnostic for the silent-fallback class; verify
  routing with RUST_LOG=graphix_compiler=trace, not by CLIF
  inspection (shared helpers make both emitters' output identical).
  F0b landed: recursive callee bodies Node-emit — NO lambda callee
  reads its GIR body on the direct mode. Two byte-identical-gated
  refactors made the seams (`emit_tail_rebind_jump` extracted from
  the GIR TailCall arm; `emit_select_node` split into
  classify + `emit_select_arms`(arm-closure) + value-arm), then the
  tail walkers mirroring lowering 1:1: `emit_body_tail` /
  `emit_select_node_tail` (arms terminate, no merge) /
  `emit_self_tail_call` / shared `emit_block_stmt`. NO in_tail flag:
  tail self-calls intercept structurally before value emission;
  value-position self-calls go through `CallSite::emit_clif` → the
  kernel's own FuncRef via `emit_lambda_call_node` (captures forward
  from own params). TRAP: inner self-callsites are #203-unresolved
  (`self.function` None) — the self check must live OUTSIDE the
  resolved-Apply block (tail worked immediately; value-position
  rec/fib silently de-fused until moved). Return-leak safety is
  structural (emit_kernel_return drops all owned at any depth).
  Non-recursive bodies keep value-position emission (per-arm returns
  = pointless CLIF churn). F0c landed: per-slot kernels route by
  `ExecCtx::direct_node_jit` (the jit_enabled pattern — fuse_callsite
  runs outside fuse()'s flag-aware loop); split branch proven live
  (`__split_*` Node-emitted under DirectJit), whole-body branch
  code-uniform but probe-less (D2 inlining subsumes its trigger
  space — the per-slot compile-failure trace is the watchdog). With
  F0a+b+c NO direct-mode kernel reads a GIR body. F1 in progress:
  generate-5000 clean; the mutation soak found **#214** (pre-existing
  at HEAD, both paths): a pending String DynCall's sentinel-zero rode
  the scalar convention into owned-ArcStr drops —
  `graphix_arcstr_drop(0)` SIGSEGV (`{let v = str::concat(); i64:0}`,
  isolated via the new GRAPHIX_FUZZ_ECHO crash forensics). Fixed:
  String dyncall results take the same site-level pending branch as
  composite/Value (shared `emit_dyncall_pending_branch` — one helper,
  six arms, both paths), and ALL five JIT drop helpers null-check +
  PANIC (aborts with message at the extern "C" boundary — the
  whole sentinel-into-drop mistake class is now loud instead of UB;
  `graphix_value_drop`/`graphix_arcstr_drop` retyped to raw words so
  the check precedes invalid-value materialization; Value disc 0 is
  unambiguous, discs are bitmasks from 0x1). The fix EXPOSED a
  residual pre-existing VALUE divergence the crash was masking: a
  pending dyncall in DEAD position bottoms the whole kernel (interp=0
  vs jit=Timeout; membership = zero-vararg variadic calls,
  `str::concat()` / `str::join(#sep:)` — zero-input nodes never fire
  canonically; used-position agrees). Decision pending (#216); repro
  at `findings/dyncall-jun2026/*.gx.pending` (excluded from regress
  until resolved). The post-fix soak then died on the OTHER
  process-killer (runaway-recursion mutant → node-walk stack
  overflow) → built **fuzzer subprocess crash isolation** (#215):
  campaign checks run in `check-one` children (stdin program →
  VERDICT line); signal-death → `crash_*.gx` finding with status +
  stderr tail; minimization isolated too via `minimize-one` (a
  REDUCTION of a benign divergence can be a crasher — dropped base
  case), child death → record unminimized; `GRAPHIX_FUZZ_INPROC=1`
  opts back; crash findings must NOT be promoted to findings/ until
  fixed (regress runs in-process). With isolation the 3000@777 soak
  COMPLETED (first time): 1 divergence (dead-pend) + 1 crash
  (runaway recursion, accepted) — ZERO unexplained. FuseExpect audit
  (`GRAPHIX_FUSE_AUDIT=1 cargo test -p graphix-tests -- jit
  --nocapture` — stdout is CAPTURED without --nocapture, silently 0
  lines): 612 fixtures → 450 OK, 155 GAINS (None→Jit), 6 LOSSES =
  two closable clusters (map literals: no direct emit_clif, #143
  unported; abstract types in composites: resolve_abstract not on
  the direct freeze, #145 unported) — values agree everywhere.
  #216 RESOLVED at the language level (Eric): a sync variadic builtin
  called with no positional arguments can never fire (no data inputs)
  — now a COMPILE ERROR (`reject_dead_variadic_call`, callsite.rs;
  labeled args are config, not data, so `str::join(#sep:)` is caught;
  first-class-value calls escape to the safe runtime bottom).
  `never()` is the sanctioned intentional bottom — reclassified
  `EffectKind::Async` (its honest contract), which exempts it and
  makes it a fusion boundary instead of an always-pending fused
  dyncall (zero FuseExpect fallout). dyncall-jun2026 findings are
  CompileErr-agreement regress guards. Audit loss clusters CLOSED:
  map literals via `Map::emit_clif`→`emit_map_new_node` (const-fold
  through shared `const_map`, classic's contract); abstract types
  via four seams (resolve_abstract TVar-deref arm — inferred binding
  types are TVar-wrapped; collect_region_inputs resolves before
  freeze; try_fuse's return gate retries through resolve_abstract;
  emit-time `BodyEmitter::type_env`→`LowerCtx.type_env`→
  `freeze_node_typ` retry at elem reads + arith prims) and
  `emit_lambda_call_node` typing arg slots from the CALLEE signature
  (`LambdaCallInfo.arg_types`) like classic. Audit now ZERO losses,
  165 gains. Classic also gained (abstract_type_in_typedef
  re-annotated Jit). Remaining before F2: one-time FuseExpect
  re-annotation of the gains → F2 flip → F3 delete → F4 EmitTags.
  **F2 FLIPPED (2026-06-13)**: CFlag::DirectNodeJit gone, fuse() =
  unconditional fuse recursion (classic planner body deleted),
  Mode::DirectJit aliases Mode::Jit, 168 FuseExpect upgrades, 5
  GirOp-tag shape pins parked for F4. Six flip-surfaced defects
  fixed (design/distributed_jit.md "F2 — the flip" has the detail):
  handler-ful `?` must not fuse (error delivery = variable write);
  feeder ref_var must key the TOP expr id (ExecCtx::fuse_top_id) or
  connect-fed regions see one update ever; DynCallSlot's first
  dispatch forces the init view (labeled defaults are init-firing
  Constants); Once/Take/Skip/Count/Uniq are Async (the dispatch
  protocol re-delivers all args every fire — update-history
  semantics unreproducible); TVar/ABSTRACT_REGISTRY lock discipline
  (clone out of with_deref before recursing; never hold a registry
  read as a match temporary — two deadlock edges wedged the parallel
  suite); dead statements eliminate at emit_block_node (classic's
  prune semantics) gated on stmt_subtree_effect_free — skipping an
  effectful stmt would DROP the effect instead of de-fusing (the
  env-accounting probe caught it). Gates: 1429/1429 ×2, 115/115,
  regress 22/22 (findings/flip-jun2026 promoted), generate clean,
  mutation 1 divergence = #219 (the DOCUMENTED pre-existing JIT
  missing-input gap, not flip-caused).
  **F3 LANDED (2026-06-12)** — the GIR IR deleted (~12.5k lines, 3
  gated chunks, ZERO FuseExpect drift; "F3 — the delete" section has
  the chunk detail + the two behavioral seams). Sig-only kernel
  builds via the shared `sig_from_inputs`; `GirKernel` dissolved
  into `Arc<KernelSig>`; `BodyEmitter` mandatory in the define loop;
  builtin-call discovery on `for_each_node` (resolved-FnType-only —
  the Expr-fallback walker died); fuzz probes two-mode
  (`agree*`/`jit_*`); node_shape matches sig facts only until F4.
  NEXT: F4 EmitTags (#213); #219 missing-input bottoms.
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
- `fusion_lowering_split.md` — **PROPOSED, not built.** Split `try_fuse`'s welded
  analysis+lowering into a pure fusion *analysis* pass (color nodes with a
  `KernelId`, build per-kernel descriptors in `ctx`) consumed by a thin lowering
  pass (descriptors → CLIF). Node graph stays the IR; descriptors are the data
  seam. Motivated by legibility (see the `feedback_clarity_as_participation`
  memory).

**Historical** (earlier fusion stages — superseded, kept for archaeology):
`fusion_architecture.md`, `unified_fusion.md`, `whole_graph_fusion.md`,
`whole_graph_fusion_m7.md`.

### Major recent changes (newest first; `git log` for detail)

- **Attributes (`#[..]`) + the `#[native]` attribute — Part C2/C3
  (2026-06-24).** Rust-style attributes written on their own line directly
  above an expression — `#[name]` / `#[name(arg, ...)]`, args are full
  expressions — captured into the existing `Expr.dec.attrs` slot exactly where
  a `//` comment is captured (every other position is a parse error). Both
  `Expr` printers emit them, so print->parse round-trips verbatim. New parser
  combinators `attribute()` + `leading_decorations()` (the latter replaces
  `leading_comments()` at the `expr()` entry, interleaving comment+attr lines;
  `leading_comments()` stays for the `.gxi` `sig_item` path). Attribute names
  are validated against an `ExecCtx` registry (`Attribute<R,E>` trait +
  `AttributeCheckFn<R,E>` + `register_attribute`, mirroring builtin
  registration so packages can add their own) — an unregistered name is a
  compile error at the single `compile()` choke point (`node/compiler.rs`,
  every Expr re-enters there once). Each known attribute carries a check fn run
  once per decorated node AFTER fusion, walked via `for_each_node` over the
  final graph (which stops at fused-kernel interiors — a node absorbed into a
  larger kernel is correctly NOT re-checked → counts as native). **First
  attribute `#[native]`** (registered in `ExecCtx::new`, compiler-internal
  check): the decorated expr must compile to native code — one fused JIT kernel
  with zero node-walk residue — else a compile error quoting `stats.failed`
  filtered to the expr's descendant ExprIds (`Expr::fold`). **DESIGN (Eric):
  function-level `#[native]` was DROPPED.** It may ONLY decorate a
  value-producing computation or a call; a lambda literal or any function-typed
  target (`Type::Fn`, which a `let f = |..|` binding reports) is a compile
  error. A `native` marker carried in `FnType` would make the function
  second-class (un-storable, un-dispatchable, can't pass to a non-fusing HOF —
  every non-static call would fail to be native), so a perf requirement belongs
  at the use site, not the definition. Dropping it deletes the riskiest planned
  work (no `FnType` field, no contains/eq threading, no static-resolution
  rule) and collapses the feature to ONE uniform check. `#[native]` is
  deliberately mode-dependent: under `--no-fusion` it errors ("cannot verify
  #[native]: fusion is disabled") rather than silently passing (all normal
  flows — run/`--check`/lsp — keep fusion on and DO enforce it), so it is
  tested with dedicated `eval` compile assertions in
  `stdlib/graphix-tests/src/lib_tests/native.rs`, not the `run!` differential
  harness (whose cross-mode value-agreement `#[native]` intentionally breaks).
  Validation: graphix-compiler 110, graphix-tests 1459 (+5 native) ×2,
  graphix-lsp 29, fuzz regress 22/0, generate soak 400/0/0. The
  `defpackage! attributes => [...]` clause is DEFERRED (no package needs it
  yet; `register_attribute` already exposes the public interface).

- **qop/cast/connect fusion + bench naturalization + LSP fusion-off
  (2026-06-24).** Three fusion gaps closed so that *natural user code* fuses
  (the bench corpus is the live witness), plus an LSP fix. **(1)
  Variable-write-in-kernel** (`graphix_set_var` helper → `DynDispatchHandle.set_var`
  → `set_var_typed::<R,E>` → `ctx.set_var`): a `connect` (`x <- e`) and a
  **handler-ful `?`** (a `?` whose error is caught by an enclosing `try`) both
  deliver via a variable write, which is *sync* (schedules next cycle); the real
  fusion boundary is the **read** side (already handled by feeders keyed to
  `fusion.top_id`), not the write. So both now fuse — `Connect::emit_clif`,
  `Qop::emit_clif` (handler error branch does `wrap_error` + set_var via
  `QopDeliverApply`/`emit_qop_deliver`, removing the `id.is_some()` bail),
  `TryCatch::fuse` recurses into try/catch bodies. Conservative guard: a connect
  whose LOCAL target is read in the same kernel de-fuses (read-after-write-same-var
  reads the stale let-local — `cx.env.lookup(bind_id).is_some()` check; full
  local-counter fix deferred). **(2) cast machinery-DynCall** (`CastApply<R,E>` +
  `FnSource::Cast`): any non-scalar-fast-path cast lowers to a one-arg dyncall whose
  callee is `target.cast_value(&ctx.env, v)` — the SAME machinery the node-walk uses,
  so interp/jit agree by construction; the scalar→scalar inline fast path
  (`compile_cast`) is preserved but now gated on `is_numeric()` on BOTH src+tgt (bool
  excluded → dyncall, fixing a `cast<i64>(true)` `unreachable!`). An inline cast in a
  HOF arg no longer de-fuses the HOF. **(3) qop/connect EffectKind → Sync**
  (`analysis.rs` `node_effect`: `Qop`/`OrNever`/`Connect`/`ConnectDeref` moved to
  `Sync`; `Sample`/`TryCatch`/`Any`/`FusedKernel` stay `Async`). Double duty: enables
  the above fusion AND **fixes the node-walk tail-loop overflow** (a qop in a
  tail-call argument made the lambda `Async` → `mark_recursion` skipped → real
  recursion → stack overflow; now `lambda_is_sync` → tail-loop engages, verified
  2M-int + 10M-float deep, both modes agree). `stmt_subtree_effect_free` (dead-stmt
  pruning) is left UNCHANGED — orthogonal: a connect is *sync* yet *not effect-free*.
  **(4) inline-HOF-callback discovery gap** (`Apply::for_each_hof_callback_body` +
  `hof_callback_body` helper, overridden by MapQ/FoldQ/Init): `for_each_node` skips
  lambda bodies, so a cast/builtin call *inside* an inline HOF callback was never
  discovered for slot registration → de-fuse; now descended.
  **Bench corpus naturalized** (`bench/`, per "judge by what a user would write"):
  dropped the outer `{ }` block wrapper (file scope is an implicit block), all
  `i64:` literal prefixes (i64 is default), and the redundant `~` self-gate on
  `sys::time::now(x)` (already triggered by its arg) — kept only the load-bearing
  `sys::exit(elapsed ~ 0)` gate; `leibniz_pi` reverted to the natural in-loop
  `cast<f64>(2*n-1)$` (the qop-in-tail-arg fix made the `d`-accumulator workaround
  unnecessary). **LSP "runtime is dead" fixed** — STRUCTURAL: `lsp_mode` now forces
  `ctx.fusion.enabled = false` at the single derivation point in `compile()`
  (`lib.rs`, `&& !ctx.env.lsp_mode`), so a check-only runtime CAN'T fuse and no
  caller can opt back in (a call-site `FusionDisabled` flag was tried then reverted —
  DRY, invalid state unrepresentable). The LSP shares ONE runtime that only
  typechecks (compiles → deletes nodes, never executes) but had fusion ON; its
  `lsp_mode` check continues past type errors and fed fusion partially-typed mid-edit
  exprs (violating fusion's well-typed invariant) → panic → bricked the shared runtime
  (no panic isolation) → every later check "runtime is dead", a `(0,0)` file-wide
  diagnostic (cached IDE data masked it). NOT the interrupt/abort change (those paths
  only run during node execution the LSP never does). Fusion-emit proven solid on
  valid programs (212 repo `.gx` + 3000-prog fuzzer soaks, 0 panics); fusion for a
  never-executing runtime was pure waste regardless. Validation: 1446×2 + 29 LSP
  green, fuzz regress 22/0, mutation soak clean (only the documented #219
  missing-input gap + accepted runaway-recursion). Open follow-ups: general
  panic-isolation in the shared check runtime (catch_unwind); and `graphix --check`
  (`Mode::Check`) still fuses-then-discards (a smaller one-shot waste — no crash,
  since `--check` bails on type errors before fusion).

- **Self-timed exec-throughput bench corpus `bench/selftimed/` (2026-06-23).** Six
  `.gx` programs + `run.sh` + README comparing JIT (fusion on, default) vs
  node-walk (`--no-fusion`) on pure computation. Each program brackets ONLY its
  computation with `sys::time::now` (parse/typecheck/kernel-compile happen
  before cycle 1, so they're excluded). Release best-of-4 on a quiet machine:
  HOF benches (fold/map/filter over 100k) **~1200–2700x** (node-walk's
  per-element node graph is its worst case); scalar tail loops (tail_sum/leibniz,
  10M) **~130–160x** (node-walk loops these iteratively via the GXLambda
  tail-loop — its *best* case). Self-timing structure + its hard-won gotchas live
  in the README and the `project_selftimed_bench_corpus` memory: trigger `now`
  with a constant (`i64:0`, NOT `once(null)` — that never fires); seed the
  compute with a separate-`let` `cast<i64>(t0)` (inlining the `$`-cast in a HOF
  arg de-fuses it — a fusion gap, not a correctness bug); gate `sys::exit` on
  `elapsed`, not on `println`'s return. **Two findings surfaced**: (1) a qop
  (`$`/`?`) inside a TAIL-CALL ARGUMENT defeats the node-walk's tail-loop
  optimization → real recursion → stack overflow, where the JIT loops fine (a
  mode divergence the differential fuzzer won't catch — overflow ≠ wrong value);
  (2) the `netidx-value` `datetime -> f64` cast returned `2*whole_seconds` with
  the sub-second part discarded (integer-div bug) — fixed (Eric committed it +
  hardened the other cast arms), which is what unblocked sub-second self-timing.
  Language note: graphix has NO unary minus on a variable (`-x` is a parse error;
  negative literals like `-2.5` are fine) — flip a sign with `(0.0 - x)`.

- **`interrupt()` / `abort()` — recover & shut down a wedged runtime (2026-06-20;
  behavior-neutral when idle, 1438×2 + 29 LSP green, fuzz regress 22/0).** An
  unbounded loop within one reactive cycle (sync tail-loop `let rec f = |v| f(v+1)`,
  or a HOF over a giant collection) wedges the runtime thread — the runtime is
  batch-atomic, so yielding mid-cycle can't preserve consistency (rejected); a
  *kill* has no resume, so it can. One mechanism, two needs: a `BitFlags`
  control flag (`CtlFlag::{Interrupt,Abort}` over `AtomicU32`, wrapped in the
  abstract `Control` type in `graphix-compiler/src/lib.rs`), held as
  `Arc<Control>` on `ExecCtx` (so generic node code reads it via
  `ctx.interrupted()` with no `Rt` method) and cloned into `GXHandleInner`.
  **`GXHandle::interrupt()`** = direct atomic store (NEVER a `ToGX` command — a
  wedged runtime can't drain its channel) → in-flight loops abort to bottom
  (`None`, a legal reactive state = a cancelled async node), runtime keeps
  running. **`GXHandle::abort()`** = the explicit shutdown; needed because the
  command methods are `async fn(&self)` borrowing the handle across `.await`, so
  you *can't drop the handle while commands are pending* — `abort()` is a
  `&self` store that fires anyway, and `GXHandleInner::drop` also sets `Abort`
  (fixing a real leak: dropping a wedged runtime's handle spun forever — both
  `task.abort()` and channel-close need the runtime to reach an `.await`/`select!`
  a wedged sync loop never does). The run loop checks `aborted()` at the top of
  `'main` (before `process_input_batch`), so a runtime that broke out of a wedged
  `do_cycle` exits next iteration; on return its `to_rt` receiver drops →
  pending commands' oneshots drop → blocked callers get errors instead of
  deadlocking. **Loop polls:** interp opt-in per builtin
  (`if ctx.interrupted() { return None }` at the loop head — tail-loop
  `GXLambda::update`, array HOFs MapQ/FoldQ/Init/Group/Iter/IterQ); fused via one
  shared `emit_interrupt_check` (calls `graphix_interrupted()` — reads the
  per-cycle thread-local `INTERRUPT_PTR`, set in `do_cycle` to handle task
  migration — and reuses the existing `emit_pending_cleanup` + `pending_exit`
  abort path, so a partial HOF result buffer on `dyncall_buf_stack` frees for
  free) at the tail-loop head + all 8 HOF scaffold loop heads. **`do_cycle`**
  clears `Interrupt` each cycle (Abort is **sticky** so the pre-cycle check sees
  it — this reverses the request's "do_cycle only clears abort", read as a slip),
  and wraps the node loop in `tokio::task::block_in_place` so a wedge doesn't
  starve IO/the interrupting caller — **flavor-guarded** (`block_in_place` only
  isolates on `multi_thread`; `current_thread` runs inline, the existing
  `run!`/`TestCtx` harness flavor). Integration tests in
  `stdlib/graphix-tests/src/lib_tests/interrupt.rs` (multi_thread, so the test
  thread fires the flag while a worker wedges): tail-loop interrupt-recovers
  (interp + fused) + abort-unblocks-pending-commands. Coverage limit noted in
  the file: the *only* constant-stack, bounded-memory wedge is the tail loop —
  the array-HOF eval-loop polls can't be driven to a real timeout without
  materializing millions of per-slot nodes (the slot *build* phase is itself the
  slow, un-polled part), and fused is too fast to wedge within any memory-safe
  N; those polls are behavior-neutral (1438×2) and the fused scaffolds share the
  tail loop's `emit_interrupt_check`. A nested recursive lambda inside an HOF
  callback (`array::map([1], |x| { let rec f...; f(x) })`) stack-OVERFLOWS rather
  than wedging (#203: it node-walks as deep recursion, ~50k frames, before any
  interrupt lands) — so it's not a usable interrupt probe. The 3000-program fuzz
  soak surfaced 1 divergence + 1 crash, both the documented pre-existing
  dead-bottom/div0 class (`findings/flip-jun2026/*_dead_bottom_stmt.gx`) —
  loop-free, so outside the loop-head-only codegen change; not interrupt-caused.

- **Fusion state extracted from `ExecCtx` into `fusion::FusionCtx` (2026-06-19;
  behavior-neutral, 1431×2 + 29 LSP green).** This session piled fusion
  machinery onto `ExecCtx` as loose fields; grouped the 8 that are genuinely
  *fusion's own state* into a non-generic `FusionCtx` struct in `fusion/mod.rs`,
  reached as **`ctx.fusion.<x>`**. Membership + rename map: `jit`→`fusion.jit`,
  `fusion_kernels`→`fusion.kernels`, `fusion_building`→`fusion.building`,
  `fusion_enabled`→`fusion.enabled`, `fusion_stats`→`fusion.stats`,
  `abstract_registry`→`fusion.abstract_registry`, `fuse_top_id`→`fusion.top_id`,
  `builtin_effects`→`fusion.builtin_effects` (dropped the now-redundant
  `fusion_`/`fuse_` prefixes). `FusionCtx::new() -> Result` (the `jit` build is
  fallible); `ExecCtx::new` calls it. Non-generic because none of the field
  types depend on `R`/`E` (`Jit`/`CachedKernel` are plain structs). **Left in
  `ExecCtx`** (Tier 3 — compile-analysis the *node compiler* owns, fusion only a
  downstream reader): `bind_to_lambda` (not read by fusion at all),
  `lambda_defs` (general `LambdaId→Value` registry), `unstable_bindings` and
  `builtin_bindings` (populated by `Connect`/`Bind::compile`, read by
  `callsite.rs`'s static-resolution). `builtin_effects` moved despite being
  populated in `register_builtin` because *only* fusion reads it. The
  `GXHandle::fusion_stats()`/`TestCtx::fusion_stats()` methods keep their names
  (they read `ctx.fusion.stats`). Rename gotcha avoided: `.jit` is also a field
  on `Kernel` (`self.jit`) and the graphix-fuzz diff (`d.jit`), so only the
  `ctx.jit`/`ec.jit` ExecCtx receivers were renamed.

- **rustfmt revived + workspace-wide import hygiene (2026-06-18/19;
  behavior-neutral, 1431×2 + 29 LSP green).** `rustfmt.toml` pinned
  `required_version = "1.4.12"` (uninstallable; only 1.9.0 exists) so **rustfmt
  had been refusing to run for ages** — which is why imports drifted. Fixed:
  dropped the pin, migrated the deprecated options (`merge_imports` →
  `imports_granularity = "Crate"`, `fn_args_layout` → `fn_params_layout`,
  `hide_parse_errors` → `show_parse_errors`), enabled `unstable_features` (the
  import-merging is nightly-only) — so **`cargo +nightly fmt` works again** and
  enforces grouping going forward. Ran it once over the whole repo (128 files,
  −464 net) to absorb 1.4.12→1.9.0 drift and merge every stacked `use crate::X;
  use crate::Y;` into `use crate::{X, Y}`. Then hand-hoisted repeated inline
  `crate::a::B` full paths into grouped `use` blocks across the fusion core
  (`fusion/{mod,lowering,emit,emit_helpers,builder,kernel}.rs`), `lib.rs`, and
  the whole `node/` cluster (`mod,op,data,callsite,bind,array,lambda,error,map,
  select`) + `graphix-lsp/state.rs`. **Use-statement standard** (now in the
  global CLAUDE.md): items used >1× → top-level (or fn-local) `use`, grouped by
  crate/module; single-use at discretion (favor importing long paths); avoid
  glob `use` except crate preludes, test-module `use super::*`, and fn-local
  enum-variant globs. **Two files deliberately keep full paths** — `fusion/
  scaffold.rs` (`use super::*` over `emit`'s scope — an extracted-from-`emit`
  submodule, de-globbing it is a 25+ item churn for no gain) and
  `graphix-package-core/src/testing.rs` (the `run!`/`run_with_tempdir!` exported
  macros need absolute `::graphix_compiler::…` paths to expand in other crates —
  same reason `$crate::` paths are sacrosanct). Mechanical-sweep gotchas worth
  remembering for next time: never shorten `$crate::` or `::graphix_compiler::`
  inside macro bodies; the path-inventory regex truncates digit-suffixed names
  (`array_slice_i64` → `array_slice_i`); doc-only refs become dangling intra-doc
  links if shortened to an unimported name (keep them fully qualified); and
  shortening `crate::m::foo` → `foo` when a same-named local `fn foo` exists
  silently creates infinite self-recursion that COMPILES (bit `state.rs`'s
  `path_to_uri` wrapper).


- **`fusion::vocab` deleted — folded into `fusion::kernel_abi`, one module
  (2026-06-18; behavior-neutral, 1431×2 + 29 LSP tests pass).** `vocab` was a
  66-line module that did three things: `pub use crate::fusion::kernel_abi::*`
  (so every ABI type had TWO reachable names — `kernel_abi::X` and the alias
  `vocab::X` — used inconsistently across ~120 sites), plus its own
  `KnownFusedFn` (the cross-kernel call signature — actually ABI) and the
  `BinOp`/`CmpOp`/`BoolOp` scalar operator enums (shared by the node-walk
  `node::op` and the JIT `fusion::emit`). Deleted `vocab`: the re-export (the
  two-name ambiguity) is gone, `KnownFusedFn` moved into `kernel_abi` (it's ABI),
  and the operator enums moved into `node::op` — they aren't ABI, and `node::op`
  is the canonical evaluator that defines those operators' semantics, so the JIT
  imports them from there (`use crate::node::op::{BinOp, CmpOp, BoolOp}`; the
  node↔fusion coupling already exists — `node::op`'s `emit_clif` methods call
  `fusion::emit`). One path per type, both clear names. ~120 `vocab::` references
  rewrote to `kernel_abi::` across the compiler + 2 stdlib crates
  (`graphix-package-array`, `graphix-tests`); external `Apply::emit_clif`
  implementors now import `fusion::kernel_abi::*` instead of `fusion::vocab::*`.

- **`kernel_abi` moved from toplevel into `fusion` (2026-06-18; pure relocation,
  behavior-neutral, 1431×2 + 29 LSP tests pass).** `kernel_abi.rs` →
  `fusion/kernel_abi.rs`; the `pub mod kernel_abi;` declaration moved from
  `lib.rs` into `fusion/mod.rs`; ~30 `crate::kernel_abi::` paths rewrote to
  `crate::fusion::kernel_abi::` (matching fusion's existing fully-qualified
  `crate::fusion::vocab::` house style), and `vocab`'s `pub use
  crate::kernel_abi::*` became `pub use crate::fusion::kernel_abi::*`. It was
  toplevel only as a GIR-removal survivor (it's the ABI contract that outlived
  `GirKernel`), never relocated. It's used exclusively by `fusion/*` plus the one
  `ExecCtx::abstract_registry` field (a fusion-only artifact written by typecheck
  solely for fusion to read); external stdlib `Apply::emit_clif` implementors
  reach it through `fusion::vocab::*` and saw zero API change. Clean leaf (depends
  only on `typ`/`BindId`), so no cycle. `lib.rs`'s two `kernel_abi::AbstractRegistry`
  references became `fusion::kernel_abi::AbstractRegistry` — consistent with how
  `lib.rs` already reaches `fusion::emit::Jit` / `fusion::FusionStats`.

- **IDE/LSP side-channels unified into one `ide::Ide` struct; new toplevel
  `ide` module (2026-06-18; behavior-neutral, 1431×2 + 29 LSP tests pass).**
  IDE tooling data (go-to-def / find-refs / cursor→scope) was spread across two
  homes split only by what the push site could reach: three loose `ExecCtx`
  fields (`references`/`module_references`/`scope_map`, pushed from `&mut
  ExecCtx`) and a separate `env::Lsp` struct (`type_refs`/`sig_links`/
  `module_internals`, pushed from `&Env` via `Arc<Mutex>`). Collapsed to ONE
  struct, `ide::Ide`, holding all six tables, owned via the shared
  `Env.ide: Option<Arc<Mutex<Ide>>>` sink (renamed from `Env.lsp` — the sink
  is general IDE state, not language-server-specific; atlas etc. may read it)
  — `ExecCtx` now carries ZERO IDE fields. The three former `ExecCtx` pushes route through new `Env` helpers
  (`push_reference`/`push_module_reference`/`push_scope_map_entry`, siblings of
  the existing `push_type_ref`/etc.), so all six tables share the one
  lsp-gated, clone-shared sink. New `graphix-compiler/src/ide.rs` owns the six
  site types (`ReferenceSite`, `ModuleRefSite`, `ScopeMapEntry`, `TypeRefSite`,
  `SigImplLink`, `ModuleInternalView`), the six pools (now private statics local
  to `Ide::new()`), and `Ide` — moved out of `lib.rs`/`env.rs`; external crates
  import from `graphix_compiler::ide::*`. `BuiltinBindInfo` stayed in `lib.rs`
  (it's fusion, not IDE). Downstream mirrors (`CheckResult`, the LSP
  `Document`/`TypecheckResult`/`ProjectResult`) each collapsed their four IDE
  fields to one `ide: Ide`, and `gx.rs::check_inner`'s three field swaps + the
  lsp swap became the single `env.ide` swap. Eric's call (vs the lighter
  "group the 3 in ExecCtx" option): full unification — one home for every IDE
  table, ExecCtx decluttered to nothing IDE-shaped, the `&mut ExecCtx` vs
  `&Env` access split erased. The added `Arc<Mutex>` on the three former-direct
  pushes is compile-time-only and lsp-gated (negligible).

- **`ABSTRACT_REGISTRY` global → per-`ExecCtx` field (2026-06-16;
  behavior-neutral, fixes an unbounded-growth leak).** The fusion abstract-type
  registry (`AbstractId → concrete Type`, written by `check_sig` during
  typecheck, read by the fusion classifiers to peek through abstract types to
  their wire shape) was a process-global `static LazyLock<RwLock<IntMap>>`. Two
  problems: (1) `AbstractId`s are minted fresh on every compile (each `ExecCtx`
  recompiles its stdlib from source), so the global grew without bound across a
  long-lived process spinning up contexts — a leak; (2) it was a global when the
  access points all have an `ExecCtx`. Moved it to a plain `ExecCtx`
  field, `pub abstract_registry: AbstractRegistry` (a newtype over
  `nohash::IntMap<AbstractId, Type>` in `kernel_abi.rs`). **No lock, no interior
  mutability**: writes (typecheck, `&mut ctx`) and reads (fusion, `&ctx`) are
  disjoint phases within a single `compile()`, and `compile()` is
  single-threaded — the old `RwLock` existed only to guard concurrent compiles
  of *different* contexts on parallel test threads, contention that per-context
  storage erases (along with the whole cross-lock deadlock-avoidance discipline).
  Per-context is also *correct*: a `Type`/`AbstractId` never escapes its owning
  context (verified), so context A never resolves an abstract context B
  registered. The three reader fns (`freeze_for_abi`, `abi_kind`,
  `resolve_abstract`) and their helpers (`freeze_for_abi_normalized`,
  `nullable_inner`, `scalar_prim`, `array_scalar_prim`, `is_value_shape`,
  `KernelSig::abi_return`) now take a non-generic `&AbstractRegistry` first param
  (kept non-generic to avoid rippling `<R,E>` into the classifiers). Threading:
  ~87 call sites. Sites with an `ExecCtx` pass `&ctx.abstract_registry`; the
  ~60 emit-path sites read it via `BodyCx::registry()` (returns the `'c`-lifetime
  borrow from `LowerCtx`, INDEPENDENT of `&self`, so a reader call coexists with
  `&mut cx`), threaded `ExecCtx → BodyEmitter::registry() → LowerCtx.registry`;
  leaf helpers got a `reg` param. The `kernel_abi` unit tests became hermetic (a
  local `AbstractRegistry`, no more global insert/remove dance). Eric's call:
  full move (vs the lighter global-with-Drop-cleanup) for the architecture win,
  knowing the cost was ~87 mechanical edits not "three places."

- **`freeze_concrete` → `freeze_for_abi` rename + scratch-Vec pooling
  (2026-06-16; behavior-neutral).** Two related cleanups in `kernel_abi.rs`.
  (1) `freeze_concrete`/`freeze_concrete_d`/`freeze_normalized` renamed to
  `freeze_for_abi`/`freeze_for_abi_d`/`freeze_for_abi_normalized` (the
  `vocab::*` glob re-export carries the path; all callers in
  `fusion/{lowering,mod,emit}.rs` + `graphix-package-array` updated). The name
  now states what it IS: not (only) a `Type → Type` normalize but the
  **kernel-ABI encodability gate** — concretizing TVars is the means, deciding
  register/`Value` encodability and returning the wire shape is the end. This
  is *why* it lives in `kernel_abi` and not on `Type`: its `None`s are valid
  types with no kernel encoding (`Fn`/`Ref`/`decimal`), its accept rule is
  `PrimType::from_typ` (a register gate), it peeks through `ABSTRACT_REGISTRY`
  (opaque to the type system), and the `Map`/`Error` pass-through stops
  recursion exactly at the `AbiKind::Value` boundary (carried as an opaque
  two-register `Value`, params never reach the wire) — none of which a generic
  type normalize would do. The pure type-level concretization it rests on
  already lives on `Type` as `with_deref`/`normalize`. Expanded the fn doc to
  record this. (2) The five transient scratch `Vec`s in `freeze_for_abi_d`
  (Tuple/Struct/Variant elems + Set variant-union outer & inner) now collect
  into `LPooled<Vec<_>>` and `drain(..)` into `Arc::from_iter`:
  `triomphe::Arc::from_iter` allocates its own backing and never adopts the Vec
  (unlike `std::sync::Arc`), so the collect Vec was pure transient garbage —
  pooling it turns the warm path from two allocs (scratch + Arc) to one. Design
  point that fell out of this (Eric): the pooling reinforces the placement —
  we pool scratch in service of building a *wire-layout descriptor*, not
  normalizing a type.

- **Type concretization: fixed depth caps → real cycle detection (2026-06-16;
  predictable-performance fix + a latent hang closed).** The three functions
  that concretize a type by expanding nullary `Type::Abstract` (and, for
  `resolve_abstract`, named `Type::Ref`) through `ABSTRACT_REGISTRY` —
  `freeze_for_abi` + `abi_kind` (`kernel_abi.rs`) and `resolve_abstract`
  (`fusion/lowering.rs`) — used a fixed `depth > 16` cap to terminate on
  recursive types. The cap incremented on EVERY recursion (structural nesting
  too), so it silently refused to fuse deeply-nested but FINITE, non-recursive
  types (a 17-deep array/tuple/struct) — a predictable-performance violation.
  Replaced with a stack-allocated `Seen` cons-list (`kernel_abi::Seen` /
  `ExpandKey`, zero-heap) that cycle-detects by EXPANSION IDENTITY (full
  `TypeRef` / `AbstractId`): structural depth no longer trips it (finite types
  of any depth fuse), while true recursion — self / mutual / Ref-mediated /
  option-of-self — terminates by re-occurring-key detection → `None`/opaque
  (correct: a recursive type has no fixed ABI layout). `freeze_for_abi` needs
  no backstop (nullary abstracts can't grow params); `resolve_abstract` keeps a
  generous 256-EXPANSION backstop only for non-regular recursion (`type T<'a> =
  T<Array<'a>>`, which identity can't catch). Structural recursion is
  intentionally unbounded, consistent with sibling type-walks
  (`normalize`/`contains`/`resolve_tvars`) — any overflow-deep type dies in the
  parser/typechecker first. **`abi_kind` was a THIRD unguarded concretizer**
  (`type A = [A, null]` would have hung it) — found and fixed by a 3-lens
  adversarial review (all lenses: "sound"). Verified: 4 unit tests (40-deep
  finite freezes; self/mutual/option-of-self recursion terminates+rejects),
  1431×2 differential tests, all `Seen` lifetimes sound (covariant cons-on-stack).

- **`JitDisabled` flag deleted — one fusion flag, and `interp` made a TRUE
  node-walk (2026-06-16; values behavior-neutral, 1431×2 tests pass).**
  `CFlag::JitDisabled` was dead (never set anywhere) and "behaviourally
  identical to `FusionDisabled`" by its own doc. Worse: `jit_enabled` (derived
  from `!JitDisabled`, so permanently `true`) gated the *runtime per-slot
  HOF-callback* fusion path — which has no compile-flag access — so `interp`
  mode (`FusionDisabled` only) was STILL fusing `map`/`filter`/`fold` callbacks
  per-slot, silently contaminating the differential oracle's "node-walk ground
  truth" (proven: an instrumented probe fired 6× under interp-only map tests).
  Collapsed to the single `FusionDisabled` flag; `jit_enabled → fusion_enabled`,
  derived from `!FusionDisabled`, now gating BOTH the compile-time phase and the
  per-slot path. `interp` is now a pure node-walk (probe fires 0× for interp
  programs), so the oracle regains its node-walk-vs-fused guarantee for HOFs —
  and the node-walk HOF path it now actually exercises agrees with fused (no
  latent bug surfaced). One bool, fusion on/off; invalid states unrepresentable.

- **`jit` → `fuse` rename + flag-check promotion + the `gir` naming scrub
  (2026-06-16; pure churn, behavior-neutral — 1431×2 differential tests pass).**
  The fusion driver `Update::jit` became `Update::fuse` (it fuses, it doesn't
  JIT), and the free function `fusion::jit_node` became `fusion::fuse`. The old
  tiny `fusion::fuse` driver — whose only job was the `FusionDisabled |
  JitDisabled` skip — was deleted and that check promoted into `compile()`, so
  it runs once instead of on every recursive step. Separately, the lingering
  `gir` naming (the GIR IR itself was deleted at F3; only the names survived as
  "deferred churn") was scrubbed: the six `gir*.rs` files moved into `fusion/`
  (`emit.rs`, `kernel.rs`, `vocab.rs`, `emit_helpers.rs`, `intern.rs`,
  `scaffold.rs`), `crate::gir_*` / `graphix_compiler::gir_*` paths rewrote to
  `fusion::*`, `GirNode → Kernel`, `GirMatcher → KernelMatcher`, `gir_*` test
  names de-prefixed, and ~186 GIR mentions in comments/strings reworded to
  current-state vocabulary. Zero `gir` remains in the codebase. See the "GIR is
  historical — do NOT rebuild it" note above for the why. (Follow-up noted by the
  scrub agents: several comments still cite *deleted-but-not-gir-named* helpers —
  `compile_ifchain`, `compile_concat`, `from_type`, `emit_lambda_call`,
  `eval_kernel_full`, etc. — left intact since they contain no `gir`; a separate
  de-staling pass could address them.)

- **`Update::splice_child` deleted — the impure-HOF split re-expressed as `fuse`.**
  The only live user of the ExprId-keyed `splice_child` / `fusion::splice_into` was the
  impure-HOF body split: `fuse_callsite`'s `build_body_split` computed `(value_id →
  kernel)` pairs and a *separate* `splice_into_body` pass re-found each node by id to
  install it — decoupled only because `fuse_callsite` borrows the CallSite immutably,
  though both operate on the SAME template body. Now `fuse_callsite` does ONLY the
  whole-body (Phase-1) attempt; when it returns `None` (impure callback), `MapQ`'s
  template build runs the canonical `fusion::fuse` on the cloned template body IN
  PLACE — fusing each maximal sync sub-region via the standard parent-swap
  (`std::mem::replace`), no id search. Gated on `ctx.jit_enabled` for exact parity with
  the old `jit_compile_split_kernel` gate (`try_fuse` doesn't self-gate, so JitDisabled
  would otherwise fuse). Deleted: `Update::splice_child` (+ Bind/Module/Block overrides),
  `fusion::splice_into`, `fusion::find_node_by_id` (orphaned), `build_body_split`,
  `splice_into_body`, `SplitKernel`, `FusedCallback.split`/`is_split` (−328 net). Removes
  a DRY violation (`build_body_split` duplicated `try_fuse`'s region-finding) and there is
  no `NodeViewMut`, so reusing `fusion::fuse`'s concrete mutable descent is the only in-place
  install that doesn't add a new abstraction. `fusion::fuse` fuses strictly MORE than
  `build_body_split` — single-expr callback bodies it skipped (it only walked block
  `let`-values; `Connect` is fusion-terminal), e.g. `array::map(a, |s| str::trim(s))` —
  so 5 lib_tests fixtures (`str::{split,rsplit,splitn,rsplitn}`, `list::find`, the
  "ASPIRE: Jit" cases) gained fusion (None→Jit) and were re-annotated: the desired,
  value-identical direction (predictable fusion). Validated: 1431×2 tests, FuseExpect
  audit 0 unexpected drift, fuzz regress 22/0 + generate 3000 + mutation 3000 = 0
  divergences (4 accepted-class crashes — huge-`init` hangs / runaway-recursion node-walk
  stack overflows, pre-existing). `design/distributed_jit.md` had scheduled this for the
  F stage.
- **`static_resolve` pass deleted — 4 compile walks → 2.** The standalone
  post-typecheck static-resolution pass (`collect_lambda_binds` + `visit_mut`)
  is folded into typecheck: `typecheck0` builds `ctx.bind_to_lambda` (via
  `Bind::lambda_def_value` + `Module::typecheck0`'s sig→impl proxy entries) and
  `CallSite::typecheck1` resolves via the new `try_static_resolve` (value-based —
  the `unstable_bindings` guard kept; queuefn wrappers/`never()` stay dynamic;
  callee bodies aren't descended, so no compile-time recursion). `Module::typecheck1`
  now drives its children under the restored env (they get finalize for the first
  time). Zero drift: 1429×2, FuseExpect audit 618 OK / 0 gain / 0 loss, fuzz clean.
- **`Apply::static_resolve_fn_args` deleted — folded into `Apply::typecheck1`**
  (now 5-arg: `+ fn_args: &[StaticFnArg]`). The HOF callback hook was a second
  compile-time `Apply` method in the same phase. `typecheck1` fires on the scratch
  `def.check` (`fn_args=&[]`) AND, HOF call sites only, the bound `cs.function`
  (with the discovered `fn_args`, via `try_static_resolve`). HOF builtins
  (`MapQ`/`FoldQ`/`Init`) gate on `fn_args.find(arg_idx==N)`; `CachedArgs`/
  `CachedArgsAsync` absorb the param (inner `EvalCached` is never a HOF, so those
  traits stay unchanged). The value-based discovery stays at the call site
  (soundness — it needs the source arg node to skip queuefn wrappers). New
  `queuefn_hof_callback` regression test. Zero drift: 1431×2, audit 618 OK, fuzz clean.
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
