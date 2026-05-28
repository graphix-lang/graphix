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

- Don't commit code unless the user explicitly asks for it
- Commit messages should be short, lowercase, and imperative (e.g., `fix many parser problems`).
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

## Recent Changes

### GUI Package (Feb 2026)

Added `graphix-package-gui` — an iced 0.14 based GUI backend. Uses iced sub-crates directly (`iced_core`, `iced_wgpu`, `iced_widget`, etc.) rather than the umbrella `iced` crate for low-level control over the rendering pipeline. Note: `iced_renderer` requires both `wgpu` and `wgpu-bare` features (the cfg checks use the `wgpu-bare` flag which `wgpu` alone doesn't set).

### Package Manager Testing (Mar 2026)

`graphix-package::download_source` may be tested directly by injecting a temporary graphix data dir path and downloading a fixed released `graphix-shell` version from crates.io (for example `0.5.0`). This avoids mutating the user's real `~/.local/share/graphix` tree and provides a regression test for archive extraction layout bugs.

### Data Table Tests & Examples (Apr 2026)

Widget test access pattern: the `GuiWidget` trait gains a `#[cfg(test)] fn as_any(&self) -> &dyn Any` (and `as_any_mut`) with a default `unimplemented!()`. Widgets that need test-only state inspection (currently just `DataTableW`) override it to return `self`; `GuiTestHarness::dt()` / `dt_mut()` helpers downcast to `&DataTableW<NoExt>` / `&mut DataTableW<NoExt>`. A `pub(crate) impl DataTableW<X>` block exposes accessors like `dt_ref_width`, `dt_sparkline_len`, `dt_sparkline_values`, `dt_push_sparkline`, `dt_meta_col_idx`, `dt_cell_bounds`, `dt_user_width`, and `dt_row_sub_count`. Tests dispatch per-column callbacks through `gx.call(callable_id, args)` via a `GuiTestHarness::call_callback` helper — the widget itself fires the same callable internally, so this mirrors runtime behavior without pixel layout.

`InternalOnly` in the test ctx DOES spin up a real in-process resolver server, so `sys::net::publish` / `subscribe` round-trips work end-to-end. However, when driving many updates rapidly (e.g. via `seq(0, N)?`), netidx publisher coalescing means the subscriber only sees a few values. For multi-point sparkline subscription tests, space updates with one-shot `sys::time::timer(duration, false)` calls — a repeating timer (`true`) keeps `drain()` looping forever because batches always arrive within its reset window.

ColumnSpec `on_resize` field fix: the .gxi types it as `&[fn(width: f64) -> Any, null]` (a ref), so the runtime struct value is a u64 BindId, not the callable directly. `parse_column_specs` must extract it as a bid (like `default_value` and `width`) and then `compile_ref` + `compile_callable_opt` to get the actual callable. The widget also tracks the inner ref in `on_resize_refs` so callable swaps at runtime recompile.

### Composite-return DynCalls in the JIT (May 2026)

`GirOp::DynCall` (HOF dispatch in the cranelift JIT) now supports
fn-typed kernel params whose args *or* return are composite
(tuple/struct/variant), not just scalars. Key pieces:

- **Fusion now builds Param-source HOF kernels.** The `tail_call_slots`
  and `source_args` builders in `fusion.rs` used to `return None` on
  any fn-typed param, so a HOF whose fn-param came from `lambda.args`
  (vs. an outer binding) never fused at all. They now `continue` past
  fn-typed params — `build_arg_layout` (gir_interp) already
  reconstructs fn-param positions separately by `FnSource::Param`'s
  `arg_pos`, so `tail_call_slots` / `source_args` intentionally
  *exclude* fn_params. A fn_param + tail-loop kernel still bails
  (a tail call can't reference a bare fn_param — no emit_expr
  lowering — so it can't fuse anyway; the `has_tail` guard is
  belt-and-suspenders).

- **Shared-module string tables must outlive the code.** A kernel
  with a `DynCall` goes through `compile_kernel_with_callees` (the
  `SHARED_JIT` path; `kernel_contains_call` counts DynCall). That
  path's `define_kernel_body` built each kernel's `KernelStrings`
  (stable `*const ArcStr` addresses baked into the code for variant
  tags / struct field names) and then **dropped it** — dangling
  pointers, segfault on the first `VariantTagEq`. Fix: `CachedKernel`
  in `SharedJit::by_kernel` now carries a `_strings: KernelStrings`
  field; `define_kernel_body` returns the table and
  `compile_kernel_with_callees` stores it into the cache entry, which
  lives as long as the shared module. Moving a `KernelStrings` is
  safe — only the `Box` pointer moves, not the heap allocation the
  baked-in pointers reference.

- **Composite DynCall arg ownership.** A composite DynCall arg uses a
  refcount-bump push (`graphix_value_buf_push_array_borrowed` /
  `_value_borrowed`) when it's a `Borrowed` source (a Local read — the
  caller still owns it), or a move push (`graphix_value_buf_push_array`
  / new `graphix_value_buf_push_value`) when it's an `Owned` source
  (an inline `TupleNew`/`StructNew`/`VariantNew`, or a composite-return
  DynCall result not bound to a local). `classify_composite_source`
  drives the choice and now classifies `GirOp::DynCall` as `Owned`.
  Using the borrowed helper on an Owned source leaks the original;
  the move helper on a Borrowed source double-frees it.

### Composite block-lets & composite IfChains in the JIT (May 2026)

Pre-M8.4 hardening — bigger fused subgraphs hit two JIT codegen
crashes that smaller kernels didn't:

- **`GirOp::Block` with a composite let panicked.** The block-
  expression arm did `prim_of(&l.value.typ)` on every let — a panic
  for any tuple/struct/variant let. It also only `mark`/`truncate`d
  `env.locals`, so composite/variant block-lets leaked into the
  enclosing scope (and weren't dropped, leaking per-iteration in a
  loop body). Fixed: the arm now routes composite/variant lets to
  `bind_composite`/`bind_variant`, snapshots all three `env` lists,
  drops the block-scoped composites/variants on exit, and runs the
  block tail through `ensure_owned_composite` (so a tail that aliases
  a block-scoped local outlives the block).

- **Composite `select`-as-expression (`GirOp::IfChain`) panicked.**
  `compile_ifchain` took a `PrimType` and `prim_to_clif`'d it for the
  merge-block param — a panic for a tuple/variant-typed if-chain. It
  now takes a `ClifType` (via the new `clif_of`, composites → `I64`)
  and runs each arm result through `ensure_owned_composite` so the
  merge always receives an owned pointer regardless of which arm won.

- **`JitEnv::mark`/`truncate` now cover all three binding lists**
  (`locals`, `composites`, `variants`) via an `EnvMark` snapshot — not
  just `locals`. `truncate` is compile-time `env`-Vec hygiene only
  (no runtime drops); drops are the job of scope-exit code
  (`GirOp::Block`) and terminating statements (`GirStmt::Return` via
  `drop_owned_composites`). `LowerCtx::param_count` became
  `param_mark: EnvMark` so a `TailCall` rebind resets every list to
  the post-param state.

- **`ensure_owned_composite`** is the single ownership choke point:
  given a composite expr + its compiled value, it refcount-clones a
  `Borrowed` source and passes an `Owned` one through. Used by
  `GirStmt::Let`, `GirStmt::Return`, the `GirOp::Block` tail, and each
  `compile_ifchain` arm. `classify_composite_source` now also
  classifies `GirOp::Block` and `GirOp::IfChain` as `Owned` (correct
  *because* those sites pre-own their result via this helper).

### AOT pipeline removed (May 2026)

The ahead-of-time Rust-source-emission backend is gone — the JIT
(cranelift) is now the only fusion backend. Removed: `fusion.rs`'s
`emit_kernel` / `emit_function_kernel*` (Rust-source emitters),
`rewrite_program*` / `rewrite_walk` / `RewriteState` / `FusedKernel`
(the AST-rewriting program pass), `emit_package` / `render_*` /
`write_package` (cargo-package emission), and `walk_and_fuse` /
`try_fuse_lambda`; `gir.rs`'s `gir_to_rust_*` emitter plus the
`rust_name`/`rust_op`/`rust_src` helper methods and the now-dead
`rust_name` field on `Input`/`ArrayInput`/`TupleInput`/`StructInput`/
`VariantInput`/`TailCallSlot`/`SelfArg`; and the `graphix compile`
CLI subcommand (`handle_compile` + the standalone-binary build
helpers) from `graphix-shell`. ~3700 lines net. The live fusion path
is unchanged: `build_kir_kernel` → `GirKernel` → cranelift JIT (or
`gir_interp`). This clears the deck for M8.4 — maximal sync subgraph
splitting is implemented exactly once, in `analyze_program`.

### Fusion refactor — Phases 0–3 (May 2026)

Multi-phase refactor aimed at eliminating duplicated paths and
preparing for nested-composite data shapes. Tests pass 1090/1090
across the workspace after each phase.

**Phase 0 — Typed AST via `Arc<OnceLock<Type>>` on `Expr`.**
Added a `typ: Arc<OnceLock<Type>>` field to `Expr` (shared via Arc so
clones see the same cell; `OnceLock` enforces write-once at the type
system level — Type's structure is set once during typecheck, TVar
refinement flows through the inner `Arc<RwLock>` of TVars). The
`Update` trait reshaped: `typecheck` is now a default method that
runs `typecheck_inner` and propagates `self.typ()` into
`self.spec().typ`. Every Update impl renamed via one sed sweep — no
per-impl discipline needed, the compiler enforces propagation.

**Phase 1a — Single GirNode init chokepoint.** All three GirNode
constructors (`new`, `with_jit`, `with_async_jit`) now take
`&mut ExecCtx` and route through a private `build` chokepoint that
runs both `pre_init_binding_slots` + `pre_init_builtin_slots`
internally. Previously the lazy-fusion path manually called both and
the FusedRegion path called only the builtin one — easy to forget,
and forgetting the builtin one panicked with "fn-arg value isn't a
LambdaDef" on first DynCall into a fused stdlib builtin. Now
impossible to skip.

**Phase 1b — Consolidated let-routing.** Three near-duplicate `match
GirType -> push to ctx.*_inputs` blocks (in `emit_do_as_expr`,
`emit_bind_stmt`, and parameter-discovery) replaced with one helper
`register_kir_binding(ctx, name, &GirType)`. Adding a new `GirType`
variant is now one site instead of three.

**Phase 2 — Widened GirType to support nested composites.**
`Array(PrimType)` → `Array(Box<GirType>)`; `Tuple(Vec<PrimType>)` →
`Tuple(Vec<GirType>)`; `Struct(Vec<(ArcStr, PrimType)>)` →
`Struct(Vec<(ArcStr, GirType)>)`; `Variant(...Vec<PrimType>)` →
`Variant(...Vec<GirType>)`. The `Input`/`ArrayInput`/`TupleInput`/
`StructInput`/`VariantInput` slot structs widened similarly. New
`GirType::as_array_prim()` helper for sites that still need a flat
`PrimType` (with `as_array_elem()` as the fully-general accessor).
Runtime layer needed no changes — `ValArray<Value>` was already
nested-capable. Today's consumers still call `as_prim()?` to bail
on composite elements where the lowering/JIT machinery doesn't yet
handle them; the type system now *allows* nested composites and
follow-up work can lower them.

**Phase 3 — Unified infer with emit.** `infer_body_rtype` no longer
runs a parallel scope-tracking AST walk. After the self-recursion +
known-fn short-circuit, it reads the cached type off `body.typ.get()`
(populated by Phase 0's propagation) and translates via
`GirType::from_type`. Falls through to `emit_expr` only for
synthesized Exprs whose `typ` cell is empty (module-kernel synth
tail, etc.). The Do/Select/ExplicitParens recursion is gone — the
typed AST means the body's *resulting* type is known without walking
control structure.

**Phase 4 — Collapsed kernel-build entry points.** The three entry
points (lambda body, region root, top-level module Do) now route
through a single unified `build_kernel(fn_name, body, value_inputs,
fn_inputs, return_type, has_tail, self_info, known, consts)`. The
slot-list routing (the giant `match RegionInputKind { Prim => ...,
Array => ..., Tuple => ..., Struct => ..., Variant => ... }` that
populates `ctx.inputs` / `ctx.*_inputs` / `params.*_params` /
`tail_call_slots` — previously duplicated in two places) collapsed
into one `populate_kernel_inputs` function. Adding a new
`RegionInputKind` variant is now one edit instead of two.

The three callers become thin translation layers:
- `build_kir_kernel_with_binding_inputs` (lambda): classifies each
  `lambda.args` entry into either a `RegionInput` (value) or a
  `FnParam` (HOF arg with `FnSource::Param`); computes `has_tail` +
  `SelfInfo` + `source_args`; calls `build_kernel`. The two parallel
  arg walks (~100 lines each, one for source_args, one for
  tail_call_slots) collapsed to one classify-pass plus a single
  source_args build.
- `build_kir_kernel_from_region` (region): trivial pass-through —
  inputs are already in `&[RegionInput]` shape, no self_info or tail.
- `build_module_kernel` unchanged at the public API level; calls
  `build_kir_kernel_from_region` (which delegates to `build_kernel`).

Net: ~800 lines deleted from `fusion.rs` across Phases 0–4.

**Phase 5 — `ctx.fn_types` sidecar deleted.** With Phase 0's typed
AST in place, the per-`ExprId` `FnType` sidecar was redundant:
- For an Apply call site, the call-site-specialized FnType lives on
  `a.function.typ.get()` (the function expression's typ cell, set
  by `Update::typecheck` propagation). `try_register_builtin_call`,
  `apply_site_effect`, and `try_build_lazy` (via the new
  `apply_site_hint: Arc<OnceLock<Type>>` parameter) all read from
  there now.
- For a lambda's spec FnType, `FusionLazyEntry` gained a
  `spec_typ: Arc<OnceLock<Type>>` field that shares the bind's
  source `Expr.typ` cell. `try_build_lazy` reads from there instead
  of `ctx.fn_types[spec_id]`.
- The two producers (`node/lambda.rs` and `node/callsite.rs`'s
  `ctx.fn_types.insert(...)` calls) are gone.
- The field, its `IntMap` allocation in `ExecCtx::new`, and the
  `CheckWithTypes` / `check_with_types` public API (originally for
  the removed `graphix compile` subcommand, no in-workspace callers
  remained) are all deleted.

`discover_callee_names` now yields `(ArcStr, Arc<OnceLock<Type>>)`
pairs instead of `(ArcStr, ExprId)` — the cell *is* the lookup
result, no sidecar indirection.

**Phase 6 — composite-element accessor lowering.** Lifted the
`as_prim()?` bail in `emit_tuple_ref` / `emit_struct_ref` /
`emit_array_ref` so composite slot types can be read. Widened
`GirOp::TupleGet.elem_typ` and `GirOp::StructGet.elem_typ` from
`PrimType` to `GirType`; `GirOp::ArrayGet` reads its element type
straight off `e.typ` (no separate field). The interpreter routes
composite-typed accesses through a new `extract_composite_or_scalar`
helper — primitive slots use the existing fast scalar extraction;
composite slots clone the slot `Value` into the matching
`EvalResult` variant. The JIT routes any kernel containing a
composite-element accessor through the interpreter via a new
`kernel_contains_composite_element_op` guard (same pattern as
`kernel_contains_string`). End-to-end test: `let s = {nested: (1,
2), flag: true}; let pair = s.nested; let first = pair.0;` works
identically under `--no-fusion`, `--whole-graph`, and lazy modes.

Producer ops (`TupleNew` / `StructNew` / `VariantNew`) still bail on
composite fields — same pattern but more surgery (the producer
GirOps' `elem_types: Vec<PrimType>` would need widening too, plus
interp/JIT side updates). Follow-up.

**M8.4(g) — flip the switch + eager lambda fusion.** Three coupled
cuts:

1. **Whole-graph default on.** `FusionConfig::whole_graph` defaults
   to `true`. CLI flag is now `--no-whole-graph` (opt-out) instead
   of `--whole-graph` (opt-in).
2. **Module-kernel dedupe.** `gx.rs::load()` now deletes the
   subsumed Bind / side-effect nodes after splicing the
   `FusedRegion`. Previously the kernel ran "alongside" the
   originals — both computed the same values, the runtime variable
   system handled the race. The dedupe removes the duplicate
   compute.
3. **Lazy lambda fusion → eager.** Added
   `fusion::eager_fuse_lambdas(ctx)`: a post-typecheck pass that
   walks every entry in `ctx.fusion_lambdas`, builds the kernel +
   JIT artifacts + kernel registry, stores the full slot in the
   entry's cache (`FusionLazyCache::Built` gained `jit`,
   `async_jit`, and `registry` fields). Wired into `compile_root`,
   `compile`, and `load` between `infer_effects` and the splice /
   module-kernel build.

   `node/lambda.rs::Lambda::compile`'s InitFn shrank from ~390 lines
   of inline discovery + build + JIT logic to ~50 lines of cache
   lookup. The closure now just reads the entry's `Built` slot and
   wraps it in `GirNode::{new,with_jit,with_async_jit}`; if the
   slot isn't `Built`, falls back to `GXLambda` (interpreter body).
   Calls to a fused lambda from any context — region inlining via
   `GirOp::Call`, dynamic dispatch through `Apply::update`,
   anything else — go through the same cached kernel + JIT, no
   per-call-site re-build.

   Known limitation: anonymous lambdas (not bound to a name, e.g.
   `(|x| x + 1)(42)` applied directly) are not in
   `ctx.fusion_lambdas` and so don't get eagerly fused. They fall
   back to `GXLambda` (interp). HOF args (`array::map(xs, |x|
   ...)`) aren't affected — those are inlined into the surrounding
   region's kernel by `emit_array_map` / etc., not dispatched as
   lambdas.

After M8.4(g): per-lambda lazy fusion is gone. `lazy_resolve_kernel`
remains as the kernel-building primitive used by `eager_fuse_lambdas`
and `analyze_program` — its body wasn't unnecessary.

**Remaining refactor work (tracked as tasks):**
- Producer-side composite-element support (TupleNew/StructNew/
  VariantNew accepting composite fields).
- M8.4(h): cleanup + docs (M4g stability gate, etc.).
- Anonymous-lambda eager fusion (currently fall back to interp).

### `GirType::Null` + null-pattern lowering (May 2026)

Foundation for fusing code that produces or pattern-matches on
`null` (graphix's `[T, null]` option shape):

- `GirOp::ConstNull` — the `null` literal. Result type
  `GirType::Null`. `emit_expr` lowers
  `ExprKind::Constant(Value::Null)` to this op.
- `GirOp::IsNull(GirExpr)` — boolean test. Operand must be
  `GirType::Null` or `GirType::Nullable(_)`; result `Bool`.
- `EvalResult::Null` — interp marshalling for null values; `IsNull`
  matches either this variant or a `Variant(Value::Null)` slot (so
  the check works whether the operand is a literal or a Nullable
  local read).
- `kernel_contains_null` JIT guard — mirrors `kernel_contains_string`.
  Kernels that touch `GirType::Null`/`Nullable`/`ConstNull`/`IsNull`
  route to the interpreter. The JIT compile-expr arm returns an
  error rather than panicking; the guard makes that error
  unreachable in well-formed routing.
- `emit_arm` now consults `pat.type_predicate` via a new
  `emit_type_predicate_cond` helper. The only narrowing it
  understands today is `Type::Primitive(Typ::Null)` → `IsNull(scrut)`;
  anything else returns `Ok(None)` (arm proceeds unconstrained,
  same as before). Lets `select x { null as _ => …, … }` patterns
  lower to a real condition instead of silently being treated as
  "always matches."

What's still missing (next step): nullable lets aren't representable
yet (`register_kir_binding` bails on `GirType::Nullable`), and
`emit_select_as_expr` requires all arms to share a GirType so a
mixed `i64`/`null` IfChain can't unify into `Nullable<i64>`. Filter-
map style code (predicate returning `[T, null]`) won't fuse end-to-
end until those two pieces land; the IR + interp side is in place.

### Nullable as fused-kernel intermediate (May 2026)

Lifted both blockers from the previous note so a `let nullable: [T,
null] = ...; select nullable { null as _ => …, T as v => … }`
pattern fuses end-to-end through the interpreter (the JIT still
bails on null-touching kernels via `kernel_contains_null`):

- **`EvalResult::Nullable(Value)`** — new variant; carries either
  `Value::Null` or `T`'s runtime form. `into_value` unwraps;
  `GirOp::IsNull` matches against it in addition to
  `EvalResult::Null` and `EvalResult::Variant(Value::Null)`.
- **`InterpEnv::nullables: Vec<(ArcStr, Value)>`** — dedicated
  slot list (semantically distinct from `variants`); paired with
  `lookup_nullable` / `push_nullable` / `rebind_nullable` helpers.
  `GirOp::Block` truncates it on scope exit alongside `arrays` and
  `variants`. `push_local` routes `EvalResult::Nullable` here; the
  bare `EvalResult::Null` case panics (GIR is malformed if it
  reaches the slot — every binding-time site widens first).
- **`GirOp::Local` for Nullable typed reads** — returns
  `EvalResult::Nullable(value)`. `GirOp::IfChain` whose declared
  type is `GirType::Nullable<T>` normalizes each arm result via
  `normalize_to_nullable` (a new helper) so a scalar-T arm and a
  null arm both surface as the `Nullable` shape regardless of
  which branch fires. `GirOp::DynCall` with a `Nullable` return
  type wraps similarly; with a `Null` return it surfaces as
  `EvalResult::Null`.
- **`GirKernel` boundary** — nothing changes at the JIT boundary
  (`kernel_contains_null` keeps null kernels off the JIT). The
  interp path's `into_value` already covers the marshalling.
- **`NullableInput` kernel slot** — new struct (`name`, `elem`,
  `bind_id`); paired `FusionCtx::nullable_inputs` + `find_nullable`
  helper. `register_kir_binding` now routes `GirType::Nullable` here
  (used to bail). `emit_expr`'s `Ref` lookup checks `find_nullable`
  after `find_variant`.
- **`emit_select_as_expr` mixed-arm unification** — new
  `unify_arm_types` helper widens `Prim(T) ∪ Null →
  Nullable<Prim(T)>`, `Variant ∪ Null → Nullable<Variant>`, etc.
  Anything that doesn't fit the `T+null` shape returns `None`
  (caller bails on fusion). Also threaded `emit_type_predicate_cond`
  into `emit_select_as_expr` (parallels the `emit_arm` flow added
  in the previous step).

What's still missing: bare `GirType::Null` as a kernel param/let
isn't a real shape — every site widens to `Nullable<T>`. Nullable
*kernel parameters* (coming in from outside the kernel) aren't
populated yet (`populate_kernel_inputs` doesn't have a Nullable
arm); only Nullable *intermediates* (let-bindings, IfChain results,
DynCall returns) work. That's a follow-up when we want filter_map's
predicate to fuse with a Nullable-typed callback arg.

### Nullable as kernel parameter / return (May 2026)

Closed the gap from the previous section — lambdas (and regions /
modules) whose argspec or return shape contains `[T, null]` now
fuse end-to-end through the interpreter:

- **`GirKernel::nullable_params: Vec<NullableInput>`** — new slot
  list paralleling `variant_params`. `eval_kernel_full` gained a
  matching `nullable_args: &[Value]` parameter that gets pushed
  into `env.nullables` at kernel entry.
- **`RegionInputKind::Nullable(GirType)`** + `populate_kernel_inputs`
  arm + `type_to_region_input_kind` arm + `region_input_kind_to_kir_type`
  arm — every region-input pathway recognises and routes Nullable
  inputs through the new slot list. Bare `GirType::Null` is still
  rejected as a free-var type (it's a producer-only shape; widen
  before binding).
- **`TailCallSlotKind::Nullable`** + `ArgKind::Nullable(u32)` —
  `build_arg_layout` walks `tail_call_slots` and routes Nullable
  positions to the dedicated index. `GirNode::update` collects a
  `nullable_args` smallvec from the incoming `args` slice and feeds
  it through to `eval_kernel_full`.
- **JIT-side**: `kernel_contains_null` already gates Nullable-
  touching kernels off the JIT, so the cranelift arg-packing path
  doesn't need to learn the new shape. `TailCallSlotKind::Nullable`
  in the JIT tail-call rebind arm returns an `Err` (unreachable
  via the guard, but defends against routing drift).
- **`GirNode::update` boundary marshalling for return** — already
  worked: when a kernel's `return_type` is `GirType::Nullable<T>`,
  the body returns `EvalResult::Nullable(Value)` and `into_value()`
  unwraps to the boundary `Value` (`Null` or `T`'s form).
- **Bulk schema update**: 30 `GirKernel { ... variant_params: vec![],
  }` construction sites across `gir_interp.rs`, `gir_jit.rs`,
  `fusion.rs` were updated to add `nullable_params: vec![],` after
  `variant_params: vec![]` via a single `sed` pass — every site has
  the same structure.

Direct interp coverage: `nullable_kernel_param_round_trip` builds a
kernel with a single `[i64, null]` arg, dispatches on `IsNull`, and
verifies both `Value::I64(42)` and `Value::Null` round-trip through
the new arg-marshalling and env slot path. Differential coverage:
`null_arg_pos`/`null_arg_null` (Nullable as kernel param) and
`null_return_then_consume`/`null_return_then_consume_pos` (one
lambda *returns* a Nullable, a second consumes it) cover the
parameter/return boundary in all four modes.

### Nullable in JIT-compiled kernels (May 2026)

The `kernel_contains_null` JIT gate was disabling JIT for any
kernel that touched `[T, null]` — way too coarse once Nullable
became a routine intermediate. Removed the gate and taught
cranelift the Nullable shape end-to-end.

**Wire shape.** A `Nullable<T>` value at the JIT ABI level is
`*mut Value` (owned) or `*const Value` (borrowed) — same shape as
a variant. Pays one heap allocation per produced value but reuses
all of variant's cleanup, ABI, and DynCall plumbing. The
alternative tagged-scalar SSA representation (bool + scalar pair)
would avoid allocation for scalar T but doubles SSA bookkeeping at
every site (lets, IfChain merges, DynCall args, etc.) and doesn't
help for `Nullable<composite>` anyway. Specialization can come
later if perf demands.

**New helpers** (`gir_jit_helpers.rs`):
- `graphix_value_new_null() -> *mut Value` — boxes
  `Box::new(Value::Null)`.
- `graphix_value_new_<T>(scalar) -> *mut Value` for every
  `PrimType` — boxes `Box::new(Value::T(scalar))`. Used by IfChain
  arm widening when a narrow scalar arm joins a `Nullable<T>` merge.
- `graphix_value_is_null(*const Value) -> u8` — borrowed read.

**JIT env** (`gir_jit.rs`):
- `JitEnv::nullables: Vec<(ArcStr, Variable)>` slot, parallel to
  `variants`. Same `*const Value` wire shape, same
  `graphix_value_drop` cleanup. `EnvMark::nullables` extended.
- `bind_nullable` / `lookup_nullable` helpers.

**compile_expr arms**:
- `GirOp::ConstNull` → `call graphix_value_new_null` → owned
  `*mut Value`.
- `GirOp::IsNull(inner)` → compile inner (must lower to
  `*const Value`), `call graphix_value_is_null` → bool.
- `GirOp::Local` for `GirType::Nullable` → read from
  `env.nullables`.

**Let** routes `GirType::Nullable` to `env.bind_nullable` via the
existing `ensure_owned_composite` (extended to clone via
`graphix_value_clone` for Nullable sources).

**IfChain widening**: `compile_ifchain` now takes the result
`&GirType` (not just CLIF type). When the merge is `Nullable<T>`
and an arm produces `Prim<T>` or bare `Null`, `widen_to_nullable`
calls the matching boxing helper before the arm jumps to the merge.
Already-Nullable arms still go through `ensure_owned_composite`
(clones a Borrowed source).

**ABI** (`define_kernel_body` + `compile_kernel_with_wrapper`):
- `n_composites` count + the `*const Value` arg slot loop both
  extended to include `kernel.nullable_params.len()`.
- `return_clif` includes `GirType::Nullable(_) => types::I64`
  (boxed-pointer return, just like variant).
- `compile_into_function` clones each `nullable_params` entry on
  kernel entry via `graphix_value_clone` (same discipline as
  variants — params are borrowed at the ABI but owned in env so
  drop logic stays uniform).

**Scope cleanup**: `drop_owned_composites`,
`emit_pending_cleanup`, and `GirOp::Block`'s scope-exit walk all
include `env.nullables` alongside `env.composites` /
`env.variants` — every entry drops via `graphix_value_drop`.

**DynCall**: arg push for `GirType::Nullable` uses the same
`graphix_value_buf_push_value` / `_borrowed` helpers as variants
(distinguished by `classify_composite_source`); return decoding
for `GirType::Nullable` uses `ret_kind=2` (Box the whole Value)
— `dispatch_typed` already wraps any Value uniformly for that
ret_kind. `ret_kind=3` (Unit) decode arm added in
`dispatch_typed` for symmetry.

**GirNode::update boundary**: nullable_args added to the JIT slot-
packing loop (pointers after variants); the return-decode arm for
`GirType::Nullable` reclaims via `Box::from_raw` and wraps in
`EvalResult::Nullable`.

**Gate removed**: `fusion.rs`'s `has_null` JIT guard deleted. The
only Nullable corner the JIT can't handle is a
`TailCallSlotKind::Nullable` rebind — that returns `Err` at
compile time and the caller logs a warn and falls back to interp.
No separate guard needed.

**JIT-direct tests** in `gir_jit::tests`:
- `wrapper_nullable_kernel_param` — `Nullable<i64>` arg + IsNull
  dispatch, returns -1 for Null and 7 otherwise.
- `wrapper_nullable_kernel_return` — IfChain widens scalar/null
  arms into `Nullable<i64>`, return is decoded by Box::from_raw.

All 8 `cross_mode_null_handling` differential fixtures continue to
pass (now exercising the JIT path in mode C/D too). 151/151
compiler + 673/673 integration tests green.

### Value passed by value in the JIT (May 2026)

`netidx::Value` is `#[repr(u64)]`, 16 bytes (`(u64 disc, u64 payload)`),
8-byte aligned — two machine words. SysV AMD64 passes 16-byte
aggregates with two integer eightbytes in two integer registers
(RDI/RSI for args, RAX/RDX for return). Cranelift's
default_call_conv matches the SysV ABI when a helper sig declares
two `I64` params/returns. **No boxing**: the previous `Box<Value>`
wrapper that turned every Value-shape value into a heap allocation
is gone; Value flows through registers across the JIT/runtime
boundary.

`netidx_value::ValArray`, `PBytes`, and `Abstract` got
`#[repr(transparent)]` markers since they're newtypes over thin
pointers (silences the `improper_ctypes_definitions` lint without
needing a module-level allow).

**Compile-time pin**: `gir_jit_helpers.rs` const-asserts
`size_of::<Value>() == 16 && align_of::<Value>() == 8`. If a
netidx release adds a wider variant payload the assertion fires.

**Helper ABI**: every Value-shape helper takes/returns `Value` by
value. `graphix_value_drop(v: Value)` consumes; every other Value-
taking helper (`graphix_value_clone`, `graphix_value_is_null`,
`graphix_variant_tag_eq`, `graphix_variant_payload_*`,
`graphix_value_buf_push_value`, `_value_borrowed`) `mem::forget`s
the input so the caller's bits (in two registers) stay valid.
CLIF sigs declare two `I64` params per Value arg, two `I64`
returns per Value return.

**JitEnv**: `variants` and `nullables` slots store `ValueVar
{ disc: Variable, payload: Variable }` pairs. Reads (`b.use_var`
on both vars) give a CLIF (disc, payload) pair; writes update
both.

**`compile_expr` shape dispatch**: returns `CompiledExpr` —
either `Single(ClifValue)` (scalar/composite pointer) or `Value
{ disc, payload }` (Variant/Nullable). `compile_expr` itself
dispatches on `GirType` and routes Value-shape exprs to
`compile_value_expr`; everything else falls through to
`compile_scalar_impl` (the old body, returning a single ClifValue
wrapped as Single). A `compile_scalar` thin wrapper exists for
the ~30 sites that only handle scalar — it asserts Single.

**Inline construction**: `GirOp::ConstNull` emits `(iconst
NULL_DISC, iconst 0)` inline; no helper call. `GirOp::IsNull`
emits `icmp_imm Equal disc NULL_DISC` inline. Nullary
`VariantNew` (Value::String(tag)) emits `(STRING_DISC,
tag_arcstr_ptr)` inline. IfChain widening to `Nullable<T>` for
narrow scalar arms packs `(prim_to_value_disc(T),
scalar_promoted_to_i64)` inline — all the `graphix_value_new_<T>
(scalar) -> *mut Value` boxing helpers are gone.

**`value_disc` module**: discriminant constants (`NULL = 0x8000`,
`I64 = 0x400`, etc.) mirror the explicit `repr(u64)` values in
`netidx_value::Value`'s definition. `prim_to_value_disc(PrimType)`
maps prims to discs; `scalar_to_payload_i64` promotes/bitcasts a
CLIF scalar to the 8-byte payload word.

**Kernel boundary ABI**: typed kernel signature + wrapper both
slot Variant/Nullable params as TWO `I64` slots each (was one
`*const Value`). Variant/Nullable returns: TWO `I64` return
values each. `GirNode::update` packs the boundary by reading the
borrowed `Value`'s two `u64` words via `*(v as *const Value as
*const u64)` and `.add(1)`; decode reclaims via `mem::transmute
::<[u64; 2], Value>`.

**`ensure_owned_value`** is the Value-shape sibling of
`ensure_owned_composite`: Borrowed sources (Local reads) get a
refcount-bump via `graphix_value_clone`; Owned sources (producers
like `VariantNew`, `ConstNull`, IfChain merges) pass through.

**Migrated to `compile_value_expr`**:
- `GirOp::ConstNull` (inline, no helper).
- `GirOp::Local` for Variant/Nullable (reads `ValueVar` pair).
- `GirOp::IfChain` whose result type is Variant/Nullable —
  `compile_ifchain` returns `CompiledExpr` with a two-word merge
  phi when the result is Value-shape; narrow arms widen inline
  via `widen_arm_to_value`.
- `GirOp::VariantNew` — nullary inline; with-payload uses
  `graphix_value_new_from_array(arr) -> Value` to unbox the
  finalized ValArray Box and return the Value's (disc, payload)
  words.

**Consumer ops updated**: `GirOp::VariantTagEq` and `VariantPayload`
in `compile_scalar_impl` now read `ValueVar` pairs and pass three
args (disc, payload, expected_or_idx) to the by-value helpers.
`GirOp::IsNull` reads its operand via `compile_value_expr` and
inlines the disc compare.

**Drop / cleanup**: `drop_owned_composites`, `emit_pending_cleanup`,
and `GirOp::Block`'s scope-exit walk all iterate `env.variants` /
`env.nullables` as `ValueVar` pairs and call `graphix_value_drop`
with two args.

**Pending-exit sentinel**: emits TWO `iconst(I64, 0)` returns for
Value-shape kernels (was single).

**Still unmigrated** (guarded via `kernel_contains_unmigrated_value_
producer` in `fusion.rs`):
- `GirOp::Block` whose tail is Value-shape — easy add (mirror the
  scalar Block but unpack `CompiledExpr::Value` at the tail).
- `GirOp::DynCall` returning Value-shape — `compile_value_expr`
  has the arm but enabling it triggers SIGILL at runtime; needs
  the dispatch_typed / pending-take return contract verified end-
  to-end before re-enabling. Code is in place behind the guard.

**Bulk tests**: 149/149 compiler + 673/673 integration all green.
Differential `cross_mode_null_handling` (8 fixtures) and the
variant integration tests all pass — proving variant + nullable
codegen still works end-to-end through the new two-register
Value ABI under modes A/B/C/D.

### Value-shape producers — full coverage in JIT (May 2026)

Migrated the remaining Value-shape producers (Block-tail Value,
DynCall returning Value, plus IfChain/VariantNew refinements)
into `compile_value_expr`'s two-register `(disc, payload)` output.
The interim `kernel_contains_unmigrated_value_producer` guard in
`fusion.rs` is gone — there's no longer any Value-shape GIR
shape the JIT routes to interp.

**`graphix_dyncall`'s return ABI changed** to a `#[repr(C)] struct
DynCallRet { word0: u64, word1: u64 }` — explicit two-word return
matching the CLIF sig's two `I64` returns and the SysV
RAX/RDX register convention. `dispatch_typed` populates both
words: `ret_kind=2` (Value-shape return) transfers Value's two
`repr(u64)` words via `ManuallyDrop` + `transmute_copy`; other
kinds leave `word1=0`. Previously `graphix_dyncall` returned
`u64` while the CLIF sig declared two returns — cranelift was
reading RDX's stale bits as the Value's payload word, producing a
malformed Value (SIGILL on subsequent op).

**Nullary `VariantNew` (Value::String(tag)) bug**: was packing
`(STRING_DISC, interned_arcstr_ptr)` inline without bumping the
ArcStr refcount. The interned ArcStr is owned by `KernelStrings`
(per-kernel statics), not by the kernel itself; the eventual
`graphix_value_drop` decremented the static's refcount,
eventually freeing the interned ArcStr and corrupting later
uses. Fix: route through `graphix_value_new_string_from_arcstr`
which does `(*tag).clone()` to bump the refcount before wrapping.
With-payload variants are unaffected — the ArcStr is pushed via
`graphix_value_buf_push_arcstr` which already clones into the
ValArray slot.

### Pending-path correctness (May 2026)

Three-agent audit (entry/exit catalog, refcount discipline,
pending-path cleanup) caught two latent bugs in the DynCall
pending machinery:

**Fix 1 — `graphix_dyncall_pending_take` was clearing.** Latent UB
on composite-DynCall pending paths: the JIT pre_pending block
peeked-and-CLEARED the flag, jumped to `pending_exit` which
emitted a null sentinel, and `GirNode::update`'s wrapper-level
pending check then saw `false` (because the JIT cleared it),
skipped the early-return, and decoded the null sentinel — `Box
::from_raw(0)` for composite returns, `transmute([0, 0]) ->
Value` for Value-shape returns (the all-zero discriminant isn't
a valid Value variant). Fix: change `pending_take` to a peek —
the flag stays set until `GirNode::update` resets at the top of
the NEXT kernel invocation. Existing tests didn't hit this
because no test actually causes a composite DynCall to pend at
runtime; pre-existing dyncall fixtures all resolve immediately.

**Fix 2 — scalar-DynCall-pending leaked composite kernel results.**
Scalar DynCalls deliberately don't emit `pre_pending` branches
(the zero-sentinel is harmless for downstream arithmetic). But
when the surrounding kernel's return path produces an owned heap
allocation (e.g., `let v = scalar_dyncall(); TupleNew(v, ...)`),
the kernel runs to completion with garbage zeros, the
`GirNode::update` wrapper-level pending check returns `None`, and
the freshly-allocated ValArray is leaked. Fix: every
`GirStmt::Return` for composite / Value-shape returns now emits
`emit_return_pending_check` — peeks the flag, and on pending
drops the about-to-return owned result via `graphix_valarray_drop`
/ `graphix_value_drop`, runs `emit_pending_cleanup` (env + in-
flight DynCall bufs), and jumps to `pending_exit`. Scalar/Unit
returns skip the check (no allocation, no leak risk).

`ReturnDropShape` enum captures the drop-helper choice at the
call site (`Composite(ptr)` vs. `Value { disc, payload }`).
`emit_return_pending_check` lives next to `emit_pending_cleanup`
in `gir_jit.rs` and reuses the lazy `pending_exit` slot.

**Direct tests**:
- `gir_jit_helpers::tests::pending_take_is_peek_not_clear`:
  asserts two successive `pending_take` calls both return 1 and
  the flag stays set.
- `gir_jit::tests::return_pending_check_drops_composite_result`:
  builds a kernel returning `(i64, i64)`, calls it once with
  pending unset (baseline — real ValArray returned), then again
  with pending pre-set (verifies the wrapper returns `0` sentinel
  instead of the ValArray pointer, and that the flag stays set).
  Implicitly proves the pre_pending branch fires; the dropped
  ValArray isn't observable in a unit test without allocator
  instrumentation.

**Audit outcome**: 1 audit fixed the boxing-related VariantNew
bug pre-emptively; 2 audits confirmed the rest of the refcount/
ownership discipline is sound. No other use-after-free or leak
issues found.

151/151 compiler + 673/673 integration all green.

### M8.4 maximal sync subgraph splitting (May 2026)

The M8.4 "initial model" — regions are fully-sync subtrees, every
Async edge splits a region — left a lot of fusion on the table.
A typical `let x = subscribe(...); let y = x + 1` couldn't fuse the
`+ 1` arithmetic into a kernel because the unstable Ref to `x` made
the parent expression Async, blocking region formation. The
"maximal" follow-up promotes Async sub-expressions to *kernel
inputs* instead of making them region boundaries — the surrounding
Sync ops still fuse into one kernel, and the runtime feeds the
Async value via a separately-compiled Node arg_node.

**Mechanism**:
- **`program_effect_map` intrinsic-only**: a node's effect is now
  its own intrinsic edge effect (unstable Ref or async-Apply
  call-site), NOT joined with children's effects. A Sync-pure
  parent of Async children stays Sync.
- **`collect_region` lifts Async children**: when recursing into
  region members, an Async child stops recursion and gets pushed
  into a sibling `lifted: Vec<Expr>` list (clone of the Expr).
  Members keep accumulating Sync descendants beyond the lifted
  boundary.
- **`RegionInputSource`** enum on `RegionInput`: `Binding` (existing
  case — free-var Ref to an outer binding; runtime compiles a `Ref`
  feeder) vs `Lifted(Expr)` (new — runtime compiles the carried
  Expr through the normal node pipeline and feeds its output).
- **`discover_region_inputs` extended**: takes the lifted list,
  builds `lifted_ids: IntSet<ExprId>` to short-circuit `walk_free_refs`
  at lifted boundaries (so internals of lifted sub-trees don't
  contribute Refs to the kernel's input set), and appends one
  `RegionInput { source: Lifted(...), name: "__lifted_<id>", ... }`
  per lifted child.
- **`FusionCtx::lifted_inputs: IntMap<ExprId, ArcStr>`** + emit_expr
  intercept: when `populate_kernel_inputs` registers a lifted
  input, it also inserts `expr_id → synth_name` into this map. The
  intercept fires at the top of `emit_expr` before the main match,
  short-circuiting the normal lowering with a `lookup_local(name)`
  call (same per-shape dispatch as the `Ref` arm, refactored into
  `FusionCtx::lookup_local`).
- **`FusedRegion::from_subgraph` per-input dispatch**: branches on
  `input.source` — `Binding` keeps the existing synthetic `Ref`
  feeder, `Lifted(expr)` calls `crate::node::compiler::compile`
  on the Expr with `BitFlags::empty()` flags to get a standalone
  Node. Same arg_nodes vector shape; GirNode's arg-packing
  loop is unchanged.

**Limits (today)**:
- `build_module_kernel` doesn't yet use the lifted mechanism (it
  passes an empty lifted list to `discover_region_inputs`). A
  follow-up can teach the module-kernel path to lift Async
  sub-expressions inside its synth Do.
- The test-framework wrapper `let result = {code}` always puts the
  test program inside a single top-level Bind. If that block
  contains a `<-` (Connect), `emit_do` bails on the Connect — the
  kernel build fails and the region stays unfused, even if the
  rest of the block is fusion-friendly. So the differential test
  `cross_mode_maximal_unstable_ref` verifies behavioral equivalence
  (all four fusion modes agree) but doesn't itself trigger a
  splice. The unit test `fusion::tests::region_kernel_lifted_input`
  directly constructs a region with a `Lifted` input and checks
  the kernel builds correctly with the synth-name slot — this is
  the deterministic verification of the maximal-fusion machinery.
- Async-Apply lifting works at the IR/kernel level (the lifted
  intercept fires on ANY ExprId in the lifted set, regardless of
  what kind of expression it is), but the existing test programs
  that COULD exercise it would need to be structured without
  Connect or other unfusable constructs.

152/152 compiler + 674/674 integration all green.

### FusedRegion JIT path + module-kernel lifting (May 2026)

Two paired follow-ups after maximal-fusion landed:

**FusedRegion JIT.** Region kernels (everything that comes out of
`analyze_program` or `build_module_kernel`) used to always dispatch
through `gir_interp` — only lambda kernels got JIT. Added `jit:
Option<SArc<WrappedKernel>>` + `async_jit: Option<SArc<AsyncJitSlot>>`
to both `FusedSubgraph` and `ModuleKernel`. New private helper
`jit_compile_for_kernel(&FusionConfig, kernel, callees, label)`
factors the eligibility gates + sync/async dispatch (mirrors the
inline block in `eager_fuse_lambdas`). Region/module-kernel build
sites call it right after `build_registry` and stash the artifacts
on the carrier. `FusedRegion::from_subgraph` and
`from_module_kernel` cascade through `GirNode::with_jit` → `with_async_jit`
→ `new`, same precedence as `Lambda::compile` uses for lazy lambdas.

Took `&FusionConfig` instead of `&ExecCtx<R, E>` so the helper is
testable without a full runtime. Two unit tests
(`region_kernel_jit_attaches_under_sync_mode`,
`region_kernel_jit_skipped_under_off_mode`) drive the helper end-to-
end with `JitMode::Sync` / `JitMode::Off` and assert the artifact
shape. `FusedSubgraph`'s `Debug` derive dropped — `WrappedKernel`
doesn't implement `Debug`, and nothing was actually using
`{:?}` on a `FusedSubgraph`.

**Module-kernel lifting.** `build_module_kernel` previously passed
an empty lifted list to `discover_region_inputs`, so any top-level
Bind whose value touched an Async edge (timer, subscribe, unstable
Ref read) silently failed to lower — no maximal-fusion benefit on
the file-load path. Now it computes `effects` via
`program_effect_map(&synth_do, ctx, ...)` and walks via a new
helper `collect_lifted_async(root, effects, lifted)` — the
minimal-version of `collect_region`'s walk that only collects
lifted candidates (no members, no apply_count). The lifted list
threads into `discover_region_inputs` exactly the way analyze-
program's path does, and `FusedRegion::from_module_kernel`
compiles each lifted Expr as a standalone Node feeder via the
existing `RegionInputSource` dispatch.

154/154 compiler + 674/674 integration all green.

### JIT GirType::String (May 2026)

The JIT used to bail on any string-touching kernel via a coarse
`kernel_contains_string` gate, routing string-producing lambdas
(typical `StringInterpolate` patterns) through `gir_interp`.
With strings being a major use case, that gate was a real perf
miss. Now lowered end-to-end.

**Wire shape**: a `GirType::String` SSA value is a single `i64`
CLIF value holding the ArcStr's thin pointer. `arcstr::ArcStr` is
`#[repr(transparent)]` over `NonNull<ThinInner>` — the raw u64
**is** a valid ArcStr bit pattern. Cheaper than the Value
two-register shape used for Variant/Nullable, and string SSA
doesn't need a discriminant.

**Helpers** (`gir_jit_helpers.rs`):
- `graphix_arcstr_clone_from_static(*const ArcStr) -> ArcStr` —
  refcount-bumps an interned static (kernel-strings table slot).
  Lowering for `GirOp::ConstStr`.
- `graphix_arcstr_drop(ArcStr)` — refcount decrement; the leaf
  drop. Not called in today's codegen (ConstStr/Concat are linear,
  no String locals to scope-drop), but exposed for completeness.
- `graphix_string_buf_{new,drop,finalize}` — heap-owned `Box<String>`
  buffer for `GirOp::Concat`. `finalize` consumes the box, returns
  the `ArcStr::from(s.as_str())`.
- `graphix_string_buf_push_arcstr(buf, ArcStr)` — append the
  ArcStr's str slice, consume the ArcStr.
- `graphix_string_buf_push_<T>` for every `PrimType` (i64/u64/i32/
  u32/i16/u16/i8/u8/f64/f32/bool) — format-Display the prim into
  the buf. Matches the interp's `Value::<T>(v).to_string()` output
  because netidx `Value`'s Display delegates to the inner type.
- `graphix_value_new_string(ArcStr) -> Value` — boundary helper
  the wrapper *could* call when wrapping into a Value::String. The
  current implementation transmutes the raw u64 back into ArcStr
  inside `GirNode::update` directly (single-instruction decode);
  the helper is registered for completeness.

**Codegen** (`gir_jit.rs`):
- `GirOp::ConstStr(s)` — fetches the interned `*const ArcStr` from
  `ctx.strings.get(s)` (the existing `KernelStrings` table already
  pre-walked these), emits the ptr as an `iconst`, calls
  `graphix_arcstr_clone_from_static`. Returns the cloned ArcStr's
  pointer as a single CLIF `i64`.
- `GirOp::Concat(parts)` — new `compile_concat` helper: call
  `buf_new`, dispatch each part by type (String → push_arcstr,
  Prim → push_<T>), call `buf_finalize`. Linear ownership: buf
  flows directly from new to finalize, no scope tracking needed
  (Concat parts can't contain DynCalls today, so no pending-path
  buf cleanup either).
- **Kernel-return marshaling**: `GirType::String` return adds a
  single `I64` to the typed sig (was an `Err`). `GirNode::update`'s
  return-decode arm transmutes `out[0]: u64` back into
  `arcstr::ArcStr` (relies on the `repr(transparent)` layout) and
  wraps in `EvalResult::String`. Pending-path sentinel is `0` (null
  pointer; never decoded because the wrapper's pending check fires
  first).
- **JIT-eligibility gate**: `has_strings` removed from both
  `jit_compile_for_kernel` (region/module-kernel path) and
  `eager_fuse_lambdas` (per-lambda path). String kernels now
  attempt JIT and fall back to interp only if the JIT compile
  itself errors.

**Limits (today)**:
- String locals / params still not supported on either backend
  (interp's `JitEnv::push_local` panics; GIR-build rejects). The
  JIT codegen's `Let` arms return defensive errors that read
  "should reject in GIR-build" rather than panic, so a future
  GIR change that forgot to update both backends would fail
  gracefully (kernel falls back to interp).
- Strings in composite slots (`Array<String>`, `Tuple<[String,
  _]>`, etc.) aren't expressible at the GIR layer — leaf-type
  treatment matches `Unit`/`Null`.

**Differential coverage**:
`cross_mode_maximal_unstable_ref` gained two string fixtures —
`string_jit_interpolate` (`"sum is [x + y]"` with two i64 args)
and `string_jit_multi_part` (`"a=[a] b=[b]"` with two f64 args).
All four fusion modes agree on the produced `Value::String`,
proving the JIT String ABI round-trips correctly under sync-JIT
(mode C) and whole-graph+JIT (mode D).

`kernel_contains_string` and its `stmt_has_string` / `expr_has_string`
helpers are deleted — the gate was the function's only user.

154/154 compiler + 674/674 integration all green.

### String locals (May 2026)

Continuation of the JIT-String work — closes the "small refactor
loses fusion" failure mode. The user's reasoning (worth keeping in
mind generally): JavaScript's reputation for unpredictable perf
comes from optimization gaps that turn trivial edits into fusion
cliffs. Graphix is semantically positioned to avoid that, but only
if we push on every corner case. Extracting a string into a `let`
binding shouldn't move the lambda from fused → GXLambda.

Previously: a `let s = "..."` inside a fusion-candidate lambda
body forced fallback to `GXLambda`. Both backends panicked at
`push_local` for `EvalResult::String`, and `register_kir_binding`
returned None for `GirType::String`. So this code didn't fuse:

```graphix
let f = |x: i64| -> string {
    let prefix = "x = "
    "[prefix][x]"
}
```

**Now**: string locals are first-class on both backends, parallel
to variant/nullable infrastructure.

- **`gir::StringInput`** — new slot record (just `name` +
  `bind_id`). No `elem` field — String is a leaf type.
- **`FusionCtx::string_inputs`** + `find_string` + `lookup_local`
  String arm. `register_kir_binding`'s String arm routes here
  (was a bail).
- **`InterpEnv::strings: Vec<(ArcStr, ArcStr)>`** with `push_string`
  + `lookup_string`. `push_local`'s String arm uses
  `push_string` (was a panic). `GirOp::Local` for a `GirType::
  String`-typed Ref reads the slot and clones (refcount bump) so
  each consumer gets its own owned ArcStr. `GirOp::Block`'s
  scope-exit snapshot/truncate includes `strings` alongside
  arrays/variants/nullables.
- **`JitEnv::strings: Vec<(ArcStr, Variable)>`** with `bind_string`
  + `lookup_string`. `EnvMark` gains a `strings: usize` field;
  `mark` / `truncate` cover it. `GirStmt::Let` + `GirOp::Block`'s
  let arms for String type bind into this slot (were errors);
  `compile_scalar_impl`'s `GirOp::Local` String arm reads via a
  new `graphix_arcstr_clone(s: ArcStr) -> ArcStr` helper (takes
  by value, `mem::forget`s the input, returns a fresh clone).
- **Scope-exit drops**: `drop_owned_composites` (function exit)
  and `GirOp::Block`'s scope-exit loop both drop `env.strings`
  via `graphix_arcstr_drop` alongside composites / variants /
  nullables. `emit_pending_cleanup` inherits the drops via
  `drop_owned_composites`.
- **`ensure_owned_composite` String pass-through**: strings are
  always owned at the SSA-value level (ConstStr clones from
  interned static, Concat finalize allocates, Local-read clones
  via the new helper), so `ensure_owned_composite` returns the
  SSA value unchanged for String — same fast-path treatment as
  scalar types.

**Differential coverage**: two new fixtures in
`cross_mode_maximal_unstable_ref`:
- `string_jit_local_used` — `let prefix = "x = "; "[prefix][x]"`.
  Exercises the bind + single read path.
- `string_jit_local_used_twice` — `let unit = "px"; "[x][unit]
  (was [x][unit])"`. Exercises the refcount-clone path of the JIT
  Local read (each `[unit]` site clones from the slot; the slot
  keeps its own ref until block-exit drops it).

All four fusion modes agree on the same `Value::String` output —
proof that string locals round-trip cleanly through both
backends without leaks or double-frees.

154/154 compiler + 674/674 integration all green.

### Anonymous-lambda eager fusion (May 2026)

Same predictable-perf reasoning as the string-locals work
([[feedback-predictable-fusion]]): an anonymous lambda
(`(|x| x + 1)(42)`, `array::map(xs, |x| x * 2)`, etc.) should
get the same JIT path as a named lambda — the user shouldn't
discover that *naming* their lambda was the optimization.

Previously only `Bind::compile` populated `ctx.fusion_lambdas`,
keyed by the binding name. Anonymous lambdas had no entry, so
`eager_fuse_lambdas` had nothing to build a kernel for, and
`Lambda::compile`'s InitFn fell back to `GXLambda`.

Now `Lambda::compile` registers anonymous lambdas itself when
`ctx.current_binding_name` is `None`. The synth_name is the same
`gir_<id>` already used as the InitFn's fusion-lookup key, so
both sides agree. The minted `BindId` is fresh (never reachable
by name → never in `unstable_bindings` → entry always fusable).

Mechanical: the new code is ~30 lines inside `Lambda::compile`
that builds a `FusionLazyEntry` with the same shape Bind::compile
uses, pulling the `Arc<LambdaExpr>` out of `spec.kind`. The
caller's `match`-on-`current_binding_name`-to-pick-synth_name
already partitions the named/anonymous cases; the registration
just hooks into the anonymous branch.

**What this actually unlocks**: HOF-inlined call sites
(`emit_array_map`, `emit_array_fold`, etc.) already inline the
anonymous lambda's body directly into the surrounding kernel —
they were fast pre-fix. The cliff this fix closes is anonymous
lambdas dispatched *dynamically* — through `GirOp::DynCall`,
through stored-in-binding-and-called-later patterns, or anywhere
the lambda value flows through `Apply<R, E>::init` instead of
being inlined at the call site. Those used to fall through to
GXLambda; now they go through the same JIT path as named
lambdas.

**Differential coverage**: `anon_lambda_hof_args` exercises a
program with two anonymous lambdas (`array::init`'s callback +
`array::fold`'s callback). All four modes agree on `I64(9900)`.

154/154 compiler + 674/674 integration all green.

### Composite-element lowering in producer ops (May 2026)

Phase 6 of the earlier refactor widened the *read* side of
composite-element accessors (`TupleGet` / `StructGet` / `ArrayGet`
with composite slot types). The *write* side — the producer ops
`TupleNew` / `StructNew` / `VariantNew` — was still primitive-only:
`emit_tuple_new` etc. called `e.typ.as_prim()?` on each field and
bailed for any non-primitive `GirType`. That left a real fusion
cliff — extracting an inner tuple into a let, then re-tupling it
on the way out, lost fusion for no semantically meaningful reason.

Now any `GirType` (Prim, Array, Tuple, Struct, Variant, Nullable,
String) is a valid field shape. Unit and bare Null still reject
(neither has a useful Value runtime representation; bare Null is
always widened to `Nullable<T>` at the construction site).

**GIR**: `GirOp::TupleNew::elem_types`, `StructNew::sorted_types`,
`VariantNew::payload_types` widened from `Vec<PrimType>` to
`Vec<GirType>` (and `Vec<(ArcStr, GirType)>` for Struct).

**Emit side** (`fusion.rs`): the three `emit_*_new` functions drop
their `as_prim()?` bails. Each field's GirType is recorded directly
into the corresponding GirOp; the outer `GirExpr.typ` is built from
the same vector.

**Interpreter** (`gir_interp.rs`): the three producer-op arms drop
the `into_scalar()` / `to_value()` chain and use `into_value()`
instead — which already handles every `EvalResult` variant
(scalar → primitive Value, ValArray → Value::Array, Variant/
Nullable → the inner Value, String → Value::String, Null →
Value::Null). The result is the same `Value::Array(...)` shape
the runtime already uses for nested composites; consumer ops
(`TupleGet` / `StructGet` etc., already-widened) read it back
out.

**JIT** (`gir_jit.rs`): new shared helper `compile_and_push_field`
that dispatches the per-field push-helper choice on `field.typ`:
- **Prim**: `graphix_value_buf_push_<T>` (unchanged).
- **Array/Tuple/Struct**: `graphix_value_buf_push_array` (owned)
  or `_borrowed` (refcount-bumped), picked by
  `classify_composite_source`.
- **Variant/Nullable**: `graphix_value_buf_push_value` /
  `_borrowed` — the Value-shape `(disc, payload)` pair, compiled
  via `compile_value_expr`.
- **String**: `graphix_value_buf_push_arcstr` (String SSA is
  always owned, per `ensure_owned_composite`'s String pass-
  through).

All three producer-op arms (`GirOp::TupleNew`, `StructNew`,
`VariantNew`) call into this helper instead of the prim-only
`value_buf_push_helper` dispatch. The arg-marshalling code at
`GirOp::DynCall` (which already had the right dispatch inline)
stays as it was — same logic, different inline.

**Differential coverage**: two new fixtures in
`cross_mode_maximal_unstable_ref`:
- `nested_tuple_producer`: `((a, b), c)` produces a nested-tuple
  composite; the outer + inner fields are accessed via the
  already-widened read side.
- `struct_with_tuple_field`: `{pair: (x, y), sum: ...}` produces a
  struct whose `pair` field is a tuple.

All four modes (no fusion / interp fusion / sync JIT / whole-graph
JIT) agree, proving the producer-op widening round-trips through
both backends with correct ownership for nested composite slots.

154/154 compiler + 674/674 integration all green.

### Three-mode test infrastructure + per-context JIT (May 2026)

Six-phase landing rebuilding the test infrastructure so every
fixture provably exercises every dispatch path — and so that JIT
correctness gets surfaced loudly when something's broken, not
silently masked by interp fallback. Driven by the post-landings
audit's "tests pass but don't prove the JIT ran" finding plus the
user's directive that predictable performance requires the test
matrix itself to be predictable.

**Phase 0 — Per-context Jit (foundational).** The old `SHARED_JIT`
process-global `LazyLock<Mutex<SharedJit>>` violated the runtime's
documented "concurrent in-process runtimes can hold different
modes independently — no shared global state" guarantee. Cargo's
parallel test runner with multiple modes per fixture would race
on the shared module. **Migration**: rename `SharedJit` → `Jit`
(no longer shared); `ExecCtx` gains `pub jit: parking_lot::Mutex<Jit>`;
`compile_kernel_with_callees` takes `&mut Jit` instead of locking
the static. The `Mutex` (no `Arc`) is interior mutability solely
for the `Sync` bound the async runtime puts on `ExecCtx`; no
shared ownership. Async-JIT worker stays process-global since
it doesn't touch shared state. WrappedKernels in the per-context
Jit get `None` for `_ctx` — the per-context module keeps them
mapped, dropping when the ExecCtx drops (memory-lifetime
improvement over the never-dropped SHARED_JIT).

**Phase 1 — `fusion_disabled` actually disables fusion.** The
field was defined since M5 but never read anywhere in the compiler
(`grep` confirmed zero use sites — only struct def + Default +
two test sites). Mode A of the old 4-mode differential set it
`true` to claim "ground truth via GXLambda" but actually ran the
same code path as Mode B. Wired: `Lambda::compile`'s InitFn skips
the `fusion_lambdas` cache lookup; `eager_fuse_lambdas` returns
early; `maybe_splice_fused_regions` and `build_module_kernel`
calls in `graphix-rt/src/gx.rs` are gated.

**Phase 2 — `JitMode::Forced`.** New enum variant. Behaves like
`Sync` (eager JIT compile) but the soft-failure paths panic
instead of silently falling back. Two panic sites:
- `jit_compile_for_kernel` / `eager_fuse_lambdas`: cranelift
  codegen failure panics with the kernel name + reason.
- `has_composite_elems` gate: under `Forced` this kernel-can't-JIT
  gate is itself a cliff — panics with "fix the cliff or migrate
  to `run_no_jit!`".

**Phase 3 — `JIT_INVOCATIONS` counter.** `cfg(debug_assertions)`-
gated thread-local `Cell<u64>` in `gir_jit_helpers.rs`. JIT
codegen emits a `call graphix_record_jit_invocation` at the start
of every wrapper function (also `cfg(debug_assertions)`-gated).
Release builds skip both helper registration and the codegen
call site — zero production overhead. Test helpers
`jit_invocations()` and `reset_jit_invocations()` for the
harness. Direct unit test
`jit_invocation_counter_bumps_on_each_call` verifies the counter
hits the expected value after N wrapper invocations.

**Phase 4 — three-mode `run!` macro + `run_no_jit!` escape
hatch.** `stdlib/graphix-package-core/src/testing.rs`: `run!`
expands a fixture into a child `mod $name { … }` containing
three `#[tokio::test]` functions — `interp` (Mode 1: fusion
disabled), `fused` (Mode 2: jit_mode=Off), `jit` (Mode 3:
jit_mode=Forced + assert JIT_INVOCATIONS > 0). The `jit` arm
is `cfg(debug_assertions)`-gated to match the counter's
availability. `run_no_jit!` is the same but skips the `jit`
test — for fixtures that legitimately can't JIT (async builtins,
network IO, language-feature tests with no fusable structure).

**Phase 5 — differential harness collapsed to three modes.**
`assert_modes_agree` now runs Mode 1/2/3 (was A/B/C/D with the
broken Mode A). Asserts `JIT_INVOCATIONS > 0` for Mode 3.
`assert_modes_agree_no_jit` mirrors `run_no_jit!`. Three
existing test functions migrated to `_no_jit` after Mode 3
surfaced cliffs: `cross_mode_null_handling` (Nullable JIT
codegen bug), `cross_mode_maximal_unstable_ref` (Connect in
emit_do), `cross_mode_producer_ops::dyncall_scalar_hof_binding`
(non-fusable callee). Each tracked as a follow-up task.

**Phase 6 — lib_tests/* bulk migration.** Tripling the ~350
lib_tests fixtures revealed that ~87% don't naturally exercise
JIT — they test language features (parsing, type errors, array
indexing, struct literals) where no lambda is invoked. Bulk-
migrated all lib_tests `run!` calls to `run_no_jit!` via sed.
Pre-existing TCP test design (hardcoded ports) flakes under the
3× parallel expansion; tracked as a follow-up. All other 1758
test instances pass under three-mode expansion.

**Total test count**: ~830 → ~1760 (every fixture now produces
1-3 tests depending on macro variant). Compiler: 156/156.
Integration: 1758/1760 (2 flaky TCP under default parallelism;
deterministic under `--test-threads=1`).

**Follow-ups tracked**:
- #79: Nullable Value-shape JIT codegen bug surfaced by Forced.
- #80: Connect in emit_do bails kernel build.
- #81: DynCall binding-source kernels don't JIT-execute.
- #82: TCP fixed-port flake under 3-mode expansion.

The infrastructure is now sound: any future fusion regression that
makes a previously-JIT'd kernel fall back to interp will fail
loudly under Mode 3. Tests that legitimately can't JIT are
explicitly marked `run_no_jit!` — no silent fallback masking.

### Post-landings audit fixes (May 2026)

Three-agent code review of the May 2026 fusion/JIT landings
(fusion-cliff audit, JIT/ABI/lifetime audit, test-coverage audit)
found four real issues fixed in this round:

1. **String pending-leak in `GirStmt::Return`** (`gir_jit.rs:2071`).
   The `_` arm grouped `Prim | Unit | String` together with a
   comment claiming "no owned allocation to leak." Untrue for
   String — `ConstStr` / `Concat` / `Local`-read of String all
   produce an *owned* `ArcStr` (refcount bumped). If an earlier
   scalar `GirOp::DynCall` set `DYNCALL_PENDING`, the kernel runs
   to completion and returns the ArcStr pointer; `GirNode::update`
   sees pending, returns `None` without transmuting — the bumped
   refcount leaks. One leak per pended invocation.
   Mirrors the composite/Value bug fixed previously
   ([[pending-path correctness]]). Fix: new
   `ReturnDropShape::String(ClifValue)` variant; `GirStmt::Return`'s
   `GirType::String` arm calls `emit_return_pending_check` which
   peek-checks `DYNCALL_PENDING`, drops the just-built ArcStr via
   `graphix_arcstr_drop`, runs `emit_pending_cleanup`, jumps to
   `pending_exit`. JIT-direct regression test
   `return_pending_check_drops_string_result` verifies the
   sentinel-0 return + flag-stays-set contract.

2. **TailCall non-slot drops** (`gir_jit.rs:2213`). `GirStmt::TailCall`
   emitted drops for *slot rebinds* (composite slot kind) but only
   called `env.truncate(ctx.param_mark)` for everything else —
   compile-time-only hygiene with no runtime drops. Any top-level
   `GirStmt::Let` between `param_mark` and the `TailCall` (not in
   a `Block` whose exit drops, not in `tail_call_slots`) leaked
   per iteration. Fix: before the truncate, iterate
   `env.{composites,variants,nullables,strings}[ctx.param_mark.*..]`,
   skip any entry whose name matches a `tail_call_slots` slot (its
   old value is already dropped above), emit
   `graphix_valarray_drop` / `graphix_value_drop` /
   `graphix_arcstr_drop` for the rest.

3. **Latent Nullable panic in `extract_composite_or_scalar`**
   (`gir_interp.rs:1378`). A composite slot with element type
   `GirType::Nullable(_)` panicked at runtime ("not yet supported
   in interp"). The composite-element accessor read paths
   (`TupleGet` / `StructGet` / `ArrayGet`) already accepted any
   GirType slot via Phase 6 of the refactor, so a fused
   `let (n: i64, m: [i64,null]) = pair; m` would build the GIR
   and then explode on the `.1` access. Fix: handle Nullable
   directly via `EvalResult::Nullable(arr[idx].clone())` (matches
   the shape used elsewhere — `GirOp::Local` for Nullable,
   `DynCall` Nullable return decode). Bare `GirType::Null`
   continues to panic since fusion always widens to
   `Nullable<T>` at construction; the panic message updated to
   reflect that.

4. **Type-predicate soundness in select arms** (`fusion.rs:1390`).
   `emit_type_predicate_cond` only emitted runtime conditions for
   `null`-narrowing predicates; every other type predicate
   returned `Some(None)` (treated as "always matches"). For a
   2-arm `Nullable<T>` select that's fine — the null arm filters,
   the non-null arm covers the rest. But for wider unions like
   `[i64, null, string]` or even `[i64, string]` without null,
   the first non-null arm would silently match every input
   regardless of the actual runtime tag — silent incorrect
   output. Fix: extend recognition to two more sound cases —
   (a) Prim scrutinee whose type matches the predicate's
   primitive (trivially always-true), (b) Nullable scrutinee with
   a non-null predicate (sound by exhaustiveness with the
   surrounding null arm). Anything else returns outer `None`,
   refusing to fuse the select so it falls back to the
   interpreter's runtime-type dispatch.

The audit also confirmed clean axes (no real bugs) on:
- String refcount discipline at producer/consumer sites.
- Composite-producer Owned vs Borrowed classification.
- String boundary return pending-check ordering.
- ABI signatures (push_value, value_clone, arcstr helpers).
- Composite locals containing strings (safe via interp fallback).

Remaining fusion cliffs identified but not fixed (substantial work
each; tracked for follow-up):
- **Composite-element arrays** — `array::map(arr_of_tuples, |t| ...)`
  still bails because `ArrayInit/Map/Filter/Fold` are scalar-element
  only on both sides. The largest user-visible cliff.
- **`emit_expr` missing arms** — `StructWith` (`{s with field: x}`),
  `Array` literal `[a, b, c]`, `Sample`, `Deref`, `ByRef`, `TryCatch`,
  `Map` (map literal). Each falls through to `_ => None`.
- **Composite variant payloads** — `GirOp::VariantPayload.elem_typ:
  PrimType` should be `GirType` (mirror of the producer-side
  widening from May 2026).
- **Composite-pattern select** — `select pair { (0, y) => y, … }`
  bails (`emit_arm_condition` only handles scalar binds and tag
  matches).
- **`RegionInputKind` String variant** — outer string bindings can't
  flow into a fused region (intra-kernel `let s = "..."` works, but
  free-var `Ref(s)` doesn't).

155/155 compiler + 674/674 integration all green.

### BindId-keyed unstable bindings (May 2026)

`ctx.unstable_bindings` was a flat `BTreeSet<ArcStr>` of names that
appeared as `<-` (Connect) targets. The name-only keying lost scope
information: an inner `let x` that shadowed an outer Connect-targeted
`x` was conservatively treated as unstable, blocking fusion of the
inner binding's uses for no good reason.

**Now**: `unstable_bindings: nohash::IntSet<BindId>`. `Connect::compile`
inserts the resolved BindId of its target (which already runs through
`env.lookup_bind` for the same purpose) — so the set records the
*specific* BindIds being written, not the names. Each inner shadow
has its own BindId and stays stable.

`scan_connect_targets` (a pre-compile AST walker that built the
name-based set) is gone — `Connect::compile` populates lazily during
the compile pass, so the three callers in `gx.rs` just clear the
set at batch entry and let compile do the rest.

Check sites that have a `Scope` (or `FusionLazyEntry`) at hand resolve
the name to a BindId before checking:

- `Bind::compile`'s lambda-registration into `fusion_lambdas` no longer
  gates on stability (Connect::compile may not have run yet at this
  point — compile order is source order). Stability is consulted at
  *use* time. `FusionLazyEntry` gained a `bind_id: BindId` field, set
  from the compiled `StructPatternNode`'s single id, so users have it.
- `resolve_binding_fn_input` looks up `bind_id` via `env.lookup_bind`,
  then checks `unstable_bindings.contains(&bind_id)`.
- `discover_builtin_fn_inputs` looks up the matching `FusionLazyEntry`
  and checks `entry.bind_id`.
- `build_module_kernel`'s top-level filter checks `entry.bind_id` too.

`program_effect_map` doesn't have ready scope info per Ref site, so
it uses a lightweight scope-tracking walk: tracks a `Vec<ArcStr>`
stack of `let`-introduced names; pushes on Bind, snapshots/truncates
on Do entry/exit. A Ref whose name is on the stack resolves to a
locally-introduced binding and is classified Sync regardless of
whether the same name is a Connect target at the outer scope. Refs
not in the local stack fall back to `env.lookup_bind` from
program-root and check the resolved BindId — conservative (might
misclassify a Ref to an inner-but-out-of-this-walk shadow), never
unsafe.

Differential test `cross_mode_maximal_unstable_ref` gained a
shadowing fixture: an outer `let x = 100; x <- 0` followed by an
inner `{ let x = 7; (x*x)-(x+x)+(x/2) }` — all four fusion modes
agree on `I64(38)`, proving the inner-x classification doesn't
regress correctness.

154/154 compiler + 674/674 integration all green.

### F64 DynCall bitcast endianness fix (May 2026)

`cast_u64_to_prim` (decode of `dispatch_typed`'s scalar return) and
`pack_reg_to_u64` (encode of float args to the kernel ABI) both used
`MemFlags::trusted()` for their bitcast instructions. `trusted()` sets
`notrap + aligned` but NOT endianness — and cranelift's verifier
rejects a `bitcast` whose memory flags don't carry `big` or `little`.
The path stayed dormant until builtin DynCall coverage exposed an F64
return (`math::sin(f64:0.0)` etc.); both bitcast sites in the F64/F32
arms then started erroring with "The bitcast instruction only accepts
the `big` or `little` memory flags". One failed compile poisoned the
shared cranelift `func_ctx` for every subsequent kernel in the same
process, so the diagnostic cascaded across unrelated kernels.

Fix: switch both sites to `MemFlags::new().with_endianness(Endianness
::Little)` (host endianness on x86_64 / ARM; no big-endian target is
supported today). `trusted()` is also incompatible because it sets
`aligned`, which only applies to memory loads/stores — not to a
register-level bitcast.

Coverage: `lib_tests/math.rs` (48 tests; 16 fixtures × 3 modes)
migrated from `run_no_jit!` back to `run!` and all `::jit` arms pass.
`lib_tests/bitwise.rs` (36 tests; 12 fixtures × 3 modes) similarly
migrated. Confirms the new sync-builtin DynCall path works end-to-end
across the F64 math builtins and the u8 bitwise builtins.

`shuffle_array` / `shuffle_empty` stay `run_no_jit!` (array literal
arg `[1, 2, 3]` not yet handled by `emit_expr`). All re-package tests
(`stdlib/graphix-package-re/src/test.rs`) migrated to `run_no_jit!` —
all of `re::*` returns `Result<T, ReError(string)>` (a Set type),
which `PrimType::from_type` rejects, so the outer Region kernel can't
form. The Apply itself could fuse via the new DynCall path once
String args are JIT-supported (currently filtered by
`is_dyncall_supported`).

### Typechecker propagates labeled-default types into call-site TVars (May 2026)

`rand_float_default` (`rand::rand(#clock:1)`) failed JIT mode despite
the bitcast fix — the call site's return type was an unresolved TVar
with constraint `[Int, Float]`. Without explicit `#start`/`#end`,
no call-site arg refined 'a, so typecheck left the apply's return
type ambiguous. The runtime later evaluated the f64 defaults and
picked f64, but that runtime decision didn't feed back into
compile-time inference.

Root cause was in `CallSite::typecheck_inner`. When a labeled arg
with a default was missing from the call site, the code at line ~451
inserted a `Nop::new(arg.typ.clone())` placeholder — a Nop carrying
the formal arg's *unresolved TVar*. The subsequent type check at
line 506 unified `farg.typ` with the Nop's `n.typ()` — which was
literally the same TVar — a trivial no-op. The default expression's
concrete type never reached 'a.

Fix is in two parts:

1. **Always insert the lambda's ID into `FnType.lambda_ids`**
   (`lambda.rs:394`, was previously gated on `needs_callsite ||
   lsp_mode`). The gate was a performance opt — non-callsite-needing
   lambdas don't need the deferred HOF refinement check, so why
   bother recording. But the ID is also the only handle from
   `FnType` back to `LambdaDef.argspec` (where defaults live).
   Recording always is cheap (single-element IntSet entry) and
   the deferred check at `callsite.rs:670` is a no-op when
   `ldef.check` is None (only populated when needs_callsite).

2. **Walk the lambda's argspec AFTER the main args typecheck loop**
   (`callsite.rs` immediately before the vargs typecheck), find
   missing-default args, look up the default expression's resolved
   type, unify into the formal arg's TVar. The ordering matters:
   explicit positional and labeled args refine the TVar FIRST. If
   `value: &"option_a"` set 'a=string, then later seeing a sibling
   `#selected: &'a = &null` default — whose type tries to set
   'a=null — should surface as a *real* unification error (which
   `farg.typ.check_contains(&ctx.env, &dt)` does), not be silently
   masked by being applied before the explicit args.

   For a callee backed by a single LambdaDef (the common case for
   stdlib direct calls), `ftype.lambda_ids` has one entry; we look
   up `ctx.lambda_defs[id]`, downcast to `LambdaDef`, scan argspec
   for the entry whose pattern's single-bind matches the missing
   arg's `name`, pull its default `Expr` and read `default.typ.get()`
   — the OnceLock is populated when the lambda's own typecheck
   compiles the default into `faux_args[i]` at `lambda.rs:496`.
   Multi-lambda call sites (`if cond then foo else bar`) skip the
   refinement; their defaults aren't necessarily uniform and
   soundness would require checking all match.

After this, `apply_expr.typ.get()` returns the concrete monomorphized
type post-typecheck (`Primitive(F64)` for `rand::rand(#clock:1)`),
and fusion lowers normally — no special-case fallback in
`try_register_builtin_call`. The radio widget (which has
`#selected: &'a = &null` alongside positional `value: &'a`) still
typechecks correctly because we unify defaults AFTER explicit args:
the positional `value` sets 'a, the default's `&null` then fails
the containment check against the now-bound 'a — but only when 'a's
actual value is incompatible with null, which is the correct error.

`rand_float_default::jit` and `rand_float_range::jit` both pass.

### String args/return in JIT DynCall (May 2026)

Lifted the String filter in `is_dyncall_supported` so sync builtins
with string args (most of `str::*`) JIT end-to-end. Three changes:

1. **`graphix_value_buf_push_string` helper** — takes `(buf,
   arcstr::ArcStr)`. ArcStr is `repr(transparent)` over a thin
   pointer so it passes as one I64. The helper wraps in
   `Value::String` and pushes (consumes the ArcStr).
2. **`dispatch_typed` ret_kind=4** — for `GirType::String` return,
   extracts ArcStr from `Value::String(s)`, `ManuallyDrop`s it,
   `transmute_copy<ArcStr, u64>` to bits, returns in `word0`.
   Caller's SSA reads the bits directly as an owned `arcstr::ArcStr`
   (no boundary decode needed — same pattern as String kernel
   return).
3. **`ensure_declared` shape match** — the shared-module pre-pass
   was prim-only (`return_type.as_prim()`); widened to mirror
   `compile_kernel_with_wrapper`'s full match (composite pointers,
   Value-shape pair, Unit/String single-I64). Without this, any
   kernel with a non-prim return signature failed at "JIT does not
   support array return types" before its body was ever compiled.

Also widened the `fuse()` Region splice's return-type filter
(`fusion/mod.rs`) from prim-only to allow Array/Tuple/Struct/
Variant/Nullable/String; only `Unit` and bare `Null` skip.

Test coverage: `lib_tests/str_tests` migrated to `run!` — 88 of 99
fixtures pass JIT mode (the 11 remaining are real cliffs, not
String-related: `str::concat`/`str::join` use variadic args,
`str::split`/`escape` family have array literals as args or
Array<string>/Option<string> returns). The 11 stay `run_no_jit!`
until variadic args + array literals land in `emit_expr`.

The `re::*` family still stays `run_no_jit!` — their return is
`Result<T, ReError(string)>`, a Set with an Error member that
`GirType::from_type` can't lower (not a simple `[T, null]` shape
and not a uniform variant union). Different cliff, separate fix.

### Array literals, variadic args, Result-return lowering (May 2026)

Pushed on the second-wave fusion cliffs to unlock more of the
`stdlib/graphix-tests/src/lib_tests` corpus. Four pieces:

1. **Array literals (`[a, b, c]`) in fused expressions.** Added an
   `ExprKind::Array` arm to `emit_expr` that lowers via the existing
   `GirOp::TupleNew` op (runtime shape — `ValArray<Value>` — is
   identical to a tuple) but tags the outer GirExpr's `typ` as
   `GirType::Array(elem)`. Elem type comes from the literal's own
   typed-AST cell (`Type::Array(inner)`) so empty `[]` still gets a
   concrete elem. Single new `emit_array_new` helper; no parallel
   `GirOp::ArrayNew` variant to thread through every walker.

2. **Result returns (`Result<T, E>` = `[T, Error<E>]`).** Extended
   `GirType::from_type`'s `Type::Set` arm to recognize this shape
   alongside the existing `[T, null]` (Option) shape; both lower to
   `GirType::Nullable(T)`. The wire shape at the JIT boundary is
   already Value-shape two-register, so a `Value::Error(...)` flows
   through opaquely to downstream consumers that store-and-read
   (Bind → Ref). Refused at `?` (Qop) / `$` (OrNever) — those need
   real null-check codegen to honor "propagate the error" semantics,
   not yet wired up. Refusing means `let v: i64 = str::parse("x")?;`
   stays on interp while `let result = str::escape(...)` fuses.

3. **`GirNode::pre_bind_builtin` env+scope restoration.** When a
   labeled-default expression references free variables visible only
   in the lambda's original module scope (e.g. `default_escape` in
   `str::escape`'s `#esc = default_escape`), compiling it in the
   kernel's own scope produces "binding not in scope". Fix mirrors
   `CallSite::bind`'s `compile_default!` macro:
   - `FnSource::Builtin` carries an `Option<LambdaId>` capturing
     the owning lambda's id at fusion-discovery time.
   - `BuiltinBindInfo` gains the same `Option<LambdaId>`, populated
     when the binding is compiled by reading the Lambda Node's
     `def.id` through a new `Lambda::lambda_id<R, E>()` accessor
     + the existing `NodeView::Lambda` view.
   - `pre_bind_builtin` takes `lambda_id`, looks up
     `ctx.lambda_defs[id]`, and wraps the default-compile call in
     `ctx.with_restored(lambda_def.env.clone(), |ctx| …)` with
     scope `{ dynamic: kernel scope.dynamic, lexical: lambda_def.
     scope.lexical }`. Falls back to the kernel's own scope when
     `lambda_id` is None (works for pure-literal defaults).
   - Per-cycle priming: `DynCallSlot` records the external `BindId`s
     each `LabeledDefault` Node references via `node.refs()`.
     `GirNode::update` walks each slot's `default_external_refs`
     and primes `event.variables[id]` from `ctx.cached[id]` (only
     when `event.variables` doesn't already have a value, so an
     outer caller's fresh update isn't clobbered). Without the
     priming, `Ref::update`'s `event.variables.get` reads `None`
     and the inner Apply's `CachedArgs` never has a complete arg
     set — the kernel hangs producing nothing forever.

4. **`str` package `escape_fn!` macro had no `EFFECT` constant** and
   was defaulting to `Async`. Added `const EFFECT: EffectKind =
   EffectKind::Sync;` — pure string transforms have no async
   dependency. Sync is the precondition for fusion-discovery to
   register the call.

Test coverage:
- New `load_array_literal_jits` — program body `[1, 2, 3]` returns
  Array<i64>, JIT_INVOCATIONS > 0.
- New `load_variadic_and_jits` — `and(true, true, false)` variadic
  builtin with uniform `@args: bool` slot, JIT_INVOCATIONS > 0.
- `lib_tests/str_tests`: 91 of 99 fixtures pass `run!` (was 88).
  Newly-JITting: `str_escape`, `str_unescape`, `str_sub`,
  `str_split_once`, `str_rsplit_once`. The 8 still on
  `run_no_jit!`:
  - `str_concat`/`str_join` — variadic args of union type
    `[string, Array<string>]`. The discovery's variadic-slot
    builder uses `GirType::from_type(fn_type.vargs)` which rejects
    Sets that aren't Option-shape.
  - `str_split` family (6 fixtures) — fixtures wrap the call in a
    Block with `array::map(arr, |s| str::trim(s))` HOF. Needs both
    intra-kernel array-iterator lowering AND nested anonymous-
    lambda fusion. Multi-cliff; out of scope.

### Generalized variadic args — per-arg type discovery (May 2026)

The previous variadic path read `fn_type.vargs` as a single uniform
type and passed it through `GirType::from_type`. That works for
homogeneous variadics like `and: fn(@args: bool) -> bool` but rejects
union-typed varargs declared as `@args: [T, Array<T>]` (the "T or
array of T" pattern used by `str::concat`, `str::join`,
`array::concat`, `sum`, `product`, `min`/`max`, `mean`). Each
individual call-site arg is a separate `Expr` with a concrete
typed-AST cell — at fusion time we know whether THIS specific
position is a `string` or an `Array<string>`, even though the
formal accepts the union.

Fix: in `try_register_builtin_call`'s varg-collection loop, drop
the up-front `GirType::from_type(fn_type.vargs)` lookup. Iterate
the remaining positional args and resolve each from its own
`arg_expr.typ.get()` cell (falling back to the formal's deref only
for synthesized exprs without a cell). Push the per-arg `GirType`
into `arg_types`. The downstream JIT push-helper dispatch in
`GirOp::DynCall` already picks the right helper per-`arg_types[i]`
(scalar, composite-pointer, Value-shape, or String), so no JIT-
side surgery was needed — the work was entirely in discovery.

Test coverage:
- `lib_tests/str_tests`: 91 → 99 (the previously-stuck `str_concat`
  and `str_join` now JIT; all 33 fixtures × 3 modes green).
- `lib_tests/array::array_concat` migrated from `run_no_jit!` to
  `run!` (mixes `[1, 2, 3]` array literals as varg slots).
- `and`/`sum`/`product` etc. still stay `run_no_jit!` because their
  fixtures use Block-with-let bodies (`let arr = [...]; sum(arr)`)
  that need Block lowering for the binding statements — separate
  cliff from variadic.

### Qop (`?`) on Result/Nullable returns (May 2026)

Added `GirOp::QopUnwrap { inner, success_typ }` plus the surrounding
plumbing so `?` on a `Result<T, E>` lowered to `Nullable<T>` can be
fused intra-kernel.

**Codegen.** The op evaluates `inner` to a Value-shape `(disc,
payload)` pair, branches:
- If `disc == Typ::Error` (`0x2000_0000`), call
  `graphix_dyncall_set_pending`, emit cleanup, jump to
  `pending_exit`. The wrapper-level pending check at
  `GirNode::update` then returns `None` to the runtime — same path
  as a regular DynCall pend. Doesn't write the error to the catch
  handler's BindId yet (that needs runtime access from inside the
  kernel — follow-up); today it just drops, matching the
  non-fused Qop's "unhandled" path with no catch in scope.
- Otherwise extract `T` from the payload according to `success_typ`:
  Prim → `cast_u64_to_prim(payload, p)`, String → payload bits are
  the owned `ArcStr`, composite → payload is the owned
  `*mut ValArray`. Value-shape success types (Variant/Nullable)
  bail with an error (`compile_expr` should have routed to
  `compile_value_expr`; not yet wired for QopUnwrap's
  Value-shape outcomes).

**New helper.** `graphix_dyncall_set_pending()` — counterpart of
the existing `_take` that the JIT emits when a kernel-internal
error check fires; flips the same `DYNCALL_PENDING` thread-local
the dispatcher uses.

**Discovery `FnType.rtype` fix-up.** Some sync builtins (notably
`str::parse`) read `resolved.rtype` at init time to decide their
target type via `extract_cast_type`. Discovery was passing the
*function-binding's* FnType — that's the lambda's signature with
unbound polymorphic TVars (`'b` in `str::parse`'s case), so
`extract_cast_type` saw a TVar and returned None, surfacing as
"parse requires a concrete type annotation" at the first dispatch.
Fix: in `try_register_builtin_call`, construct a new FnType for
`FnSource::Builtin` with `rtype` replaced by the Apply Expr's
typ-cell value (the post-typecheck monomorphized return), with
`with_deref` applied so a TVar wrapping the resolved Set
doesn't trip up the builtin's match-on-Set inspection.

**`emit_array_map` type inference.** When the callback's parameter
isn't annotated (`array::map(arr, |x| x + 1)`), the parse-time
`constraint` is None and `emit_array_map` bailed. The typechecker
already inferred the param type from the array's element type and
stored it on the lambda Expr's typ cell — read it from there as a
fallback before giving up. Doesn't help String elements yet
(`ArrayMap.in_elem: PrimType` is still prim-only), but unblocks the
common `|x| body` shape with prim element types.

Test coverage:
- New `load_qop_unwraps_result` (`re::is_match(...)?` at top
  level — exercises QopUnwrap's success extraction).
- `str_parse` migrated to `run!` — Block-with-let was already
  supported in `emit_do_as_expr`; the missing piece was the
  rtype-fixup so the runtime's `extract_cast_type` saw `Result<i64, _>`
  instead of `Result<TVar, _>`. All 3 modes pass.
- `re::*` family migrated to `run!` (14 of 15 fixtures). `re_captures`
  stays `run_no_jit!` — its `Result<Array<Array<Option<string>>>, _>`
  return is too nested for current Nullable-of-composite codegen.

Outstanding cliffs:
- **Catch handler propagation in Qop**: `?` should write the error
  Value to the catch handler's BindId before pending. Needs runtime
  access from inside the kernel (or a side-channel via the wrapper).
- **String element types in HOFs**: `emit_array_map` /
  `emit_array_filter` / `emit_array_fold` reject non-prim element
  types. Extending requires widening `GirOp::ArrayMap.in_elem`
  (and friends) from `PrimType` to `GirType` plus matching interp
  + JIT codegen. Blocks the `str_split` family's `array::map(arr,
  |s| str::trim(s))` patterns.

### Static call resolution pass (May 2026)

New post-typecheck compiler pass `static_resolve::resolve_static_calls`
that pre-binds every CallSite whose function expression provably
resolves to a single known `LambdaDef`. Runs between the deferred-
checks fixpoint and `fusion::fuse` in `compile()`. Two payoffs:

1. **Interpreter**: every dynamic call site previously paid
   `fnode.update` + `Value` equality + optional downcast on every
   invocation, plus a one-shot `bind()` on the first invocation.
   Pre-resolving runs `bind()` once at compile time and gives every
   subsequent runtime update a fast path that skips the lazy
   resolution arm entirely.

2. **Fusion (Stage 2, not yet wired)**: with the lambda eagerly
   bound, the resulting `GXLambda` carries the lambda body as a
   concrete `Node<R, E>`. Fusion's walker can in principle descend
   through resolved CallSites straight into the body — eliminating
   today's `LambdaBind`-as-fusion-candidate special case. That
   descent requires extending NodeView (today the resolved Apply
   is stored as `Box<dyn Apply>` so the body Node isn't visible
   through `view()`) — tracked as task #109.

**Resolvability rules** (mirrored in
`static_resolve::try_resolve_callsite`):
- `fnode` is `Ref(bind_id)`, `bind_id ∉ ctx.unstable_bindings` (no
  `<-` Connect target), and `bind_id` was bound by a Bind whose
  value Node is a `Lambda`.
- `fnode` is a `Lambda` Node directly (rare: `(|x| x+1)(42)`).

The first walk collects `bind_id → LambdaDef Value` from every
Bind→Lambda pair under Module/Block/Bind containers. The second
walk descends Module/Block/Bind/CallSite via `Any`-downcast on
`&mut dyn Update`, calling `cs.resolve_static(ctx, def, fv)` at
each candidate. `Update`'s `Any` supertrait + stable trait
upcasting (Rust 1.86+/edition 2024) let the walker mutate without
adding a `view_mut`/`for_each_child_mut` method to every node.

**`CallSite::resolve_static`** mirrors `bind()` but is event-free:

1. Resolve TVars to populate `resolved_ftype` if not already done.
2. **Strip Nop placeholders** the typechecker inserted for missing
   labeled-default args (callsite.rs:typecheck_inner). `bind()`
   does the same cleanup at re-bind time — skipping it leaves a
   `Nop` in `self.args` for the default slot, and `Nop::update`
   always returns `None`, so the kernel never sees the default's
   value and the entire CallSite returns `None` forever. (This was
   the lone bug surfaced by the first integration test pass:
   `labeled_args`/`mixed_args`/`arg_name_short`/`nested_optional0`
   all timed out with one arg permanently missing from
   `event.variables`.)
3. Build `arg_refs` in function-signature order, compiling each
   labeled default that the call site omitted.
4. Call `(def.init)(...)`. For user lambdas this produces a
   `GXLambda`; for builtins, the wrapped `BuiltInLambda`.
5. Typecheck the resulting Apply with `TypecheckPhase::Lambda`.
6. Store `self.function = Some((fv, rf))`. Set
   `statically_resolved = true` and `first_static_update = true`.

The runtime `CallSite::update` gains a fast-path arm at the top
(after the standard arg-Node update loop):

- If `statically_resolved`, skip `fnode` resolution entirely (the
  `fnode` Node still updates for side effects but its produced
  value is ignored at the call-site level).
- On the first such update only (`first_static_update`), mirror
  the dynamic `bind=true` arm: force `event.init = true` and prime
  `event.variables` from `ctx.cached` for every external Ref the
  function's body reads — without this, a body that references
  outer bindings whose values fired earlier this cycle (or
  earlier) would see `None` on its first execution.
- All subsequent updates run the bare `f.update(...)` path with
  no priming, matching the dynamic `bound=false` arm.

**Tree-walker scope today**: Module, Block, Bind, CallSite. Select
arms, TryCatch handlers, arithmetic operands, Sample, etc. aren't
descended through yet — CallSites nested inside those are missed
by static resolution and fall through to the dynamic path. Adding
descent is mechanical (more `Any::downcast_mut` arms in
`visit_mut`); the limitation is intentional staging while the
architecture lands.

**Verified**: 1767/1767 graphix-tests + 156/156 graphix-compiler
unit tests pass after the landing. Includes all the failing-then-
fixed labeled-default tests above.
