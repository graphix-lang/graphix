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

> **CORRECTION (2026-06-06):** this entry and the "Module-kernel
> lifting" half of the next one describe the maximal/lifted split
> (`RegionInputSource::Lifted`, `collect_region`, `collect_lifted_async`,
> `discover_region_inputs`) as landed. A code trace found it was **never
> wired**: `RegionInputSource::Lifted` has zero constructors in the live
> tree (only the enum variant, an unreachable consumer at
> `lowering.rs:3609`, and `FusionCtx::lifted_inputs` written solely in
> that dead branch survive); the named `collect_region`/
> `collect_lifted_async`/`discover_region_inputs` functions **do not
> exist** (only `collect_region_inputs` at `mod.rs:368`, which emits
> `RegionInputSource::Binding` only). The live pipeline still uses the
> initial model — an Async edge *splits* a region, it is not lifted
> across it. (Tells were already in the entry: the differential test
> "doesn't itself trigger a splice"; only a now-dead unit test
> `region_kernel_lifted_input` exercised the path.) Building the lift —
> bidirectionally, and applied to lambda bodies for per-slot HOF
> callbacks — is the `design/impure_hof_fusion.md` milestone. Treat the
> two M8.4 entries below as *aspirational design notes*, not landed work.

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

### On-demand lambda kernel cache + closure conversion (Phases A–D, May 2026)

Lambdas now fuse end-to-end through a per-`ExecCtx` kernel cache and
cross-kernel `GirOp::Call`. Four coupled phases:

**Phase A — thread `&mut ExecCtx` through the fusion emit path.**
`emit_node` and every emit helper (`emit_known_fused_call`,
`emit_select_as_expr`, `emit_do_as_expr`, `emit_body`/`emit_tail`/
`emit_select`/`emit_arm`, `build_kernel`/`finish_kernel`/
`build_kir_kernel_from_region`, `build_region`) gained an `ec: &mut
ExecCtx<R, E>` parameter alongside `&mut FusionCtx`. The
`GirEmitter::emit_gir` trait method and the `MapFn`/`FoldFn::emit_gir`
hooks (graphix-package-core, graphix-package-array) gained it too.
`FusionCtx::with_input` became `&mut self` (push/emit/pop) so the
HOF intercepts thread the same ctx rather than cloning. Pure
plumbing — no behavior change; 1767/1767 stayed green.

**Phase B — `(LambdaId, Arc<FnType>)` kernel cache.** `ExecCtx`
gained `fusion_kernels: Mutex<BTreeMap<(LambdaId, Arc<FnType>),
CachedKernel>>`. `GXLambda` gained an `id: LambdaId` field (+
`id()` accessor). New `ensure_lambda_kernel(g, source_name, ctx,
ec)`: at a `NodeView::CallSite` whose `resolved_apply()` is
`ApplyView::Lambda(g)`, compute the cache key (`g.id()`,
`g.typ().resolve_tvars()`), look up / build the kernel, register it
in `ctx.known_fns` + `ctx.called_kernels`. `CachedKernel { fn_name,
kernel, signature, captures }`. To make `FnType` a hashable key:
manual `Hash` on `Type`/`TVar`/`TypeRef`/`FnType`/`FnArgType`/
`FnArgKind` (lock-the-RwLock pattern, like `TVar`'s existing
`PartialEq`). `FnType`'s `Hash`/`PartialEq`/`Ord` deliberately
EXCLUDE `lambda_ids` (provenance) AND `explicit_throws` — the
pretty-printer doesn't preserve `explicit_throws` through round-trip
when `throws == Bottom`, so including it breaks the parser
round-trip proptests (`expr_round_trip*`). The splice's
`FusedKernel::new` populates the interp `KernelRegistry` from
`called_kernels` (keyed by source name = `GirOp::Call.fn_name`).

**Phase C — closure conversion (captures as extra kernel args).**
`ensure_lambda_kernel` collects the lambda's formal-arg BindIds
(from `g.args()`), walks `g.body().refs()`, and treats every
external Ref NOT in the formal-arg set as a capture. Critically the
formal-arg ids must be excluded explicitly — `g.body().refs()`
reports the body's refs but the *lambda arg patterns* bind their
ids in the lambda Node, outside the body, so a body `Ref(x)` to a
formal `x` would otherwise be mis-collected as a capture (the bug
that made the first cut produce `captures=2` for `|x| x + y`).
Captures are sorted by `bind_id.inner()` (deterministic kernel
signature → stable cache entry) and appended to the kernel inputs
AFTER the formal args; each becomes a `CaptureSlot { bind_id, name,
typ }`. `emit_lambda_call` forwards each capture's CURRENT value
(live semantics, matching the GXLambda interpreter) via the new
`FusionCtx::lookup_local_by_bind_id` — BindId-keyed, NOT name-keyed,
so an outer capture isn't shadowed by a same-named parent local
(`let y=7; let f=|x| x+y; { let y=100; f(5) }` → 12, not 105). The
`NodeView::Ref` arm now prefers `lookup_local_by_bind_id(r.id)` with
name-keyed fallback. The cascade is automatic: `CallSite::refs`
recurses into a statically-resolved lambda's body via
`GXLambda::refs`, so a lambda's captures transitively surface as
external Refs of the enclosing region — `collect_region_inputs`
(was `collect_scalar_inputs`, now generalized via
`GirType::from_type` + `gir_type_to_region_input_kind`) registers
them as parent inputs with their BindId, and `lookup_local_by_bind_id`
finds them. `RegionInput` / the input slot structs carry an
`Option<BindId>` populated by `populate_kernel_inputs`. Fn-typed
externals are skipped (statically-resolved callees lower via their
own CallSite → `GirOp::Call`; the body emit bails if a fn is needed
as a value).

**Phase D — JIT cross-kernel calls.** `fuse()` builds a `callees`
map from `built.called_kernels` and passes it to
`compile_kernel_with_callees` (was `&Default::default()` — empty,
which made any `GirOp::Call`-containing kernel fail to JIT and fall
back to interp). Now scalar closures JIT end-to-end.

**SCALAR-ONLY GUARD (important correctness boundary).** The
cross-kernel `GirOp::Call` path only handles scalar args + scalar
return today, on BOTH backends:
- JIT (`gir_jit.rs` `GirOp::Call` ~3104): `compile_scalar` per arg,
  passed in POSITIONAL order; but the callee's CLIF signature
  (`ensure_declared`) groups params by KIND (scalars, then composite
  pointers, then value-shape pairs). For a `(composite, scalar)`
  arg order these diverge and a scalar gets routed into a pointer
  slot → confirmed misaligned-pointer crash (`g((10,20), 5)` deref'd
  `5` as a `*ValArray`).
- Interp (`gir_interp.rs` `GirOp::Call` ~970): `.into_scalar()`s
  every arg.
So `ensure_lambda_kernel` bails (→ correct interp fallback, no
fusion) if any formal arg, capture, or return type is non-`Prim`.
Composite/value-shape captures (e.g. a tuple capture) therefore
fall back to the interpreter — correct value, no JIT. Lifting this
guard (kind-grouped reordering + 2-register value args + arg
ownership on the JIT side, typed-slice arg routing on the interp
side) is tracked as a follow-up; it is COUPLED to making
lambda-binds-inside-fused-bodies fuse (nested-closure support),
which makes function-name shadowing reachable and forces a switch
from source-name kernel keying to unique per-`LambdaId` names (see
the SAFETY INVARIANT comment at `ensure_lambda_kernel`'s
early-return).

**Verification**: 1773/1773 graphix-tests + 125/125 graphix-compiler
unit tests. Closure smoke tests in `lang/fusion.rs`:
`closure_primitive_capture` (prim capture, asserts JIT fired),
`closure_capture_respects_shadow` (BindId-keyed correctness),
`closure_fn_external_static` (lambda calling another lambda),
`closure_nested_capture` + `closure_tuple_capture_falls_back`
(interp fallback, correct value), and
`call_arg_order_composite_then_scalar` (crash-regression guard for
the composite-arg ordering). The crash was caught by running the
code, NOT by the adversarial multi-agent review (whose ordering
dimension missed it) — empirical execution remains essential.

### Bidirectional fusion-expectation harness + fusion-state metric (May 2026)

`run_no_jit!` is gone. Every test fixture now runs through `run!`
with an explicit, **bidirectionally-checked** fusion expectation —
the test fails if observed fusion differs from the assertion (fuses
when it shouldn't, or fails to fuse/JIT when it should). This turns
the suite into a live, drift-detecting map of the fusion frontier.
Full design + the current metric live in `design/fusion_state.md`.

- **`FuseExpect::{Jit, Interp, None}`** + `check_fuse_expectation`
  in `graphix-package-core/src/testing.rs`. `run!(name, code, pred)`
  defaults to `Jit`; explicit form is a trailing `; FuseExpect::X`
  (the `;` separator made the 466-site `run_no_jit!`→`run!`
  migration a uniform "insert before the closing paren").
- **`FUSION_INVOCATIONS` counter** (`gir_jit_helpers.rs`, sibling to
  `JIT_INVOCATIONS`), bumped at `GirNode::update`'s commit point —
  counts every fused-kernel execution (JIT or interp). Together with
  `JIT_INVOCATIONS` it distinguishes Jit (`JIT>0`) / Interp
  (`FUSION>0 && JIT==0`, checked in fused mode since a jit-mode
  JIT-compile failure makes `fuse()` skip the splice) / None
  (`FUSION==0`).
- **Discovery mode**: `GRAPHIX_FUSION_DISCOVERY=1` makes the jit arm
  measure + print `FUSEMAPF`/`FUSEMAPJ` per fixture instead of
  asserting — harvests the whole-corpus map in one run.

**Real bug surfaced + fixed**: forcing the jit arm on previously-
`run_no_jit!` fixtures hit `assertion failed: func_ctx.is_empty()`
panics in cranelift (`array_sort`, `hbs`). Root cause: a kernel that
errored mid-compile left the shared `FunctionBuilderContext`
(`jit.builder_ctx`) dirty (it's only reset by `FunctionBuilder::
finalize`, skipped on the `?` error path), poisoning the *next*
kernel's `FunctionBuilder::new`. Fix: defensively reset
`jit.builder_ctx = FunctionBuilderContext::new()` (and
`clear_context` the `func_ctx`) at the start of `define_typed_kernel`
/ `define_kernel_body` / `define_wrapper`. This unblocked JIT for
`array_sort`/`hbs`/`array_indexing`/the `str_split` family — they
JIT now. (Same class as the documented F64-bitcast poison.)

**Metric** (graphix-tests, 543 `run!` fixtures): **385 Jit (71%),
1 Interp, 157 None**. Of the None, **63 are aspiration gaps** (sync
computation that should JIT but hits a cliff) — each annotated in
its test file with `// ASPIRE: Jit (currently None) — blocked on:
<cliff>`; the other 94 are genuinely-None (async/IO/error/typecheck).
Biggest gap clusters: composite cross-kernel calls (#131, ~12), map
literals/ops (10), datetime/duration arithmetic (8), nested variant
payloads (6), labeled-arg lambdas (5), assorted sync builtins
(sum/product/all/divide/filter_err). Closing a cluster = lower the
cliff, then the bidirectional check forces upgrading the annotation
to `Jit` (and deleting the ASPIRE comment). Distance to end-state:
386 of ~449 should-fuse fixtures fuse today (86%).

### Kernel ABI layout extracted to one derived source (May 2026)

Prereq refactor for #131 (composite cross-kernel calls). The
"kind-grouped" kernel calling convention — scalar params first, then
composite pointers (array/tuple/struct, one `I64` each holding a
`*ValArray`), then two-word `Value` params (variant/nullable, each
`(disc, payload)`) — used to be **hand-replicated at five sites** with
no shared definition: the two JIT signature builders
(`define_typed_kernel`, `ensure_declared` — byte-identical
copy-paste), the wrapper unpacker (`define_wrapper`), the entry binder
(`compile_into_function`, four parallel loops + a manual `next`
counter), and the runtime arg packer (`GirNode::update` in
`gir_interp`). The return-shape match was duplicated in the two sig
builders too. A drift at any one silently reinterpreted bits (a scalar
as a `*ValArray` → crash) rather than failing a type check — the same
class as the #131 cross-kernel-arg crash.

**Discussion that drove it** (worth keeping): the question wasn't
"kind-grouped vs positional." C/SysV-AMD64 *also* kind-groups via
register classes (INTEGER vs SSE banks, source order within each);
ARM AAPCS64 likewise. What makes C ABIs robust is that the layout is
**derived from the signature by one algorithm** that caller and callee
both run — not hand-replicated per site. The JVM does the same (i2c/c2i
adapter stubs generated from the signature; our `define_wrapper` is
exactly that adapter). So the fix is "derive it once," and the
grouping choice becomes a cheap, localized, reversible decision. We
kept **kind-grouped** (matches the kernel's existing entry ABI; keeps
the per-kind clone-on-entry / drop-on-exit codegen a clean per-list
walk). Foreign callers, if ever wanted, get a separate generated
C-ABI wrapper — they don't constrain this internal convention, so no
need to optimize for them now. Our per-arg wire reps are already
C-clean anyway (`Value` is `repr(u64)` 16 bytes = two SysV eightbytes;
`ValArray`/`ArcStr` are thin pointers).

**The single source** lives in `gir.rs`:
- `GirKernel::abi_params() -> impl Iterator<Item = AbiParamDesc>` —
  yields params in canonical kind-grouped order, each tagged with its
  `AbiParamKind` (Scalar/Array/Tuple/Struct/Variant/Nullable) and its
  starting `wire_slot` (running `u64`-slot offset). A `scan` over the
  chained per-kind param vecs computes the offsets; `wire_words()`
  (Variant/Nullable = 2, else 1) is the single home of the slot-count
  rule.
- `GirKernel::abi_param_wire_slots() -> usize` — total wire footprint;
  the single home for the `n_pointer_slots + n_value_slots` arithmetic.
- `GirKernel::abi_return() -> Option<AbiReturn>` — return wire shape
  (`One { prim }` / `Two`); `None` = the invalid bare-`Null` return
  (callers attach their own error context).

**Consumers** (all now derive, none hand-walk): `gir_jit.rs`'s
`push_abi_params` / `push_abi_returns` (shared by both signature
builders — the duplicate block is gone), the `define_wrapper` unpack
loop (offsets from `wire_slot`), and `compile_into_function`'s entry
binder (one `match d.kind` loop replacing four loops + the `next`
counter). `gir_interp.rs`'s packer uses `abi_param_wire_slots()` for
its slot-buffer capacity and a `debug_assert_eq!` drift guard after
packing. The packer's per-vector push order (scalars, then
array/tuple/struct, then variant/nullable pairs) still encodes the
canonical order structurally — low drift risk, and the assert catches
a miscount.

Net ~+92 lines (gir.rs +143 for the single source; the five sites net
−51 from collapsing the duplication). Adding a new `AbiParamKind`
variant now forces a compile error at every consuming `match`
(exhaustiveness) instead of silently drifting one site. Behavior-
preserving: 1773/1773 graphix-tests + 125/125 graphix-compiler unit
tests green. #131 will now *consume* `abi_params` for the cross-kernel
call (emit args in `wire_slot` order) instead of adding a sixth
mirror.

### #131 composite/value-shape cross-kernel calls — interp side (May 2026)

A fused lambda invoked from inside another fused kernel lowers to
`GirOp::Call`. Until now `ensure_lambda_kernel` bailed (no kernel
built → interp fallback via `GXLambda`) whenever any formal arg,
capture, or return was non-scalar — the `all_scalar` guard. This
lands the **interpreter** half of lifting that guard; the JIT half
(the calling kernel JIT-ing a composite call) is the tracked
`#131-JIT` follow-up.

- **Guard relaxed to a return-only gate** (`lowering.rs`
  `ensure_lambda_kernel`). Args/captures are already restricted to
  the six value-bearing `GirType`s by `gir_type_to_region_input_kind`
  (String/Unit/Null bail there), so the only remaining shape to gate
  is the *return*: String / Unit / bare-Null returns aren't marshalled
  across the `GirOp::Call` boundary yet, so those still decline.
- **Interp `GirOp::Call` arm** (`gir_interp.rs`) now buckets each arg
  by its `GirType` into the six `eval_kernel_full` slices (scalar /
  array / tuple / struct / variant / nullable) instead of
  `.into_scalar()`-ing everything. Args appear in inputs order
  (formals then captures); bucketing preserves within-kind order,
  which matches the callee's per-kind param vectors (both built by
  walking the same input list), so the zip lines up — no reorder
  needed. New `EvalResult::into_nullable`; the dead-code allows on
  `into_valarray`/`into_variant` dropped (now used).
- **JIT `GirOp::Call` arm** (`gir_jit.rs`) returns `Err` for any
  non-scalar arg or return, so `fuse()` skips JIT and runs the parent
  kernel on the interpreter (which now routes the call). The `jit`
  test arm uses `BitFlags::empty()` (graceful fallback), not
  `Forced`, so this Err doesn't panic.

**Key findings (the catalog was optimistic):**
- **A top-level `let f = <lambda>; f(x)` does NOT produce a
  `GirOp::Call`.** The lambda binding bails the enclosing Do, so `f`
  runs as its own eagerly-fused kernel with composite *params*
  (pre-existing machinery). To get a real cross-kernel `GirOp::Call`
  with a composite arg, the call must sit inside ANOTHER lambda's
  body (`let h = …; let g = |a,b,c| h((a,b), c); g(…)` → `g`'s kernel
  contains `GirOp::Call(h, …)`).
- **The FUSION/JIT counters can't isolate a cross-kernel call's fusion
  state.** The callee `h` is itself eagerly fused and JITs, so a
  composite-call fixture reads `Jit` even though `g`'s kernel falls
  back to interp for the call. There is no observable `Interp`
  intermediate for these — so "interp-first" doesn't show up in the
  metric; it's about correctness + fused-mode behavior.
- **The #131-catalog fixtures were mis-attributed.** `tuples1` /
  `tuples2` / `bindstruct` are let/select *destructuring*, not
  cross-kernel calls; `lambdamatch2` uses a struct *destructure
  pattern* arg (`ensure_lambda_kernel` only accepts named/labeled
  args — bails on patterns); the `list_*` fixtures hit a separate
  upstream cliff (a nullable-returning `select` lambda doesn't build
  a kernel at all). Lifting the guard didn't flip any of them; they
  need different work.

**Tests:** `lang::tuples_structs::{call_tuple_arg, call_struct_arg}`
(end-to-end, observed `Jit` — composite tuple/struct args route
through a real `GirOp::Call`); `gir_interp::tests::call_nullable_arg_routes`
(direct — a `Nullable` arg routed through the Call arm's
`into_nullable` bucket into `nullable_args`, frontend-independent —
the one new path the e2e fixtures don't cover, since value-shape
*returns* are unchanged passthrough through `eval_kernel_full`).
1779/1779 graphix-tests + 126/126 graphix-compiler green.

**Remaining (tracked):**
- Destructure-pattern lambda args (`|{a, b}|`, `|(x, y)|`) — separate
  from arg *kind*; `ensure_lambda_kernel` needs to accept arg patterns.
- Nullable/variant-returning `select` lambdas not building a kernel —
  an upstream fusion-frontend gap that blocks the value-shape-return
  end-to-end path.

### #131 composite/value-shape cross-kernel calls — JIT side (May 2026)

The JIT half: a *calling* kernel now JIT-compiles a `GirOp::Call`
whose args/return are composite or value-shape (previously the JIT
Call arm returned Err → the calling kernel fell back to interp).
Consumes #132's `abi_params` for the ordering, so the historical
"scalar routed into a tuple-pointer slot → misaligned-deref crash" is
structurally impossible rather than guard-avoided.

- **`compile_call_clif_args`** (new shared helper, `gir_jit.rs`):
  assembles the CLIF call-arg list in the callee's kind-grouped ABI
  order — all scalars, then composite pointers (array, then tuple,
  then struct), then value-shape (variant, then nullable — two words
  each). Args arrive in inputs order (formals then captures); emitting
  kind by kind preserves within-kind order, which matches the callee's
  per-kind param vectors (built by walking the same input list), so it
  lines up slot-for-slot with the signature `push_abi_params` emits.
  Returns the owned composite/value args to drop after the call.
- **Ownership**: the callee clones every composite/value param on
  entry (`compile_into_function`: `graphix_valarray_clone` makes a
  fresh `Box<ValArray>`; `graphix_value_clone` bumps + `mem::forget`s).
  So args are passed AS-IS (borrowed). An `Owned`-source arg (a
  `TupleNew`/`StructNew`/`VariantNew`/`IfChain`/`Block`/`DynCall`/`Call`
  producer) leaves the caller holding the original after the callee
  took its clone → dropped after the call via `emit_call_arg_drops`
  (`graphix_valarray_drop` / `graphix_value_drop`). `Borrowed` (Local
  reads) aren't dropped — the env owns them. `classify_composite_source`
  gained a `GirOp::Call => Owned` arm (a composite/value Call *return*
  is an owned pointer/Value, same as DynCall).
- **Two Call arms**: `compile_scalar_impl`'s handles scalar /
  composite-pointer returns (1 CLIF result); `compile_value_expr`
  gained a `GirOp::Call` arm for value-shape (Variant/Nullable)
  returns (2 results → `(disc, payload)`). `compile_expr`'s type
  dispatch routes each Call to the right arm. Both share
  `compile_call_clif_args` + `emit_call_arg_drops`.

**Direct JIT tests** (`gir_jit::tests`, via `compile_kernel_with_callees`
— the metric can't observe whether the *calling* kernel JITs, so these
force-compile + run it): `cross_kernel_call_tuple_arg` (composite arg,
kind-grouped reorder, owned `TupleNew` drop with real ValArray
refcounting — the crash-class regression guard), `cross_kernel_call_nullable_return`
(value-shape return decode), `cross_kernel_call_owned_nullable_arg`
(value-shape `Owned` arg + post-call `graphix_value_drop`). The
end-to-end `call_tuple_arg` / `call_struct_arg` fixtures now JIT the
composite call through the real fusion pipeline (no regression).
129/129 graphix-compiler + 1779/1779 graphix-tests green.

**Off-topic bug surfaced + fixed (#133).** The JIT `GirOp::IsNull`
arm (`gir_jit.rs` ~2908) called `compile_scalar(b, inner, …)` on its
operand, but a `Nullable` operand now lowers to a two-register
`(disc, payload)` pair (since the by-value `Value` migration), so
`.single()` rejected it — AND the arm passed one arg to
`graphix_value_is_null(v: Value)` which takes two. The arm was never
updated for the by-value ABI. It was masked in the differential suite
by graceful interp fallback (a JIT-compile Err → `fuse()` runs the
kernel on interp → same value); the #131 force-compile (`.expect`)
test was the first to surface it. Impact: null-narrowing `select` arms
(`select nullable { null as _ => …, T as v => … }`) silently didn't
JIT. **Fix:** compile the operand via `compile_expr(…).value()` to get
`(disc, payload)`, then inline `icmp_imm Equal disc value_disc::NULL`
(an `I8` 0/1 bool, same shape `GirOp::Cmp` produces) — no helper call
(`graphix_value_is_null` stays for the interpreter / direct tests).
Regression test `gir_jit::tests::jit_is_null_on_nullable_param` (a
`Nullable`-param kernel whose body is `is_null(m)`, force-compiled via
`compile_kernel_with_wrapper`). The differential `cross_mode_null_handling`
fixtures now exercise the IsNull JIT path in mode C/D instead of
falling back. 130/130 compiler + 1778/1779 graphix-tests green (the
1 is the pre-existing fs-parallelism flake, passes single-threaded).

### #139 String / value-shape kernel params (region inputs) (Jun 2026)

The meaty cliff the user pushed on: a sync computation that *consumes*
a `String` / `DateTime` / `Duration` / `Bytes` value from an external
or async source couldn't fuse, because those four were the only value-
bearing `GirType`s that `gir_type_to_region_input_kind` rejected (→
`None`) — they worked as kernel *locals* but not as kernel *params*.
So `let t = now(); t + duration:1.s` (a timer-sourced datetime + a
ValueArith) and `let s = subscribe(...); str::to_upper(s)` (a string
fed to a sync builtin) fell off the fusion path. Struct/variant/
nullable/array/tuple/prim params already worked — the cliff was
specifically these four leaf value-shapes.

**The empirical correction that scoped it.** The original problem
statement ("a Value-shaped param blocks fusion") was imprecise. A
direct cross-compile probe (compile `let p = {x,y}`, then `p.x+p.y`;
repeat for variant / string / datetime) proved struct + variant region
inputs *already fuse*; only String/Bytes/DateTime/Duration were
blocked. The json/pack identity fixtures (`{let v: T = read(...); v}`)
were red herrings — they're hollow identity kernels, so they made the
whole feature *look* hollow; the real payoff is the meaty-calc case.
Lesson reinforced: reason from a probe, not from a plausible read
([[feedback-run-the-code]]).

**ABI.** A String param rides one machine word (its `ArcStr` thin
pointer, `repr(transparent)`); a bare value-shape (DateTime/Duration/
Bytes) param rides two words (`Value`'s `(disc, payload)`, same shape
as Variant/Nullable). New, all driven off the single ABI source
(`GirKernel::abi_params` / `abi_param_wire_slots`):
- `GirKernel.string_params: Vec<StringInput>` (1-word) +
  `value_params: Vec<ValueInput>` (2-word).
- `AbiParamKind::String` (1 word) / `Value` (2 words); `wire_words()`
  + `abi_params()` canonical order = scalars, arrays, tuples, structs,
  **strings**, variants, nullables, **values**.
- `RegionInputKind::String` / `Value(GirType)`;
  `gir_type_to_region_input_kind` now returns `Some` for all four (was
  `None`). `populate_kernel_inputs` routes them to the new slots +
  `ctx.{string,value}_inputs` (so `Local` reads resolve) +
  `TailCallSlotKind::String` / `Value`.
- `ArgKind::String(u32)` / `Value(u32)` + `build_arg_layout` counters;
  `eval_kernel_full` gained `string_args: &[ArcStr]` /
  `value_args: &[Value]` (pushed into `env.strings` / `env.nullables`
  at entry — value-shape rides the nullables slot, re-wrapped to the
  declared type on `Local` read). 6 callers updated.
- Interp `GirNode::update` builds `string_args` / `value_args`
  smallvecs; the JIT path packs strings (1 word, borrowed ArcStr ptr)
  after structs and values (2 words) after nullables.
- JIT: `push_abi_params` / `define_wrapper` unpack / `compile_into_
  function` entry binder all gained String (`bind_string` after a
  `graphix_arcstr_clone`) and Value (`bind_nullable` after
  `graphix_value_clone`) arms. Tail-call rebind for String/Value
  returns `Err` (graceful interp fallback — recursive lambdas with
  these param shapes are rare). `compile_call_clif_args` (cross-kernel
  `GirOp::Call`) bails *exhaustively* on String/value-shape/Unit/Null
  args — fixes a latent silent-drop where a DateTime arg matched no
  emit filter. The interp cross-kernel Call arm gained `strings` /
  `values` buckets so a callee with those params dispatches correctly.

**Tests.** Direct ABI round-trip: `gir_interp::tests::
string_and_value_kernel_params` + `gir_jit::tests::
jit_string_and_value_kernel_params` (string identity + datetime
`ValueArith`, both backends). End-to-end (real region param, not
const-inlined, via cross-compile): `lang::fusion::external_string_
region_param` (`str::len(s)` over an external string → I64(5), JIT
confirmed) + `external_datetime_region_param` (`d + duration:1.s` →
datetime, JIT confirmed). The bidirectional harness flagged 24
fixtures (string/datetime/bytes results) that flip None→Jit; all
upgraded (json_string/pack_string/pack_bytes ASPIRE comments
rewritten — their cliff is closed).

**The identity-kernel suppression — kept, and it exposed an inflated
metric.** `GirKernel::is_identity_passthrough` (body is exactly a single
`Return(Local(x))`) + a skip in `fuse()` stops forming hollow kernels
that do no work — the runtime `Ref` feeder already produces the value.
Turning it on broke **316 fixtures**, and that is the *valuable* result,
not a reason to revert (the user's call): the `run!` harness wraps every
fixture as `let result = {code}`, which yields a separate
`Return(Local("result"))` identity region. For any fixture whose *body*
region doesn't lower on its own (array-slice blocks, struct/IO results,
async reads, …), that hollow `result` wrapper was the *only* kernel
carrying the fusion signal — so the bidirectional `FuseExpect` metric
had been counting the wrapper, not real body fusion. Suppressing it
re-derives the truth.

Re-annotation (honest state, all deterministic — verified across 3
parallel full-suite runs, so this is NOT flakiness):
- **310 fixtures → `None` + an `ASPIRE: Jit` comment.** Their bodies
  genuinely don't fuse (real cliffs: array slice = "GIR emission
  failed", async IO, error-operator chains, etc.). Mechanically applied
  by a string-aware `run!`-block rewriter (`/tmp/reannotate.py`-style:
  paren-match the macro call, drop any trailing `; FuseExpect::X` +
  `; shape:` clause, append `; …::FuseExpect::None`, prepend the
  comment).
- **6 fixtures → `FuseExpect::Interp`** (`array_indexing0`,
  `structaccessor`, `array_sort1/3`, `hbs_invalid_template`,
  `hbs_strict_missing`). These deterministically fuse on the
  interpreter (`::fused` green) but their *body* fails to JIT-compile,
  so in jit mode `fuse()` skips the splice (FUSION==0, JIT==0) — exactly
  what `FuseExpect::Interp`'s arms assert. The single-threaded
  `GRAPHIX_FUSION_DISCOVERY` map confirmed all six are Fuses+NoJit.

**The JIT cliff the six exposed — fixed (variant-DynCall arg).** The
root cause for `array_sort`/`hbs` was a Value-shape DynCall arg, e.g.
`array::sort(#dir: \`Descending, a)`. `compile_scalar_impl`'s DynCall
arm compiled *every* arg via `compile_scalar` / `CompiledExpr::single()`,
which bails on a `(disc, payload)` pair ("expected single CLIF value,
got Value-shaped pair") and would have passed one word to the 2-word
`push_value` helper anyway. The `compile_value_expr` arm had a parallel
latent bug — its per-arg `match` only special-cased `Variant|Nullable`,
so `DateTime|Duration|Bytes` args fell to the `compile_scalar` default
too. **Fix:** both DynCall arms now route arg marshalling through one
shared `marshal_dyncall_args` (the CLAUDE comment literally said "factor
when the third Value-shape consumer lands"). It dispatches on
`GirType::is_value_shape()` — value-shape args compile via
`compile_value_expr` and push their two `(disc, payload)` words;
scalar/composite/string args push one. `array_sort1/3` and the two
`hbs_*` fixtures upgraded Interp→`Jit` (their `::jit` arms now assert
JIT>0 — the regression coverage). `array_indexing0` and `structaccessor`
stay `Interp` for a *different*, pre-existing cliff: a composite-element
accessor (`a[0]`, `s.bar : string`) routes to the interpreter via
`kernel_contains_composite_element_op` — a separate follow-up.

132 compiler + 1788 graphix-tests, green and parallel-deterministic (3×).

### #134 option-typed block values silently de-fused (May 2026)

The archetypal predictable-performance cliff, caught by the
bidirectional harness: pulling a `select` (or any option-producing
expression) into a named helper *inside a block* silently dropped the
whole block off the fusion path, with no signal. E.g.
`let result = { let f = |x: i64| select x { 0 => null, n => n }; f(5) }`
did not fuse, while the bare `{ let f = …; f(5) }` and a direct
`let result = select 5 { … }` both did.

**Root cause.** Graphix's option type `[T, null]` has TWO
representations in the typechecker: `Type::Set([Prim(T), Null])` AND
the collapsed `Type::Primitive(T | Null)` bitflag — the typechecker
collapses the union into a single multi-bit primitive when `T` is a
primitive (a `select x { 0 => null, n => n }` infers `i64 | null`; a
block whose tail has that type surfaces the collapsed form).
`GirType::from_type` (gir.rs) handled the `Set` form (→ `Nullable`)
but its `Primitive` arm rejected any multi-bit flag — the comment even
admitted "multi-flag primitives with Null + other variants fall
through to `PrimType::from_type`, which rejects them." So the collapsed
option type returned `None`.

That `None` made `infer_body_rtype`'s typed-AST fast path miss and
fall back to `emit_node(body)`. For a *direct* `select` value the
fallback succeeds (`emit_select_as_expr` unifies the arms to
`Nullable`); but for a *block* value the fallback walks the block
(`emit_do_as_expr`) which can't emit a `let f = <lambda>` and bails →
`infer_body_rtype` returns `None` → `build_region` errors → no fusion.
The `Nullable` return type was the trigger; the lambda-in-block was
incidental (a scalar block in the same shape hits the single-bit fast
path and fuses fine — `emit_body` handles the lambda bind without
issue).

**Fix** (gir.rs, ~20 lines): a `from_type` arm for
`Type::Primitive(p)` where `p == T | Null` (exactly two bits, one
`Null`) → `GirType::Nullable(inner)`, where `inner` is `Prim(T)` or
`String` (mirroring the `[T, null]` `Set` arm's recursion so both
representations agree). No other site changed.

**Diagnosis lesson** (reinforces [[feedback-run-the-code]]): the
fusion machinery had refactored well past CLAUDE.md (no
`analyze_program` / `eager_fuse_lambdas` / `fusion_lambdas`; it's
`fuse()` → walker candidates → `build_region`, lambdas via `ctx.ln`).
A general-purpose subagent's static trace was confidently WRONG TWICE
(it blamed `emit_bind_stmt` bailing on the lambda bind — contradicted
by instrumentation showing `BINDSTMT bail` never fired and a scalar
block fusing). Only empirical bisection (`GRAPHIX_KDBG` eprintln
instrumentation in `finish_kernel` / `infer_body_rtype` / the CallSite
arm, run on the actual binary) found the real `from_type=None for
typ=Primitive(I64 | Null)` smoking gun. Reason about refactored code
from execution, not from the docs or from a plausible-sounding trace.

**Unblocked** (the bidirectional check flagged them automatically):
`lang::tuples_structs::call_nullable_return` (i64-option,
`let f = |x| select x { 0 => null, n => n }; f(5)`) and
`lib_tests::dirs::{home_dir, config_dir, data_dir}` (string-option —
`sys::dirs::*` return `[string, null]`) — all `None` → `Jit`. This is
also what finally makes #131's value-shape-return Call path reachable
end-to-end (previously only direct unit tests). 130/130 compiler +
1782/1782 graphix-tests green.

Not covered: variant-returning `select` lambdas (`select x { … => `A,
… => `B(n) }`) weren't separately verified — they produce a
`Type::Variant` / `Type::Set`-of-variants shape, a different path that
may already work via the existing `Variant`/`Set` arms or may be a
separate cliff.

### NodeShape — graph-shape assertion harness (the third test axis) (Jun 2026)

`run!` checks a fixture's *value*; `FuseExpect` checks *whether*
fusion fired. Neither sees *what fused into what*. `NodeShape`
(`graphix-compiler/src/node_shape.rs`) closes that gap — it's a
**declarative spec** of a compiled (sub)graph that the runtime checks
against the *live* post-fusion graph and returns a verdict. Replaces
the deleted `lowering.rs` `emit_expr`-based unit tests (#124), whose
hand-faked `FusionCtx` couldn't drift-detect the way walking the real
graph does.

**Design (user's, refined):** `NodeShape` is the *spec*, not an
extracted result. The rt owns the matching — a test declares the spec
and gets pass/fail back; the walk-and-compare runs once, in-task,
against the real nodes (so only owned data crosses the channel; no
`TestRt` needed).

- `enum NodeShape { Any | Node{kind: Option, children: Vec} |
  Fused(GirMatcher) | Contains(Box) }`. `Contains` matches if any
  node in the subtree matches the inner spec — lets a test assert
  "this program contains a kernel shaped like X" without spelling out
  the wrapper path (module Do / binds). Builder helpers:
  `NodeShape::{node,any_node,fused,contains,contains_fused}` +
  `.child(...)`.
- `GirMatcher` — *partial* criteria, every field a wildcard until set
  (matcher, not transcript): `.returns(GirType)`, `.params(&[...])`,
  `.contains(GirOpTag)`. `GirOpTag` mirrors all 29 `GirOp` variants
  (exhaustive `gir_op_tag`, so a new op forces an update).
- `match_node(&Node, &NodeShape) -> Result<(), String>` — path-tracked
  mismatch reason on failure. One generic `visit_ops` GIR visitor (did
  NOT add a 5th copy of the recursion next to the 4 `expr_has_*`).
- `node_children` enumerates **every** `NodeView` variant's children
  (binop family via a macro; deterministic CallSite arg order;
  `FusedKernel` → its feeders so `Contains` descends through a kernel;
  true leaves → empty). `describe_node(&Node) -> String` renders the
  actual graph as an authoring aid (run once, read the shape, write
  the spec).

**Runtime hooks** (`graphix-rt`): `ToGX::MatchShape`/`DescribeShape` +
`GXHandle::match_shape(eid, spec) -> Result<()>` /
`describe_shape(eid) -> String`. Accessors `GirNode::kernel()`,
`FusedKernel::kernel()`/`feeders()`, `Module::source()`.

**`run!` integration:** optional trailing `; shape: <NodeShape>`,
checked in **fused** mode only (graph shape is backend-independent;
`interp` has no kernel, `jit` is redundant). Threaded as a 4th
`check_shape` arg + a `shape_spec()` fn per expanded module.

Ported assertions (one-liners now): `node_shape_external_scalar`
(`foo*6` → Bin, + positive/negative teeth), `tuples0` (TupleNew),
`structs0` (StructNew), `tupleaccessor` (TupleGet), `array_indexing0`
(ArrayGet). Remaining old assertions are incremental `; shape:`
additions.

**Bug caught by the negative test** ([[feedback-run-the-code]] again):
`find_match` (the `Contains` walker) calls `node_children` on *any*
node incl. `FusedKernel`, which was grouped into the binop
`unreachable!` arm → panic on a mismatch path. The deliberately-wrong
spec surfaced it immediately; the positive-only run would have masked
it. Fixed: `FusedKernel` arm returns its feeders. 130/130 compiler +
1783/1783 graphix-tests green.

### datetime/duration as a Value-shape GirType (Jun 2026)

`datetime`/`duration` arithmetic (`datetime ± duration`, `duration ±
duration`, `duration {*,/} number`, `number * duration`) now fuses +
JITs. Runtime reps are `Value::DateTime(Arc<DateTime<Utc>>)` /
`Value::Duration(Arc<Duration>)` — thin `Arc` payloads, so they're
**Value-shape** (two-register `(disc, payload)`), reusing the
Variant/Nullable ABI throughout. Arithmetic computes via netidx's
`impl {Add,Sub,Mul,Div,Rem} for Value` on BOTH backends, so the fused
result is byte-identical to the non-fused arith node (`lhs $op rhs`).

**Type + IR:** `GirType::{DateTime,Duration}` + `GirType::from_type`
(single-bit `Typ::DateTime`/`Typ::Duration` primitives) +
`is_value_shape()` helper (Variant|Nullable|DateTime|Duration). New
ops `GirOp::ConstValue(Value)` (datetime/duration literals — a
`ConstVal` is `Copy`, an `Arc` payload isn't) and `GirOp::ValueArith
{op, lhs, rhs}` (either operand scalar-or-Value, result always
Value-shape). `EvalResult::{DateTime,Duration}(Value)` carriers;
`wrap_value_shape(v, &typ)` picks the carrier.

**emit side** (`lowering.rs`): `NodeView::Constant` lowers
`Value::DateTime`/`Duration` → `ConstValue`; the 5 arith arms route
through a new `emit_arith` wrapper that emits `ValueArith` when either
operand is datetime/duration (result type per the typechecker's
rules), else the scalar `gir::arith`.

**Locals:** datetime/duration let-bindings ride the existing
`nullables` Value slot on both backends (a name→Value map); the
`Local` read dispatches by the ref's GirType to re-wrap. A new
`ctx.value_inputs: Vec<ValueInput>` (carries the full GirType, unlike
`NullableInput`'s inner-only `elem`) makes Ref resolution produce the
correct `DateTime`/`Duration` type. **Params/region-inputs deferred** —
`gir_type_to_region_input_kind` returns `None` (like String); the
fixtures only use datetime/duration as intermediates + returns.

**JIT codegen** (`gir_jit.rs`): `KernelValues` per-kernel constants
table (mirrors `KernelStrings` — `Box<[Value]>` with stable
`*const Value` addresses, kept alive on `CompiledKernel`/
`WrappedKernel`/`CachedKernel` via `_values`). New helpers
(`gir_jit_helpers.rs`): `graphix_value_clone_from_static(*const Value)
-> Value` (ConstValue: bake ptr, clone = Arc bump) and
`graphix_value_{add,sub,mul,div,rem}(Value, Value) -> Value` (the
arith — consume both args, matching netidx's by-value operators).
`compile_owned_value_operand` produces each ValueArith operand as an
OWNED `(disc, payload)`: a Value-shape operand clones a Borrowed Local
via `ensure_owned_value`; a scalar promotes to a fresh `Value::<prim>`
(`prim_to_value_disc` + `scalar_to_payload_i64`, no Arc → trivially
owned). ~40 `Variant|Nullable` match arms across gir_jit/gir_interp/
lowering/gir.rs/mod.rs/node_shape extended to include DateTime/Duration
(the 14th-commandment compiler enumerated every one).

**Two-milestone landing.** Interp first (`None → Interp`, kernel forms
+ runs via `gir_interp`), validated + regression-tested; then JIT
(`Interp → Jit`). The interp checkpoint relied on a key finding: the
"`JitMode::Forced` panics on JIT-compile failure" behavior in older
CLAUDE.md notes is **stale** — current `fuse()` gracefully `continue`s
on a JIT `Err` (skips the splice in jit mode → `FUSION==0`), and
`check_fuse_expectation(Interp)` passes cleanly. So datetime kernels
that couldn't yet JIT just fell back to interp without panicking.

**Bug caught by running the JIT path** ([[feedback-run-the-code]]):
the `GirStmt::Return` value-shape arm grouped `Variant|Nullable` but
missed DateTime/Duration → they fell to the scalar arm →
`compile_scalar` on a `(disc,payload)` pair → JIT-compile Err (silently
swallowed by `fuse()`'s `Err(_) => continue`, surfacing only as
`FUSION==0`). Found by temporarily logging the swallowed error.

**Fixtures:** the 7 `datetime_arith0{0..6}` arith fixtures went
`None → Jit`; the 11 invalid-op fixtures (`duration*duration`,
`datetime-int`, …) stay `None` (typecheck errors, never compile); one
checked-overflow (`*?`) was already `Jit` via the checked-op path. All
modes agree on the produced `Value::DateTime`/`Duration`.
130/130 compiler + 1783/1783 graphix-tests green.

### bytes as a Value-shape GirType (Jun 2026)

`bytes` (runtime `Value::Bytes(PBytes)`, a refcounted thin pointer) is
now a `GirType` — and the key decision was to make it **Value-shape**
(two-register `Value`, like the datetime/duration work) rather than a
single-register leaf like `GirType::String`. That reuses the *entire*
Value-shape pipeline with almost no new code: a `bytes` literal lowers
to `GirOp::ConstValue(Value::Bytes(..))` (baked via the existing
`KernelValues` table + `graphix_value_clone_from_static`); bytes locals
ride the `value_inputs` slot + `nullables` env slot; the two-register
ABI, `graphix_value_clone`/`drop`, `ensure_owned_value`, Return/Let/
DynCall arg+return arms all already handle it. **No bytes-specific JIT
helpers, slot lists, or codegen** — vs. String's single-register leaf
machinery, this was far smaller and DRY. (The redundant disc word vs.
String's single register is negligible.)

Mechanical surface: `GirType::Bytes` + `from_type` (`Typ::Bytes`
single-bit) + `is_value_shape()`; `EvalResult::Bytes(Value)` +
`wrap_value_shape` arm; `gir_type_to_graphix_type` → `Typ::Bytes`;
`emit_node`'s `Constant` arm lowers `Value::Bytes` → `ConstValue`. The
~40 `Variant|Nullable|DateTime|Duration` Value-shape match arms gained
`| GirType::Bytes` (added by two `perl` passes — line-start `| GirType::
Duration =>` groups and single-line `DateTime | Duration =>` groups —
the compiler's exhaustiveness then confirmed the rest). Bytes correctly
does NOT join the arith arms (`emit_arith`, ValueArith) — no bytes
arithmetic.

Fixtures: `test_write_bin_then_read_bin` (fs binary round-trip, the
ASPIRE'd "string twin fuses" gap) went `None → Jit`; new
`bytes_const_local` (`{ let x = bytes:..; x }`) covers the bytes-literal
+ local + return path, `Jit`. `pack_bytes` stays `None` — `bytes` is no
longer the blocker (annotation corrected); it's the `buffer::to_string`
→ `Result<string,_>` + `$`/`?` error-operator chain. 130/130 compiler +
1786/1786 graphix-tests green.

### Gap-map accuracy pass — the loose-`Number` correct-None theme (Jun 2026)

Autonomous gap-clearing session. Net code change: ~zero (no new fusion);
net value: the ASPIRE gap map is now **honest**, several "gaps"
reclassified as correct-by-design, and two root causes pinned via the
`describe_shape` introspection tool (#124) — which earned its keep here.

**The loose-`Number` theme (correct-None, not gaps).** A recurring
mis-categorization: fixtures whose result type is the loose `Number`
*set* (not a `'a: Number` type variable) can't fuse because the kernel
return is an unbound `Number` TVar that `GirType::from_type` won't
lower — and that's *correct*, because `Number` means "each value may be
a different number type" (genuinely dynamic). This covers
`sum`/`product`/`divide`/`all` (re-annotated earlier) AND, newly,
**3 of the 4 "labeled-arg lambda" fixtures** (`labeled_args`,
`mixed_args`, `arg_name_short` — all `|#foo: Number, #bar: Number|
foo + bar`). Proof the labeled-arg machinery itself is fine: the
identical lambda with concrete `i64` params fuses + JITs. So
"labeled-arg lambda call" was a red herring; the blocker is the dynamic
`Number` result. `arg_subtyping` (the 4th) is actually HOF (fn-typed
arg), re-annotated as such. All re-annotated `// Not fused, by design`
(plain `None`, no `ASPIRE`) — see [[feedback-predictable-fusion]]'s
complement: don't contort to fuse explicitly-dynamic types.

**json/pack string-return root cause (#136, via describe_shape).** NOT
a json-specific string-return issue. `{let v: T = json::read(...)?; v}`:
the trailing `v` (a Ref to the result) forms a trivial identity kernel
`params=["v"], ops={Local}` for the i64/array/struct siblings, but
`gir_type_to_region_input_kind` returns `None` for
String/Bytes/DateTime/Duration — they can't be **region inputs / kernel
params** (only locals). So the identity kernel won't form for a String
result. (The json::read DynCall itself doesn't fuse in *either* case —
`NEEDS_CALLSITE` builtins aren't discovered as DynCalls; the i64 "Jit"
is just that hollow identity kernel.) `json_null`/`pack_null` are the
degenerate `null` sub-case (bare Null is by-design widened to Nullable;
a null→null identity is information-free → borderline correct-None).

**Deferred: String/value-shape kernel params (#139).** The real fix for
json_string/pack_string is a String (1-register) + value-shape
(2-register) kernel-param ABI — also closes the deferred datetime/bytes
param cases and enables the genuinely-valuable "fuse sync computation
over an async-sourced string/bytes/datetime" pattern. Sized: ~13 logic
changes + ~38 mechanical `string_params: vec![]` construction sites +
runtime arg-packing (segfault surface). DEFERRED for greenlight because
the *tested* payoff (json/pack-string) is a hollow identity kernel; the
real value isn't fixture-covered. A large ABI change for a marginal
tested win is the wrong autonomous trade.

### Fusion gap-map correction + cross-module call resolution (Jun 2026)

A FUSEBAIL-instrumented harvest corrected the fusion metric and landed
the foundational cross-module-call fixes. **The headline: the corpus
was at 119 Jit, not the 385 the doc claimed** — #139's identity
suppression had already dropped real body-fusion to ~115, but the doc
was never updated. `design/fusion_state.md` now reflects reality
(119 Jit / 2 Interp / 426 None across 547 fixtures) with an accurate
blocker taxonomy.

**FUSEBAIL instrumentation** (kept — it's the harvest tool the doc
references). Debug-only thread-local in `gir_jit_helpers`
(`record_fuse_bail`/`take_fuse_bails`/`reset_fuse_bails`), recorded at
every fusion give-up site: `node:<Variant>` (emit_node catch-all),
`dostmt:<Variant>` (block statement), `emit:<Helper>` (producer/
accessor sub-emitter), `call:<name>`/`callqual:<path>` (un-lowerable
call). The `run!` discovery branch prints `FUSEBAIL <path> <tags>` so a
single `GRAPHIX_FUSION_DISCOVERY=1` run maps every None fixture to its
*reason*. This is what turned "269 fixtures are mysteriously EMPTY"
into the actionable taxonomy. `node_view_name` + `rec_bail` in
`fusion/lowering.rs` are its helpers.

**The big finding — qualified user-fn calls never resolved.**
`ident_of` returns `None` for any module-qualified path
(`list::is_empty`, `inner::make`), and BOTH the lambda-resolution path
(emit_node CallSite arm) and `emit_known_fused_call`'s `ident_of(name)?`
silently bailed on it. So calls to *graphix-implemented* stdlib
functions (`list::*`, graphix `array::*` HOFs, `buffer::*`, interface-
module functions — ~132 fixtures) never fused. Two fixes made them
*resolve*:

1. **Interface-proxy resolution** (`static_resolve.rs` +
   `node/module.rs`). A signed module re-exports each impl binding's
   value to its public `val` signature binding under a *different*
   BindId (the runtime forwards impl→sig via the Module's `proxy` map,
   `module.rs` update()). The caller references the *signature* BindId,
   which `collect_lambda_binds` never had in its `bind_to_lambda` map —
   so `resolved_apply()` was `None` for every cross-module-through-
   interface call. Added `Module::proxy()` (`pub(crate)`) and taught
   `collect_lambda_binds`'s Module arm to forward `out[sig_id] =
   out[impl_id]` for each `(impl_id, sig_id)` proxy entry. Now
   `inner::add(10, 20)` resolves to the impl `LambdaDef`.

2. **Qualified source-name** (`fusion/lowering.rs` emit_node CallSite
   Lambda arm). The kernel registry / `GirOp::Call` key now falls back
   to the full path (`name.0`) when `ident_of` rejects a qualified
   name; bare idents stay verbatim (so unqualified self-recursive calls
   still key consistently).

**Type-rep is the *next* blocker** (resolution alone gave +1). After a
qualified call resolves, `ensure_lambda_kernel` still bails on
`GirType::from_type`: `/list` needs recursive variant types
(`List<'a> = [\`Cons('a, List<'a>), \`Nil]` — infinite type),
`/array` needs composite-element HOFs, `/buffer` needs bytes+mutable-
refs, `/inner` needs abstract-type rep (`from_type(T)` can't know an
opaque type's concrete runtime shape without the impl's private
typedef). Each is a separate type-system feature.

**TypeDef/Use skip in blocks** (`fusion/lowering.rs`). `type X = ...`
and `use foo` statements inside a block are compile-time-only — they
now skip like a `Nop` in `emit_do` / `emit_do_as_expr` instead of
bailing the whole block. Flipped `rectypes0`, `rectypes1`,
`typedef_tvar_ok` (+ `interface_no_abstract_types` from the proxy fix)
None → Jit.

**Tried + reverted: lambda-bind-in-body skip.** Skipping a `let f =
|x| ...` bind so block-local lambda calls resolve via the CallSite
Lambda arm crashed 3 fixtures (`GirOp::Call to f not in kernel
registry`) — the call emitted a `GirOp::Call` via `known_fns` without a
matching `called_kernels`/registry entry. node:Lambda (32 fixtures)
needs the deeper "lambda-binds-in-bodies" work (unique per-LambdaId
kernel naming + registry coordination, the SAFETY INVARIANT in
`ensure_lambda_kernel`) — deferred.

Net: 115 → 119 Jit, suite green (132 compiler + 1788 graphix-tests),
parallel-deterministic.

### Composite / string / value-shape element reads in the JIT (Jun 2026)

`a[i]` / `t.0` / `s.field` whose element type is non-primitive used to
bail the JIT (`ArrayGet`/`TupleGet`/`StructGet` did `elem.as_prim()?`
→ Err → interp fallback). The interp already handled them via
`extract_composite_or_scalar`; this teaches cranelift the same.

Six helpers (`gir_jit_helpers.rs`), mirroring the interp:
`graphix_{valarray,struct}_get_{array,arcstr,value}`. Each returns an
OWNED value — `*mut ValArray` (composite elem, fresh box), `ArcStr`
(string elem, refcount bump), or two-word `Value` (Variant / Nullable /
DateTime / Duration / Bytes elem, clone) — so the source ValArray keeps
its ref and the kernel's scope-exit drop + the consumer's drop don't
double-free. `struct_get_*` does the two-level kv-pair read.

`element_read_helper(elem, struct_access)` + `compile_element_read`
(`gir_jit.rs`) centralize the dispatch: a value-shape elem returns
`CompiledExpr::Value` (two-register), everything else `Single`. The
three accessor arms in `compile_scalar_impl` now call it + `.single()`;
`compile_value_expr` gained parallel arms for the value-shape-elem case
(routed there by `compile_expr`'s `is_value_shape()` dispatch).
`classify_composite_source` classifies the three accessor ops as
`Owned` (their results are fresh clones).

`structaccessor` (`{let x = {foo: "bar", ...}; x.foo}` — string field)
flipped Interp → Jit. `kernel_contains_composite_element_op` (the
never-called gate the old bail comments referenced) is now moot; left
in place.

119 → 120 Jit, Interp 2 → 1 (`structaccessor`). Suite green.

### `array[i]` bounds-check seam — one shared `array_index` (Jun 2026)

A correctness bug the composite-element work surfaced: `array[i]` is
`[elem, Error<…>]` (out-of-bounds → `ArrayIndexError`), but
`emit_array_ref` typed the `ArrayGet` op as the **bare element**, and
the gir-interp/JIT did **unchecked** indexing — the interp would *panic*
on OOB and the JIT read garbage, both diverging from the node-walk. It
escaped because **no fixture exercised OOB/negative indexing in a
fusable context** (`array_indexing0` only tested `a[0]`, in-bounds, and
was `Interp` so the JIT path wasn't even compared).

Fix — extract the runtime semantics into one shared
`crate::node::array::array_index(elts, i) -> Value` (bounds check,
negative-from-end, the OOB error) and route **all three backends**
through it:
- node-walk `ArrayRef::update`'s array branch calls it (the two
  positive/negative branches collapse to one call).
- `GirOp::ArrayGet` is now typed `Nullable<elem>` (`emit_array_ref`);
  gir-interp's arm calls `array_index` → `EvalResult::Nullable`; the JIT
  routes via `compile_value_expr` calling the new `graphix_valarray_index`
  helper (→ two-word elem-or-error `Value`). ArrayGet is removed from
  `compile_scalar_impl` (always value-shape now). `RegValue::as_i64`
  added (signed index for negatives).

The negative branch tests `i >= 0` after `len + i`, so `a[-len]`
reaches the first element (offset 0) and `a[-(len+1)]` underflows to an
error — a single point where all three backends agree. (The original
node-walk had a latent `i > 0` here that made element 0 unreachable via
a negative index; consolidating to one source made fixing it a one-line
change.)

**The missing tests, added** (`lang/arrays.rs`, all via `run!` so all 3
modes must agree): `array_index_oob_pos` (`a[10]` → error),
`array_index_neg_last` (`a[-1]` → last), `array_index_neg_mid`,
`array_index_neg_first` (`a[-len]` → first element),
`array_index_neg_underflow` (`a[-(len+1)]` → error), `array_index_is_err`
(the error flows into `is_err`, which also fuses+JITs). The interp unit
test `array_len_and_get` → `array_get_bounds_checked` (in-bounds / OOB /
negative-from-end / underflow at the interp level). `array_indexing0`
flipped Interp → Jit.

`array_indexing0` flipped Interp → Jit; 6 new bounds-check fixtures all
fuse+JIT. Jit 120 → 127, Interp 1 → 0, corpus 547 → 553. Suite green
(132 compiler + 1806 graphix-tests). (One earlier observed `db_get_type`
failure was a pre-existing sqlite/tempdir parallelism flake — passes
isolated and on re-run.)

### GirType::Map (Value-shape) — constant map literals + map builtins (Jun 2026)

`Value::Map(CMap)` is a thin-pointer Value variant (disc `0x0800_0000`),
so `GirType::Map` is a **Value-shape** leaf — the same pattern as
`bytes`: it reuses the two-register `Value` ABI, `graphix_value_clone`/
`drop`, the `value_inputs`/`nullables` slots, `ConstValue` +
`KernelValues`, and DynCall arg/return marshalling, with **no
map-specific JIT helpers**. The 14th-commandment exhaustiveness check
drove it: adding the variant produced ~24 compile errors, each a
value-shape match site that gained `| GirType::Map` (a `perl` pass over
the `| GirType::Bytes` arms), plus `EvalResult::Map(Value)`, the
`from_type(Type::Map)` arm, `wrap_value_shape`, and
`gir_type_to_graphix_type`.

**Producer — constant fold.** `emit_node`'s new `NodeView::Map` arm
(`emit_map_new`) builds the `CMap` at compile time (mirroring the Map
node's `insert_cow`) **when every key and value is a compile-time
constant** (`node_const_value` sees through `ExplicitParens`), emitting
`GirOp::ConstValue(Value::Map(...))`. A dynamic entry bails (the runtime
map producer isn't lowered).

**Off-topic latent bug fixed.** The gir-interp `GirOp::DynCall` arg-
marshalling had **no value-shape arm at all** — a datetime / duration /
bytes / map DynCall arg would hit the catch-all `panic!("DynCall arg
shape mismatch")`. It had simply never been exercised (those types
were only ever intermediates/returns, never DynCall args) until
`map::len(m)`. Added one arm covering all four value-shape carriers.

Flipped **9 fixtures None → Jit**: `map0/1/2/map_empty` (constant
literals) and `map_len`, `map_get_present/absent`,
`map_get_or_present/absent` (literal + `map::*` builtin DynCalls). Jit
127 → 136.

**Deferred (still None):** `m{key}` access (`MapRef` op),
map-mutation builtins (`map::insert`/`remove`/`map_change_*`), map HOFs
(`map::map`/`filter`/`fold`/`iter`), and non-constant/composite map
literals (`map_nested`, `map_with_arrays`, `map_complex_keys`).

Suite green (132 compiler + 1806 graphix-tests).

### Scalar-callback `/array` HOF fusion — the analysis_pred refs leak (Jun 2026)

`array::map`/`filter`/`fold`/`init` with a scalar-element callback
(`array::map(a, |x| x>3)`) now fuse + JIT end-to-end (task #144). The
fusion *design* the user articulated — a post-typecheck pass
materializes each HOF builtin's callback into an analysis-only prototype
(`Apply::static_resolve_fn_args` → `MapQ::analysis_pred`), `emit_gir`
lowers it to a self-contained loop op (`GirOp::ArrayMap`/`ArrayFold`/
`ArrayFilter`/`ArrayInit` with the callback body inlined), and the
runtime `update()` is left untouched so async per-slot semantics survive
— was already **fully built**. Two things stopped it firing for
`array::map`, found by instrumenting the actual run (the prior session's
"static-dispatch hang" diagnosis was a wrong reconstruction):

1. **Three plumbing gaps** kept `static_resolve_fn_args` from ever
   reaching `MapQ`, so `analysis_pred` stayed `None` and `emit_gir`
   bailed at its first line:
   - *Resolution* (`static_resolve.rs` `try_resolve_callsite`): the
     stdlib is compiled separately, so `array::map`'s `let map = |a,f|
     'array_map` Bind isn't in the fixture tree → absent from
     `bind_to_lambda`. Added a `ctx.cached.get(&r.id)` fallback (the
     LambdaDef value persists there); the step-2 downcast filters
     non-lambda cached values.
   - *Fn-arg detection* (`invoke_apply_fn_arg_hook`): at a monomorphized
     call site the callback formal is a **TVar bound to `Fn`**, not a
     bare `Type::Fn`, so `matches!(&farg.typ, Type::Fn(_))` missed it.
     Now `farg.typ.with_deref(|t| matches!(t, Some(Type::Fn(_))))`.
   - *Hook delegation* (`node/lambda.rs`): `BuiltInLambda` (wraps every
     builtin Apply) forwarded `view`/`update`/`refs`/… but **not**
     `static_resolve_fn_args` → it hit the no-op trait default instead
     of `MapQ`'s. Added the one delegating method.

2. **The actual hang — a `refs()` leak (the real bug).** With #1 fixed,
   `array::map` fused but hung at runtime. Cause: `MapQ`/`FoldQ`/
   `Init::refs` walk `analysis_pred` (needed, so a callback's *real*
   captures surface as kernel inputs), but `analysis_pred` is a
   synthetic node holding `genn::bind`-minted internal bindings — a
   per-element `x`/`acc`/`i` and the callback-function handle
   (`predid`/`fid`) — that are *referenced* but have no binding-site
   node in that subtree. So `refs()` put them in `refed` but never
   `bound`, and `collect_region_inputs` (= `refed − bound`) registered
   e.g. `("x", BindId)` as a region input. Fusion then built a feeder
   `reference(synthetic_id)` that nothing ever produces → the kernel
   blocked forever. Fix: new `pub Refs::mark_bound(id)` (the `bound`
   field is crate-private); each builtin's `refs()` marks its synthetic
   ids bound before walking `analysis_pred`. A `refs()` that surfaces a
   binding the node *owns* as a free variable is wrong independent of
   fusion — this is a real correctness fix, not a fusion band-aid.

**Method note** ([[feedback-run-the-code]], reinforced hard): the
multi-agent architectural map + the prior session's notes both asserted
the blocker was a runtime hang from `resolve_static` setting
`cs.statically_resolved = true` (the static-dispatch path "not driving
MapQ's per-slot synthesis"). Instrumenting the actual `fuse()` run
(`HOFDBG` eprintlns on region inputs + the splice) showed the kernel
*did* splice and the only problem was one spurious `("x", BindId)`
input. A speculative "reset `statically_resolved` for FusedBuiltin HOFs"
fix was added, then **removed and the suite stayed green** (incl. the
async `array::map(a, |v| net::publish(...))` `queue` round-trip) —
proving static dispatch of a HOF builtin is fine (they're never Connect
targets, already excluded by `unstable_bindings`). Reconstruction was
wrong twice; one instrumented run settled it.

Flipped **7 fixtures None → Jit**: `array_map0`, `array_filter`,
`array_fold0`, `array_init0/1/2`, `gir_fused_deferred_map` (the last
fuses `init`+`fold` in one program). 136 → 143 Jit.

**Still None in `/array`:** the non-`emit_gir` HOFs (`find`/`find_map`/
`filter_map`/`flat_map`/`group` — no per-builtin codegen override yet)
and nested/composite-element maps (`array::map(a, |x| array::map(...))`,
`array_map1`, `array_init3/4`).

Suite green (132 compiler + 1806 graphix-tests).

### `array::find` + `array::filter_map` scalar lowering — interp + JIT (Jun 2026)

Two more `/array` HOFs get an `emit_gir` override (continuing the
refs-leak unlock above): `GirOp::ArrayFind` (predicate `bool` →
`Nullable<elem>`, early-exit on first match) and `GirOp::ArrayFilterMap`
(body `Nullable<out>` → collect the non-null results into `Array<out>`).
`FindImpl::emit_gir` mirrors `FilterImpl` (predicate must be `Bool`);
`FilterMapImpl::emit_gir` mirrors `MapImpl` but pulls `out_elem` from the
body's `GirType::Nullable(inner)` (the #134 option-typed-`select`
lowering produces that shape). Interp arms in `gir_interp.rs`;
~12 shared-walker arms each (`expr_has_*`, `collect_*_expr`,
`classify_composite_source`, `node_shape`'s `GirOpTag`/`visit_ops`) —
the 14th-commandment exhaustiveness check enumerated every site.

Landed interp-first, then JIT (the datetime/bytes two-step). JIT codegen:
- `ArrayFilterMap` (Array result → `compile_scalar_impl`): an
  `ArrayFilter`-style collect loop, but each iteration compiles the body
  via `compile_expr` to a Value-shape `(disc, payload)`, branches on
  `disc == value_disc::NULL`, and on non-null pushes
  `cast_u64_to_prim(payload, out_elem)` into the buf.
- `ArrayFind` (Nullable result → `compile_value_expr`): an early-exit
  loop whose found / not-found edges jump to a two-word `(disc, payload)`
  merge block (the same Value-shape phi `compile_ifchain` builds); the
  found edge packs `(prim_to_value_disc(elem), scalar_to_payload_i64(..))`,
  the not-found edge packs `(NULL, 0)`. Block-call args are
  `BlockArg::Value(..)` (cranelift's newer jump-arg API).
+3 Jit fixtures: `array_find_scalar`, `array_find_scalar_none`,
`array_filter_map_scalar`.

`array::flat_map` (scalar, body `Array<out>`) landed the same way —
`GirOp::ArrayFlatMap` concatenates each body array into the output. Its
JIT is *linear* (no nested cranelift loop): the body compiles to an
owned `*ValArray` and a new `graphix_value_buf_extend_from_array` helper
(two-pointer sig like `push_array`) clones each element into the buf and
drops the array — the inner loop runs in Rust. `classify_composite_source`
needed an explicit `ArrayFlatMap => Owned` arm (its `_ => Borrowed`
catch-all would otherwise silently mis-classify the owned result and
clone-then-leak it). Flipped the existing `array_flat_map` None → Jit
(147 Jit). The scalar non-`emit_gir` HOF sweep (find/filter_map/flat_map)
is complete.

**Fixture findings (why the *existing* fixtures stay None):**
- `array_find` / `array_find_map` use `Array<(string, i64)>` (composite
  tuple elements) + destructure-pattern callbacks `|(k, v)|` — the
  composite-element-HOF cliff (`in_elem`/`out_elem` are `PrimType`-only),
  not addressed here. Needed new scalar fixtures to exercise the ops.
- `array_filter_map` stays None because its `false => x ~ null` arm uses
  the **Sample operator `~`**, which `emit_expr` doesn't lower (`_ =>
  None`) — so the body bails before `ArrayFilterMap` is even reached.
  `array_filter_map_scalar` is the same op with `false => null`.

Follow-ups: composite-element HOFs (widen `in_elem`/`out_elem`
`PrimType`→`GirType`) to flip the existing composite `find`/`find_map`
fixtures + nested maps; `array::group`/`find_map` (no `emit_gir` yet);
`Sample` lowering in `emit_expr` (would flip `array_filter_map`).

Suite green (132 compiler + 1815 graphix-tests).

### Composite-element *output* for `array::init` + `array::map` (Jun 2026)

`array::init(n, |i| <composite>)` and `array::map(arr, |x| <composite>)`
where the callback returns a tuple/struct/variant/nested-array now fuse +
JIT (the *input* element stays scalar — `in_elem: PrimType`). The change
was a simplification, not new machinery: `GirOp::ArrayInit.elem_typ` and
`GirOp::ArrayMap.out_elem` (both `PrimType`) were *removed* — they always
equalled `body.typ`, so they were redundant. The output push now dispatches
on `body.typ`:
- **interp** (`gir_interp.rs`): `eval(body).into_value()` (handles any
  `EvalResult` shape — scalar → prim Value, ValArray → `Value::Array`,
  Variant/Nullable → inner Value, String → `Value::String`) instead of
  `into_scalar().to_value()`.
- **JIT** (`gir_jit.rs`): `compile_and_push_field(b, env, ctx, buf, body)`
  — the same per-shape push helper `StructNew` uses (prim → `push_<T>`,
  composite → `push_array` owned/borrowed, value-shape → `push_value`,
  string → `push_arcstr`).
- **`emit_gir`** (`MapImpl`/`Init`): element type is `body_kir.typ.clone()`
  (any GirType; only `Unit`/`Null` bail), result `Array(body.typ)`.

Walkers destructure `ArrayInit`/`ArrayMap` with `{ body, .. }`, so removing
the field only touched the interp/JIT arms + the two `emit_gir`s.

Flipped `array_init3` (`|i| (i, i*i)` → `Array<(i64,i64)>`) None → Jit;
added `array_map_tuple` (`|x| (x, x*2)`) to cover `map`'s composite output
without nesting. 147 → 149 Jit.

**Empirical correction:** `array_map1` (`array::map(a, |x| array::map(b,
|y| x+y))`) was expected to flip too — but it stays None for a *different*
reason than the output type (verified: it fuses-not, value is correct via
interp). The inner `array::map(b, ...)` captures the outer element `x`,
and that nested HOF doesn't lower yet (the capture + inner array input
aren't threaded into the inner kernel). Reverted its annotation to None
with the blocker documented; added the non-nested `array_map_tuple` so the
composite-output path is actually exercised.

Remaining `/array`: composite-element **input** (`array_find`/`find_map`
read `Array<(string,i64)>` *and* use destructure-pattern callbacks
`|(k,v)|` — two separate blockers: composite element read+bind, and
`ensure_lambda_kernel` accepting arg patterns); nested-HOF-with-capture
(`array_map1`); `array::group` (no `emit_gir`).

Suite green (132 compiler + 1818 graphix-tests).

### `/buffer` EFFECT=Sync + a latent value-shape-DynCall panic (Jun 2026)

The pure buffer builtins (`buffer::to_string`/`from_string`/`len`/
`concat`/`to_array`/`from_array`/`to_string_lossy`) fuse + JIT now. They
are `EvalCached` impls (`buffer.rs`) that never set `EFFECT`, so they
inherited `EvalCached`'s conservative `Async` default — and fusion
discovery (`try_register_builtin_call_from_callsite`) only registers
`Sync` builtins, so it skipped them entirely (FUSEBAIL
`callqual:/buffer/to_string` — the call fell through to the name-based
path and bailed on the qualified name). **Exactly the `str::escape`
class.** Fix: `const EFFECT: EffectKind = EffectKind::Sync` on all 9
`buffer.rs` `EvalCached` impls (every buffer op is pure binary
manipulation — no I/O).

That exposed a **latent general JIT bug**, not buffer-specific: the
value-shape-return `DynCall` arm in `compile_value_expr` (`gir_jit.rs`,
`ret_kind=2`) `debug_assert!`'d `matches!(return_type, Variant | Nullable)`
and *panicked* (release: UB) on a `bytes` return. `ret_kind=2` boxes any
`Value` into two words, so every value-shape return routes there —
widened the assert to `return_type.is_value_shape()`. This was latent
because no fixture had a bytes/datetime/duration/map-**returning** DynCall
until buffer; the fix covers all of them.

Flipped 7 simple buffer fixtures (149 → 156 Jit). Still None: byte
indexing/slicing (`bytes_index`/`bytes_slice*` — `a[i]` on bytes) and the
`encode`/`decode`/`varint`/`zigzag` family (callbacks take
`Array<Encode>`/`Array<Decode>` spec args — composite-element + ref
shapes, a separate cliff).

**Tooling note:** flipping single-line `run!(name, C, |v| {..});`
fixtures with a script needs *substring* matching of the
`}; …FuseExpect::None);` close, not line-equality — a line-equality pass
skips an inline close and corrupts the *next* fixture's annotation
(caught by the full suite: `bytes_index` flipped instead of `bytes_len`).

Suite green (132 compiler + 1818 graphix-tests).

### `GirOp::ValueEq` — non-primitive `==` / `!=` (Jun 2026)

`gir::cmp` only lowered comparisons whose operands were *primitive*
(`lhs.typ.as_prim().is_some()`); everything else returned `None`, so a
fused expression ending in `Map == Map`, `s == "foo"`, `tuple == tuple`,
etc. bailed. New `GirOp::ValueEq { ne, lhs, rhs }` lowers `==`/`!=` on any
non-prim same-typed operands (ordering operators on non-prim stay
unlowered; Unit/Null have no comparable form). It mirrors `ValueArith`:
- **helper** `graphix_value_eq(l: Value, r: Value) -> u8` — netidx
  `Value` `PartialEq`; CONSUMES both (the owned-operand contract).
- **interp**: compares both operands' `into_value()` (already general
  over every shape — scalar/string/composite/value-shape).
- **JIT** (in `compile_scalar_impl`, result is `Bool`): compiles both
  operands to owned `(disc, payload)` via `compile_owned_value_operand`,
  calls the helper, XORs with 1 for `!=`.

`compile_owned_value_operand` (shared with `ValueArith`) gained the
operand shapes `ValueEq` can present beyond datetime/duration: Variant/
Nullable (already value-shape → `ensure_owned_value`), String (compile to
the owned ArcStr, wrap via `graphix_value_new_string`), and composite
Array/Tuple/Struct (compile to `*ValArray`, `ensure_owned_composite` to
clone a Borrowed local, wrap via `graphix_value_new_from_array`). The
wrap helpers return a two-word `Value`, consumed by `graphix_value_eq`.

Flipped `map_insert`/`map_remove` (both end in `m == {literal}`). No
*existing* fixture was blocked solely on string/composite `==` (the full
suite flipped zero when that arm landed), so added `value_eq_string`,
`value_eq_string_ne`, `value_eq_tuple` to exercise + cover the new JIT
operand paths (don't ship untested codegen). 156 → 161 Jit (560 fixtures).

Walker note: `ValueEq` shares `ValueArith`'s `{ lhs, rhs, .. }` shape, so
adding it to each recursion site was `| GirOp::ValueEq { lhs, rhs, .. }`
beside every `GirOp::ValueArith { lhs, rhs, .. }` (plus the `node_shape`
`GirOpTag`). A `replace_all` only hit the arms with `=>` on the same
line; the multi-line OR-group heads (collect-walkers) needed manual arms.

Suite green (132 compiler + 1818 graphix-tests; the 3 new lang fixtures
bring graphix-tests to 1827).

### `GirOp::BytesIndex` — `bytes[i]` (Jun 2026)

`bytes[i]` now fuses + JITs. Extracted a shared
`node::array::bytes_index(b: &PBytes, i: i64) -> Value` (bounds-checked,
negative-from-end, → `Value::U8` or the OOB error — same seam as
`array_index`) and refactored the node-walk `ArrayRef::update`'s two
inline bytes arms to call it, so node-walk / gir-interp / JIT agree
bit-for-bit. `emit_array_ref` tries the array path first
(`resolve_array_input` → `ArrayGet`); if that returns `None` it
`emit_node`s the source and, when its type is `GirType::Bytes`, emits
`GirOp::BytesIndex { bytes, idx }` typed `Nullable<u8>`
(`[u8, Error<…>]`). JIT (in `compile_value_expr`, value-shape result):
`graphix_bytes_index(v: Value, i: i64) -> Value` over an *owned* bytes
operand (`compile_owned_value_operand`, which the `ValueEq` work already
taught to wrap any value-shape/string/composite). Flipped `bytes_index`
(`b[0]`) + `bytes_neg_index` (`b[-1]`); 161 → 163. Still None in
`/buffer`: `bytes_slice*` (`b[a..c]?` — needs slice lowering + the `?`
error operator) and the `encode`/`decode`/`varint`/`zigzag` family.

Suite green (132 compiler + 1827 graphix-tests).

### Abstract-type resolution in fusion (Jun 2026)

A cross-module call to a graphix function whose arg/return is an
**abstract** type couldn't fuse. Abstract types are declared opaque in
a `.gxi` (`type T;`) with the concrete definition private in the `.gx`
(`type T = i64`). At the call site `inner::get(inner::make(42))`, the
arg type reaching `ensure_lambda_kernel` is a `Type::Ref("T")` — and
`GirType::from_type` has no `Env`, so it can't resolve the name to the
concrete rep and returns `None`, bailing the kernel build. (An empirical
`elk` probe pinned it: `arg '/inner/get' typ=T from_type=None`.)

**Two pieces:**

1. **`gir::ABSTRACT_REGISTRY`** — a global
   `LazyLock<RwLock<IntMap<AbstractId, Type>>>`. `node::module::check_sig`,
   which already builds the transient abstract→concrete map to verify the
   impl matches the signature, now also persists each entry
   (`td.typ.scope_refs(&scope.lexical)`) keyed by the globally-unique
   `AbstractId`. The type system stays unchanged — abstracts remain
   opaque everywhere; only the optimizer consults the registry.

2. **`resolve_abstract(typ, env, depth)`** (fusion-internal, in
   `fusion/lowering.rs`) — resolves `Type::Ref` via `lookup_ref(env)` and
   `Type::Abstract { params: [] }` via the registry, recursing through
   `Tuple`/`Array`/`Set`/`Struct` composites so nested abstracts resolve
   too. A depth cap (16) + return-as-is-on-failure makes recursive types
   (`List<'a> = [`Cons('a, List<'a>), `Nil]`) terminate — `from_type` then
   yields `None` and they simply don't fuse, which is correct. Applied at
   the 3 `GirType::from_type` sites in `ensure_lambda_kernel` (arg,
   capture, return). `from_type`'s own `Type::Abstract` arm stays as a
   defensive net for any direct-`Abstract` path; the registry lookup is a
   no-op when the program has no abstract types.

`params.is_empty()` gates both the registry insert-consumer and
`from_type`'s arm, so **parameterized abstracts** (`Box<'a>` — the
registry holds the unsubstituted body) correctly stay `None`.

Flipped **11 `/inner` fixtures None → Jit** (`abstract_type_basic`,
`byref`, `multiple`, `different_modules`, `in_array`, `in_tuple`,
`map_key`/`map_value`/`map_key_and_value`, `nested_module`,
`two_modules_combined`) and **`abstract_type_in_typedef` None → Interp**
(its `Pair = {first: First, second: string}` struct param has a string
field — a composite-with-string *kernel param* isn't JIT-lowerable yet,
so it fuses on the interpreter). 163 → 174 Jit, 0 → 1 Interp. Still None:
parameterized abstracts, `recursive` (`List`), `in_variant` (abstract
variant payload), `struct_impl` (cross-module string return).

Suite green (132 compiler + 1827 graphix-tests).

### `GirOp::MapRef` — `m{key}` map access (Jun 2026)

`m{key}` now fuses + JITs, the same value-shape-result seam as
`bytes[i]`. Extracted a shared `node::map::map_get(src: &Value, key:
&Value) -> Value` (the looked-up value, or the `MapKeyError` "map key
not found" error — the exact `[V, Error]` the node-walk `MapRef`
produced inline) and routed the node-walk `MapRef::update`, the
fusion interpreter, and the JIT through it so all three agree
bit-for-bit. `emit_map_ref` (`fusion/lowering.rs`, dispatched from
`emit_node`'s new `NodeView::MapRef` arm) lowers a `MapRef` node whose
source is `GirType::Map` to `GirOp::MapRef { map, key }` typed
`Nullable<V>` (from the node's `[V, Error]` Set result via `from_type`).
Both operands compile through `compile_owned_value_operand` →
`graphix_map_ref(map: Value, key: Value) -> Value` (consumes both,
returns the result Value's two words — same 4-word-in / 2-word-out
helper sig as `graphix_value_add`). `classify_composite_source` treats
`MapRef` as `Owned` (its result is a fresh Value). Flipped `map_ref0`
(`m{"b"}`), `map_ref1` (int key), `map_ref2` (bool key), and
`map_ref_missing` (the error arm); 174 → 178 Jit. Still None:
`map_nested`/`map_complex_keys`/`map_with_arrays` (their composite map
*literals* don't constant-fold — `emit_map_new` bails on
non-constant/composite entries, a separate cliff) and
`map_ref_wrong_type` (a typecheck error — correct-None). Remaining map
ops: `map::change`/HOFs (`map`/`filter`/`fold`/`iter` — MapQ-style
codegen over `CMap`) and `map::insert`/`remove`.

Suite green (132 compiler + 1827 graphix-tests).

### Composite-literal const-folding in map literals (Jun 2026)

`node_const_value` (the const-folder behind `emit_map_new`, which builds a
map literal as one `GirOp::ConstValue`) only handled `Constant` /
`ExplicitParens` — so a map literal whose value was itself a composite
literal (`{"outer" => {"inner" => 42}}`, `{"nums" => [1,2,3]}`) bailed,
defeating the whole map literal's fusion. Now it recurses into
`NodeView::Array` / `Tuple` (→ a flat `Value::Array(ValArray)`, the shared
array/tuple runtime shape) and `NodeView::Map` (→ `Value::Map`) when every
element folds. Two shared helpers: `const_valarray(elems)` and
`const_map(keys, vals)` — the latter now also backs `emit_map_new` (was a
near-duplicate inline loop). Flipped `map_nested` None → Jit (178 → 179).
Did NOT flip `map_with_arrays` (its `m{"nums"}` result type is a
heterogeneous union `[Array<i64>, Array<string>]` — `from_type` returns
None; a union-value-lowering cliff, not const-folding) or `map_complex_keys`
(struct keys are `let`-bound `Ref`s, which `node_const_value` doesn't
resolve — needs let-const-propagation). The recursion is broadly useful
beyond maps: any nested-composite constant in a fused expression now folds
to one `ConstValue`.

Suite green (132 compiler + 1827 graphix-tests).

### `GirOp::ArraySlice` — `a[i..j]` (Jun 2026)

`a[i..j]` / `a[i..]` / `a[..j]` / `a[..]` now fuse + JIT, the same
value-shape-result seam as `bytes[i]` / `m{key}`. Two shared helpers in
`node::array`: `array_slice(src, start, end)` (usize bounds — the
Array/Bytes subslice + out-of-bounds / type errors; the node-walk
`ArraySlice::update` is refactored to call it) and `array_slice_i64(src,
start, end)` (the fused-kernel representation — i64 bounds → usize, a
negative bound is the node-walk's "expected a non negative number" error).
`emit_array_slice` lowers an `ArraySlice` node (source `GirType::Array` or
`Bytes`, optional integer-scalar bounds) to `GirOp::ArraySlice { source,
start, end }` typed `Nullable<source>` (the node's `[source, Error]` via
`from_type`). Interp calls `array_slice_i64`; JIT routes through
`compile_value_expr` calling `graphix_array_slice(src: Value, start, end,
flags)` (flags bit0/bit1 = start/end present; absent bounds pass 0). Source
compiles via `compile_owned_value_operand` (wraps the array/bytes operand
into an owned `Value`); `classify_composite_source` treats `ArraySlice` as
`Owned`. Flipped `array_indexing1`/`2`/`3`/`4` (all four bound
combinations) None → Jit + added 2 error-path fixtures (`array_slice_oob`,
`array_slice_oob_is_err` — OOB slice → error, agreeing across all three
backends). 179 → 185 Jit. The helper also handles `Bytes`, so the 3
`bytes_slice*` fixtures (`buffer::to_string(b[1..4]?)`) flipped None →
**Interp**: the ArraySlice + `?` + `buffer::to_string` chain now fuses on
the interpreter, but JIT-compiling a `QopUnwrap` whose success type is
value-shape (bytes) isn't wired yet (`compile_value_expr`'s QopUnwrap arm),
so jit mode falls back to interp (1 → 4 Interp). Wiring that arm is the
follow-up to make them Jit.

Suite green (132 compiler + 1833 graphix-tests).

### Value-shape `QopUnwrap` JIT codegen (Jun 2026)

The follow-up the ArraySlice entry flagged: `?` (`GirOp::QopUnwrap`) on a
`Nullable<T>` whose success `T` is **value-shape** (Variant / Nullable /
DateTime / Duration / Bytes / Map) had no `compile_value_expr` arm — the
scalar arm (`compile_scalar_impl`) only handled Prim / String / composite-
pointer success types and `Err`'d on value-shape, so a value-shape `?` chain
fell back to the interpreter. Added the `compile_value_expr` arm:
`compile_expr` already routes a value-shape-typed `QopUnwrap` here (its
`typ == success_typ`, set by `wrap_qop`). The inner `Nullable<T>` compiles
to an owned `(disc, payload)`; an `icmp_imm Equal disc Typ::Error` branches
to a `pre_pending` block that **drops the owned error Value**
(`graphix_value_drop`), signals pending (`graphix_dyncall_set_pending`),
runs `emit_pending_cleanup`, and jumps to `pending_exit`; the non-error path
passes the `(disc, payload)` straight through as the result `T` (the
consumer takes ownership — no clone). The error-path drop is the correctness
nuance the scalar arm omits (its error Value leaks on the rare pending
path); the value-shape arm does it right. Flipped the 3 `bytes_slice*`
fixtures Interp → Jit (the `b[1..4]?` → `buffer::to_string` chain JITs
end-to-end). 185 → 188 Jit, 4 → 1 Interp.

Suite green (132 compiler + 1833 graphix-tests).

### `{s with f: x}` (StructWith) via expand-to-StructNew (Jun 2026)

A struct functional-update `{s with f: x, ...}` now fuses — with **no new
GirOp**. The struct's type (hence its sorted field set) is known at compile
time, so `emit_struct_with` (`fusion/lowering.rs`) expands it into a
`GirOp::StructNew` whose fields are, in sorted order, either the per-field
replacement value (`emit_node` of the `with` expr) or a `GirOp::StructGet`
copying the unchanged field from the source. `source` must be a `Ref` to a
struct kernel input (same constraint as `emit_struct_ref`); the
`StructInput`'s `name`/`fields` are cloned up front so the `find_struct`
borrow drops before the per-field `emit_node` calls. Both `StructNew` and
`StructGet` are already lowered on both backends, so nothing else changed.
Flipped `structwith2` (`{selected with y}` field-shorthand) and
`structwith3` (`{selected with y: selected.y + 1}` — the replacement reads
the source) None → Jit. Stays None: `structwith0` (typecheck error —
replacing a `string` field with an `int`); `structwith1` (its struct has a
`string` field copied via StructGet — the composite-with-string-in-block
cliff, same as `structaccessor`); `structwith4`/`5` (`node:Lambda`). 188 →
190 Jit.

Suite green (132 compiler + 1833 graphix-tests).

### Composite-element + destructure-pattern HOF fusion — foundation (Jun 2026)

The largest remaining tractable cluster (composite array HOFs + the whole
map-HOF cluster) is blocked by **four compounding cliffs**: composite-
element input, `|(k,v)|` destructure-pattern callbacks, string-in-composite
locals, and CMap iteration. Full multi-phase plan in
`design/composite_hof_fusion.md`. **Phases 1–2 (interp) landed** —
`array::map(a, |(k, v)| k + v)` over `Array<(i64,i64)>` now fuses
(interp-first; `array_map_destructure` fixture, `Interp`).

- **Phase 1 (trait widening):** `MapFn`/`FoldFn::emit_gir` `in_elem`
  `PrimType` → `GirType` (graphix-package-core); `MapQ`/`FoldQ::emit_gir`
  pass `ai.elem.clone()`. The 6 array impls keep `as_prim()?` (composite
  still bails there) — behavior-preserving.
- **Phase 2 (destructure):**
  - `StructPatternNode::tuple_leaves(&self) -> Option<Vec<(BindId,
    usize)>>` (node/pattern.rs) — `node::pattern` is `pub(crate)`, so core
    reaches the `|(k,v)|` leaves through this accessor rather than matching
    the enum.
  - `register_kir_binding_bid` — tags a kernel slot with a `BindId` (today
    `register_kir_binding` hard-coded `None`); the Ref arm prefers
    `lookup_local_by_bind_id`, so a leaf slot tagged with the pattern's
    BindId resolves the body's `Ref`s without a source name.
  - `FusionCtx::input_snapshot`/`input_restore` (scoped multi-slot
    registration) + `emit_hof_body_destructured(ctx, ec, body, elem_local,
    in_elem, leaves)` — registers each leaf (synthetic `__leaf_<bindid>`
    name, BindId-tagged) and builds the body as `Block { lets: [Let{leaf,
    TupleGet(elem_local, pos)}…], tail: body }`. The body references the
    leaves, not `elem_local`, so only the leaves need registering; the leaf
    lets reference `elem_local` via manually-built `TupleGet`.
  - `MapFn`/`FoldFn::emit_gir` gain `elem_binds: &[(BindId, usize)]`;
    `MapQ`/`FoldQ::emit_gir` extract it via `tuple_leaves` (synthesizing a
    `__elem` name for the composite element). `MapImpl` lowers the
    destructure; the 5 other array impls explicitly bail on non-empty
    `elem_binds`.
  - `GirOp::ArrayMap.in_elem`: `PrimType` → `GirType`. The interp loop
    (`gir_interp.rs`) branches on `in_elem.as_prim()` — `Some` = today's
    scalar `locals` fast path; `None` = composite, binds the element
    `Value::Array` into the `arrays` slot per-iter (the body's `TupleGet`
    reads it). The JIT arm `as_prim().ok_or(…)?`-bails on composite (→
    interp).

**Phase 2-JIT** then flipped the new `array_map_destructure` fixture
(`[(1,2),(3,4)]`, `|(k,v)| k+v` → `[3,7]`) Interp → **Jit**: the JIT
`ArrayMap` arm branches on `in_elem.as_prim()` — `Some` = scalar fast
path; `None` = composite, per-iter `graphix_valarray_get_array` (owned
`*mut ValArray` clone) → `bind_composite(elem_local)` → body (`TupleGet`s
clone the fields) → `graphix_valarray_drop` of the owned per-iter element
(no leak/double-free — getter and field reads clone, the drop frees the
clone). 190 → 191 Jit, 2 → 1 Interp.

**`array::fold` extended the same way** (`array_fold_destructure`,
`array::fold(a, 0, |acc, (k,v)| acc+k+v)` → `10`, **Jit**):
`ArrayFold.elem_typ` `PrimType` → `GirType`; the interp + JIT loops got the
identical scalar/composite element split (the accumulator stays scalar);
`FoldImpl::emit_gir` lowers the destructure via `emit_hof_body_destructured`
nested inside a `with_input` for the scalar acc. 191 → 192 Jit. The
composite-element + destructure mechanism is now proven on **two** HOFs.

**`array::filter` extended too** (`array_filter_destructure`,
`array::filter(a, |(k,v)| v>3)` → kept composites, **Jit**):
`ArrayFilter.elem` `PrimType` → `GirType`; the interp loop pushes the
*original* composite element on a `keep`. The JIT `ArrayFilter` arm
branches scalar/composite: composite gets each element via
`graphix_valarray_get_array` (owned `*ValArray`), binds it, evals the
predicate, then on `keep` **moves** it into the output
(`graphix_value_buf_push_array`) / on not-keep **drops** it
(`graphix_valarray_drop`) through a dedicated `drop_block` — the owned
per-iter element is consumed exactly once (no leak / double-free). This is
the **conditional-drop** wrinkle distinct from map/fold's unconditional
consume. (Watch for over-broad `perl` edits on `*elem` — they silently hit
the sibling `ArrayFind`/`ArrayFilterMap` arms whose `elem` is still
`PrimType`; the build caught it but scope `*elem` edits per-arm.)

**All 3 array HOFs (map/fold/filter) now JIT composite elements +
destructure.** Remaining (tracked, task #149): the other 3 array HOFs
(`Find`/`FilterMap`/`FlatMap` — `Find` also needs composite *output*
`Nullable<composite>`, `FilterMap` a conditional transform); composite-with
-string elements (string leaf → interp; flips the existing `find`/`find_map`
cliffs which ALSO need composite-output / the `~` Sample operator); and
Phase 3–4 = map `MapFn` impls + CMap iteration for the map-HOF cluster.

Suite green (132 compiler + 1842 graphix-tests).

### Sample operator `~` lowering (guarded) (Jun 2026)

`a ~ b` (sample `b` when `a` fires) now lowers in a fused kernel — to
`b` — but **only when sound**. The subtlety: a fused sync kernel
recomputes its *whole* output whenever *any* input fires, whereas `~`
emits `b` *only* when the trigger `a` fires and holds its value between
`a`'s firings. So `a ~ b ≡ b` is sound iff `b` can't update without `a`
also updating — i.e. **every external ref of the value `b` is also an
external ref of the trigger `a`** (a recompute caused by a value-dep
change then implies a trigger-dep change too). `Sample` is a *stateful*
node (a `triggered` counter that gates emission), so lowering it loses
that gating — the guard is what keeps the loss sound. The `emit_node`
`NodeView::Sample` arm walks `s.trigger.refs()` and `s.arg.node.refs()`,
collects the trigger's external refs, and lowers `emit_node(arg)` only if
every arg external ref is in that set; otherwise it bails (the Sample
doesn't fuse). The trigger's refs still surface as region inputs via
`Sample::refs`, so the kernel re-fires on `a`. Flips `array_filter_map`
(its `false => x ~ null` arm — `null` has no external refs, trivially ⊆
the trigger's) None → **Jit** (193 → 194); `array_filter_map_scalar`
already fused (no `~`). The common reactive `trigger ~ state` shape
(where `state` depends on inputs the trigger doesn't) correctly does
*not* fuse.

Suite green (132 compiler + 1842 graphix-tests).

### `GirOp::ArrayFindMap` — `array::find_map` (Jun 2026)

Closes the `array_find_map` cliff (composite `(string,i64)` element +
`|(k, v)|` destructure + a `false => v ~ null` Sample arm — all the pieces
landed above). The new op = `ArrayFilterMap`'s Nullable body + `ArrayFind`'s
early-exit: iterate, eval the body (`Nullable<out>`), return the **first
non-null** body result as that `Nullable<out>` (or `null`). `in_elem` is a
`GirType` (composite-capable), bound via the proven scalar/`arrays`-slot
split. `FindMapImpl::emit_gir` (previously absent → DynCall) builds it with
the `elem_binds.is_empty()` scalar vs `emit_hof_body_destructured`
composite branch. **Interp-first**: `ArrayFindMap` is value-shape
(`Nullable<out>`) and composite-input; the JIT arm `Err`s ("not yet
JIT-lowered") so `compile_value_expr`'s catch-all → `fuse()` runs it on the
interpreter — lands **Interp** (1 → 2 Interp). The walker/`node_shape`
exhaustive arms mirror `ArrayFilterMap` (`{ body, .. }`); added a
`GirOpTag::ArrayFindMap`.

**JIT done (for all-prim composite elements).** The `compile_value_expr`
`ArrayFindMap` arm mirrors `ArrayFind`'s early-exit value-shape merge, but
compiles the body to its `Nullable<out>` `(disc,payload)` directly (not a
predicate) and drops the owned per-iter composite element *every* pass (the
result is the body value, not the element — unlike `ArrayFind` where the
found result IS the element). `(bdisc,bpayload)` computed in `loop_body`
dominates `found`, so the found edge jumps to the `exit` phi with them
directly (no `found` block params). New `array_find_map_prim`
(`array::find_map([(1,10),(2,20),…], |(k,v)| select k==2 {true=>v,
false=>null})` → `20`) fuses + **Jit** — validating the codegen on an
all-prim `(i64,i64)` element. The sibling `array_find_map` **stays
Interp**: its `(string, i64)` element keeps the kernel off the JIT — a
separate **composite-with-string-element** cliff (the all-prim case JITs
cleanly through the very same arm, proving it's the string field, not
`ArrayFindMap`). 194 → 195 Jit.

Suite green (132 compiler + 1845 graphix-tests).

### Block-let-String JIT codegen gap (Jun 2026)

Found by a `JITDBG` probe (temporary `eprintln!` on the swallowed
`compile_kernel_with_callees` error at `fusion/mod.rs`, since `fuse()`
silently `continue`s on a JIT-compile `Err`): the `GirOp::Block` let arm
in `gir_jit.rs` (~3167) **errored** on a `GirType::String` value
("String locals aren't supported on either backend"), even though the
May-2026 *String locals* work made them first-class. That work updated
`GirStmt::Let` + `bind_string` but **missed the parallel `GirOp::Block`
let arm** — so any kernel with a string `let` *inside a block* (vs a
top-level `GirStmt::Let`) silently failed JIT compile and fell back to
interp. The most common trigger: a `|(k, v)|` destructure callback over
`Array<(string, _)>`, where `emit_hof_body_destructured` wraps the body
in a `Block` whose leaf `let k = TupleGet(elem, 0)` binds a string local.
Fix mirrors `GirStmt::Let`'s String arm: `compile_scalar` (the String SSA
is an owned ArcStr) → `bind_string`; the block scope-exit already drops
`env.strings`. This is a correctness/completeness fix beyond the metric —
string locals now JIT in *all* contexts. It flipped `array_find_map`
(`|(k, v): (string,i64)|` …) Interp → **Jit** (195 → 196); the suite found
no other fixture exercising the gap. **Lesson** ([[feedback-run-the-code]]):
the swallowed-error `continue` in `fuse()` hides JIT-compile failures as
silent interp fallback — a `JITDBG` env probe on that arm is the way to
surface *why* a should-JIT kernel runs on the interpreter.

Suite green (132 compiler + 1845 graphix-tests).

### `array::flat_map` composite element + destructure (Jun 2026)

The 5th and last linear/conditional array HOF gets composite-element +
destructure support, completing the foundation: **map / fold / filter /
flat_map / find_map all JIT composite elements + `|(k,v)|` destructure
callbacks**. `GirOp::ArrayFlatMap.in_elem` widened `PrimType` → `GirType`
(mirror of the earlier 4); the interp + JIT loops got the identical
scalar/composite element split — `in_elem.as_prim()` is `Some` for the
scalar `locals`/`valarray_get_<prim>` fast path, `None` for the composite
`arrays`-slot (interp) / `graphix_valarray_get_array` owned-clone + `bind_
composite` + per-iter `graphix_valarray_drop` (JIT) path. `flat_map`'s
output stays an owned `Array<out>` per iteration that the existing linear
`graphix_value_buf_extend_from_array` concatenates + drops — so the
composite *element* is the only new owned per-iter value to drop (the
`Some(elem_var)` guard drops it after the body, before `extend`). No
early-exit / conditional-drop wrinkle (unlike `find_map` / `filter`).
`FlatMapImpl::emit_gir` lost its `!elem_binds.is_empty() => None` bail and
now mirrors `MapImpl`/`FilterImpl`: scalar via `with_input`, destructure
via `emit_hof_body_destructured`. New `array_flat_map_destructure`
(`array::flat_map([(1,10),(2,20)], |(k,v)| [k,v])` → `[1,10,2,20]`) fuses +
**Jit**. 196 → 197 Jit.

**Composite-element + destructure is now proven across all 5 array HOFs.**
Remaining (tracked, task #149): composite-with-**string** elements (string
leaf forces interp via the `find_map` `(string,i64)` case — would flip the
existing `array_find`/`array_find_map` cliffs); composite *output* for
`find` (`Nullable<composite>`); and the map-HOF cluster (CMap iteration
GirOps + `MapFn` impls).

Suite green (132 compiler + 1848 graphix-tests).

### `array::find` composite element + composite output (Jun 2026)

Closes the last array-HOF composite gap: `array::find` over a composite
element where the *result* is the matched element itself — a
`Nullable<composite>` (the composite-**output** case, distinct from the
composite-**element** case the other HOFs needed, since `find` doesn't
transform the element). `GirOp::ArrayFind.elem` widened `PrimType` →
`GirType`; `FindImpl::emit_gir` gained the destructure
(`emit_hof_body_destructured`) + composite-element branch and types the
result `Nullable<in_elem>` (composite).

The interp arm got the standard scalar/composite element split; `found`
holds `arr[i].clone()` (the whole `Value::Array`) for composite, surfaced
as `EvalResult::Nullable(Value::Array(..))`.

The JIT arm is the interesting half — a **conditional-consume** of the
owned per-iter composite element, like `ArrayFilter` but feeding a
value-shape early-exit merge: the element is fetched owned via
`graphix_valarray_get_array`, bound (`bind_composite`) for the predicate;
on the **found** edge it's wrapped into a value-shape
`(ARRAY_DISC, payload)` Value via `graphix_value_new_from_array` (consumes
it) and jumps to the two-word merge; on the **advance** edge (not matched
this iteration) it's dropped via `graphix_valarray_drop`. The scalar case
is unchanged (found wraps via `prim_to_value_disc` + `scalar_to_payload_i64`,
no drop). The `(elem_var, elem_prim)` tuple from the bind match carries the
`Variable` + scalar-prim flag to both exit edges (cranelift SSA: `elem_var`
is defined in `loop_body`, which dominates `found`/`advance`).

`array_find` (`(string, i64)` element, `|(k, _)| k == "bar"`) flipped
None → **Jit**; new `array_find_composite` (`(i64, i64)`, `|(k, _)| k == 2`
→ `(2, 20)`) exercises the composite-output JIT codegen without a string
leaf. **All 5 array HOFs now JIT composite elements + `|(k,v)|` destructure,
and `find` JITs composite output.** 197 → 199 Jit.

Suite green (132 compiler + 1851 graphix-tests).

### Adversarial review of the fusion/JIT batch — 9 ownership/divergence fixes (Jun 2026)

After the user committed the session's fusion/JIT work (range
`ec08094..HEAD`, ~10k lines), a multi-agent adversarial review
(7 finder angles → perspective-diverse verification → synthesis,
~50 agents) surfaced **14 findings → 9 distinct confirmed bugs**,
all fixed. The headline lesson reinforces [[feedback-run-the-code]]:
the bugs were in **untested paths** (negative/error/no-match inputs,
Borrowed-vs-Owned classification, pending edges) that the 3-mode
harness — which only checks *value* equality on the *tested*
fixtures — structurally cannot see. Most are leaks/double-frees
invisible without allocator instrumentation; the review's ownership
trace caught them by reading every control-flow path.

**The fixes** (all `gir_jit.rs` unless noted):

1. **CRITICAL — value-shape `QopUnwrap` double-free.** The `?` error
   edge unconditionally `graphix_value_drop`'d its inner `(disc,
   payload)`, then `emit_pending_cleanup` → `drop_owned_composites`
   dropped the env slots too. A Borrowed (Local-read) inner is owned
   by its env slot → double Arc decrement / UAF. Fix: drop on the
   error edge only when `classify_composite_source(inner) == Owned`;
   on the success edge route through `ensure_owned_value` so the
   result matches QopUnwrap's now-`Owned` classification (a Borrowed
   inner gets cloned, else it'd alias the env slot the consumer also
   drops). The interp `?` never double-dropped — JIT-specific.

2. **`classify_composite_source` missing owned producers.**
   `ConstValue` (datetime/duration/bytes/map literal — owned via
   `graphix_value_clone_from_static`'s Arc bump), `ValueArith`
   (owned via `graphix_value_<op>`), `ArrayFindMap` (owned
   `Nullable<out>`), and `QopUnwrap` all fell to the `_ => Borrowed`
   catch-all → `ensure_owned_value` re-cloned + `mem::forget`'d the
   original → **+1 refcount leak per kernel execution** (unbounded in
   a reactive kernel). Live fixtures `bytes_const_local`, `map0`,
   `datetime_arith*` leaked. Fix: add all four to the Owned arm.

3. **value-shape `Block` arm leaked block-local Strings.** Its
   scope-exit dropped composites/variants/nullables but not
   `env.strings` (the scalar Block arm had it). A `|(k,v)|`
   destructure over `Array<(string,_)>` (the `array_find_map` shape)
   binds a String leaf into a Block → one ArcStr leak per element.
   Fix: add the `env.strings[mark..]` `graphix_arcstr_drop` loop.

4. **scalar `QopUnwrap` missing error-edge drop.** Asymmetric with
   the value-shape arm — the scalar `?` pending edge never dropped
   its owned inner error Value → leak per failed `?` (`str::parse
   (...)?`). Fix: drop if Owned. Also made its String/composite
   success path clone a Borrowed inner (QopUnwrap is now Owned).

5. **`array::init` negative-`n` runtime abort.** The node-walk clamps
   `n.max(0)` → `[]`; both fused backends aborted — interp
   `as_usize()` wraps `-1` to `usize::MAX` → `reserve` panic; JIT
   `buf_new(neg)` reserves `usize::MAX` → panic across the FFI
   boundary (UB). Fix: clamp `max(0)` in both (interp `as_i64().max
   (0)`, JIT `select(n<0, 0, n)` before `buf_new`). Regression:
   `array_init_negative` → `[]` all 3 modes.

6. **`compile_and_push_field` value-shape dispatch desync.** The
   helper-selection routed all six value-shapes to `push_value`
   (3-arg), but the compile-dispatch only sent `Variant|Nullable`
   down the 2-register path → a DateTime/Duration/Bytes/Map field in
   a tuple/struct/array literal fell to `.single()`, Err'd, and
   silently **de-fused the whole kernel**. Fix: both dispatches key
   on `is_value_shape()`. Regression: `tuple_duration_field`
   (`(duration:1.s, 2)`) None → Jit.

7. **`array_slice_i64` negative-bound payload divergence**
   (`node/array.rs`). The fused path rejected negatives with
   "expected a non negative number"; the node-walk's
   `cast_to::<usize>()` does `i as usize` (verified in vendored
   `convert.rs:283`), wrapping `-1` → `usize::MAX` → a different
   "out of bounds" error. Fix: `array_slice_i64` now wraps `i as
   usize` too — it's a thin wrapper over `array_slice` (the
   node-walk's own fn), so all three backends produce the identical
   error by construction. Regression: `array_slice_negative`.

8. **HOF output buf leaked on a mid-loop pend** (latent). The 5
   accumulating arms (Map/Filter/FilterMap/FlatMap/Init) allocated
   the output `buf` as a bare SSA value registered nowhere; a
   value-shape/composite DynCall or `?` in the body that pends jumps
   to `pending_exit`, and `emit_pending_cleanup` dropped only the
   env + DynCall-arg bufs — the half-built output buf (+ accumulated
   elements) leaked. Fix: new `register_hof_buf` / `unregister_hof_buf`
   helpers mirror the DynCall arg-buf registration (push the buf
   Variable onto `dyncall_buf_stack` after `buf_new`, pop before
   `finalize`). The ~20 existing HOF JIT fixtures regression-cover
   the normal (non-pending) push/pop path.

The review also **confirmed clean** (no real bug) on: composite
producer/consumer refcounting, the ABI kind-grouped ordering across
all consumers, the `ValueEq`/`BytesIndex`/`MapRef` seams, and
cranelift block/SSA discipline in the new loops.

Fixes verified: 132 compiler + 1863 graphix-tests green (+12 over the
prior 1851 — three regression fixtures × 3 modes + the earlier
flat_map/find composite work). The leak fixes (#2–#4, #8) aren't
observable in the value-only harness; they're sound-by-construction
(mirroring proven owned/borrowed discipline) and the suite confirms
no regression on the common paths.

### `GirType::Error` (Value-shape) — `error(v)` fusion (Jun 2026)

`error(v)` — the core `fn(e: 'a) -> Error<'a>` Sync builtin — now fuses
+ JITs. Runtime `Value::Error(Arc<Value>)` is a refcounted thin pointer,
so `GirType::Error` is a **Value-shape** leaf reusing the *entire*
two-register `Value` pipeline (the bytes/map pattern): `is_value_shape()`,
the kind-grouped ABI, `graphix_value_clone`/`drop`, DynCall value-shape
arg/return marshalling (`ret_kind=2`), and the `value_inputs`/`nullables`
slots — **no error-specific JIT helpers, ops, or slot lists**. `error()`
lowers as an ordinary value-shape `DynCall`; no new `GirOp`.

The only real change beyond the mechanical exhaustiveness sweep:
`GirType::from_type` gained a **bare** `Type::Error(_) => GirType::Error`
arm. The `[T, Error]` Result/Option shape is unaffected — it still lowers
via the `Type::Set` arm to `GirType::Nullable` (the error becomes the null
case); only a *standalone* `Type::Error` (like `error`'s `-> Error<'a>`
return) routes to the new leaf. `is_dyncall_supported` + the `fuse()`
return-type filter accept it (was the blocker — `from_type` returned None,
so `error` was never discovered as a DynCall).

The 14th-commandment exhaustiveness check drove the rest: adding the
variant produced ~29 non-exhaustive match errors, each a value-shape
dispatch site that gained `| GirType::Error` (one `perl` pass over the
`| GirType::Bytes | GirType::Map` inline groups, then 2 standalone arms —
`gir_type_to_graphix_type` → `Type::Error(tvar)`, and the `fuse()` filter).
`EvalResult` needs no new carrier — `wrap_value_shape`'s `_ => DateTime`
catch-all + `into_value`'s unwrap-any-carrier already marshal it (verified:
all 3 modes return `Value::Error`).

Flipped `error` None → **Jit** (203 → 204). `filter_err` stays None — it's
a streaming/Async builtin over `array::iter`, a different cliff.
132 compiler + 1863 graphix-tests green.

### Impure HOF callback fusion — maximal sync-subgraph split (Jun 2026)

The real M8.4 work, landed end-to-end: an **impure** HOF callback
(`array::map(a, |x| { let v = bigcalc(x); publish(p, v); v })` — a sync
calc followed by an async op) now **splits at the async boundary**,
fuses + JITs the sync sub-region, and runs it per-slot while the async
residue stays interpreted. User's framing: "scan a node subtree, split
it at async ops into multiple subtrees, fuse the parts with no async ops
and make them inputs into the async parts."

**Why a split, not whole-body fusion.** A HOF callback runs **per array
element** via MapQ's per-slot machinery (`update()`'s
`while self.slots.len() < a.len()` loop — one graph per element, fed via
`ctx.cached.insert(s.id, v)`). Async ops keep per-slot state (a
`publish`/`<-`/`subscribe`/`count` is keyed by compile-time BindId), so
the async residue **must** stay per-slot — it can't be hoisted into one
shared kernel. Only the **pure** sub-computation is BindId-free and thus
shareable as one `Arc<GirKernel>`. The sync/async boundary coincides
exactly with the shareable/rebuild boundary.

**Concrete types are on the CallSite, not the Lambda.** A `LambdaDef`
carries unbound polymorphic TVars; the monomorphized `FnType` lives on
the call site (`CallSite.resolved_ftype` via `resolve_tvars()`). So
fusion of a HOF callback is a **CallSite-phase** operation — same
constraint as `NeedsCallSite` builtins. `MapQ::static_resolve_fn_args`
(a compile-time `BuiltIn` hook) is where the callback's synthetic,
concretely-typed CallSite (`analysis_pred`) is built and where
`fuse_callsite` runs.

**Compile-time split engine** (`fusion/lowering.rs`):
- `fuse_callsite(cs, ec) -> Option<FusedCallback>`: gets the callback
  `g` via `cs.resolved_apply()` → `ApplyView::Lambda(g)`; tries
  `build_lambda_kernel` on the **whole** body first (the pure-callback
  Phase-1 path); on `None` (body has async ops) falls to
  `build_body_split`.
- `build_body_split(body, ec) -> Vec<SplitKernel>`: walks the body
  `Block`'s children and, for each `let`-value, tries `build_region` on
  it. **`build_region` succeeding *is* the sync/async classifier** — it
  bails on async ops / unrepresentable types, so a success is exactly "a
  fusable sync sub-region." Each → a `SplitKernel { value_id, kernel:
  Arc<GirKernel>, wrapped: Option<WrappedKernel>, inputs, … }`. Reuses
  the same `collect_region_inputs` + `walk_node_for_builtin_calls` +
  `build_region` + `compile_kernel_with_callees` machinery `fuse()` uses
  for the program tree, pointed at the callback body.
- `FusedCallback.kernel: Option` (None = split path) + `split:
  Vec<SplitKernel>`; `is_split()` = `kernel.is_none() && !split.is_empty()`.
- `FusedCallback::splice_into_body(ctx, body: &mut Node, scope, top_id)`:
  per `SplitKernel`, `find_node_by_id(body, value_id)`,
  `collect_region_inputs` on the per-slot subtree, build feeders matched
  **by name** against the slot's region inputs (`genn::reference` per
  input), `FusedKernel::new(... sk.kernel.clone() ...)`, `splice_into`
  then `old.delete(ctx)`. The shared `Arc<GirKernel>` is reused across
  all slots; only the feeders + the `FusedKernel` wrapper are per-slot.

**Runtime per-slot splice** (`MapQ::update`, graphix-package-core):
- **Force-bind at slot construction (observable path).** After building
  the slot's fresh `genn::apply` `CallSite` `pred`, the split case
  force-resolves it event-free: `cs.resolve_static(ctx, def, fv)` where
  `fv = ctx.cached[predid].clone()` and `def =
  fv.downcast_ref::<LambdaDef>()` (the same value `bind()` downcasts at
  `callsite.rs:524`). Then `splice_into_body(ctx, g.body_mut(), …)` via
  `cs.resolved_apply_mut() -> ApplyViewMut::Lambda(g)`. This splices the
  sync kernel **before the slot's first run**, so even a one-shot
  (static) array fuses on its only cycle. `slot_spliced[i] = true` once
  resolved (the deferred path then skips it). `resolve_static` is
  event-free (binds the `GXLambda`, no body run); the element `s.id` is
  primed later (the `ctx.cached.insert(s.id, v)` loop runs before the
  drive loop), so the spliced kernel's element feeder reads a live value.
- **Deferred splice in the drive loop (safety net).** After each
  `s.pred.update`, an un-spliced split slot whose `CallSite` has lazily
  bound (`resolved_apply_mut() == Lambda(g)`) gets the same
  `splice_into_body`. The pre-splice cycle runs interpreted but yields
  the same value (kernel == interpreted sync region), so it's
  correctness-safe — it only catches slots the force path missed.
- `MapQ.slot_spliced: Vec<bool>` parallels `slots` (push on grow, pop on
  shrink).

**Plumbing made `pub(crate)`/`pub`:** `fusion::{FreeVarInput,
collect_region_inputs, find_node_by_id}`; `ExecCtx.jit_enabled: bool`
(set from `CFlag::JitDisabled` in `compile()`); `CallSite::
{resolve_static, resolved_apply_mut}`. `FusedCallback` has a manual
`Debug` impl (MapQ derives Debug; `WrappedKernel` isn't `Debug`).

**Phase 1 was NOT inert** (an earlier wrong prediction): the per-slot
shared-kernel dispatch flipped `list::map/filter/find_miss` None → Jit,
because non-array HOFs (recursive-variant `List`) never batch-loop, so
per-slot dispatch is their *only* fusion path.

**Proven**: `lang::fusion::impure_hof_callback_splits` —
`array::map([1,2,3], |x| { let v = x*2+1; counter <- v; v })` → `[3,5,7]`
**and** `jit_invocations > 0` (the sync `x*2+1` JIT-ran per slot). The
async `counter <-` stays interpreted. 132 compiler + 1864 graphix-tests
green, no regression.

**Perf note**: force-resolve compiles the per-slot `GXLambda` + feeders
at construction (runtime) — a cost that already existed for per-slot
graphs (`genn::apply` builds one graph per element); the splice adds
feeder compilation. Net win for arrays that update repeatedly; possible
setup-cost loss for very large one-shot arrays. Correctness unaffected.

**Follow-ups**: split only scans block `let`-values today — not the
block *tail* expr, not a sync region nested directly in an async op's
argument (`publish(p, bigcalc(x))` with no intervening `let`), not
multi-level `select`/`try` bodies. Each extends the same
`build_body_split` scan. See `design/impure_hof_fusion.md`.

### Impure-HOF fusion rebuilt on `clone_rebind` — clone the fused template per slot (Jun 2026)

Replaced the MapQ-special split machinery (`FusedCallback`/`build_slot`/
per-slot `splice_into_body`/`slot_spliced`/force-bind/deferred-splice) with
a uniform, general mechanism the user drove: **fuse the `analysis_pred`
template once, then `clone_rebind` it per array slot.** "Fuse a partly-async
callback" is now just "fuse a node tree, then clone it" — no per-case types.

**The core insight (user's).** The per-slot path was *already* a rebinding
clone — it recompiled the callback body from its `Expr` per slot, minting
fresh `BindId`s (that's how each slot gets independent async state). The only
reason the splice machinery existed is that recompile-from-`Expr` yields an
*unfused* body (fusion is node-level). So: fuse the template once, and
`clone_rebind` it (preserving the fused structure) instead of recompiling.

**Why no RemapTable.** A `BindId` is not a number on a node — it's a key into
three registries (`env.by_id`, the scope `name→id` map, the runtime
`ref_var` table). Re-minting *requires* `env.bind_variable`, which updates the
scope name map — and then refs resolve by name. **The env's scoped name map
IS the remap.** A side table would duplicate the registration we must do
anyway. (Verified against the live tree; this also killed the early
"deep-clone with a RemapTable" idea.)

**`clone_rebind` contract** — `clone_rebind(&self, ctx, scope) -> Node` (on
`Update`; `-> Box<dyn Apply>` on `Apply`): an independent copy as if compiled
fresh in the same lexical spot — bindings the subtree introduces get fresh
env-registered ids; internal refs resolve to the copy's fresh ones; captures
(external refs) keep the outer id; immutable fused-kernel `Arc`s are shared;
stateful builtins get fresh state. It's the symmetric twin of `delete` (which
tears down those same three registrations).

**The default is RE-INIT (not panic).** `Update::clone_rebind`'s default
`compile`s the node's `spec` afresh in `scope` — which re-mints owned
bindings and re-resolves refs by name, i.e. the clone contract — correct for
any *residue* node (no fused descendants). Only the **fusion spine** overrides
with a structural clone, because re-compiling a node with a spliced
`FusedKernel` descendant would lose the fusion. (Started with a panic default;
the full suite immediately showed 38 residue node types — `Eq`, comparisons,
`Select`, … — so the recompile default is both correct and the right call.
[[feedback-run-the-code]].)

**Spine impls** (all verified, structural so fusion survives):
- `Ref` (`node/bind.rs`): name from `spec.kind` (`ExprKind::Ref`) or
  `env.by_id[id].name` (synthesized NOP-spec feeders) → `ModPath::from_iter`
  → `lookup_bind(scope, name)` → fresh id, else keep (capture); `ref_var`.
- `StructPatternNode` (`node/pattern.rs`): walk the enum, re-mint each
  `Bind(old_id)` via `env.by_id[old_id]` → `bind_variable` (`by_id` is `pub`).
- `Bind`: `rec`-aware order (non-rec value-then-pattern; rec pattern-then-value).
- `Block`/`Do`: recurse children in lexical order (name map is transient).
- `GXLambda` (`Apply`): re-mint arg patterns, then `clone_rebind` body
  STRUCTURALLY (preserve fusion, NOT re-init); keep `LambdaId` (share kernel).
- `BuiltInLambda`: delegate to inner `apply.clone_rebind`.
- `CallSite`: Bind-shaped — re-mint the `args`-map ids (a local old→new map;
  they're genn-minted, not env-named), rebuild `arg_refs` at the fresh ids,
  `clone_rebind` `fnode` + the bound callee. (Not special vs Bind — the user's
  call; I'd over-framed it as hard by conflating clone with re-bind.)
- `Connect`: re-resolve target by name (outer `counter` → unchanged; internal
  → fresh), re-insert unstable, recurse RHS.
- `FusedKernel`/`GirNode` (`fusion/builder.rs`, `gir_interp.rs`): an ordinary
  Node — recurse feeder deps + `dyn_slot` Applys, SHARE the immutable
  `Arc<GirKernel>`/`Arc<WrappedKernel>`/`Arc<KernelRegistry>` (the kernel is
  incidental — the precompiled update logic), re-init scratch + dyn_slots via
  a new `GirNode::clone_shared` that re-runs the `build` chokepoint with the
  shared Arcs.

**Lazy template fuse — the key bug + fix.** Fusing the template at *compile
time* (in `static_resolve_fn_args`) POISONED the surrounding region's JIT for
HOFs that region-fuse (the common pure case: `array::filter_map(a, |x| …)`
fuses into the program region as `GirOp::ArrayFilterMap`, so MapQ never runs
and the template is unused — but building+JITing it still broke the region's
JIT, dropping it to interp). Fix: defer the fuse to the **first `update()`**
that grows slots (`template_fused: bool` flag). Region-fused HOFs never call
`update()` → no compile-time interference → region JITs normally; MapQ-run
HOFs (impure, or non-region-fused like recursive-variant `list::*`) fuse the
template lazily. `fuse()` (the walker) does NOT splice a lambda-body-internal
sync region, so the lazy fuse reuses the proven `fuse_callsite` →
`splice_into_body` (split/impure) / `build_slot`+replace-body (whole-body/pure
Phase-1). So `FusedCallback`/`SplitKernel`/`build_body_split`/`splice_into_body`/
`build_slot`/`fuse_callsite` are NOT deleted — they're now the lazy-fuse
*mechanism*, run once per MapQ.

**MapQ runtime** (`update`'s slot-grow loop): bind a fresh element `"x"` in
scope (`env.bind_variable`, no ref_var/node), then `template.pred
.clone_rebind(ctx, scope)` — the cloned arg[0] element ref + the FusedKernel's
element feeder both resolve `"x"` to this slot's fresh id via the name map.
Removed: the old per-slot `build_slot`, the force-bind/deferred-splice dance,
`slot_spliced`, `fused_callback`.

**Proven**: `lang::fusion::impure_hof_callback_splits` + `…_split_captures`
(impure `array::map([1,2,3], |x| { let v=x*2+1; counter <- v; v })` → `[3,5,7]`
with the sync `x*2+1` JIT-ran per slot; capture variant `x*k` → `[10,20,30]`).
Chained sync lets verified by probe. **132 compiler + 1865 graphix-tests, 0
failed** — the clone path runs for EVERY HOF callback (pure region-fused +
pure MapQ + impure) with no regression.

**Follow-up (#157):** a builtin call in an impure callback's async residue
(`array::map(a, |x| { calc; net::publish(p, x) })`) would hit the `Apply`
panic default (CallSite → BuiltInLambda → inner builtin `clone_rebind`).
Current tests use `Connect` residue only. Fix: either builtins own
`clone_rebind` (macro default + stateful overrides), or `CallSite::clone_rebind`
leaves a builtin function unbound so it lazily re-inits fresh per slot
(re-bind is correct for builtins — they don't fuse their body). Option (b) is
likely cleaner.

### Structural clone_rebind for residue + separate-clone template (Jun 2026)

Retired the fragile `clone_rebind` recompile-default for value-computing
residue and re-enabled MapQ's separate-clone template — biting the bullet on
a broad mechanical change to kill latent tech debt rather than paper over it.

**Why.** The earlier `clone_rebind` shipped with two shortcut defaults: a
panic for `Apply` (builtins) and a recompile-from-spec for `Update` (residue).
Both broke on inputs the tested paths didn't hit. The recompile-default's
killer: it re-resolves every `Ref` by name in the clone's scope, but a CAPTURE
was resolved in an *outer* scope not lexically visible from the callback's
`/array/...` scope → `compile` hard-fails ("k not defined"). Proven by the
`x*k` separate-clone attempt. The structural `Ref::clone_rebind` handles this
(keep the original id when a name doesn't resolve = capture preserved); the
recompile couldn't. ("Imagine a swarm of users feeding valid code to break the
compiler" — every valid program must clone, not just the tested ones.)

**Builtins (#157) — narrow, closed.** `CallSite::clone_rebind` clones a
`GXLambda` callee structurally but leaves a *builtin* callee unbound
(`function: None`, `statically_resolved: false`) so it re-inits fresh per slot
via its own `init` — zero per-builtin code. The panic was narrower than it
looked: inner builtin CallSites in the analysis-only template are *already*
unbound (handled by re-bind), so it was only reachable for a direct
async-builtin callback (`array::map(a, async_builtin)`). Common
builtin-in-residue already worked.

**Structural clone_rebind for value nodes (#161).** Recurse children (each
`Ref` goes through `Ref::clone_rebind`, so captures are preserved); clone
leaves. Done for: `op.rs` (all 19 arithmetic/comparison/logical + `Not` — one
edit per the 3 macros, `Cached::new(child.clone_rebind())`), `data.rs`
(Struct/StructWith/Tuple/Variant/StructRef/TupleRef), `array.rs`
(Array/ArrayRef/ArraySlice), `map.rs` (Map/MapRef), `select.rs` (Select +
`PatternNode::clone_rebind` re-minting arm patterns *before* cloning the body
so its Refs resolve to the fresh ids), `error.rs` (Qop re-resolves the catch
id via `lookup_catch`; OrNever), `mod.rs` (ExplicitParens/TypeCast/
StringInterpolate/Any/Constant/Nop).

**Recompile-default RETAINED but capture-safe — for env/reference nodes
only.** For nodes whose `compile` sets up env state no hand-clone could safely
duplicate (`TryCatch` catch-scope+handler, `Module` env-snapshots+proxy+
check_sig, `Lambda`, `Use`/`TypeDef`, and the byref/synthetic-id nodes
`ByRef`/`Deref`/`Sample`/`ConnectDeref`), recompile-from-spec IS the correct
structural-equivalent — duplicating Module's env logic by hand would be the
real tech-debt hazard. Made it capture-safe: before `compile`, alias each
*unresolved* external ref's name → its original id into the clone scope (via
`env.alias_variable`); internal re-minted refs already resolve and are left
alone. A `TryCatch` recompile absorbs all its children (Qops get their catch
id from `compile`), so the structural `Qop::clone_rebind` only ever sees
standalone (unhandled) Qops.

**Separate-clone template (the MapQ cleanliness win, now unblocked).** MapQ no
longer mutates `analysis_pred` in place. On the first slot-growing `update()`
it `clone_rebind`s the pristine `analysis_pred` into a SEPARATE
`fused_template: Option<Node>` and fuses *that clone*; the prototype `emit_gir`
reads at compile time is never touched (the two readers don't share a mutable
object — structural separation, not temporal ordering). Per slot it clones
`fused_template`. This is what the `x*k` capture case validates: cloning the
pristine `Mul{Ref(x), Ref(k)}` now recurses through structural ops →
`Ref(k)::clone_rebind` keeps the capture → `impure_hof_callback_split_captures`
passes (it failed before the structural work). `template_fused: bool` →
`fused_template: Option<Node>` (presence is the "built" flag).

**Note on the recompile-default's reachability.** Lazy-mutate fed
`clone_rebind` only post-fuse SPINE residue, so the structural value impls were
never exercised by the impure fixtures. The separate-clone feeds the PRISTINE
body, so it exercises the structural ops for every MapQ callback — which is why
re-enabling it is both the cleanliness win and the validation.

132 compiler + 1866 graphix-tests green.

### clone_rebind test campaign (4 efforts) (Jun 2026)

Full plan + per-effort results in `design/clone_rebind_testing.md`. The
suite was green but mostly proved *value correctness on the tested
paths*; this campaign targets the three things that nag about the
clone_rebind design — exactly the classes a passing test can hide. The
user's framing: "imagine a vast swarm of agents (every user) whose only
goal is to break the compiler by feeding it valid code. Welcome to
compilers." Goal: find a bug, not pass.

- **#1 Clone-equivalence matrix** (`lang::fusion::clone_*`, 13 fixtures).
  One fixture per residue node shape (arith, compare, bool, select,
  tuple/struct/array/map/variant producers + accessors, string interp,
  nested), each forced through MapQ's per-slot CLONE path (an `impure`
  callback via `counter <- x`) AND capturing an outer `k`. A dropped
  child / swapped field / lost capture makes the value wrong. **Found a
  real PRE-EXISTING fusion bug (#162):** `let r = select x { 1 => k, n =>
  n*k }; r` panics "undefined scalar local `n`" — a let-bound (non-tail)
  `select` with an arm BINDING. NOT clone_rebind: the region-fuse path
  (`region_select_let_bound`, no clone) panics identically → it's in
  `emit_select_as_expr` / the let-value emit. The 2 reproducing tests are
  `#[ignore]`'d as live docs, queued for the off-topic-bug conversation.

- **#2 Accounting invariant** (`env_accounting_grow_shrink`). New
  `GXHandle::env_stats() -> EnvStats { by_id_len, ref_var_keys,
  ref_var_total }` introspection hook (a `ToGX::EnvStats` mirroring
  `match_shape`/`describe_shape`; the gx.rs handler reads
  `ctx.env.by_id.len()` + walks the runtime `by_ref` registry). Drives an
  impure HOF (`arr` bound at root, driven by BindId via `GXHandle::set`,
  so the apparatus mints no bindings) up to N=4 and back to 0, 4×,
  snapshotting at each bottom. **The invariant HOLDS tightly:** all four
  arr=[] bottoms byte-identical (`759/750/758`), peak ≫ bottom
  (`771/766/782` — each grow mints +12/+16/+24, each shrink reverses ALL
  exactly). So the scariest nag — silent unbounded `env.by_id`/`by_ref`
  growth, "nothing turns red" — is now an executable, sharp, green
  invariant. A single-binding-per-cycle leak → `bottoms[1] != bottoms[0]`.

- **#3 Env-node-in-callback fixtures** (4). The recompile-default
  `clone_rebind` (lib.rs) is the ONLY path that `alias_variable`-mutates
  the clone scope's name map; it fires for env/reference nodes with no
  structural override (TryCatch, Sample, ByRef/Deref, …). Each plants one
  inside an impure callback capturing `k`: `clone_byref_deref_capture`,
  `clone_sample_capture` (`x ~ k`, not soundly fusable → stays a
  structural Sample), `clone_trycatch_try_capture` (non-firing), and
  `clone_trycatch_catch_capture` (firing via the Connect pattern). **All
  pass — the alias path is SOUND, no cross-slot contamination.** Surfaced
  the **catch-is-side-effect-only** semantic: `try BODY catch(e) => H`
  evaluates to BODY's value *always*; a direct-value catch
  (`catch => 99`) surfaces NOTHING (when BODY is all-error → `never`, the
  slot times out). Every real firing-catch must `binding <- e` and return
  the binding separately (cf. CHECKED_DIV0). Two of my fixtures were wrong
  on this; localizing via the PLAIN non-fused path (not the clone)
  separated "my mental model is wrong" from "clone is broken"
  ([[feedback-run-the-code]], in reverse — verify the EXPECTATION too).

- **#4 proptest swarm** (`clone_matches_reference`, 128 cases). Random
  i64-valued nested bodies over `x`+`k` (`+ - *`, literal-pattern select,
  tuple/struct accessors as `{ let p = (a,b); p.0 }` block-exprs); assert
  CLONE path == non-clone REFERENCE path (`pure_map`). **All 128 agree.**
  Surfaced an off-topic PARSER limitation: the first grammar emitted
  `(a,b).0`, proptest shrank to `((x,x).0)` — a parse error (field access
  needs a bound name, not a paren-literal). Reshaped the grammar to the
  binding-first block form.

**Bottom line:** clone_rebind validated across every axis — per-shape
(matrix), accounting (no leak), env/alias path (sound), random
composition (proptest). NO clone_rebind defect. Two off-topic findings:
#162 (real fusion bug, `#[ignore]`'d) and the `(a,b).0` parser
limitation. New: `GXHandle::env_stats` + `EnvStats` (graphix-rt),
`proptest` + `netidx-value` dev-deps (graphix-tests).

132 compiler + 1885 graphix-tests green (2 ignored — the #162 docs).

### #162 FIXED — fused `select` arm binding panic (Jun 2026)

The bug the test campaign found. A fused `select` with an arm BINDING
pattern (catch-all `n =>`, typed capture `i64 as n =>`, guard-using
captures) panicked at runtime: "undefined scalar local `n` — GIR is
malformed" (`gir_interp.rs:999`). Diagnosed empirically (reproduced +
bounded with the binary) THEN corroborated by a 4-agent understanding
workflow — both reached the identical root cause + fix. Broader than the
"let-bound" framing: it hit ANY fused select with an arm binding (tail /
let-value / arithmetic-operand position alike); the matrix's
`clone_select_*` fixtures "passed" only because the impure clone path
doesn't fuse the block TAIL, so the select ran interpreted and dodged it
([[feedback-run-the-code]] — a passing test that wasn't exercising the
path it claimed).

**Root cause** (`fusion/lowering.rs`, `emit_arm_condition`'s
`StructurePattern::Bind(name)` arm): it pushed `arm_ctx.inputs.push(Input
{ name, prim, bind_id: None })` — registering the bound name as a kernel
input FEEDER with no value. The arm body's `Ref(name)` then lowered to a
dangling `GirOp::Local(name)` with nothing to feed it. (The in-code
comment already admitted the gap: "The scrutinee value itself isn't
auto-bound by name here.")

**Fix** (one arm): bind `name` to the scrutinee via
`arm_ctx.known_consts.insert(name, KnownConst { expr: scrut.clone() })` —
the exact inline-expr channel the sibling variant-payload binds already
use; the arm body's `Ref(name)` resolves to the scrutinee through
`find_const`. Backend-agnostic (emit layer) so interp AND JIT benefit;
verified correct under full fusion+JIT. Plus a **shadow guard**: the
`Ref` arm checks `lookup_local` (inputs) before `find_const`, so an arm
binding shadowing a same-named kernel INPUT can't win — that case bails
to the interpreter (no wrong value) rather than reading the outer input.

**Regression suite** (`lang::fusion`): un-`#[ignore]`'d the two #162
tests; added 4 `fused_select_*` (catch-all / typed-capture / guard+
capture / arith-wrapped) that assert the value AND `fusion_invocations >
0` (the PURE-HOF form actually fuses the select, unlike the dodging
clone form); the proptest grammar gained an arm-binding `select` arm
(fresh name `q`, so it can't hit #167). 132 compiler + 1891 graphix-
tests green (1 ignored — the #167 doc).

**#167 surfaced + FIXED (a node-walk correctness bug).** A select arm
binding that shadows an outer name a SIBLING arm references — `let n =
100; … select x { 1 => n, n => n*2 }` in a per-slot HOF — HANGS,
producing no value. Reproduced under `CFlag::FusionDisabled` (no
fusion), so a **node-walk** bug, not a fusion bug.

Root cause (instrumented `bind_variable`/`Ref::compile` via a
`GRAPHIX_167DBG` env probe, diffed shadow vs no-shadow; corroborated by
a 3-agent workflow): `Select::compile` gives each arm a FRESH unique
sub-scope (`scope.append("sel{SelectId::new()}")`) so arms are isolated
(`lookup_bind` walks ancestors only). But `Select::clone_rebind` — which
MapQ uses to build the per-slot node graph even with fusion off — re-
minted every arm's pattern + cloned every arm's body in ONE shared
`scope`. So arm 2's binding `n` polluted the shared scope; a *later*
clone (template → per-slot) resolved arm 1's `Ref(n)` to that stale
sibling binding, which is only written when arm 2 fires
(`bind_event` runs for the selected arm only) — so when arm 1 fires it
reads an unwritten BindId, produces nothing, and the map never emits.

**Fix** (`node/select.rs`): `Select::clone_rebind` appends a fresh
per-arm `sel<SelectId::new()>` sub-scope, mirroring `Select::compile`.
Per-arm isolation is a `select` correctness invariant; clone_rebind must
preserve it. **The node walk is graphix's canonical execution model and
must always be correct — never sidestep a node-walk bug by fusing**
([[feedback-node-walk-is-canonical]], the user's principle that drove
fixing #167 properly instead of leaving the #162 guard to route around
it). The guard now composes cleanly: its bail-to-interp yields the
CORRECT value, not a hang. Block/Lambda clone_rebind are unaffected —
their bindings are SEQUENTIAL (later shadows earlier, which is correct);
only select's PARALLEL arms have the sibling-isolation requirement.

Regression: `shadow_arm_binding_outer_ref` (un-`#[ignore]`'d → `[100,4,
6,8]`) + `shadow_arm_binding_node_walk` (same under `FusionDisabled`,
asserting the canonical model directly). 132 compiler + 1897 graphix-
tests green, **0 ignored**.

**Pulling the thread — an adversarial review of the #162 fix found TWO
MORE confirmed wrong-output bugs** ([[feedback-run-the-code]] — both
empirically reproduced, neither caught by the existing value-only
fixtures because they used idempotent/non-colliding scrutinees):

1. **Non-idempotent scrutinee DUPLICATED.** The `known_consts` channel
   (and the PRE-EXISTING arm-condition `gir::cmp(scrut.clone(), …)`)
   INLINE the scrutinee GirExpr at every use. For a `Local` scrutinee
   that's invisible; for a non-deterministic one it diverges. `select
   rand::rand(…) { n => n == n }` returned `false` (each `n` re-
   dispatched `rand`); `n - n` returned nonzero. Partly pre-existing —
   a multi-literal-arm select duplicated the scrutinee across arm
   conditions regardless of the #162 binding work. **Fix:**
   `stabilize_scrutinee` (`lowering.rs`) binds a non-`Local` SCALAR
   scrutinee to a fresh temp local once (`let __sel_scrut_<exprId> =
   scrut`) and feeds `Local(temp)` to every condition / type-predicate /
   binding — expr form wraps the `IfChain` in a `GirOp::Block`, stmt
   form prepends a `GirStmt::Let`. The temp needs no
   `register_kir_binding` (runtime Block/Let provides the value;
   `GirOp::Local` dispatches on the expr's own `typ`). Scalar-only for
   now (the confirmed bug is scalar; value-shape non-idempotent
   scrutinees are a follow-up).

2. **Variant-payload arm had the SAME shadow bug** the scalar `Bind`
   guard fixed — a payload bind colliding with a kernel input read the
   input, not the payload (`select \`Pair(x, b) => x+b` with element `x`
   in scope → `[4,5,6,7]` instead of `[14,15,16,17]`). **Fix:** the same
   `lookup_local` shadow guard in the variant payload loop.

Regression: `fused_select_scrutinee_evaluated_once` (rand `n==n` → true),
`_once_subtract` (rand `n-n` → 0), `fused_select_stabilize_multiref`
(arith scrutinee bound + referenced 3×, JIT path), `fused_variant_
payload_shadow`. 132 compiler + 1895 graphix-tests green (1 ignored —
#167). The review CONFIRMED-clean the scalar `Bind` correctness and the
guard's non-shadow completeness.

### #168 FIXED — nested-HOF grandparent capture lost in `Lambda::clone_rebind` (Jun 2026)

Found by AUDITING for more `clone_rebind`-vs-`compile` divergences after
#167 (the user's "audit whether any other clone_rebind impl silently
diverges"). A HOF whose INNER callback references a GRANDPARENT capture —
a binding outside BOTH HOFs — hung, producing no value:

  let n = 100; array::map([1, 2], |y| array::map([1], |x| x + n))   // HUNG

Narrowed empirically (printed-value signal, since the reactive CLI stays
alive regardless): inner refs the OUTER ELEMENT `y` works; inner refs a
grandparent module `n` hangs; routing `n` through a parent-callback `let
w = n` works; single-level map capture works; nested `fold` hangs too. So
the inner callback could capture its IMMEDIATE parent's bindings but not a
grandparent's. Affects fused / clone / genn paths alike, AND the node walk
(`CFlag::FusionDisabled`).

Root cause (a 3-agent workflow with decisive runtime traces): the inner
`array::map`'s callback `|x| x+n` is a BARE, unresolved Lambda node (the
static-resolve walker never descends into Lambda bodies). When MapQ clones
the outer callback per-slot, the outer (resolved) callee clones
structurally — preserving the `Ref`-to-`n` capture, which is why single-
level works. But the inner bare Lambda falls to the recompile-DEFAULT
`clone_rebind` (lib.rs), which recompiles the lambda spec in the per-slot
clone scope (rooted at `/array`, the array package, NOT the program
scope). The recompile-default's capture-safety alias loop sources from
`self.refs()` — but `Lambda::refs` is intentionally EMPTY (a lambda value
has no runtime refs until applied) — so `n` is never aliased; the recompile
then can't resolve `n` (it lives in the program scope, unreachable from
`/array`) → "n not defined" → the inner map produces nothing → hang. The
outer element `y` works because MapQ binds it into the `/array` scope
per-slot; a grandparent binding has no such path.

**Fix** (`node/lambda.rs`): give `Lambda` a structural `clone_rebind` that,
before recompiling, walks the spec body for free-var names and aliases each
one that's UNRESOLVABLE in the clone scope but RESOLVABLE in the lambda's
`def.env` (where it was captured). Slot-local captures (the enclosing HOF's
element) already resolve in the clone scope and are left untouched —
additive, so existing cases are unaffected. Over-collection is harmless:
an inner-let / nested-lambda-arg name won't resolve in `def.scope` either,
so it's skipped. Mirrors `Select::clone_rebind`'s #167 fix in spirit —
clone_rebind must faithfully reproduce `compile`'s scoping
([[feedback-node-walk-is-canonical]]).

Regression: `nested_hof_grandparent_capture` ([[101],[101]]),
`nested_hof_capture_element_and_grandparent` (both `y` slot-local and `n`
grandparent → [[7],[8]]), `nested_fold_grandparent_capture`, and
`nested_hof_grandparent_capture_node_walk` (same under `FusionDisabled` —
the canonical model). 132 compiler + 1901 graphix-tests green, **0
ignored**.

The deeper enabler the workflow flagged: statically resolve nested-HOF
call sites inside callback bodies so the inner `array::map` gets an
`analysis_pred` and clones structurally like the single-level case — a
larger architectural change; the `Lambda::clone_rebind` alias directly
closes the capture-loss leak. (Turned out NOT to be needed for the
function-capture case — see #169.)

### #169 FIXED — `Expr::fold` silently skipped the Apply callee (Jun 2026)

Continuing the post-#168 capture/scope correctness sweep: a FUNCTION-
typed grandparent capture CALLED in a nested HOF still hung:

  let f = |z| z*2; array::map([1, 2], |y| array::map([1], |x| f(x)))   // hung

Value/composite grandparent captures all worked after #168 (a 14-shape
sweep confirmed it); only *function-position* captures failed. It LOOKED
like the larger static-resolve-descent architectural change — but a
2-agent diagnostic workflow (which built + instrumented + reverted)
pinned it to a **one-line root cause**: `Expr::fold` (expr/mod.rs)'s
`Apply` arm was `ExprKind::Apply(ApplyExpr { args, function: _ })` — it
folded the call's ARGUMENTS but **skipped the `function` expression**. So
the #168 `Lambda::clone_rebind` alias pass (which `fold`s the body to
collect free-var names) never saw the callee `f` — `f` was in function
position — so it was never aliased and the inner callback's recompile
couldn't resolve it. Decisive control: `|x| { let g = f; g(x) }` (where
`f` is a value Ref in a Bind, which `fold` DOES visit) worked — same `f`,
same scopes, the ONLY difference call-position vs value-position.

**Fix** (`expr/mod.rs`): make the `Apply` arm fold `function` before
`args` — `Expr::fold` is now a true full-tree walk (it was silently
incomplete). Blast radius audited: `fold`'s two callers
(`Lambda::clone_rebind` — the intended beneficiary — and
`Expr::has_unresolved_modules`) both benefit or are unaffected (a call's
function is a Ref/Lambda, never an unresolved module path). The fix also
enables nested ANONYMOUS-lambda calls (`(|z| z+n)(x)`) — fold now descends
the callee Lambda and collects its captures too.

**Lesson:** a localized fix that newly *depends* on a shared helper can
surface a latent incompleteness in that helper — `Expr::fold` had been
silently dropping the Apply callee forever; it only mattered once
`Lambda::clone_rebind` relied on `fold` being complete. The "it must be
the big architectural change" instinct was wrong; the diagnostic workflow
(build + instrument + control-case) found the one-liner instead
([[feedback-run-the-code]]).

Regression: `nested_hof_function_capture` ([[2],[2]]),
`nested_hof_function_and_value_capture` (both arms of fold),
`nested_hof_anon_lambda_capture`, `nested_hof_function_capture_node_walk`
(`FusionDisabled`). 132 compiler + 1905 graphix-tests green, **0 ignored**.

**The full cascade from "fix #162":** #162 → scrutinee-duplication →
variant-payload-shadow → #167 (Select::clone_rebind per-arm scopes) → #168
(Lambda::clone_rebind value-capture alias) → #169 (Expr::fold callee) →
#170 (Expr::fold StructWith.source + lambda arg defaults). Seven bugs,
each one level below the last.

### #170 — `Expr::fold` was incomplete in TWO more positions (Jun 2026)

The post-#169 capture sweep found that the `Apply.function` gap wasn't
the only `Expr::fold` hole. Two more, same root-cause class (a capture in
that position inside a nested HOF wasn't aliased → hang):

1. **`StructWith.source`** — `ExprKind::StructWith(StructWithExpr {
   replace, .. })` folded `replace` but the `..` SKIPPED `source`.
   Confirmed bug: `let base = {a:1,b:9}; … {base with a: x} …` with `base`
   a grandparent capture hung; parent-let control + single-level worked.
2. **Lambda `Arg.labeled` defaults** — the `Lambda` arm folded only the
   body, skipping labeled-arg DEFAULT exprs (`Arg.labeled:
   Option<Option<Expr>>`). A `#off = n` default capturing a grandparent
   wasn't aliased.

Fix (`expr/mod.rs`): the `StructWith` arm now folds `source` then
`replace`; the `Lambda` arm folds each arg's labeled default then the
body. `Expr::fold` is now a true full-tree walk for every current
`ExprKind`. (Future-proofing landed — see the next entry.)

Regression: `nested_hof_structwith_source_capture` ([[9],[9]]),
`nested_hof_labeled_default_capture` ([[101],[101]]). 132 compiler + 1907
graphix-tests green, 0 ignored.

**Also confirmed NOT bugs** (expected semantics, the sweep flagged them as
hangs): a direct-value catch (`catch(e) => 99`) is side-effect-only → no
value → a HOF slot over it never completes; `$` over an unparseable
element swallows to no value likewise. Both are the documented try/catch
+ `$` semantics, not nested-HOF defects (single-level behaves the same).

The capture/scope/fold space is now swept clean across value, composite
(tuple/struct/variant/map), string, function, struct-with-source, and
labeled-default capture positions — verified by a broad empirical battery
+ the proptest swarm + `FusionDisabled` node-walk regression variants.

### `Expr::fold` future-proofed — completeness is now compiler-checked (Jun 2026)

The #169/#170 cascade was three instances of one failure mode: `Expr::fold`
(`expr/mod.rs`) — the canonical full-tree `Expr` walker that
`Lambda::clone_rebind`'s capture-alias pass depends on — silently dropped a
child `Expr` because a `match` arm used `..` (or partial destructuring) that
happened to skip an `Expr`-typed field (`Apply.function`, `StructWith.source`,
the lambda `Arg.labeled` defaults). Each skipped field became a "capture in
position X inside a nested HOF hangs" bug. Fixing the instances closed the
holes; this closes the **class**.

Removed **every** `..` from the `fold` match. Each arm now names all of its
variant's fields explicitly — `Expr`-typed fields are folded, non-`Expr`
fields are bound to `_`. The only remaining `..` patterns are inside the
nested `ModuleKind` destructures' siblings, which are also now explicit
(`Resolved { exprs, sig: _, from_interface: _ }`, etc.). Tuple variants
(`TypeDef(_)`, `Constant(_)`) are self-guarding — a new positional field
breaks `(_)` at compile time. Net effect: adding any field to any `ExprKind`
or `ModuleKind` variant now produces a compile error *in `fold`*, forcing a
human to decide whether the new field is an `Expr` to walk — exactly the
14th-commandment "make the compiler check what the human would otherwise have
to remember." Had this been in place, #169/#170 would have been compile errors
at the variant-definition commit, not runtime hangs found by a capture sweep.

Behavior-preserving (the set of folded fields is unchanged; only the skipped
non-`Expr` fields became explicit `_`). 132 compiler + the full graphix-tests
suite green.

### Checked-arithmetic overflow detection fixed + arith edge tests (Jun 2026)

Surfaced by hand-probing while setting up float edge-case tests for the
forthcoming differential fuzzer (`design/graphix_fuzz.md`). **`+?`/`-?`/`*?`
never detected integer overflow** — they silently wrapped and returned the
garbage value (`is_err(i64:MAX +? 1)` was `false`); only `/?`/`%?` worked.

Root cause: `node/op.rs`'s `arith_op!` macro ran every op via the bare Rust
**operator** (`$op:tt` = `+`) and only treated a `Value::Error` *result* as the
error signal. In netidx-value, `impl Add/Sub/Mul for Value` use `wrapping_*`
(op.rs:456 — never error), while `impl Div/Mod` use `checked_div` (error on
div-by-zero). So `/?` inherited a checking operator and `+?` a wrapping one.
netidx-value already had `Value::checked_add`/`checked_sub`/`checked_mul`/
`checked_div`/`checked_rem` methods (tested in its own suite) — graphix just
called the wrong thing.

Fix (graphix-side, ~5 lines): the `arith_op!` macro now takes a `$method:ident`
and calls `lhs.clone().$method(rhs.clone())`. Unchecked variants use the
operator-trait methods (`add`/`sub`/`mul`/`div`/`rem` — brought into scope via
`use std::ops::{Add as _, …}`, the `as _` avoiding a name clash with the
generated `struct Add`); checked variants use the `checked_*` inherent methods.
**Unchecked stays wrapping** (the fast path) and **floats are untouched**
(`checked_add` on floats does bare `+` → `inf`, never an error). Confirmed:
checked overflow now errors for i64 + u8 across add/sub/mul; unchecked still
wraps; `1.0/0.0 → inf`, `0.0/0.0 → NaN`.

Scope check that made this safe: checked ops **don't fuse** (`emit_node` has
arms only for unchecked `Add/Sub/Mul/Div/Mod`; `gir::BinOp` has no checked
variants), so the fix is entirely in the node-walk macro — no JIT/interp
counterpart to keep in sync, and no risk of *introducing* a divergence. (By
contrast, changing *unchecked* overflow to error would have to touch the
node-walk AND the fused paths together or it would create one — left as an open
question; unchecked-wraps is a defensible "unchecked = no check" semantics.)

New `lib_tests/arith.rs` pins primitive-operator edges the `math::*` builtin
tests didn't cover: float inexact add, no-FMA-contraction, div-by-zero →
inf/NaN, signed zero, subnormals, f32; and the checked-overflow regression
(i64/u8 add/sub/mul → error, no-overflow → bare value, `/?` div0 → error,
unchecked wrap pinned). 48 tests, all green; every fixture also serves as a
cross-mode float-determinism guard via `run!`.

**Two findings, two classes** (a useful contrast for the fuzzer design): the
overflow bug is a "both models agree on the wrong answer" bug — the differential
oracle (interp vs jit) can **not** catch it (both wrap identically); only a
semantics-aware check (a hand spec test, or the design doc's agent sources B/E)
does. The NaN-equality divergence below is the opposite — the differential
oracle catches it directly.

### #174 FIXED — fused float comparison now uses graphix's total order

Found by the new arith.rs cross-mode tests, then confirmed + re-confirmed by the
`graphix-fuzz` oracle (below). The node-walk compares floats via
netidx-value's **total** `Value::partial_cmp`/`eq` (op.rs:153,179): `NaN ==
NaN`, and `NaN` sorts **below every non-NaN value** — this is what makes `Value`
`Ord`/`Hash` so it can be a map key. The fused interp (`gir_interp` `eval_cmp`)
and JIT (`gir_jit` `compile_cmp`) compared **raw f64** (IEEE: `NaN != NaN`, all
NaN orderings false). So *every* float comparison involving NaN diverged.

**User decision (definitive):** IEEE `NaN != NaN` is the wrong choice for
graphix — total equality is needed for maps and is saner in a dataflow language.
So the node-walk is correct and the fused paths are the bug.

Fix: both fused backends now compute the **total order** matching
`Value::partial_cmp` (`(NaN,NaN)→Equal`, `(NaN,_)→Less`, `(_,NaN)→Greater`, else
IEEE `partial_cmp`) and derive all six comparisons from it.
- `gir_interp`: a `cmp_dispatch_float!` macro (3-way `Ordering` → the 6 ops),
  used for the F32/F64 `eval_cmp` arms.
- `gir_jit`: `compile_cmp`'s float branch builds `eq`/`lt`/`gt` from
  `fcmp Equal`/`LessThan`/`GreaterThan` OR'd with the NaN cases — a NaN is the
  only value `Unordered` with itself, so `fcmp(Unordered, x, x)` tests "x is
  NaN"; `bxor_imm(v, 1)` is logical NOT for the `Ne`/`Lte`/`Gte` derivations.
Ordering with NaN is now total on all three paths (`NaN < 1.0` → true,
`1.0 < NaN` → false, `NaN <= NaN` → true).

The original "only equality" fix was incomplete: it made `==`/`!=` agree but the
oracle immediately surfaced that `<`/`>`/`<=`/`>=` still diverged (the node-walk
orders NaN as least, the fused path was IEEE-false) — a clean demonstration that
the differential oracle catches the *whole class*, not just the one case hand-
tested. arith.rs gained `nan_eq_self`/`nan_ne_self`/`nan_lt_value`/`value_lt_nan`
cross-mode fixtures.

### graphix-fuzz V1 — the differential oracle lands (Jun 2026)

First milestone of the differential model-checking fuzzer (`design/graphix_fuzz.md`,
#172): the **oracle**, the foundation both the mechanical fuzzer and the
adversarial agent sources depend on. New `graphix-fuzz` crate (workspace member;
replicates graphix-tests' `TEST_REGISTER` — `#[cfg(test)]`-gated, not importable
— to wire the full stdlib).

- `run_program(code, mode, timeout) -> Outcome` — runs `let result = {code}`
  under one `Mode` (`Interp` = `FusionDisabled` reference / `Fused` =
  `JitDisabled` / `Jit` = no flags), driving to the first `Updated(result)`.
  `Outcome` = `Value | CompileErr | RuntimeErr | Timeout`. Fresh `ExecCtx` +
  in-process resolver per call (no JIT/fusion-state leak between runs, matching
  the test harness). Reuses `graphix_package_core::testing::init_with_flags_and_setup`.
- `check(code) -> Option<Divergence>` — runs interp vs jit; on disagreement also
  runs fused and returns the `Divergence` with a `bisect()` label
  (`interp != fused` ⇒ GIR/emit bug; `interp == fused != jit` ⇒ cranelift
  codegen bug — exactly the #162–#170 diagnosis pattern). `Outcome::agrees_with`
  uses `Value` equality (so the now-total NaN semantics are respected);
  different outcome *kinds* always disagree (catches asymmetric hangs / fusion-
  introduced errors).
- CLI: `graphix-fuzz check <file>` (exit 1 + bisected report on divergence) and
  `graphix-fuzz run <file>` (all three modes).

V1 scope = single-snapshot oracle over pure-sync terminating programs. Validated
live on #174: `check` on `{ let x = f64:0.0/f64:0.0; x != x }` reported
`DIVERGENCE — GIR/emit bug (interp false, fused/jit true)` before the fix and
`AGREE` after — and surfaced the ordering divergence the hand-test missed.

### graphix-fuzz V1 — Source A (mutation engine + fuzz loop) (Jun 2026)

The mechanical front-end onto the oracle. `mutate.rs` parses a seed expression
(`parser::parse_one`), applies 1..=5 AST mutations, pretty-prints, and runs the
mutant through `check`. Three mutations, all **type-blind** (the oracle's compile
step is the validity filter):
- **transplant** — replace a random subtree with a subtree from a donor seed
  (the "mash up two fixtures → novel interaction" idea; high structural novelty,
  low typecheck yield, but the survivors are interaction-rich).
- **swap_binop** — change a binary op within its class (arith/checked/cmp/bool);
  type-preserving.
- **perturb_literal** — push a numeric/bool literal to an edge (0, ±1, MIN/MAX,
  inf, NaN); exercises overflow / float edges.

The AST rebuild (`replace_at` / `for_each_child`) mirrors `Expr::fold`'s exhaustive
child enumeration — preorder index → node, rebuild with one node replaced. Macros
(`r!`/`ra!`) do the recursion (two closures can't both hold the `&mut ctr`
borrow). `corpus.rs` is a curated ~33-seed set biased to the bug-rich shapes
(nested HOFs + captures, composite destructure, select arm bindings, `?`, checked/
value-shape arith, maps, strings). Deterministic xorshift RNG → any run replays
from its `seed`. CLI: `graphix-fuzz fuzz [iters] [seed]` → divergences printed +
written to `fuzz/crashes/`.

**The fuzzer found a bug in itself on its first run** — and the fix is a general
hardening: a mutant `"sum is [|(k, v)| k + i64:7]"` (a lambda interpolated into a
string) reported a "divergence" because a lambda's `Display` embeds a
process-global lambda id that differs per compile (`Abstract(lambda#9489)` vs
`#10249` vs `#9869`) — nondeterministic output, not a backend bug. Fix (per the
design doc): a **double-run nondeterminism guard** in `check` — on a *suspected*
divergence (so nearly free), re-run interp; if it disagrees with *itself*, the
program is nondeterministic (leaked identity/rand/time) → quarantine, not a
finding. Sound: a deterministic real bug has `interp == interp2`, so it still
reports.

Still V1-pending (design doc): the **typed-AST minimizer** (HDD: reduce a found
divergence to a tiny repro), then Source E (adversarial agents — needs only the
existing `check` CLI).

### #175 — graphix-fuzz's first real find: float `%` trapped in the JIT (Jun 2026)

A 150-iteration campaign (`fuzz 150 7`) found a genuine JIT bug at iteration 93:
`f64:0.1 + f64:-1. % f64:3.` → interp/fused `F64(-0.9)`, jit `CompileErr("runtime
did not respond")`, bisected (`interp == fused != jit`) to a cranelift codegen
bug. Hand-minimized to **`f64:7.0 % f64:3.0`** (float modulo); controls confirmed
float `+` and int `%` JIT fine.

Root cause: `compile_bin`'s float-`Mod` arm (gir_jit.rs) emitted a runtime
**trap** — a *deliberate* known gap (cranelift has no `frem`; the comment even
said "arith corpus don't hit this path"). The fuzzer proved that bet wrong, and a
runtime trap crashes the whole runtime task (hence "runtime did not respond"),
not just the kernel. Fix: `compile_bin` now returns `Result<ClifValue>` and the
float-`Mod` arm returns `Err` instead of trapping → the kernel fails to JIT-
compile → `fuse()` falls back to the interpreter, which computes float `%`
correctly. Its sole caller (the `GirOp::Bin` arm) already propagated a `Result`.
Verified via the oracle: `f64:7.0 % f64:3.0` → `AGREE` (all modes `1.0`) after
the fix. Regression: `lib_tests/arith.rs::float_mod` (`FuseExpect::Interp` — it
fuses on the interpreter but the JIT bails). Follow-up: wire the `fmod` libcall so
float `%` actually JITs (needs a module handle threaded into `compile_bin`).

This is the headline validation of the whole effort: a mechanical mutation fuzzer,
~3 days after the oracle landed, surfaced a real latent JIT crash that the hand-
written 1900-fixture suite never hit — exactly the "find the thing neither of us
thought of" goal. The trap was a *known* gap the author judged unreachable; the
fuzzer's value is proving reachability. Smoke runs of the seed corpus otherwise
execute clean.

### graphix-fuzz V1 — typed-AST minimizer (Jun 2026)

The reducer that turns a found divergence into a tiny repro (every fusion bug
this session was diagnosed from a hand-reduced program; #175 was hand-minimized).
Hierarchical delta-debugging on the parsed `Expr`, reusing Source A's AST
machinery:
- `mutate::reductions(prog, target)` — candidate replacements for a node: each of
  its direct children (hoist a sub-expression up) + a few minimal constants
  (`i64:0`/`f64:0.0`/`true`/`null`, collapse a computation to a literal).
- `mutate::replace` / `node_count` / `parse` — the same `replace_at` rebuild.
- `lib::minimize(code, timeout, budget)` — confirm the divergence, capture its
  `bucket` (bisect class + interp/jit outcome kinds), then repeatedly apply
  reductions, keeping any that still parse AND reproduce the **same bucket** (so a
  reduction can't turn bug A into a different bug B); restart the scan on each
  smaller program; stop at a fixpoint or the oracle-call budget (partial minima
  accepted — the oracle is ~3s/check, so the budget is the real bound). The
  oracle's compile step is the type filter — reductions are type-blind, and a
  reduction that breaks typing makes both modes `CompileErr` (agree → rejected).

Wired into the fuzz loop: every divergence is auto-minimized before it's recorded
to `fuzz/crashes/` (both the raw mutant and the reduced form). CLI: `graphix-fuzz
minimize <file>`.

Next (design doc): the richer sources — Source C (typed generation-from-context),
Source B/E (agent harvest + adversarial agents, which need only the existing
`check` CLI), the full-corpus harvest — plus coverage-guided selection (Source D).
And the `fmod` libcall so float `%` actually JITs (#175 follow-up).

### graphix-fuzz V1 — full-corpus harvest (Source A breadth) (Jun 2026)

The mutation source's reach is bounded by its seeds. The hand corpus (~33) had
saturated (clean across hundreds of mutants), so `build.rs` now harvests the
**entire graphix-tests fixture corpus** as seeds: it parses every `.rs` under
`stdlib/graphix-tests/src/{lib_tests,lang}` with `syn` and extracts each
`const NAME: &str = "..."` value (all ~470 `run!` fixtures use this shape; 0
inline strings), `LitStr::value()` resolving `\`-continuations + escapes. Output
→ `$OUT_DIR/harvested_seeds.rs` (`pub static HARVESTED: &[&str]`), `include!`d by
`corpus.rs`; `all_seeds()` chains the hand seeds (guarantee the bug-rich shapes)
with the harvest (breadth — every construct the stdlib tests exercise). The
transplant cross-product grew from ~33² to ~506² (473 harvested + 33 hand).
`syn` is a `[build-dependencies]` with `features = ["full", "parsing"]` (workspace
`syn` is bare `"2"`); `cargo:rerun-if-changed` per test file keeps it fresh.

Nondeterministic harvested fixtures (rand/timer/net/fs) are safe — the double-run
guard quarantines any divergence they cause. Reactive fixtures return their first
`result` value fast (the drive loop takes the first `Updated`), so they don't
stall the campaign.

### Source E adversarial hunt — 8 agents found 10 bugs, 9 fixed (Jun 2026)

The multi-agent Source E hunt (the `design/graphix_fuzz.md` adversarial-agent
source) paid off massively where random mutation had hit its ceiling: 8 agents,
each given the spec + CLAUDE.md bug taxonomy + the `graphix-fuzz check` CLI and
told to reason about where JIT codegen and the interpreter could diverge, found
**10 confirmed deterministic divergences in 4 root-cause clusters** — after a
400-mutant random campaign over the full ~506-seed corpus found ZERO. Intelligent
search assembles the coordinated multi-feature shapes random mutation can't. 6 of
the 10 were *process-crashing* (SIGFPE/panic killing the runtime), not value
disagreements. The negative space was also valuable: across ~449 programs the
higher-level fusion machinery (closures, nested HOFs, composites, select,
strings-as-values, bounds seams) held firm — strong evidence of solidity.

**Cluster A (6 bugs, CRITICAL) — integer div/rem by zero & signed MIN/-1
overflow crashed both fused backends.** `i64:10 / i64:0`, `i64:MIN % i64:-1`,
`i32:MIN / i32:-1`, `array::map([0], |x| 1/x)`. The node-walk turns these into an
arith error → bottom (Timeout); the JIT emitted raw cranelift `sdiv`/`srem` (→
`#DE`/SIGFPE process crash); the fused-interp used `wrapping_div`/`wrapping_rem`
(→ PANIC on div0, silent wrap on MIN/-1). The exact integer sibling of #175
(float-% JIT trap), which only ever covered floats. Fix: (JIT, `gir_jit.rs`
`GirOp::Bin`) a divisor/overflow guard `brif` to `pending_exit` (kernel returns
None = bottom), mirroring the QopUnwrap pending pattern; (interp, `gir_interp.rs`
`eval_bin`) `checked_div`/`checked_rem` → on None, signal pending and `return
None` (eval_bin now returns `Option<RegValue>`, caller propagates with `?`). Both
now match the node-walk's bottom. Valid division still JITs (the guard's `bad`
branch is dead for nonzero divisors).

**Cluster B (2 bugs, HIGH) — `GirType::Error` missing from the gir-interp DynCall
arg-marshalling match.** `is_err(error(1))` crashed the fused-interp:
`wrap_value_shape` carries an `error(..)` as `EvalResult::DateTime(Value::Error)`,
but the value-shape arg arm declared `DateTime | Duration | Bytes | Map` and
OMITTED `Error` → catch-all `panic!`. The recent GirType::Error work added the
variant to ~29 exhaustiveness-forced sites, but this arm is a *tuple match behind
a catch-all*, which exhaustiveness doesn't force — so it slipped (a recurring
14th-commandment hazard: catch-all-guarded value-shape enumerations). Fix: add
`| GirType::Error` to the arm.

**Cluster D (1 bug, MEDIUM) — StringInterpolate over a non-scalar part crashed
the fused Concat.** `"[words[0]]"` interpolates `words[0]` (typed
`Nullable<string>`, the bounds-check `[elem, Error]` shape); the interp `Concat`
arm only handles String/scalar parts → panic. The JIT already returns Err on a
non-scalar part and falls back to the node-walk; the frontend emitted GIR the JIT
refuses but the interp ran and crashed on. Fix: `StringInterpolate`'s emit arm
bails (`rec_bail`) when any part's GirType isn't String/Prim, matching the JIT.

**Cluster C (1 bug, MEDIUM, SEMANTICS — flagged, not fixed) — value-shape
unchecked arith doesn't replicate the node-walk's error-drop.**
`duration:1.s - duration:2.s` (underflow): the node-walk unchecked op drops the
`Value::Error` → bottom (Timeout); the fused `GirOp::ValueArith` emits the error
as a value. Like NaN (#174), this is a deliberate semantics call the user should
make: match the node-walk (drop → bottom, the canonical-node-walk default), or
change all three to *propagate* the error value (arguably better UX). Deferred to
the user.

**META-1 (oracle blind spot, FIXED) — `graphix-fuzz check` compared only
interp-vs-jit**, so it was structurally blind to the `interp == jit != fused`
class (clusters B and D — gir-interp panics the JIT dodges via node-walk
fallback); those were only found via 3-mode `run`. Fixed `check` to compare all
three modes (interp == fused == jit) — strictly stronger, catches the class
automatically.

**META-2 (minimizer, follow-up) — the minimizer core-dumps on div-mutations**
(it re-runs the trapping JIT in-process). Run candidates in a subprocess (or
catch the signal) so it can auto-minimize crash classes. (Moot for the int-div
crashes now they're fixed, but the isolation is still correct.)

Regressions in `lib_tests/arith.rs`: `div_valid`/`mod_valid` (guard didn't break
division — still JITs), `div_in_untaken_arm`, `err_as_dyncall_arg` (fuses+JITs),
`interp_nonscalar_part` (bails → None). The div0-crash itself can't be a `run!`
fixture (bottom → timeout); it's verified via the oracle (`check` AGREE, all
Timeout) and the full suite confirms division is intact. 132 compiler + 1976
graphix-tests green.

**The lesson (validates the whole design):** the mechanical differential fuzzer
(Source A) and the agent sources (B/E) are complementary, not redundant — random
mutation swept the corpus clean, then reasoning agents found 10 real bugs in it.
Source E needed *nothing new built* — only the `check` CLI. It is the
single highest-value bug-finder we have.

### Cluster C fixed (saturating duration sub) + auto-running regression corpus (Jun 2026)

**Cluster C resolved the right way** (the user's call): `duration:1.s -
duration:2.s` diverged because `std::time::Duration` is unsigned (no negative
durations) — the unchecked `-` underflowed to a `Value::Error` that the node-walk
dropped to bottom but the fused backend emitted. Fix is in **netidx-value**
(`op.rs` `impl Sub for Value`): the unchecked Duration−Duration case now
`saturating_sub`s to `0s` instead of `checked_sub`→error. `checked_sub` (the `-?`
operator) still detects the underflow, so `duration -? duration` errors as
expected. Now `1s - 2s = 0s` in all three modes — the divergence dissolves (no
error → nothing to drop or emit). The residual general value-shape unchecked-arith
error-drop (datetime/duration *overflow* on add/mul) is the open part of #177.

**Auto-running regression corpus.** `graphix-fuzz/findings/**/*.gx` is now a
committed regression corpus, embedded at build time (`build.rs` →
`REGRESSION_CORPUS: &[(name, program)]`, stripping the `//`-comment headers).
`graphix-fuzz regress` runs every finding through the (now 3-way) oracle and
reports any that diverge — a fixed bug coming back. `graphix-fuzz fuzz` runs this
gate *before* each mutation campaign (short 3s per-program timeout: a regression
surfaces fast; a legitimately-bottom div0 program just confirms "still
all-Timeout"). The dir grows by adding `.gx` files (the 10 round-1 finds are
seeded there with their bisect + root-cause in the header) and rebuilding. All 10
findings currently `AGREE` (every bug fixed). `lib::run_regression` /
`regression_corpus_len` back the CLI.

**The loop** (user's directive — "more subagents, more programs, saved in corpus,
until we stop finding bugs"): each round runs a bigger Source E hunt, fixes the
confirmed finds, saves the minimal repros to `findings/` (auto-run thereafter),
and repeats until a round comes back empty. Round 1: 8 agents → 10 bugs. Round 2:
12 agents on deeper/unexplored angles (bit-ops, casts, exotic ints/decimal, maps,
deep arrays, error-flow, strings/bytes, datetime/duration, variants/ADTs, select/
sample, closures/cross-kernel, interpolation/Display), told the round-1 classes
are already fixed so they hunt new ones.

### graphix-fuzz Source C — type-directed program generator (Jun 2026)

Built while the Source E agent rounds were API-rate-limited (the generator is
purely mechanical — no API — so it can run for as long as you let it; the user's
"leave it running for a month on a cloud instance" use case). Unlike Source A
(mutating seeds), this builds valid programs from NOTHING, reaching shapes no
fixture contains.

`generate.rs`: TYPE-DIRECTED text generation. `gen_typed(ctx, ty, depth)` emits a
graphix expression *of type `ty`*, recursing through only the constructions that
produce `ty` (literals, in-scope refs of `ty`, arith for numeric, cmp/bool for
Bool, tuple/array constructors, `select`). This guarantees type-correctness BY
CONSTRUCTION — the killer constraint (a random parse-valid graphix program
typechecks ~never; the existing `expr/test.rs` proptest is a *parser* round-trip
for exactly this reason). It emits TEXT while tracking each subexpression's
`GenType`, so the oracle compiles from text (no AST/typecheck-cell plumbing). A
`GenCtx` of in-scope `(name, GenType)` bindings means `let`-bound vars are
referenced by later expressions — the internal data dependencies that stress
fusion's region/dataflow analysis. Only pure deterministic constructs (no rand/
time/net/fs), so the oracle stays sound. Deterministic xorshift seed → replayable.

V1 covers i64/f64/u8/bool/string scalars, +/-/*/`/`/% (biased toward +/-/* since a
generated `/0` drops to bottom = slow Timeout check), comparison, &&/||/!, tuples,
arrays, and `select` (numeric scrutinee + literal arm + catch-all). **90% of
generated programs compile** (type-directed; the ~10% rejects CompileErr in all
modes → agree → harmlessly filtered). CLI: `graphix-fuzz generate [iters] [seed]`
(runs the regression gate first, then generates + checks + auto-minimizes + saves
divergences); `graphix-fuzz gen [n] [seed]` prints generated programs for
eyeballing.

The four sources now exist: A (fixture mutation), C (this generator), E
(adversarial agents), plus the regression corpus + minimizer + 3-way oracle.
Follow-ups: composite accessors (`t.0`, `a[i]`), HOFs, structs/variants/maps,
value-shape types (datetime/duration/bytes/error), and coverage-guided selection
(Source D) to steer generation toward new GirOp/FUSEBAIL coverage instead of
uniform random — the "evolutionary" layer that makes a month-long run efficient.

### `wait_result_or_idle` + fuzzer parallelism/corpus + dead-statement elimination (Jun 2026)

Three coupled pieces from a fuzzer-driven session.

**`GXHandle::wait_result_or_idle(eid)` (graphix-rt).** A quiescence-aware
result wait the main loop services: returns `Some(v)` the cycle `eid`
emits `v`, or `None` the moment `cycle_ready()` goes false (no future
cycle can produce a result). `ToGX::WaitResultOrIdle` registers a
`result_watch: Option<(ExprId, oneshot::Sender<Option<Value>>)>` on `GX`;
`do_cycle`'s node-emit loop fulfills it `Some` when the watched expr
emits, and the top-of-loop idle check fulfills it `None` on quiescence.
The fuzz oracle's `drive` replaced 2ms polling with one
`wait_result_or_idle` + an rx-drain on `None` (a synchronous program
normally emits its value during the compile cycle, before the watch is
registered, so the value lands in the event stream, not the watch). A
bottom program (div0) is now detected in ~0.46s instead of sleeping out
the timeout.

**Fuzzer parallelism + persistent deduped corpus (graphix-fuzz).**
`parallelism()` 2×→8× cores (measured ~26%→~91% of 14 cores); `check`'s
three modes run via `tokio::join!`; and the inline-minimize-stalls-the-
pool problem (minimize is ≈80 serial checks that drained the cores
between bursts) is gone — `run_pool` now spawns a bounded-parallel
minimize+record task per divergence so the check pool stays saturated.
New `Corpus` struct: loads existing `*.gx` at startup (dedup key = the
minimized program text), and `record()` writes each genuinely-new
divergence to its own file IMMEDIATELY on discovery (not at run end).
`generate`/`fuzz` accept `iters = forever`/`0` to run until killed,
printing new divergences live with a periodic heartbeat. NB: the
workspace target dir is redirected to `/home/eric/tmp/target` — run the
binary via `cargo run -p graphix-fuzz` or the real path, not
`./target/...`.

**Dead-statement elimination — fused block bottom-poison (fusion).** The
first real generate campaign found a 37/500 divergence cluster, all one
root cause: a fused block (`emit_do`/`emit_do_as_expr` → `GirOp::Block`)
evaluated EVERY statement eagerly, so a bottom in a non-tail statement
(div/mod-by-zero → `DYNCALL_PENDING`) poisoned the whole kernel — while
the node-walk treats each statement as an independent node (an
unreferenced bottom binding never fires; the tail still produces). Fix
(user chose dead-statement elimination over dependency-isolation): a
backward liveness pass (`prune_dead_stmts` for the `out: Vec<GirStmt>`
stream, `prune_dead_lets` for the expr-form `lets`+`tail`) drops a
non-tail `let` whose binding is unreferenced by the tail/later-live
statements AND whose value is PURE (`!gir::expr_has_call` — no
DynCall/Call, so no side effect beyond the dropped arith-error log), and
a pure bare `Discard`. Backward order + not-propagating-dropped-refs
collapses dead chains in one sweep (`{let v=1/0; v; 42}` → 42).

The liveness MUST count ALL references, not just `GirOp::Local`: accessor
and array-HOF ops reference a composite/array binding via a `name`/
`array` field (e.g. `array::map(a, …)` reads `a` through `ArrayMap.array`,
not a `Local`). A first cut used a `Local`-only `visit_ops` scan, wrongly
dropped the live array binding, and crashed 58 tests with "undefined
array `a`". Fixed with `op_ref_name(&GirOp) -> Option<&ArcStr>` —
EXHAUSTIVE over `GirOp` (a new name-bearing op forces an update here
rather than a silent use-after-free), combined with `visit_ops` for the
sub-expr recursion. `expr_has_call` + `visit_ops`/`visit_ops_stmt` are
now `pub(crate)`.

The fix cleared 36/37; the remaining 1 is a DEEPER class (a bottom
binding that IS referenced, but only by a select arm that isn't taken at
runtime — the node-walk is lazy per-arm, the fused kernel is eager;
saved to `fuzz/crashes`). Regression fixtures in `lib_tests/arith.rs`
(`dead_let_bottom`, `bare_bottom_stmt`, `dead_let_fixpoint`,
`dead_let_keeps_live`) assert all three modes agree. 132 compiler + 1997
graphix-tests green.

### Select-arm sinking — fused kernel matches node-walk per-arm laziness (Jun 2026)

The dead-statement-elimination fix above cleared 36/37 of the first
generate campaign's divergences; the 37th was a DEEPER class the fuzzer
kept finding: a bottom (div0/mod0) in a block-`let` that IS referenced —
but only by a select arm that isn't taken at runtime. The node-walk
evaluates select arms LAZILY (an unselected arm's body never fires via the
demand-driven `update` walk), so `{ let v=1/0; select 5 { 2 => v, _ => 99 } }`
yields 99; the fused kernel evaluated every block-`let` EAGERLY before the
select and so the bottom poisoned the whole kernel → Timeout.

**Design via workflow** (understand → 3 approaches × adversarial judges).
An empirical 12-case truth-table (run through the oracle) pinned the exact
boundary: a bottom INLINE in an un-taken arm BODY already AGREES across all
modes — both fused backends ALREADY evaluate arm bodies lazily (interp
`eval_body` runs only the matched arm; the JIT compiles each arm into its
own `brif`-reached CLIF block; `GirOp::IfChain` returns on first match).
So the ONLY problem is hoisted block-lets being eager. The judges scored
**sinking** (correctness 7, cost 8, **perf 10**, value-SOUND) over
poison-as-value (correctness 6, cost 3 — and it diverges on async-DynCall
bottoms it carves out of the model) and confirmed sinking can never produce
a wrong finite value — it only relocates WHERE a let is evaluated; the
runtime still picks the arm.

**The fix (compile-time GIR→GIR, ZERO backend/GirOp changes), all in
`fusion/lowering.rs`:** `sink_into_select_stmts` (statement form, trailing
`GirStmt::Select`) and `sink_into_select_lets` (expression form,
`GirOp::Block { lets, tail: IfChain }`) move a pure, arm-exclusive block-let
DOWN into the arm body/value that consumes it, where it's lazy for free.
Run after `prune_dead_stmts`/`prune_dead_lets` in `emit_do`/`emit_do_as_expr`.

Rules: a let stays EAGER if referenced by an arm CONDITION/guard or the
stabilized scrutinee (those pick the arm), by another still-outer let, or
is impure (`expr_has_call` — moving it would change side-effect timing). A
single-arm let is MOVED; a *bottom-capable* (`is_bottom_capable`: int
div/mod or `?`) let used by ≥2 arms is DUPLICATED into each (sound because
pure ⇒ idempotent; gated so harmless work isn't cloned). A fixpoint re-scan
collapses dependency chains (`let v=1/0; let w=v+1; …w…`: `w` sinks first,
then `v`'s only use is inside the arm, so `v` follows). Expr form wraps the
arm value in `GirOp::Block { lets:[l], tail: old_value }`; it peels the
scrutinee-stabilization `Block` wrapper to reach the `IfChain` and treats
the scrut temp as eager. Termination: each progressing iteration strictly
shrinks the outer let count.

All 12 truth-table cases AGREE (the 5 that diverged — one-arm, multi-arm via
duplication, chain via fixpoint, nested-select, mod — now match; the 7 that
already agreed stay agreeing). Composite/string/value-shape sunk lets and
multi-arm composite duplication all verified value-correct on both backends.
Regression fixtures in `lib_tests/arith.rs` (`sink_one_arm`, `sink_mod`,
`sink_multi_arm`, `sink_chain`, `sink_nested`, `sink_literal_arm_taken`,
`sink_expr_operand`). Post-fix fuzz campaigns (seed 7 that found the
original, + fresh seed 99) find 0 divergences. 132 compiler + 2018
graphix-tests green.

**Known gaps (both safe Timeout-vs-finite, NOT wrong values; tracked):**
(1) a bottom in an arm GUARD (`n if v>0 =>`) — the node-walk falls through
to the next arm (a bottom guard fails to match → 99), but the guard is
evaluated to PICK the arm so it can't be sunk; it stays eager → Timeout.
The fuzzer's generator never emits guards, so it can't auto-find this; the
real fix is bottom-guard → arm-skip in IfChain/Select codegen (a separate
mechanism). (2) value-shape unchecked-arith overflow that emits an error
VALUE rather than bottom (#177) — orthogonal, already tracked.

### Select-arm sinking — adversarial-review hardening (Jun 2026)

A focused adversarial-review workflow (4 attack angles → verify) on the
sinking pass found 5 confirmed divergences in TWO classes (all
safe-direction Timeout, never wrong values) — fixed 4, the 5th deferred:

1. **Call-bearing bottom-capable lets** (`str::parse(x)?`, `1/0 +
   str::len(x)`): the `expr_has_call` purity gate ran BEFORE the
   bottom-capability check, excluding any bottom-capable let that also
   contained a (pure) call. Since `EffectKind` has no purity flag (rand is
   `Sync` but nondeterministic), the gate is now **bottom-capable OR pure**:
   a pure let always sinks; a call-bearing let sinks only when bottom-capable
   (where sinking is REQUIRED to match the node-walk's lazy value, and the
   call is a value-returning builtin — side-effecting I/O builtins return
   Unit → `Discard`, never a value-let). An impure NON-bottom let (a bare
   `rand`) stays eager so its effect can't become arm-conditional (and since
   it doesn't bottom, eager eval agrees anyway).
2. **Multi-arm dependency chains** (`let x=len(); let w=x/0; let v=w+1;
   select x { 6 => v, _ => v }`): the multi-arm-duplication gate was
   `is_bottom_capable`, which blocked sinking a PURE intermediate (`v`) that
   a bottom-capable producer (`w`) feeds. Dropped the gate — duplicating a
   sinkable let into each consuming arm is **runtime-neutral** (exactly one
   arm fires, so it runs once, same as eager) and value-safe (pure ⇒
   idempotent). Now any sinkable arm-exclusive let, single or multi, sinks.
3. **Nested-tail selects** (`select s { 7 => select t { 3 => v, _ => 50 },
   _ => 99 }`): sinking `v` into the OUTER arm body left it eager *before*
   the inner select. Both forms now RECURSE — after the fixpoint,
   `sink_into_select_stmts` re-runs on each arm body and `sink_into_select_lets`
   drives `sink_nested_value` into each arm value (a `Block` wrapping a deeper
   `IfChain`, or a bare `IfChain`).

Regression fixtures added (`sink_call_bearing`, `sink_qop_parse`,
`sink_multi_arm_chain`, `sink_nested_stmt`, `sink_rand_stays_eager`). All 12
truth-table cases + the 4 fixed findings agree; 132 compiler + 2018+
graphix-tests green; two 600- and one 1500-program fuzz campaigns find 0
divergences.

**The 5th finding — DEFERRED (a known gap):** a bottom feeding an un-taken
arm of a select that is BOUND TO A LET and used downstream (`let r = select
s { 7 => select t { 3 => v, _ => 50 }, _ => 99 }; r + 0`). The select isn't
the block tail (the tail is `r + 0`), so the tail-only sink never reaches it.
The mechanical generator does NOT produce this pattern (0 divergences across
3600+ generated programs; it took an adversarial agent to construct), so the
fuzzer can't auto-find it. Fully closing it requires generalizing the pass to
sink into a select WHEREVER it appears in the block (any let value, not just
the tail) — i.e. general partial-lazy-code-motion, an open-ended optimization.
Deferred pending a decision on the investment level. The two earlier gaps
(bottom in an arm GUARD; #177 value-shape error-value) also remain.

### Representable value-bottom — interp phase landed (Jun 2026)

The principled fix the user directed ("of course bottom has to be a value,
just like in the interpreter"): a value-level bottom (integer div/mod-by-
zero, signed MIN/-1, `?` on an error value) is now a REPRESENTABLE VALUE in
the fused **interpreter**, replacing the global `DYNCALL_PENDING` abort for
value-errors. Full design + adversarial review in
`design/representable_bottom.md`. The JIT half (per-value validity bit +
taint) is the tracked continuation.

**Why bottom was a problem (the root cause):** the node-walk's `None`-from-
`update` is LOCAL to one node; fusion collapses N nodes into one kernel, so
signalling bottom via the global flag aborts the WHOLE kernel even when the
output never consumes the bottom (an un-taken select arm, a dead let, a
let-bound select used downstream, an arm guard). Sinking + dead-elim were a
structural band-aid with an edge-case tail.

**The interp model (`gir_interp.rs`):** new `EvalResult::Bottom` (payload-
free, like node-walk `None`). The two value-bottom producers — `eval_bin`
Div/Mod and `GirOp::QopUnwrap`'s error branch — emit `Bottom` instead of
`set(DYNCALL_PENDING)+None`. A new `bottoms: Vec<ArcStr>` env slot holds
bottom let-bindings (`push_local` routes `Bottom` there; `GirOp::Local`/the
name-reading accessors return `Bottom` for a bottom binding). An `ev!`
operand macro ABSORBS bottom through every pure op (Bin/Cmp/BoolBin/Not/
Cast/ValueArith/ValueEq/IsNull/Concat/producers/accessors/slice/map/bytes/
ArrayInit) — any bottom operand → bottom result. `BoolBin` short-circuit is
preserved. Select/IfChain already evaluate only the taken arm, so an
un-taken arm's bottom is never computed; the taken arm's bottom flows out
(absorb); a bottom GUARD → arm does NOT match → fall through (byte-for-byte
the node-walk's `is::match`). The boundary `into_value_checked()` at
`GirNode::update` converts a bottom OUTPUT → `None` (the ONE site) — a
non-bottom output marshals normally.

**Async-pending stays a whole-kernel abort, cleanly separated:** the only
remaining `DYNCALL_PENDING`/`None`-channel use is the DynCall dispatcher
returning None (genuine "no value this cycle"). The four value-bottom sites
no longer touch the flag (the cosmetic rename → `ASYNC_PENDING` is deferred;
the separation itself is done). Per the adversarial review, a bottom DynCall/
Call ARGUMENT does NOT absorb — the node-walk's bottom arg-edge doesn't fire
but the call still dispatches, so the faithful fused analogue is "no fresh
value this cycle" (the kernel emits `None`); the arg loops `return None` on a
bottom arg, keeping value-bottom and call-dispatch disjoint by construction.

**Sinking + dead-elim are now OPTIMIZATIONS, not correctness** — they elide a
provably-dead bottom so it's never represented; a bottom that survives them
flows as a value and is absorbed at the output. Kept on (the suite stays
green; bottoms are elided in existing fixtures, so the bottom-as-value path
is exercised by the validation probes, not the differential suite — the
finding-5/guard fixtures land with the JIT, since `jit` mode still aborts).

**Validated:** fused-interp == node-walk on the full truth-table AND the two
cases the sink pass could NOT handle — `guard_bottom` (`n if v>0 =>` with
v=1/0 → 99) and `finding5` (let-bound nested select, bottom in an un-taken
inner arm → 50) — resolved UNIFORMLY by the same mechanism. 132 compiler +
2033 graphix-tests green.

**Interp gaps to close before the final fuzz campaigns** (not suite-
exercised): a bottom produced PER-ELEMENT inside a HOF body (`array::map(a,
|x| x/0)`) still hits an `into_value` on `Bottom` (the array-HOF SOURCE being
bottom IS handled); a bottom tail-call arg conservatively returns a bottom
output. Both rare; tracked.

**Next (JIT, Phases 3–4):** per-value validity — tainted scalars as
`CompiledExpr::Scalar2{value,valid}` (non-tainted stay `Single`, zero cost);
composites/strings use a NULL marker, value-shape a reserved `BOTTOM_DISC`
ABOVE the netidx disc space (the review caught `0x4000` colliding with
`Bool`) with guarded decode; the div guard becomes a branchless
`valid=!bad` sentinel; the IfChain merge threads validity; the boundary
decodes the output marker → None; cross-kernel `Call` bails on a tainted
scalar arg/return (V1). Then demote sink/dead-elim + add the finding-5/
guard/`async_call(x/0)` all-modes fixtures + fuzz campaigns.
