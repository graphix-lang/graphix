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
cargo test -p graphix-compiler       # Test specific crate
cargo test pattern                   # Run tests matching name
```

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

`KirOp::DynCall` (HOF dispatch in the cranelift JIT) now supports
fn-typed kernel params whose args *or* return are composite
(tuple/struct/variant), not just scalars. Key pieces:

- **Fusion now builds Param-source HOF kernels.** The `tail_call_slots`
  and `source_args` builders in `fusion.rs` used to `return None` on
  any fn-typed param, so a HOF whose fn-param came from `lambda.args`
  (vs. an outer binding) never fused at all. They now `continue` past
  fn-typed params — `build_arg_layout` (kir_interp) already
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
  drives the choice and now classifies `KirOp::DynCall` as `Owned`.
  Using the borrowed helper on an Owned source leaks the original;
  the move helper on a Borrowed source double-frees it.

### Composite block-lets & composite IfChains in the JIT (May 2026)

Pre-M8.4 hardening — bigger fused subgraphs hit two JIT codegen
crashes that smaller kernels didn't:

- **`KirOp::Block` with a composite let panicked.** The block-
  expression arm did `prim_of(&l.value.typ)` on every let — a panic
  for any tuple/struct/variant let. It also only `mark`/`truncate`d
  `env.locals`, so composite/variant block-lets leaked into the
  enclosing scope (and weren't dropped, leaking per-iteration in a
  loop body). Fixed: the arm now routes composite/variant lets to
  `bind_composite`/`bind_variant`, snapshots all three `env` lists,
  drops the block-scoped composites/variants on exit, and runs the
  block tail through `ensure_owned_composite` (so a tail that aliases
  a block-scoped local outlives the block).

- **Composite `select`-as-expression (`KirOp::IfChain`) panicked.**
  `compile_ifchain` took a `PrimType` and `prim_to_clif`'d it for the
  merge-block param — a panic for a tuple/variant-typed if-chain. It
  now takes a `ClifType` (via the new `clif_of`, composites → `I64`)
  and runs each arm result through `ensure_owned_composite` so the
  merge always receives an owned pointer regardless of which arm won.

- **`JitEnv::mark`/`truncate` now cover all three binding lists**
  (`locals`, `composites`, `variants`) via an `EnvMark` snapshot — not
  just `locals`. `truncate` is compile-time `env`-Vec hygiene only
  (no runtime drops); drops are the job of scope-exit code
  (`KirOp::Block`) and terminating statements (`KirStmt::Return` via
  `drop_owned_composites`). `LowerCtx::param_count` became
  `param_mark: EnvMark` so a `TailCall` rebind resets every list to
  the post-param state.

- **`ensure_owned_composite`** is the single ownership choke point:
  given a composite expr + its compiled value, it refcount-clones a
  `Borrowed` source and passes an `Owned` one through. Used by
  `KirStmt::Let`, `KirStmt::Return`, the `KirOp::Block` tail, and each
  `compile_ifchain` arm. `classify_composite_source` now also
  classifies `KirOp::Block` and `KirOp::IfChain` as `Owned` (correct
  *because* those sites pre-own their result via this helper).

### AOT pipeline removed (May 2026)

The ahead-of-time Rust-source-emission backend is gone — the JIT
(cranelift) is now the only fusion backend. Removed: `fusion.rs`'s
`emit_kernel` / `emit_function_kernel*` (Rust-source emitters),
`rewrite_program*` / `rewrite_walk` / `RewriteState` / `FusedKernel`
(the AST-rewriting program pass), `emit_package` / `render_*` /
`write_package` (cargo-package emission), and `walk_and_fuse` /
`try_fuse_lambda`; `kernel_ir.rs`'s `kir_to_rust_*` emitter plus the
`rust_name`/`rust_op`/`rust_src` helper methods and the now-dead
`rust_name` field on `Input`/`ArrayInput`/`TupleInput`/`StructInput`/
`VariantInput`/`TailCallSlot`/`SelfArg`; and the `graphix compile`
CLI subcommand (`handle_compile` + the standalone-binary build
helpers) from `graphix-shell`. ~3700 lines net. The live fusion path
is unchanged: `build_kir_kernel` → `KirKernel` → cranelift JIT (or
`kir_interp`). This clears the deck for M8.4 — maximal sync subgraph
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

**Phase 1a — Single KirNode init chokepoint.** All three KirNode
constructors (`new`, `with_jit`, `with_async_jit`) now take
`&mut ExecCtx` and route through a private `build` chokepoint that
runs both `pre_init_binding_slots` + `pre_init_builtin_slots`
internally. Previously the lazy-fusion path manually called both and
the FusedRegion path called only the builtin one — easy to forget,
and forgetting the builtin one panicked with "fn-arg value isn't a
LambdaDef" on first DynCall into a fused stdlib builtin. Now
impossible to skip.

**Phase 1b — Consolidated let-routing.** Three near-duplicate `match
KirType -> push to ctx.*_inputs` blocks (in `emit_do_as_expr`,
`emit_bind_stmt`, and parameter-discovery) replaced with one helper
`register_kir_binding(ctx, name, &KirType)`. Adding a new `KirType`
variant is now one site instead of three.

**Phase 2 — Widened KirType to support nested composites.**
`Array(PrimType)` → `Array(Box<KirType>)`; `Tuple(Vec<PrimType>)` →
`Tuple(Vec<KirType>)`; `Struct(Vec<(ArcStr, PrimType)>)` →
`Struct(Vec<(ArcStr, KirType)>)`; `Variant(...Vec<PrimType>)` →
`Variant(...Vec<KirType>)`. The `Input`/`ArrayInput`/`TupleInput`/
`StructInput`/`VariantInput` slot structs widened similarly. New
`KirType::as_array_prim()` helper for sites that still need a flat
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
`KirType::from_type`. Falls through to `emit_expr` only for
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
`KirOp::TupleGet.elem_typ` and `KirOp::StructGet.elem_typ` from
`PrimType` to `KirType`; `KirOp::ArrayGet` reads its element type
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
KirOps' `elem_types: Vec<PrimType>` would need widening too, plus
interp/JIT side updates). Follow-up.

**Remaining refactor work (tracked as tasks):**
- Producer-side composite-element support (TupleNew/StructNew/
  VariantNew accepting composite fields).
- M8.4(g): flip the switch + delete lazy path. Phase 4's unification
  makes this a much smaller delta.
