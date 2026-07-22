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
- `EFFECT` (default `Async`): sync/async classification — `Sync` iff every
  output appears on the same cycle as its trigger (fusion boundary otherwise)
- `STATELESS` (default `false`): declare `true` iff deleting the builtin's
  `Apply` and re-initing it fresh is unobservable — no cross-invocation state
  (`count`/`sum` accumulate), no per-invocation effect (`print` emits), no
  external-value mutation (`buffer::encode`); internal memos (a compiled
  `Regex`, scratch buffers, a typecheck-derived cast type) are fine. Only
  consulted for `Sync` builtins, by the transient-recursion gate
  (`design/transient_recursion.md`) — a wrong `true` is a semantics bug, a
  wrong `false` only costs memory. Both consts are pulled through
  `EvalCached`/`CachedArgs` and recorded per name as `BuiltinFacts`
  (`ctx.builtin_effect`/`ctx.builtin_stateless`).

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

### Permanent debug env vars (fusion/typecheck)

- `GRAPHIX_DBG_BIND=1` — print every `InitTVars` tvar bind in `contains`
  (name, cell addr, bound type). The tool for "who bound this cell" —
  found the select-arm greedy narrowing (soak jul05 item 12) twice.
- `GRAPHIX_DBG_KERNELS=1` — print each lambda kernel built by
  `build_lambda_kernel` (name + frozen return type + AbiKind). Locates
  which per-slot/cross-kernel callee actually compiled.
- `GRAPHIX_DBG_INVOKE=1` — print each fused-kernel runtime invocation
  (kernel name, `event.init`, per-input fired/present). Pins WHICH
  kernel a JIT crash happened in (the frame is unsymbolized native code).
- `GRAPHIX_DBG_REGION=1` — dump fused-region input wiring (name/BindId/
  type+deref/constraints/slot kind).
- `GRAPHIX_DBG_FREEZE=1` — dump region freeze outcomes.
- `GRAPHIX_DBG_DEPTH=1` — print the lambda id + `tail_loop` gate at every
  call-depth-guard trip, and each `mark_recursion` decision (id/self_bind/
  structural/sync). The tool for "why didn't this recursion tail-loop" —
  found the runtime-clone back-edge effect miss (soak jul08g div 4).
- `GRAPHIX_DUMP_CLIF=1` — dump every compiled kernel's CLIF (note: the
  display shows `u0:N` func indices, not helper names; map N to the
  registration order of the helper table in `emit_helpers.rs`).
- `GRAPHIX_DBG_VARS=1` — print every runtime variable event (`REF_VAR`/
  `UNREF_VAR` wake-interest refcounts, `SET_VAR` cross-cycle writes,
  `NOTIFY_SET` same-cycle bind delivery + interest map). The tool for
  "who publishes/wakes this bind" — found the dead-eliminated module
  statement (a region waiting forever on a feeder whose producer was
  spliced away, 2026-07-08). Lives in graphix-rt (rt.rs).
- `GXDBG_EFFECT=1` — effect-analysis debugging: `EFFECT-ASYNC-NODE`
  names each node that makes a body read async, and
  `EFFECT-ASYNC-FALLBACK` marks every call site whose callee couldn't
  be resolved and defaulted Async. The tool for "why did this lambda
  classify Async" — the surviving core of the old `GXDBG_FOR` (which
  also traced the For node's sync gate; For is gone, the effect prints
  found the subtree-analysis effect fact miss, jul10e, 2026-07-11).
- `GXDBG_INSTANCE_FUSION=1` — print each per-callsite instance's
  region-fusion pass in `GXLambda::fuse` (fused delta + new failures).
  The tool for "did this monomorphic instance body fuse and what
  blocked it".
- `GXDBG_CS=1` — print every CallSite dispatch (spec, bound-this-
  cycle, apply kind lambda/builtin, any-arg-fired). The tool for
  "does this call dispatch and to what".
- `GRAPHIX_DBG_TVAL=1` — print every `TVal` render step (deref'd type
  + naked value) as the typed printer walks. The tool for "why did
  this value print in this form" — found the union-member selection
  picking the never() arm's ⊥-settled cell over the concrete member
  (jul19f divergence_000000, the interp-vs-jit tuple-render split).
- `GXDBG_RESOLVE=1` — print every static-resolution read (`RESOLVE`:
  spec, BindId, unstable/b2l/cached hit), the index writes
  (`B2L-INS` at Bind tc0, `B2L-PROXY` at interface re-export
  bridging), and `RESOLVE-DISCARD` when a static bind is dropped back
  to dynamic on `AbstractOpaque`. The tool for "why didn't this call
  site statically resolve" — found the batch-entry
  `bind_to_lambda.clear()` that made shell fusion a race (the jul12
  resolution flap).
- `GRAPHIX_DBG_PERF=1` — cumulative runtime-lazy-bind phase counters
  (bind/setup/typecheck1/analyze/transient-gate times, prime/replay
  times, park delete/refs times), dumped to stderr every 250ms by a
  background thread (`perfdbg.rs`). The tool for "why is the interp
  slow on re-fired lazy binds" — found BOTH jul22b transient-recursion
  perf dragons (prime-park thrash + the `lambda_defs`/`LambdaIds`
  typecheck1 degradation) via growth-law analysis of the dumps.
- `GRAPHIX_DBG_CYCLE_BT=1` — print a backtrace at every
  `cycle_refused` mark (the occurs-refusal poison bit, both the
  `mark_cycle_refused` sites and the TVar×TVar positional guard).
  The tool for "which walk refused this merge" — established that
  the jul22e flap class's marks are channel-indistinguishable from
  genuine infinite types (~5% name-walk, rest positional), killing
  the scoped-aliasing remodel in an hour (see
  design/tvar_constraints.md's 2026-07-22 note).

### Type Alias Expansion in Contains

When `contains` encounters a `Type::Ref` (e.g. `Result<T, E>`), the Ref case at `contains.rs:56` expands both sides via `lookup_ref(env)` before recursing. This means TVar bindings established during `contains` store the **expanded** form (e.g. `[T, Error<E>]` instead of `Result<T, E>`). Code that inspects resolved types must handle both the `Type::Ref` form and the expanded `Type::Set` form — see `extract_cast_type` in `graphix-package-core/src/lib.rs` for an example.

### Env-independent TypeRefs (carried resolution cells)

`TypeRef` carries a write-once `Arc<Mutex<Option<Arc<ResolvedRef>>>>`
cell caching its NAME resolution (`design/env_independent_typerefs.md`,
2026-07-14) — a ref first resolved in its native env becomes an
env-independent value, so retained instance signatures stay
NAME-COMPRESSED instead of being eagerly expanded (the expansion was
the 41GB GUI wedge and the `contains` exponential residual; both gone,
GUI suite 163/163 in ~5s). Rules that matter when touching types:

- The cell is params-independent — rebuilds use `TypeRef::with_params`
  (SHARES the cell; `reset_tvars`/`replace_tvars` copies must keep
  seeded cells) vs `with_scope` (fresh — scope changes the resolution).
  Never overwrite a filled cell; contexts needing a different view
  rebind (`rebind_resolution`, fresh pre-filled cell).
- Seeding is LAZY by default — a fill is correct only when the
  resolving env holds the name's FINAL target, and mid-compile envs
  are truncated by registration order (eager transitive seeding
  captured the list PACKAGE's `List` for tui's `list::List` submodule
  ref; removed twice). `Type::seed_refs` (explicit transitive walk)
  runs only at provably-safe times: `check_sig`'s registry copy
  (post-module-body) and the privatize walk's rebinds (typecheck1).
- `same_def` (structural — gates the Ref×Ref name fast paths via
  `cells_agree`) vs `same_view` (body ALLOCATION identity). The
  privatize walk rebinds a ref to the env's allocation only on
  same-def/different-view divergence (interface typedef bodies are
  registered twice, equal-but-differently-viewed); a DIFFERENT def in
  the env is a stale-horizon artifact and the cell wins.
- Typecheck-side bridging is `fusion::lowering::privatize_type` —
  name-preserving, same-size output (setup_static_bind against
  `f.env`; check_instance_type's AbstractOpaque retry against
  `ctx.env`). The fusion-side `resolve_abstract` (capped, expanding)
  is unchanged; `freeze_for_abi`/`abi_kind` expand refs env-free
  through the cell (`TypeRef::expand_cell`), unfilled → de-fuse.

### Two-Phase Typecheck

Every node implements `typecheck0`/`typecheck1` (two passes over the whole
graph). `typecheck0` also builds `ctx.bind_to_lambda`; `CallSite::typecheck1`
pre-binds statically-resolvable calls (`try_static_resolve`) and re-drives the
bound instance's body typecheck with the call's fn-typed args registered
(per-callsite elaboration), so calls to a lambda *parameter* resolve statically
inside each instance. The old `NeedsCallSite`/deferred-check machinery is gone
— a builtin that needs call-site types reads them from its `typecheck1`
`resolved` argument.

### Collection intrinsics (MapQ/FoldQ as compiler nodes)

The Array/List/Map traversal HOFs are compiler-owned Nodes
(`node/collection.rs`, `design/collection_intrinsics.md`). The stdlib `.gx`
signatures are ordinary lambdas whose builtin-reference bodies use reserved
marker names (`'array_map`, `'list_fold`, `'map_filter`, …);
`CollectionIntrinsic::from_name` intercepts those names during lambda
construction (before the registered-builtin table — `register_builtin` rejects
them) and builds a `MapQ`/`FoldQ` node as the lambda's body
(`LambdaDispatch::Collection` — the dispatch charges no call-depth unit; only
the per-element callback dispatch does). The node owns callback instantiation
(one prototype CallSite for typecheck/analysis/emission + one live CallSite per
collection position at runtime), slot identity and prefix retention across
resizes, per-slot firing/taint/sleep/replay, and result construction. Effect
inference needs no HOF special case: the prototype's CallSite is a normal call
site, so an async callback flips the collection lambda Async through the
ordinary M6 fixpoint.

## Fusion / JIT subsystem (current state)

> Durable current-state summary — what the subsystem IS, not how it got here.
> Per-change history is in `git log`; deep design rationale is in `design/`
> (indexed at the end of this section).

**Two evaluators, one canonical:**

- **Node-walk** (`node/*.rs` — the `Box<dyn Update>` reactive graph) is the
  **canonical execution model** and the **universal fallback** for any subtree
  the JIT can't compile. It runs when fusion is off, and it must ALWAYS be
  correct (global `node-walk-is-canonical` memory). A fusion bug can *lose
  fusion* (a perf regression) but can never produce a *wrong answer* —
  correctness is structural.
- **Fusion → cranelift JIT** (`fusion/`, emitter in `fusion/emit/` — split
  per area: `jit`/`lower`/`abi`/`body`/`nodes`/`flow`/`select`/`call`/
  `scalar` + `scaffold`, façade re-exports in `emit/mod.rs`) identifies
  sync (pure) subtrees and compiles them to native kernels. **Success → splice
  the kernel + delete the originals; failure → don't splice, the originals
  node-walk.** There is no third evaluator.

**The pipeline is `Expr → node graph → CLIF`.** The node graph IS the IR: each
node's `Update::emit_clif` emits its own CLIF (`Apply::emit_clif` for builtins;
`MapFn`/`FoldFn::emit_clif` + the `fusion::emit::scaffold` loop scaffolds for
HOFs).
Fusion recursion is `Update::fuse` (driven from `compile()`, gated once on
`ctx.fusion.enabled`); `fusion::try_fuse` is the mechanics-only library. **Kernel
builds are pure signature derivation** — `sig_from_inputs` is the single sig
builder, the `Arc<KernelSig>` is the compiled-callable handle, and "is it
fusable" IS the compile attempt. The kernel-ABI vocabulary
(`KernelSig`/`abi_kind`/`freeze_for_abi`/slots/`FnSource`/`BuiltinSlot`/`KnownFusedFn`)
lives in `fusion/kernel_abi.rs`; the `BinOp`/`CmpOp`/`BoolOp` scalar-operator
enums are *not* ABI (shared by node-walk and JIT) and live in `node::op`, which
`fusion::emit` imports.

> **Do NOT reintroduce a parallel typed IR or a third evaluator.** The old GIR
> (a `GirExpr`/`GirOp`/`GirType` IR plus a GIR *interpreter*) was deleted
> deliberately: (1) the interpreter forced every semantics fix to be written
> THREE times (node-walk + GIR-interp + JIT) — a standing drift hazard; (2) the
> closed op-set was a vocabulary tax — every new op/shape had to be added in
> three places; (3) emission keys off the netidx `Type` + `abi_kind`, never off
> op *structure*, so the node graph already IS the IR. The only part worth
> keeping was the ABI contract, which survives as `KernelSig`/`abi_kind`. Keep
> the node graph as the single IR and distribute codegen as `emit_clif` per node.

**Value & type representation — the netidx types, no parallel copies:**

- **Values:** netidx `Value` everywhere (`#[repr(u64)]`, 16 bytes = (disc,
  payload)). `Value::copy_unchecked` is the branch-free copy for proven scalars.
- **Types:** netidx `Type` everywhere. Runtime shape comes from
  `abi_kind(&Type) -> Option<AbiKind>` + `freeze_for_abi` (`fusion/kernel_abi.rs`);
  `PrimType` is the closed register-scalar set, exhaustively matched in codegen.

**Semantics — node-walk and JIT must agree bit-for-bit (the differential fuzzer
enforces it):**

- `let rec` is **MONOMORPHIC-recursive** (2026-07-06): during the def-time
  body check a self-call unifies against the def's OWN ftype cells
  (`ExecCtx::rec_defs`, the tc0 knot in `CallSite::typecheck0`) — the
  μ-equation collapses (`'r ⊇ [T, 'r]` binds `'r := T`) and a self-call arg
  that disagrees with the entry call's narrowing is a def-time compile error.
  The prior "polymorphic" admission was unsound (the orphaned cell widened
  the signature to Any and crashed the JIT).
- `select` **exhaustiveness is enforced for bare-variant arm sets**
  (2026-07-06): `` `A ``/`` `B `` arms are NOT wildcards
  (`StructPatternNode::matches_anything` drives the wildcard test, not
  `is_refutable`, whose payload-only contract refutable-`let` depends on) —
  a select over `` [`A, `B] `` missing a tag is a compile error.
- Union COLLAPSE requires strict tvar identity (`union_identical`,
  typ/setops.rs): `TVar::eq` calls two distinct UNBOUND cells equal
  (None == None — fine for interface/alpha equivalence), but a union that
  collapses on that verdict drops the discarded cell's future binding.
- `&&`/`||` are **STRICT** — both operands required, `false && ⊥ = ⊥`. Not
  short-circuit (a dataflow value reflects all its inputs).
- Float comparison uses graphix's **TOTAL order** (`Value::partial_cmp`):
  `NaN == NaN`, `NaN` sorts below every non-NaN (so `Value` is map-key-able).
  Not IEEE.
- Checked arith (`+?`/`-?`/`*?`) detects overflow via `Value::checked_*` and
  yields the catchable `ArithError` *value*; unchecked wraps; integer div0 /
  signed `MIN`-/-1 → bottom.
- **Swallowed-error diagnostics are node-walk-only:** unchecked-arith errors,
  handler-less `?`, and `$` log (`error!`/`warn!`/eprintln) in the node-walk;
  a fused kernel produces the same bottom value silently — deliberate (the
  logs are a debugging aid, not value semantics). Use `--no-fusion` when
  debugging swallowed errors.
- `a[i]` / `a[i..j]` / `bytes[i]` / `m{key}` are bounds-checked through shared
  `node::array` / `node::map` helpers — one semantic seam, all backends agree.
- **Bottom** ("no value this cycle" — div0, `?`-error, an unfired input, a Sync
  builtin returning `None`) is `None`-from-`update` in the node-walk. In the JIT
  it is the **taint channel** (#219): a missing/unfired input becomes a
  taint-marked, helper-safe placeholder (`Value::Null` / empty `ValArray` /
  empty `ArcStr`), taint propagates through pure ops (`propagate_taint`), and
  the kernel forces bottom (emits `None`) only if the taken output path
  *consumes* a tainted value (`is_tainted`) — so a missing input no longer
  de-fuses the whole region. A **pended DynCall** (the builtin returned no
  value — `buffer::encode`'s Pad guard) rides the same channel since 2026-07-06:
  each site take-and-clears `DYNCALL_PENDING` and continues with the tainted
  placeholder, so `DYNCALL_PENDING` reaching `Kernel::update` means only a
  GENUINE whole-kernel abort (interrupt poll, depth trip, return-gate force,
  callee abort propagated at the call site by `emit_lambda_call_node`). The
  old `array::init` runaway-length whole-kernel abort is gone: an over-limit
  count taints and clamps to zero length in `emit_init_loop`, matching the
  node-walk's log-and-no-fire. `design/representable_bottom.md`.
- **Taint never reaches builtins** (Eric's rulings 2026-07-19/20:
  taint == bottom == no input to a builtin — authors must never see
  the taint channel). EVERY builtin's arg seam converts a poisoned
  production to ABSENCE: the `CallSite` publish loop gates it to
  silence (`gate_tainted_args`, set at both callee-binding sites —
  once/take/skip don't count poison), and a fused DynCall passes a
  per-arg taint MASK through `graphix_dyncall` so masked slots
  deliver nothing (the whole-call any-tainted skip was wrong — the
  cached slot RIDES its previous state and EVAL decides what a
  missing arg means; a masked arg folds as neutral STALE into the
  result disc; >64 args de-fuse). Symmetric with the kernel output
  boundary, which forces a fused arg region's tainted result to None.
  Lambda callees keep poisoned formals. `CachedArgs`' any_tainted arm
  is an unreachable-by-construction backstop. Pinned by
  `builtin-taint-gate-jul2026` + `dyncall-partial-args-jul2026` (the
  latter also fixed `array::window` to require ALL its args — its
  eval produced `[]` at `#n: 0` with the val slot absent).
- An **infinite PURE tail recursion hangs** the JIT (a native loop can't yield to
  the scheduler) — accepted/correct; the reactive node-walk's per-cycle
  "continue" is the artifact.

**Per-cycle firing (the STALE fired-bit):** a fused kernel must replicate the
node-walk's non-async firing — an output fires only when an input that feeds it
actually fired this cycle. A "fired-this-cycle" (`STALE`) bit rides each kernel
param's disc; a lifted let-bound `connect`-target counter is threaded in as a
kernel input so reactive counters fuse. Collection-loop firing is
`scaffold::SlotFlags`: per-iteration body discs fold into a slots word, a
per-instance state word holds the previous source length for exact resize
detection, and `apply` reproduces the interpreted MapQ/FoldQ rule (fires iff
resized ∨ a slot fired ∨ the source fired empty; fold results are acc-carried
via `result_is_firing`). A same-length source refresh with a quiet body does
NOT re-fire — the per-slot precision the P4 sequential loops had lost.
Guarded selects in loop bodies have PER-SLOT selection memory at ANY nesting
depth (2026-07-15, Eric's firing rule: an arm fires once when it BECOMES
selected + when its body deps fire; guard updates that don't change the
selection are quiet; guarded selects never de-fuse): each select site anchors
a chain of owning tables mirroring its loop nesting — one static anchor word,
one directory level per enclosing loop, a leaf word per slot
(`graphix_slot_state_table` + `own_levels`; MapQ prefix retention applied per
level, ragged inner lengths fine; chains freed by `Kernel::drop` via
`WrappedKernel::slot_table_words`) — `design/kernel_instance_state.md`
"Per-slot state tables". CALLEE bodies have PER-CALL-SITE state blocks
(2026-07-16, wire slot 2 / `CTX_WIRE_SLOTS` 3): the callee's claims are its
`SiteLayout` (callees define before parents; a missing layout = recursive
back-edge, pass 0 + null-guards = fresh transient semantics); callers allocate
from their own storage (root: contiguous words with anchor translation;
in-loop: chain leaves with `words` stride, `SiteAnchor`/`SiteLeaf` recursive
free for deep compositions). The select identity algebra is CLOSED — (region)
× (loop chain) × (call chain). The tail emitter needs no selection memory
(entry-derived seam tags on both backends). Remaining select-adjacent item:
arm-lifted connects in loops/callees still de-fuse (coverage, not
correctness).

**Testing is differential:**

- `run!` (`graphix-package-core/src/testing.rs`): each fixture runs in 2 modes —
  `interp` (node-walk, fusion off) and `jit` (fusion+JIT) — asserting equal
  values. `FuseExpect::{Jit, None}` asserts *whether* it fuses (a bidirectional
  drift check). Optional `; shape:` asserts the compiled graph via `NodeShape`
  (`node_shape.rs`, currently signature-fact-only — see F4/#213 below).
- **graphix-fuzz** (`graphix-fuzz/`): the differential model-checking fuzzer —
  node-walk (trusted) vs JIT (under test), with `check`/`run`/`generate`
  (`--reactive` for multi-cycle programs)/`fuzz`/`minimize`/`regress`/
  `selfcheck`/`gen-check`; the committed `findings/` corpus is the regression
  gate. Since V2 (2026-07-03) the oracle compares **per-cycle traces**
  (runtime-side recording via `ToGX::TraceStart`/`TraceWaitIdle`; a
  `TraceDiff` class — Missing/ExtraFire, Pacing, etc. — keys dedup), and
  programs can carry a `// schedule-v1:` header injecting input epochs
  atomically via `set_many` (inputs use the `let inN = d; inN <- never(d)`
  contract so fusion binds them as region inputs). `selfcheck`
  (same-mode-vs-itself, 100% required) gates oracle soundness; `rand::`/
  `sys::`/`http::` programs are excluded from divergence recording (async
  IO races trace quiescence). `detcheck [n] [seed]` is the fusion-shape
  DETERMINISM gate (#19): every Exact-tier corpus program (+n generated)
  runs to quiescence in two fresh child processes (fresh ASLR each) with
  `GRAPHIX_DUMP_CLIF=1`, and the counter-normalized dumps must match — a
  flap means the compiled shape depends on allocation order somewhere in
  typing/resolution/fusion. Soak ops: `GRAPHIX_FUZZ_PAR`,
  `GRAPHIX_FUZZ_CORPUS` (separate corpus dir PER campaign — shared dirs
  clobber), launch campaigns under `nice -n 19` (workers inherit —
  keeps interactive builds fast while soaks saturate the idle cores),
  and launch from a campaign-private COPY of the binary (`cp` it to
  `~/tmp/target/fuzz/<campaign>/graphix-fuzz` first) — workers exec
  the binary path per subject, so a rebuild mid-campaign swaps code
  under a running soak and its findings become mixed-version garbage
  (jul10h lost its tail this way, 2026-07-11). Campaign output
  defaults OUTSIDE the repo (`~/tmp/target/fuzz/` — the repo's fuzz/
  dir is syncthing-synced; soak corpus dirs go under
  `~/tmp/target/fuzz/<campaign>/`, durable triage summaries stay in
  the repo by hand). Worker children run in PARENT-owned sandbox cwds
  (`sandbox_cwd`, lib.rs — a child-owned tempdir leaked per subject
  via `process::exit` and a soak exhausted /tmp's INODES, jul10d), and
  the pool has an environment-broken backstop (`BreakageWindow`): a
  majority of findings over a 200-subject window aborts the campaign
  instead of flooding the corpus at disk speed; finding-write failures
  are fatal. `design/graphix_fuzz.md` §12.
- **`FusionStats`** (`fusion/mod.rs`): per-`ExecCtx` compile-time counters
  (`attempted`/`fused`/`failed: Vec<(ExprId, reason)>`), exposed via
  `GXHandle::fusion_stats()` / `TestCtx::fusion_stats()`. Read `failed` as a
  blocker profile, not a gap count (the attempt-then-recurse protocol logs
  Module/Bind misses even for a wholly-fused program).
- **`GRAPHIX_FUSE_AUDIT=1 cargo test --workspace -- jit --nocapture`** prints
  a per-fixture `FUSEAUDIT <name> <expected> <actual> OK|MISMATCH` line plus the
  blocker list — the annotation-vs-reality audit (stdout is captured without
  `--nocapture`). Sweep the WORKSPACE, not just `-p graphix-tests`: the stdlib
  package crates carry their own `run!` fixtures and drift invisibly otherwise
  (`rand_float_default::jit` broke for a week unseen — 2026-07-03).
- A divergence is **at least as likely a fused/JIT bug as a node-walk one** —
  verify the intended semantics against the node-walk before touching it.

**Collection HOF execution (compiler-owned nodes, 2026-07-13):** the `sync`
subset, the `For` node, the sync desugar, and the in-language HOF bodies are
all DELETED (`design/collection_intrinsics.md`; the P4 arc concluded the sync
subset of Graphix is Rust). MapQ/FoldQ are back as the canonical per-slot
interpreters — but as compiler Nodes (`node/collection.rs`), not package
builtins. Fusion: `GXLambda::emit_clif` (the `Apply` hook, consulted FIRST at
`CallSite::emit_clif`) recognizes a collection-bodied callee and inline-emits
the loop at the call site via `MapQ/FoldQBase::emit_clif_call`, swapping the
callsite's actual source/init arg nodes for the lambda-param references —
supported Array shapes compile through the per-op `MapFn`/`FoldFn::emit_clif`
impls into the `scaffold::emit_{init,map,filter,filter_map,flat_map,find,
find_map,fold}_loop` emitters; refusal leaves the node intact on its
interpreted per-slot semantics (async callbacks always interpret). **List and
Map HOFs lower too (2026-07-14)** via the FLATTEN boundary: the collection
Value crosses through `graphix_list_to_valarray`/`graphix_cmap_to_pairs`
(consuming; canonical `list::*`/`make_pair` seam — one semantic seam with the
interpreted finishes), the ARRAY scaffold loop runs unchanged (the SlotFlags
rule over the flattened length IS the interpreted ordinal-slot rule), and
collection results rebuild at `graphix_valarray_into_{list,cmap}`.
Prerequisite: recursive types freeze to an OPAQUE LEAF
(`freeze_for_abi_d` Seen-hit returns the matched outer ref, params frozen,
256-chain backstop) so a List crosses kernel boundaries as a 2-word Variant
and list-typed DynCalls (`from_array`/`to_array`/`cons`/...) register.
`FoldAcc::Value` carries Value-shaped ACCUMULATORS (owned two-word loop
slot, real disc carried whole with TAINT|STALE in the tag bits) —
nullable max-by and map group-by folds fuse; the cons-building reverse
still interprets (abstract-id identity mismatch at the prototype's
return check — two AbstractIds both denoting list::List; pinned by
`list_fold_list_acc_interprets`). The entitled abstract bridges:
`BuiltInLambda::typecheck0` and `CallSite::typecheck0`'s per-arg checks
(`check_site_arg`) retry through `privatize_type` under the CALLEE
DEF's scope on `AbstractOpaque` (a def sees through its own
signature's abstracts; privatized instance signatures mix private
forms into outside-module callback bodies by design).
Benches: `list_fold_sum` 151x, `list_map_fold` 142x — the ~15x gap to the
array twins is the cons representation's per-element allocation, not loop
overhead. `for_each_emitted_node` descends
collection callback bodies during discovery so callee kernels and DynCall
slots inside callbacks are found. `find`/`find_map` scan ALL slots in both
modes (a bottom predicate after the match bottoms the find — pinned by
`find_bottom_after_match`); the P4 sequential early-exit is gone with the
sequential semantics. Cross-kernel call sites force the callee's init flag on
the first call ever (a state word — the kernel mirror of `Callee::Static`'s
`first_update` priming).
Durable notes from the P4 arc: (1) the SHELL resolution flap is FIXED
(2026-07-12) — RT batch entries prune only the outgoing batch's `<-` targets
(`unstable_bindings`) from `bind_to_lambda` instead of clearing it, and
`Bind::delete` removes its ids; the racy `rt.cached()` fallback REMAINS for
destructured/`<-`-retarget shapes the index can't know — flagged for review.
(2) builtin-bodied lambdas' `intrinsic_effect` is read from `BuiltinFacts`,
not constructed `Sync`.

**Kernel ABI:** kind-grouped params — scalars, then array/tuple/struct pointers,
then string, then 2-word variant/nullable/value — derived from a single source
(`fusion/kernel_abi.rs`: `KernelSig::abi_params`/`AbiParamKind`). Any region width
fuses (the #219 taint rides each param's disc, so there is no input-count cap).

**Emit contracts** (the invariants a new `emit_clif` must respect — full detail in
`design/distributed_jit.md`, "Semantic contracts for emit work"): replayability ≠
`Sync` (an effect that re-delivers all args per fire is `Async`); effects
de-fuse, never silently skip; first dispatch forces the init view; wake-ups key on
`(BindId, fusion.top_id)`; clone types out of `with_deref`/the abstract registry
before recursing (lock discipline); dead statements eliminate at emit only when
the stmt subtree is effect-free.

### Coverage (current)

Measured by the FuseExpect audit above (numbers last measured pre-collection-
intrinsics, 2026-07-08 — re-run the audit for current figures): **~71% of the
`run!` corpus fuses+JITs, and all bench programs (`bench/`) fuse fully.** The
value-computing vocabulary is essentially complete:
all scalar arithmetic/comparison/logical/cast/checked-arith, every producer
(struct/tuple/variant/array/map-literal incl. `{s with f: v}`) and accessor
(field/index/slice/`m{key}`), `?`/`$`, all eight array HOFs as native loops
(map/filter/flat_map/filter_map/find/find_map/fold/init — over scalar, composite,
**String, and value-shape elements**, with `|(k,v)|` destructure leaves of any of
those shapes, and HOF-of-HOF fused into one multi-loop kernel; **fold
accumulators may be composite or string, not just scalar** — tuple/struct/array/
string accs carry loop-OWNED with clone-borrowed/drop-replaced discipline, acc
patterns may destructure (`|(a, b), v|`), and the freeze authority is the
RESOLVED acc type from the prototype callback's `typ().rtype`, since an
instance's `body.typ()` re-mints generalized tvars unbound), **`select`
structural destructuring** (tuple/struct/slice patterns with scalar leaf binds,
anonymous-rest prefix/suffix, nested patterns via borrowed interior reads, owned
fresh-producer scrutinees in value position — each arm's length test doubles as
the #219 taint gate), **`connect` of any RHS shape** (owned marshal into a
consume-always `set_var`) including **lifted composite/string/struct
accumulators** (`data <- array::push(data, x)`, `s <- "[s]x"`,
`st <- {st with n: st.n+1}` — the sliding-window idiom, seed-select with
clone-vs-seed branches), every Sync core/str/re/map/math/rand builtin via the
generic DynCall path, cross-kernel lambda calls (incl. recursive self-calls:
tail → rebind-and-jump loop, non-tail → native recursion), transitive callees,
and builtin/cast/qop calls inside lambda bodies.

The **correct-None denominator** (principled, never a gap): async/streaming
builtins (timers, IO, netidx, `never`, `queue`, `once`/`take`/`skip`), cross-cycle
nodes (`~`, `Any`, `TryCatch`'s catch-read), and non-register-encodable types
(`decimal`, `Fn`, `Ref`, recursive `List`/ADTs — no fixed ABI layout — and unbound
TVars). Note that fusion recursion (`Update::fuse`) descends only through
Module/Block/Bind/CallSite/TryCatch/Lambda — a sync expression under `~`, `<-`,
`select`, or an operator fuses only as part of an enclosing block/bind region
that fuses as a whole, so `clock ~ (a + b)` leaves the `a + b` node-walking
unless it is hoisted into its own `let` (accepted current design, 2026-07-02).

The remaining missed-fusion tail (each pinned by a `#[native]` de-fuse test or an
ASPIRE comment where noted):

1. **HOF callback calling a nested/rec local lambda** — simple captured-local
   calls now resolve (per-callsite elaboration + `for_each_emitted_node`
   discovery), but a rec callee inside a fold callback still keeps the
   collection on the node-walk (pinned by `fold_callback_name_collision`,
   FuseExpect::None).
2. **select residue**: whole-composite/`@`/NAMED-rest binds (owned arm locals —
   `JitEnv::truncate` emits no drops), nested/non-scalar variant payloads,
   owned scrutinees in TAIL position (no merge point to drop at).
3. Lower-impact: non-scalar string-interp parts, String-returning cross-kernel
   callees, dynamic map literals, `array::group`, `filter_map`/`init`
   string/value-element widening, ByRef/Deref, decimal arith.

(The former "struct-parent nested-pattern TVar inference" gap is FIXED: `_`
infers `Type::Any` — load-bearing for exhaustiveness/dead-arm/runtime dispatch
— but `T.contains(Any)` is false and the select typecheck's bool-discarding
unification walk short-circuits composite pairs, so every pattern slot AFTER a
`_` never narrowed. The select arm unification now runs through
`Type::any_as_tvar()` — a view sharing all TVar cells with `Any` leaves
swapped for throwaway fresh TVars — node/select.rs `typecheck0`.)

**F4/#213 (EmitTags) is settled: retired unbuilt.** Per-op body tags would
resurrect the GIR vocabulary tax; the shape oracle is the differential value
check + `KernelMatcher` signature facts + the `#[native]` attribute (zero
node-walk residue at a source location; a no-op under `--no-fusion`, so it works
in `run!` fixtures and bench programs). The decision is recorded in
`node_shape.rs`.

### Design documents (`design/`)

- `final_jit_architecture.md` — the end-state architecture (`Expr → node graph →
  CLIF`), now realized.
- `distributed_jit.md` — how the GIR IR was removed and fusion distributed as
  `emit_clif`/`fuse` per node; holds the emit contracts and the ABI-contract
  rationale.
- `representable_bottom.md` — bottom semantics (the taint channel).
- `graphix_fuzz.md` — the differential fuzzer.
- `collection_intrinsics.md` — **current:** the Array/List/Map HOFs as
  compiler-owned Nodes (reserved marker names → `CollectionIntrinsic` →
  MapQ/FoldQ nodes; per-slot interpreted semantics + inline CLIF loops).
  Supersedes the sync subset, `value_returning_loops.md` (planned, never
  built), and the `clone_rebind` machinery.
- `impure_hof_fusion.md`, `composite_hof_fusion.md`, `clone_rebind_testing.md` —
  SUPERSEDED (historical): the per-slot template / `clone_rebind` era.
- `queue_fn.md` — `queuefn` feature design.
- `replay_frames.md` — **BUILT (2026-07-11), v2 same day:**
  `reset_replay` (required `Update`/`Apply` method, replay caches vs
  semantic state) + evaluation FRAMES (tail-loop jumps run against a
  private variables map) + **TagValue as
  the interpreter currency** (Eric's call; v2): `Update::update`
  returns `Option<TagValue>` and `Event.variables` carries it — the
  kernel's STALE/TAINT disc bits ride every interp value, ops
  propagate them per the CLIF rules, `Apply::update` stays clean
  `Value` with `Apply::out_tag` surfacing the tag, and the kernel
  gains a `last_result` value-channel slot. The v1 `frame_bottom` bit
  and the fired re-delivery hack are deleted (jul10e broke both
  within an hour of soaking).
- `sync_subset.md`, `sync_control.md`, `value_returning_loops.md` —
  SUPERSEDED (historical): the `sync { }` block prototype (P0–P3 built
  2026-07-09, P4 2026-07-10) and the never-built generalized-loop plan.
  Unwound 2026-07-13: modeling slot lifetimes showed a reactive
  collection HOF is a live per-position subgraph, not a sequential
  loop — the sync subset of Graphix is Rust. See
  `collection_intrinsics.md`. Per-callsite elaboration (the P4 gate)
  survives and is load-bearing for collection callback resolution.
- `fusion_lowering_split.md` — **proposed, not built:** split `try_fuse`'s welded
  analysis+lowering into a pure analysis pass (color nodes with a `KernelId`,
  build per-kernel descriptors) consumed by a thin lowering pass. Motivated by
  legibility.
- `type_operation_scaling.md` — **built (2026-07-13):** COW/DAG walks +
  per-pass memos for every core type operation (the six tree-walk
  explosions the static-instance wedge exposed); holds the "invariants
  for future type walks". Its open `contains` residual is RESOLVED by:
- `env_independent_typerefs.md` — **built (2026-07-14):** `TypeRef`'s
  carried resolution cell (Eric's ruling) — name-compressed,
  env-independent instance signatures; the privatize walk; seeding
  invariants; `same_def`/`same_view`; freeze/abi_kind cell Ref arms.
  Carries one open finding: the two `#[native]`-in-List-callback tests
  passed vacuously and now fail honestly (pending ruling).

## Stdlib package notes

- **`sys::process` draft (PR #13, `unified-fusion-proto`).** Managed child
  processes live in the opaque `Proc` value and use weak polling plus
  `kill_on_drop`; the public Graphix API provides `options` and `stdio` named
  argument constructors. Stdio is an explicit `Pipe`/`Inherit`/`Null` variant
  and defaults to `Inherit`. The polling task is the sole child reaper; `wait`
  subscribes to its watch status so concurrent waits and kill-during-wait work.
  Rust wire conversion uses `netidx-derive`; the one
  exception is `SpawnOptions.env`, because `immutable_chunkmap::Map` does not
  implement `FromValue`, so a derived wire struct validates and extracts its
  `Value::Map`. Shell-based tests are Unix-gated, with `cmd.exe` equivalents
  for stdout and exit-status behavior on Windows.
- **GUI** (`graphix-package-gui`, iced 0.14): uses the iced sub-crates directly
  (`iced_core`/`iced_wgpu`/`iced_widget`/…) not the umbrella crate, for
  render-pipeline control. `iced_renderer` needs both `wgpu` and `wgpu-bare`
  features (the cfg checks key off `wgpu-bare`). GUI/TUI examples are visual —
  test manually (`cargo run --bin graphix -- examples/gui/hello.gx`).
- **Package manager** (`graphix-package`): `download_source` is testable by
  injecting a temp graphix data dir and downloading a fixed released
  `graphix-shell` from crates.io (e.g. `0.5.0`) — avoids mutating the user's
  `~/.local/share/graphix` and regression-tests archive-extraction layout.
- **Package manager — `packages.toml` v2 + `update` rework (2026-06-25).** The
  stdlib is special-cased: stdlib packages no longer carry versions (they always
  track the shell version). `packages.toml` format v2 is a `[stdlib]` table with
  `installed`/`removed` name arrays plus a `[packages]` table for EXTERNAL
  (third-party) packages only (still version-or-path). The in-memory model is
  `Packages { stdlib_installed, stdlib_removed, external }`. `read_packages`
  detects the old flat `[packages]`-only format by ABSENCE of `[stdlib]` and
  migrates once (stdlib names present → installed; absent → removed; non-stdlib →
  external; stdlib path overrides are dropped — stdlib can't be path/version
  pinned anymore), persisting the upgrade in place (best-effort) on first read.
  `LEGACY_REMAP` (`fs`/`net`/`time` → `sys`) handles the pre-`sys` reorg: a
  migrated file with those old top-level packages drops the dead name (its crate
  has no shell-compatible version, so it would break the build) and installs the
  replacement `sys` in its place, preserving the user's intent.
  `combined_map(build_version)` is the single bridge to the unchanged build
  machinery (`generate_deps_rs`/`update_cargo_toml`): stdlib → `Version(build_version)`
  plus externals verbatim. `rebuild` was split into `prepare_source` (delete
  scratch + unpack) + `install_from_source` so `update` can unpack the latest
  source once (to enumerate new stdlib) and reuse it for the build. The
  authoritative stdlib set at a version is enumerated from that shell source's
  `Cargo.toml` `graphix-package-*` deps (`stdlib_packages_in_source`);
  `DEFAULT_PACKAGES` (now `&[&str]`, 19 user-facing names) is only the
  fresh-install/migration bootstrap. `INTERNAL_PACKAGES = ["bench"]` is a denylist
  (shell dep, never auto-surfaced, still `add`-able). `update(assume_yes)` now:
  discovers a maskable change set — shell bump (current→latest via semver
  `version_gt`, a new workspace dep), NEW stdlib (source set − installed∪removed −
  internal; only when a bump exists), and EXTERNAL updates (per installed Version
  external, one bad crate warns+skips not aborts) — `present`s it, then `[Y/e/n]`
  prompts (numbered toggle list for `e`; declining the shell auto-deselects new
  stdlib; deselecting a new stdlib in edit → `removed`, never re-asked; `n`/cancel
  writes nothing). New stdlib only applies when `build_version == latest`. Builds
  BEFORE writing `packages.toml` (failed `cargo install` ≠ corrupt manifest).
  Non-TTY without `--yes` is a HARD ERROR (no silent CI mutation). The pure core
  (`parse_packages`/`to_toml_string`/`compute_update_plan`/`apply_selection`/
  `parse_toggles`/`stdlib_packages_in_cargo_toml`) is unit-tested with no
  stdin/network/fs (`test::pure`); the prompt IO reads via `spawn_blocking` +
  `std::io::stdin().read_line` and is verified by pty-driven manual runs.
- **GUI widget tests**: `GuiWidget` has a `#[cfg(test)] as_any`/`as_any_mut`
  (default `unimplemented!()`); widgets needing test-state inspection (e.g.
  `DataTableW`) override it, and `GuiTestHarness::dt()/dt_mut()` downcast. Tests
  fire per-column callbacks via `gx.call(callable_id, args)` (mirrors the
  widget's own dispatch). `InternalOnly` test ctx DOES spin up a real in-process
  resolver, so `sys::net` round-trips work — but publisher coalescing means
  rapid updates collapse; space them with one-shot timers for multi-point tests.
