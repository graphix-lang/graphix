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
- **Fusion → cranelift JIT** (`fusion/`, emitter in `fusion/emit.rs`) identifies
  sync (pure) subtrees and compiles them to native kernels. **Success → splice
  the kernel + delete the originals; failure → don't splice, the originals
  node-walk.** There is no third evaluator.

**The pipeline is `Expr → node graph → CLIF`.** The node graph IS the IR: each
node's `Update::emit_clif` emits its own CLIF (`Apply::emit_clif` for builtins;
`MapFn`/`FoldFn::emit_clif` + the `fusion::scaffold` loop scaffolds for HOFs).
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
- **Bottom** ("no value this cycle" — div0, `?`-error, an unfired input) is
  `None`-from-`update` in the node-walk. In the JIT it is the **taint channel**
  (#219): a missing/unfired input becomes a taint-marked, helper-safe placeholder
  (`Value::Null` / empty `ValArray` / empty `ArcStr`), taint propagates through
  pure ops (`propagate_taint`), and the kernel forces bottom (emits `None`) only
  if the taken output path *consumes* a tainted value (`is_tainted`) — so a
  missing input no longer de-fuses the whole region. `design/representable_bottom.md`.
- An **infinite PURE tail recursion hangs** the JIT (a native loop can't yield to
  the scheduler) — accepted/correct; the reactive node-walk's per-cycle
  "continue" is the artifact.

**Per-cycle firing (the STALE fired-bit):** a fused kernel must replicate the
node-walk's non-async firing — an output fires only when an input that feeds it
actually fired this cycle. A "fired-this-cycle" (`STALE`) bit rides each kernel
param's disc; a lifted let-bound `connect`-target counter is threaded in as a
kernel input so reactive counters fuse. HOF results inherit capture-aware firing
(`inherit_hof_firing`: a result is STALE unless the source array, the HOF init,
and every feeder the callback body captures fired).

**Testing is differential:**

- `run!` (`graphix-package-core/src/testing.rs`): each fixture runs in 2 modes —
  `interp` (node-walk, fusion off) and `jit` (fusion+JIT) — asserting equal
  values. `FuseExpect::{Jit, None}` asserts *whether* it fuses (a bidirectional
  drift check). Optional `; shape:` asserts the compiled graph via `NodeShape`
  (`node_shape.rs`, currently signature-fact-only — see F4/#213 below).
- **graphix-fuzz** (`graphix-fuzz/`): the differential model-checking fuzzer —
  node-walk (trusted) vs JIT (under test), with `check`/`run`/`generate`/`fuzz`/
  `minimize`/`regress`; the committed `findings/` corpus is the regression gate.
  `design/graphix_fuzz.md`.
- **`FusionStats`** (`fusion/mod.rs`): per-`ExecCtx` compile-time counters
  (`attempted`/`fused`/`failed: Vec<(ExprId, reason)>`), exposed via
  `GXHandle::fusion_stats()` / `TestCtx::fusion_stats()`. Read `failed` as a
  blocker profile, not a gap count (the attempt-then-recurse protocol logs
  Module/Bind misses even for a wholly-fused program).
- **`GRAPHIX_FUSE_AUDIT=1 cargo test -p graphix-tests -- jit --nocapture`** prints
  a per-fixture `FUSEAUDIT <name> <expected> <actual> OK|MISMATCH` line plus the
  blocker list — the annotation-vs-reality audit (stdout is captured without
  `--nocapture`).
- A divergence is **at least as likely a fused/JIT bug as a node-walk one** —
  verify the intended semantics against the node-walk before touching it.

**Per-slot HOF callbacks** (`array::map(a, cb)` etc.): the callback fuses into a
template kernel once at compile time, then `clone_rebind`s per array slot (each
slot gets fresh async state). An *impure* callback splits at the async boundary —
the sync part fuses + JITs per slot, the async residue node-walks.
`design/impure_hof_fusion.md`, `design/composite_hof_fusion.md`,
`design/clone_rebind_testing.md`.

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

Measured by the FuseExpect audit above: **~71% of the `run!` corpus fuses+JITs
(≈487 `Jit` / ≈195 `None`, zero annotation drift), and all bench programs
(`bench/`) fuse fully.** The value-computing vocabulary is essentially complete:
all scalar arithmetic/comparison/logical/cast/checked-arith, every producer
(struct/tuple/variant/array/map-literal incl. `{s with f: v}`) and accessor
(field/index/slice/`m{key}`), `?`/`$`, all eight array HOFs as native loops
(map/filter/flat_map/filter_map/find/find_map/fold/init — over scalar, composite,
**String, and value-shape elements**, with `|(k,v)|` destructure leaves of any of
those shapes, and HOF-of-HOF fused into one multi-loop kernel), **`select`
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

1. **HOF callback capturing a *local* lambda** (`array::map(a, |x| g(x))`) — the
   `g(x)` call inside the per-slot template isn't statically resolved (harder:
   resolution of captured locals in cloned templates; in `notes`).
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
- `impure_hof_fusion.md`, `composite_hof_fusion.md`, `clone_rebind_testing.md` —
  HOF fusion (per-slot templates, impure split, the `clone_rebind` contract).
- `queue_fn.md` — `queuefn` feature design.
- `fusion_lowering_split.md` — **proposed, not built:** split `try_fuse`'s welded
  analysis+lowering into a pure analysis pass (color nodes with a `KernelId`,
  build per-kernel descriptors) consumed by a thin lowering pass. Motivated by
  legibility.

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
