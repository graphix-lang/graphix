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

The compiler and runtime depend only on netidx's VALUE layer (`netidx-core`/`netidx-value` — `Value`, `Type`, `Path`, Pack); the netidx NETWORKING crates appear only in stdlib packages (`sys`, `db`, ...). The netidx repo is expected at `../netidx/` (sibling directory). See "Netidx extraction" below.

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

Do not build this project in release mode unless you must, it takes a very long time.

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

### The `slow-tests` feature

A handful of tests dominate the suite's wall time, and what they cover
moves rarely — package builds, the stack-depth guards. They are marked
`#[cfg_attr(not(feature = "slow-tests"), ignore = "slow-tests")]`, so a
plain `cargo test --workspace` skips them (they report as `ignored,
slow-tests` — visible, not hidden) and the RELEASE GATE runs them:

```bash
cargo test --workspace --features slow-tests
```

They still COMPILE in the default build, so they cannot rot unnoticed.
Behind the feature today:

| test | crate | cost |
|---|---|---|
| `reduced_feature_build_drops_packages` | graphix-package | vendor.py + a full shell build |
| `created_package_compiles` | graphix-package | `cargo check` of a generated package |
| `build_standalone_produces_working_binary` | graphix-package | a full standalone build |
| `download_source_extracts_package_at_expected_root` | graphix-package | downloads a released crate (needs network) |
| `deep_ast_drops_without_overflow` | graphix-compiler | ~40s, 50k-deep AST teardown |
| `deep_nesting_does_not_overflow` | graphix-shell | ~80s, 22 shapes x 2 depths in child processes |

Measured 2026-08-24: **19.5 min of test wall time down to 6.1**, and
`graphix-package` alone goes 705.8s -> 0.03s (its other 26 tests were
never the cost). The rest stays in the default run on purpose —
`graphix-tests` (174s, 2129 fixtures), the compiler proptests (33s),
`examples_compile` (32s) and the GUI harness (23s) all find bugs on
ordinary changes. So does `graphix-fuzz`'s 66s, which is ONE test:
`jit_generated_sweep` runs 120 fixed-seed generated programs through
both engines — the oracle in miniature, and the one gate here that
works the way the fuzzer does. It also gets more reliable now that
705s of concurrent cargo builds is gone: it had to re-check any
timeout at 4x because the suite ran ~13x slower than a solo run.

Gate a new test only when it is BOTH slow and testing something that
moves rarely (a build, an environment, a hard limit); never gate a
language-semantics test. Toggling the feature rebuilds the crates that
declare it and their dependents, so the gate run pays one rebuild —
use a separate `--target-dir` if you want to keep the incremental
artifacts. A test that re-executes its own binary must pass
`--include-ignored` to the child (see `deep_nesting.rs`), or the child
skips the test and exits 0, which reads as success.

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
- `Scope`: the lexical path (module + block nesting, a `ModPath`) and the
  dynamic scope (`DynScope`: the chain of error handlers visible to a `?`,
  one node per `catch` install, following the CALL chain — an instantiated
  body starts from its call site's; `Scope::append` extends the lexical
  path only, `Scope::with_catch` the dynamic one)

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
- Variable references and updates
- Timer events
- Spawned tasks and watch channels (`spawn`, `spawn_var`, `watch`, `watch_var`) — the generic conduits packages use to feed external events into the graph

Event processing is batch-based: the runtime collects all simultaneous events into an `Event` struct and delivers them to the graph in one cycle. Multiple updates to the same variable in one cycle must be queued for the next cycle.

### Netidx extraction (2026-07 — the core is network-free)

`graphix-compiler` and `graphix-rt` have ZERO netidx networking dependency (`design/netidx_extraction.md`). The architecture:

- **Module loading** is the `ModuleResolver` trait (`expr/resolver.rs`): async `resolve`/`for_source`/`fetch_source`; `VfsResolver`/`FilesResolver` live in-core, the netidx loader is `NetidxResolver` in `graphix-package-sys/src/loader.rs`. `ResolverFactory` (GRAPHIX_MODPATH `scheme:` registry) receives `&mut LibState`, so package factories share state with their package's builtins.
- **sys::net owns its netidx** via `NetState` in `ctx.libstate` (`graphix-package-sys/src/netstate.rs`): one subscription pump (netidx batches → `Rt::watch_var`, with shared-Dval fan-out routing — netidx SHARES Dvals by path), writes/RPC-server calls as `CustomBuiltinType` events with reply channels, a package-side coalescing publish flusher, a 60s Dval unsubscribe graveyard, and an on-use-GC'd RPC client cache.
- **`NetHandles`** is a standalone shared libstate entry holding the raw publisher/subscriber: BOTH the module loader and `NetState` materialize through it, whichever touches netidx first — one universe per context. Materialization reads the seeded `NetConfig` (package-core: `Ready`/`Config`/`Internal`); unseeded defaults to `Internal` — a process-internal netidx built on demand on a dedicated side thread. Fuzz/test children that never touch sys::net have zero network (this killed the soak port-exhaustion ceiling).
- **The shell library is netidx-agnostic**: `ShellBuilder::setup_context` (a `FnOnce(&mut ExecCtx)` run at init) is the generic embedder hook for seeding package libstate entries, and `resolver_factories` passes scheme registrations through to `GXConfig`. The CLI (`main.rs`) is the netidx-aware embedder: it seeds `NetConfig`/`NetTimeouts` in the hook and registers the `netidx:` factory. `GXHandle::with_ctx` (a boxed-closure `ToGX` message) is the handle-side bridge to `ctx.libstate` for code without a ctx (the gui data_table fetches the subscriber through it).

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
- `EFFECT` (default `Effect::Async`, consolidated 2026-09-02 — Eric:
  "make it easy to classify, and force awareness of `FastCall`"): the
  builtin's ONE classification, `Effect::{Async, Sync,
  Stateless(Option<FastCall>)}` (`effects.rs`, re-exported at the
  root). `Async`: output may land on a later cycle than its trigger,
  autonomously, or never. `Sync`: same-cycle, but the instance keeps
  cross-invocation state (`count`/`sum`/`min`/`uniq`/`once`/`take`/
  `skip`/`hold`/`window`/`group`/`and`/`or` accumulate or remember)
  or its result depends on WHICH args were delivered (the
  partial-delivery producers: opt's `or`/`and`/`contains`/
  `or_default`/`ok_or`/`zip`, `filter_err`, `divide`). `Stateless(f)`:
  same-cycle and a pure function of its args — no cross-invocation
  state; internal memos/scratch buffers fine; effects fine (`print`/
  `log`/`exit`/`now` emit once whichever instance runs them). It is
  consulted by the TAIL-LOOP COLLAPSE GATE (`analysis::
  lambda_is_stateless`, `design/recursive_activations.md` §2): a
  tail-recursive body reuses ONE activation across its iterations only
  when every builtin it reaches is stateless; a wrong `Stateless` is a
  semantics bug (iterations would share per-iteration state), a wrong
  `Sync` only costs the loop. The payload is the direct-call entry the
  JIT uses at every fused site, `FastCall::{Plain(fn(&[Value]) ->
  Option<Value>), Typed(fn(&Env, &Type, &[Value]) -> Option<Value>)}`,
  or `None` for a builtin that can never be one — an effect (a kernel
  is a pure function of its inputs and may re-evaluate) or a
  partial-delivery producer (a fast fn sees EVERY arg present). Under
  strict fusion a builtin fuses iff it carries a fast fn; everything
  else node-walks, and the invalid combinations are unrepresentable
  (the old `STATELESS`/`SLEEP_RESTARTS`/`FASTCALL`/`FASTCALL_TYPED`
  consts and `register_builtin`'s refusals are gone; `SLEEP_RESTARTS`
  had no reader since the strict flip — the restart builtins clear
  their latches in their own `sleep()`). The fast fn: the site stores
  the args' (disc, payload) pairs in a STACK slot and the trampoline
  views it as `&[Value]`; the kernel's arg discs decide the tag (a
  tainted arg bottoms the call without invoking the fn; all-stale args
  make the result stale; `None` is this cycle's bottom), through
  `graphix_fastcall`; `eval` delegates to the same fn through
  `graphix_package_core::fast_eval` (one implementation). A LABELED
  DEFAULT the call left unwritten marshals the CallSite's own compiled
  default node (`MarshalArg::Default`) — `sort(a)`, `escape(s)`,
  `hbs::render(t, d)` fuse in their common spelling. `Typed` is for a
  result DIRECTED by its return type: the site calls it with its
  resolved `CallSite::typ()` baked beside the fn pointer under the
  kernel's env loan (`graphix_typedcall`; `str::parse` casts to its
  `'b` target this way, and the non-inline `cast<T>(x)` pseudo-site is
  the same dispatch with `cast_typed`). A CONFIGURATION a fast fn
  compiles from its args (a regex, an escape table, a template
  registry) lives in a bounded thread-local `FastMemo` keyed by the
  configuring values — a cache, never state. The stdlib is opted in
  broadly (~110 pure builtins across core, array, map, list, str, re,
  hbs, sys::time, sys paths, the json/toml/pack writers); what is left
  is out by rule: the partial-delivery producers, the stateful family,
  the lambda-taking HOFs, effects, and json/toml/pack `read` (async by
  design). The facts are recorded per name as `BuiltinFacts` (`ctx.
  builtin_effect`/`builtin_stateless`/`builtin_fastcall`).

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

- Dev builds are UNOPTIMIZED (opt-level=0, no LTO) since 2026-08-10 —
  roughly half the clean build time of the old opt-level="s"/lto="thin"
  profile. What used to force optimization was stack: unoptimized frames
  are ~6x their optimized size (~420KB per `expr` parse nesting level,
  so a 2MB thread parsed 5 levels). See "Stack discipline" below.
- Release builds use full optimization (opt-level=3, codegen-units=1, lto=true)
- Rust edition 2024 is used throughout
- The project uses `triomphe::Arc` instead of `std::sync::Arc` for better performance
- Pooling is used extensively (`poolshark`, `immutable-chunkmap`) to reduce allocations

### Stack discipline (2026-08-10)

The engine gets embedded and compiles programs it didn't write, so
nesting depth is attacker-controlled. Stack overflow aborts the process
— it can't be caught — so it is closed off two ways at once.

**Guards.** `crate::stack::ensure_sufficient` (`stacker::maybe_grow`,
1MB red zone / 32MB segments) moves a deep recursion onto heap
segments. The red zone has to exceed what ONE level costs between
checks. Wrap any new recursion a program can drive arbitrarily
deep. Currently wrapped: every parser knot (`expr`, `arith`,
`arith_term`, `typ`, `structure_pattern`, `interpolated`, `sig_item`,
and the netidx `literal()` boundary) via the `GrowStack` combinator in
`expr/parser/grow.rs`; `node::compiler::compile`; `Display` for `Expr`,
`ExprKind` and `Type`; `Expr::fold`; `for_each_node`;
`node_const_value`; `Type::{contains_int, normalize_int,
scope_refs_int}`; `would_cycle_seen`; `freeze_for_abi_d`;
`StructurePattern`'s walks in both `expr/pattern.rs` and
`node/pattern.rs`; the node-walk's non-tail lambda dispatch; and
`Type::is_a_int` (a runtime type test recurses through VALUE
structure, so a recursive ADT makes its depth program-driven —
found by P2b's fold_list fixture, 2026-08-25).

**`Node` is a newtype, not `Box<dyn Update>`.** That is what makes the
tree passes tractable: its inherent methods shadow the nine recursive
`Update` methods (`update`, `delete`, `typecheck0/1`, `refs`, `sleep`,
`reset_replay`, `emit_clif`, `fuse`) and run each vtable call under the
guard, and a node's children are `Node`s — one funnel for the whole
family instead of ~1000 call sites. Non-recursive methods reach the
trait through `Deref`. Construct with `Node::new`.

**Destructors too.** Drop glue IS a function (`drop_in_place`) and it
does recurse — but it is compiler-generated, and the FIELD glue runs
after your `Drop::drop` returns, so a guard written inside `drop` has
already unwound by the time the children are destroyed. The teardown
has to become an EXPLICIT call you can place inside the guard, and
there are two ways to get one:

- `ManuallyDrop` on the field, then destroy it yourself — `Node` and
  `TVar` (`ensure_sufficient(|| ManuallyDrop::drop(&mut self.0))`).
  Needed when there is no cheap inert value to leave behind (both are
  newtypes over a pointer).
- `mem::replace` the field with an inert value and drop what you took
  — `Expr` (leaves `ExprKind::NoOp`). No unsafe, and no churn at use
  sites.

The `mem::replace` form works ONLY when the taken value's type does
not itself carry the guarding `Drop`: `Expr::drop` takes an `ExprKind`,
which has no `Drop`, so destroying it recurses into the CHILD's
`Expr::drop`. A `Type` → `Type` handoff makes no progress and spins
forever, whatever guard condition you put on it (`*self = Bottom` is
worse — assignment drops in place, so it re-enters immediately). That
is why `Type` is the one cycle left uncovered: fixing it means
`struct Type(TypeKind)` or a newtype on each recursive edge, since an
enum cannot wrap "the fields of whichever variant". The limit is what
keeps it unreachable.

(The twins in `immutable_chunkmap::avl` and netidx-value's
`ValArrayBase` predate stacker and use a deferred queue instead — ~170
lines each of global mutex, bucketed queue, type erasure and depth
counter for the same effect, and they reorder destruction.)

**The limit.** `parser::DEFAULT_MAX_NESTING` (1000, settable via
`set_max_nesting`) is what makes overflow unreachable rather than
merely expensive, and it is load-bearing for the drop cycles above
rather than just defense in depth. It is counted in parser recursion
knots, not source constructs (one `(1 + …)` level costs three), and
enforced in the same `GrowStack` that claims the stack. Constructs
parsed by an ITERATIVE loop that folds into a nested AST bypass that
counter and are capped separately at the fold — `arith_term`'s postfix
chain (`s.a.a.a…`, `a[0][0]…`) and `arith`'s operator chain
(`1 + 1 + 1 + …`). A new `many(...)`-into-nested-AST parser needs the
same cap.

combine merges a committed error with the surrounding alternatives'
expectations, so a refusal's own message does NOT survive to the top
(a too-deep program reported ``Unexpected `+` ``). Refusals set a
thread-local instead, and every parser entry point runs through
`grow::parsing`, which reports the real reason. Set the flag
(`note_refused`) from any new refusal site.

**Parse errors report the FURTHEST point reached** (2026-09-03, ledger
1 of the admin campaign): combine reports a failed statement at
whichever alternative failed last and `attempt` resets the input on
the way out, so three different mistakes on line 5 of a select arm all
reported ``Unexpected ` ` `` at line 2 (the space after `select`).
Every `GrowStack` knot now records its input position (on success
too — the combinator right after a knot, the `]` an interpolation
expects, is seen by no knot) and `grow::parsing` reports the furthest
one with the SOURCE LINE and a caret; combine's own message is kept
only when it failed there too. Site notes ride the same reporter
(`grow::note_reason(pos, span, reason)`): a reserved word in a name
position (span = the word — it explains a failure only inside that
word, since `true` in `let x = true +;` was probed as a name and
parsed as a literal), a `[` that opens no `[expr]` in a string, a
`duration:` unit netidx does not know (`min` parses as `m`+`in`), and
`///` in a `.gx`. Pins: `parser/test.rs` `parse_errors_report_the_
furthest_point`, `reserved_word_as_a_name_is_named`.

Nesting costs the compiler ~326KB of RSS and ~7ms per level at
opt-level 0 (~5x less optimized), so the limit also bounds how much a
small hostile input can amplify: 1000 knots is ~330 levels of
`(1 + …)`, ~110MB and ~2s. The guards themselves cost nothing
measurable — `examples_compile` 25.4s and a node-walk bench 2.7s with
them and without.

**netidx-value has the same treatment**, because a bracket literal is
also a valid netidx `Value` and `literal()` runs it through
`netidx_value::parser::value` — a recursion this crate can neither
count nor wrap. Its own `GrowStack` + `DEFAULT_MAX_NESTING` live in
`netidx-value/src/parser.rs` (sibling repo). Its Pack (wire) path is
already safe by a different route: `encode`/`decode`/`encoded_len` are
ITERATIVE over explicit worklists, which beats a growable stack for
code you control — no stack proportional to depth at all. Reach for
stacker only where a worklist is impractical.

`graphix-compiler/tests/deep_drop.rs` covers the destructors directly:
the limit is set low enough that the pipeline test never reaches them,
so this one raises it and tears down a 50,000-deep AST on a 512KB
stack. Its own test binary because `set_max_nesting` is process-global.
Note that `#[cfg(test)]` code is invisible to a plain `cargo check` —
use `--all-targets` when a change can break a move out of a field.

`graphix-shell/tests/deep_nesting.rs` is the regression net — 22 shapes
× two depths, each in a CHILD PROCESS on a 512KB stack (a quarter of a
tokio worker), batched 8 at a time. Child processes because an overflow
aborts, so it can't be caught in-process and the child is what names
the case. The ACCEPTED depth (derived from the limit, not fixed) must
PARSE — a case the limit refuses exercises nothing, so the test asserts
it wasn't refused. The REJECTED depth only has to come back at all:
whether the limit fires is shape-dependent (`uniontyp` at 100k is a
FLAT union, not nesting), so `parens` is the canary that proves it
fires. Add a case when you add a recursive construct.

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
  (name, cell addr, bound type), plus `FIND-IMPL` (each impl head tried
  against a receiver, both verdicts), `APP-SPLIT` (a constructor
  recovered through the heads), `BIND ctor` (a constructor variable
  bound by name), `SETTLE-INFINITE` (the cell an occurs-check
  refusal left unbound), `CHK-CONTAINS`/`CONTAINS` (every top-level
  unification's operands and verdict — a passing check that prints NO
  interior events between them concluded on a fast path without
  committing anything), `SET-T` (the general Set⊇t arm's whole/prims
  probe verdicts) and `REF-MEMO-HIT` (the ref-expansion cycle memo
  answering a pair). The tool for "who bound this cell" — found the
  select-arm greedy narrowing (soak jul05 item 12) twice, the P2 trio
  (pre-unified return cell, alias-chain fact, union-scrutinee
  narrowing) in one afternoon, and the aug25a Set-equality fast-path
  hole (the verdict prints are what made "true with zero events"
  visible).
- `GRAPHIX_DBG_KERNELS=1` — print each lambda kernel built by
  `build_lambda_kernel` (name + frozen return type + AbiKind). Locates
  which per-slot/cross-kernel callee actually compiled. Also prints
  `KERNEL DEFINED` per body: state words, site words, site replay
  words, and per-activation block roots (`SelfBlock`) — the tool for
  "does this recursive kernel have interior memory, and where does it
  live".
- `GRAPHIX_DBG_INVOKE=1` — print each fused-kernel runtime invocation
  (kernel name, `event.init`, per-input fired/present). Pins WHICH
  kernel a JIT crash happened in (the frame is unsymbolized native code).
- `GRAPHIX_DBG_REGION=1` — dump fused-region input wiring (name/BindId/
  type+deref/constraints/slot kind).
- `GRAPHIX_DBG_FREEZE=1` — dump region freeze outcomes.
- `GXDBG_TAIL=1` — print every tail-loop dispatch pass (`TAILDBG`: lambda
  id, reentered/framed/init flags, the pass result value+tag, the pending
  tail call's rebind args). The tool for "what did this tail loop actually
  compute per pass" — found the quiet-poll re-derivation clobber in one
  run (aug13i: the settled resident overwritten by a stale entry-formal
  re-read).
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
- `GXDBG_DYNC=1` — print every fastcall/castcall trampoline dispatch
  (arg count, taint/stale masks, the resulting tag). The tool for
  "what did the CLIF marshal actually hand this call".
- `GRAPHIX_DBG_TVAL=1` — print every `TVal` render step (deref'd type
  + naked value) as the typed printer walks. The tool for "why did
  this value print in this form" — found the union-member selection
  picking the never() arm's ⊥-settled cell over the concrete member
  (jul19f divergence_000000, the interp-vs-jit tuple-render split).
- `GXDBG_TYPEREF=1` — on every "undefined type" refusal, print the
  ref's name+scope and every `env.typedefs` scope holding that name
  (`TYPEREF-MISS`). The tool for "is this a scope-path mismatch or is
  the typedef GONE from the env" — one run split exactly that for the
  private-type-union-member recurrence of module-system finding 1
  (the typedef was gone: the instance body typechecked under the
  caller's env; 2026-08-31).
- `GXDBG_LETBIND=1` — print every `let` binding's publication decision
  (`LETBIND`: spec pos, production tag, whether the binding has ever
  published, frame depth, wake-hold, publishing y/n). The tool for
  "does this binding's value ever reach the store" — showed the
  arm-local that never published inside a recursion frame
  (`findings/arm-local-bind-aug2026/`, 2026-08-14). Pairs with
  `GXDBG_REF=1`, which shows the resulting read MISS.
- `GXDBG_SLOT=1` — print each collection cycle's per-slot production
  tag (`SLOT call[i] produced tag=..`) and the resulting fold decision
  (`SLOT map prod=.. resized=.. forced=.. poisoned=.. slots=[..]`).
  The tool for "why did this map/init/find/filter fire (or not)" —
  found the `merge_tag` fired-bit loss in one run by showing
  `call[0] produced tag=64` (fresh bottom) against `prod=Some(96)`
  (standing), which is the whole bug.
- `GXDBG_SHALLOW=1` — print each select arm's sealed shallow
  discriminator (`SHALLOW <pred> => <shallow>|deep`) at the select's
  first consult. The tool for "did this arm's type test stay O(1) or
  fall back to the deep walk" (`Type::shallow_discriminant`).
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
- `GXDBG_RPC=1` — trace the whole sys::net rpc path (`RPCDBG`:
  server proc publish/republish, client call start + reply, NetState
  pump receipt, PublishRpc queue/dispatch/reply/sleep). Lives in
  graphix-package-sys (netstate.rs `rpc_dbg()` + net.rs). The tool
  for "where did this rpc call stall" — found the netidx publisher
  receipt/read deadlock behind the net_rpc0 flake (2026-07-23,
  netidx aede75e6): combine with `RUST_LOG=netidx=debug` +
  `--log-dir` to see the subscription/durable-retry side.

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
- Seeding is LAZY — a fill is correct only when the resolving env
  holds the name's FINAL target, and mid-compile envs are truncated by
  registration order (eager transitive seeding captured the list
  PACKAGE's `List` for tui's `list::List` submodule ref; removed
  twice). Refs fill when a typecheck-time walk needs them, plus ONE
  eager pass: `Env::seed_typedef_refs` walks every typedef body right
  before fusion (after typecheck — every name's final target is
  registered, the one order-correct moment), because a recursive
  type's INNER occurrence is reached by no typecheck walk (the
  Ref×Ref name fast path answers without expanding) and fusion must
  expand it env-free (`list::List` de-fused without it, 2026-08-22).
- `same_def` (structural — gates the Ref×Ref name fast paths via
  `cells_agree`); a DIFFERENT def in the env is a stale-horizon
  artifact and the cell wins. The privatize walk, `same_view`,
  `private_view` and `rebind_resolution` died with inside-module
  transparency (nominal abstract types, 2026-08-22).
- `freeze_for_abi`/`abi_kind` expand refs env-free through the cell
  (`TypeRef::expand_cell`), unfilled → de-fuse; the fusion-side
  `expand_refs` (capped, expanding, env-backed) is the pre-pass for
  kernel-sig derivation. An abstract type is an opaque 2-word Value
  to both.

### Two-Phase Typecheck

Every node implements `typecheck0`/`typecheck1` (two passes over the whole
graph). `typecheck0` also builds `ctx.bind_to_lambda`; `CallSite::typecheck1`
pre-binds statically-resolvable calls (`try_static_resolve`) and re-drives the
bound instance's body typecheck with the call's fn-typed args registered
(per-callsite elaboration), so calls to a lambda *parameter* resolve statically
inside each instance. The old `NeedsCallSite`/deferred-check machinery is gone
— a builtin that needs call-site types reads them from its `typecheck1`
`resolved` argument.

**The recursion knot keys on INSTANTIATION IDENTITY (2026-08-30,
Eric's call).** While an instance body typechecks, its def is entered in
`ctx.resolving_lambdas`; a site reaching the def again in that window
is a self-call and shares the resolving instance — that is what bounds
instantiation regress for recursion. Since `b386f97d` a HOF site's
callbacks premat INSIDE that window, so a use of the same HOF nested
under its own callback (`fold` inside `fold`'s callback, `apply(|y|
apply(g, y), x)`) also arrives with the def active — and keyed on def
alone the knot stamped it with the OUTER instance: `fold -> callback ->
fold` in the static graph, `mark_recursion` marked both sites cyclic,
the emitter refused the region ("mutually recursive static call edge"),
and the shape node-walked into the interp's per-slot lazy binds
(quadratic — the `LambdaIds` hub, open). The knot now compares
`FnArgIdentity` — per argument, the SOURCE lambda (`LambdaDef::source`,
the literal's `ExprId`, stable across instance re-compiles) it resolves
to — and `resolving_lambdas` holds a STACK per def (`ResolvingStack`):
same identity = self-call (reuse; `f(n-1)`, `f` through a forwarded
param, the CPS wrapper `f(n-1, |y| g(y+1))` knots at depth two because
the literal's source repeats); different identity = a fresh instance even
mid-resolution. A bare VALUE reference (`bind.rs`) has no arguments to
key on and takes the innermost active instance, as before. Pins:
`lang/collection.rs` `nested_same_intrinsic`/`nested_map_in_map`/
`user_hof_nested`/`nested_mixed_types` (all `Jit`),
`lang/functions.rs` `cps_wrapper_recursion`. Any special-casing of
collection intrinsics here is the wrong fix — it stops working the day
`fold` is written in Graphix. **An instantiation SNAPSHOTS its def's
`LambdaIds`** (`LambdaIds::instantiate`, from `FnType::reset_tvars_int`,
which is unconditional for fn types): a new node with the def's `own`
and a one-way copy of its links, so def-body facts carry (a returned
lambda still resolves — `returned_lambda_resolves`) while a site's
inflows land on the site's copy. Sharing the node made the def's param
cell a hub every retained instance's callback linked into: `ids()` was
O(instances) per lazy bind (a nested HOF in a callback was quadratic on
the node-walk) and a site could not resolve its own callback
(`hof_nested_map_json_read` was pinned as a "limitation").

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

> Current rules only. History and rationale live in `design/` (index:
> `design/README.md`), pins in `graphix-fuzz/findings/` and
> `stdlib/graphix-tests`, per-change detail in `git log`. When a rule
> here disagrees with the tree, the tree wins and this file is stale —
> fix it in the same change.

**Two evaluators, one canonical.** The node-walk (`node/*.rs`, the
`Box<dyn Update>` reactive graph) is the canonical execution model and
the universal fallback; it must always be correct. Fusion → cranelift JIT
(`fusion/`, emitter split per area under `fusion/emit/`) compiles sync
subtrees to native kernels: success splices the kernel and deletes the
originals, failure leaves the originals to node-walk. There is no third
evaluator, and **no parallel typed IR** — the node graph IS the IR
(`Expr → node graph → CLIF`; each node's `Update::emit_clif` emits its
own CLIF, `Apply::emit_clif` for builtins, the `scaffold` loops for
HOFs). The old GIR was deleted because it forced every semantics fix to
be written three times and taxed every new shape; only its ABI contract
survives (`KernelSig`/`abi_kind`/`freeze_for_abi` in
`fusion/kernel_abi.rs`; the scalar-operator enums in `node::op` are
shared, not ABI). Fusion recursion is `Update::fuse` (from `compile()`,
gated on `ctx.fusion.enabled`); kernel builds are pure signature
derivation (`sig_from_inputs`) and "is it fusable" IS the compile
attempt. Values are netidx `Value` (16 bytes, (disc, payload)); types are
netidx `Type`; `PrimType` is the closed register-scalar set.

**A fusion bug can lose fusion, never produce a wrong answer** — the
differential fuzzer enforces bit-for-bit agreement, and a divergence is
at least as likely a JIT bug as a node-walk one: adjudicate against the
INTENDED semantics, never by trusting either engine.

**STRICT FUSION** (Eric's ruling 2026-09-01, `design/strict_fusion.md`
— "complexity needs to pay rent"; flipped `fa08136a`, the stateful
machinery deleted the same day): fusion admits PURE COMPUTATION ONLY.
A builtin fuses iff its `Effect::Stateless` carries a `FastCall` (a
direct trampoline call on a stack buffer of borrowed `Value`s; the
typed form also gets the site's resolved return type under the
kernel's env loan); the non-inline `cast<T>(x)` is a typed site
whose fn is `cast_typed` — the interp's exact `cast_value`. A `?`
fuses WITH OR WITHOUT a covering `catch`
(Eric 2026-09-01: a cliff that depends on whether a catch is in
scope is the worst kind for predictable performance): the kernel
never writes the handler's variable — a failing handler-ful `?`
RAISES its error onto the invocation's delivery queue
(`graphix_qop_raise`, `QOP_RAISES`) and `Kernel::update` drains it
after the run, in execution order, through `deliver_error`, the
one handler path `Qop::update` also uses (same-top Vacant-insert,
cross-top `set_var`, in-frame `frame_outbox` parking). A kernel is
thus a pure function of its inputs to (result, deliveries).
Everything else — a builtin without a fast fn (stateful, effectful,
seam-gated), `connect` — refuses emission and node-walks,
transitively through callees. There is NO DynCall
dispatcher, no inner Apply inside a kernel, no per-site identity, no
selection memory, no arm-lift, no wake hint; a kernel's only
cross-invocation memory is the firing boundary (prev-length words
for exact HOF resize detection, first-call words for a callee's init
view, the per-call-site blocks and per-activation trees that give
those words per-slot/per-activation multiplicity) — no replay
caches, so `Kernel::reset_replay` is a no-op. The runtime loans a
kernel invocation exactly four things through scoped thread-locals:
`KERNEL_ABORT`, `KERNEL_ENV`, `QOP_RAISES` and the core-trait value
hooks. Measured at the flip: 94% of
kernels kept, benches flat, every bench program still fuses fully.
The follow-on calls: pure selects fuse; grow the fast-fn set
maximally (done 2026-09-02 — see the `EFFECT` entry above for the
converted set and the by-rule remainder); `#[native]` is THE
advertised performance model.

### Semantics both engines implement

- **`let rec` is monomorphic-recursive**: a def-time self-call unifies
  against the def's own cells (`ExecCtx::rec_defs`), the μ-equation
  collapses (`'r ⊇ [T, 'r]` binds `'r := T`), and the collapse looks
  through binding cells (`{let t = …; t}` spellings inherit the bare
  spelling's verdict). Bound-cell pairs walk their bindings; only a
  both-open constraint-graph cycle refuses. `let rec f = |n, acc| f` is
  refused at its definition. Pins: `lang/types.rs` `rec_block_*`,
  `connect_self_nesting_*`, `findings/bound-cell-cycle-accepts-aug2026/`.
- **Static-call instantiation keys on identity** (`FnArgIdentity`,
  "Two-Phase Typecheck" above): a nested use of a HOF under its own
  callback is a fresh instance, not recursion.
- **`select` exhaustiveness is enforced for bare-variant arm sets**
  (`StructPatternNode::matches_anything` drives the wildcard test, not
  `is_refutable`); slice-pattern length ladders count as coverage.
- **Union collapse requires strict tvar identity** (`union_identical`):
  two distinct unbound cells are not the same member.
- **`&&`/`||` are strict** (`false && ⊥ = ⊥`): a dataflow value reflects
  all its inputs.
- **Float comparison is Graphix's total order** (`Value::partial_cmp`:
  `NaN == NaN`, `NaN` below every non-NaN) so `Value` is map-key-able.
- **Checked arith** (`+?`…) yields the catchable `ArithError` value;
  unchecked wraps; integer div0 / signed `MIN/-1` → bottom. Swallowed-
  error DIAGNOSTICS (`error!`/`warn!` for unchecked arith, handler-less
  `?`, `$`) are node-walk-only — a kernel produces the same bottom
  silently; debug with `--no-fusion`.
- **Indexing** (`a[i]`, slices, `bytes[i]`, `m{key}`) is bounds-checked
  through the shared `node::array`/`node::map` helpers on all backends.
- **Bottom is dense** (`design/dense_delivery.md`): `Update::update`
  returns `&TagValue` every cycle — `Fired(v)`/`Stale(v)`/`FreshBottom`/
  `StaleBottom`, the orthogonal fired×bottom algebra (`TagValue::view()`
  is the consumption API). A standing bottom re-delivers `StaleBottom`
  and never re-fires consumers; bottomness joins by OR over consumed
  productions. In the JIT the same bits ride each param's disc (bottom =
  TAINT bit + a helper-safe placeholder payload; TAINT|STALE for a
  standing bottom). A pended DynCall taints at its site and continues;
  `DYNCALL_PENDING` reaching `Kernel::update` is a genuine whole-kernel
  abort only.
- **Bottom never reaches builtin authors**: a bottomed arg makes the
  wrapper bottom the invocation without calling `eval`
  (`CachedVals::any_bottom`); raw `Apply` authors read args through
  `seam_arg`/`seam_tick`/`seam_value` (package-core). Bottoms flow
  in-band with honest tags on both engines.
- **THE ORGANIC FIRING RULE** (`design/organic_firing.md`): a node fires
  iff a consumed input fires; nothing stores a previous value or
  selection to decide a tag; `uniq`/`filter`/`~` are the cadence tools.
  A select emits per fired input — scrutinee delivery, a CONSULTED
  guard's production, or the taken arm's own production (`own_fired`,
  node/select.rs; the kernel folds the scrutinee and prologue-guard
  STALE bits at every merge). Same-arm re-matches emit the arm's
  current value; an untaken arm's body is not a consumed input.
  Selection memory survives only for sleep/wake routing and the
  arm-lift re-seed. Constants fire at init only. Recursion fires like
  the hand-inlined chain with no extra machinery. Ruled deltas +
  red→green protocol: `organic_deltas.rs`.
- **Bottom scrutinee ⇒ bottom select** (Eric 2026-08-29): a select whose
  scrutinee bottoms produces nothing this cycle, no held-arm re-run,
  even if the taken arm is an active async producer — write `hold` on
  the scrutinee to persist across a bottom cycle. There is NO stored-
  selection ride of any kind (the scrutinee, guard, selection and
  unified rides are all deleted — do not reintroduce one). What
  survives is organic own-firing: a STALE-PRESENT scrutinee still routes
  the taken arm's own fires through the retained selection
  (`ChainOut::Quiet`), which is why `select p { null => 42, p =>
  subscribe(p) }` updates when `subscribe` does. Pins:
  `findings/{select-bottom-out-hold,tail-select-bottom-out}-aug2026`.
- **THE CONSULTED-GUARD RULE** (`design/activation_state.md`): arms are
  consulted top-down, structure first, guard second; a consulted guard
  whose current channel is bottom makes the selection undecidable (the
  chain stops, the select bottoms); guards of structure-failed or
  below-the-stop arms are irrelevant. A never-produced guard is unknown,
  not false (the init-phantom guard: a guarded select bottoms at init
  until the guard is evaluable).
- **The bottom-out rule + state multiplicity = activation multiplicity**
  (`design/activation_state.md`): held state never determines output
  bottomness (bottom in, bottom out); non-tail recursion is an
  activation per level, a STATELESS tail loop is one activation reusing
  its one state, collection slots are activations. A tail loop collapses
  to one activation only when the body is stateless
  (`analysis::lambda_is_stateless`, `Effect::Stateless`).
- **Recursion** (`design/recursive_activations.md`,
  `design/atomic_recursion.md`): activations ARE collection slots.
  Instances are retained unconditionally (no park, no budget — "you
  can't fix stupid"; fuzz children run under an 8GB `RLIMIT_AS`).
  **Shrink = delete**: a depth not reached this cycle is deleted and
  re-reaching it is a fresh activation (interp: `ctx.shrink_unwind`
  makes a cyclic-SCC `CallSite::sleep` delete its callee; JIT:
  `Kernel::update` reclaims `SelfBlock` subtrees not stamped with the
  current reach generation, in safe Rust). **No depth limit** — depth is
  bounded by memory on both engines (the kernel re-enters through a
  spill thunk on a fresh stack segment; `GRAPHIX_STACK_BUDGET` aborts a
  runaway like Ctrl-C). **Evaluation is atomic within a cycle**: a
  program may legally spin forever inside one on both engines;
  containment is the cooperative interrupt (`GXHandle::interrupt`,
  polled by the interp's tail driver and every emitted loop head),
  armed by the shell on Ctrl-C or by an embedder's watchdog, observable
  by no program. Pins: `recursion_shrink_deletes_unreached_activations`,
  `fused_recursion_sheds_unreached_blocks` (lift.rs),
  `lib_tests/interrupt.rs`, `graphix-shell/tests/interrupt_wedge.rs`.
- **Sleep is PAUSE, not reset** (Eric 2026-07-31): value-channel state
  survives an arm's sleep — `Held` residents at the three ride sites
  (select scrutinee, pattern guard, `~`'s arg), `CachedVals` staging,
  collection slot values — so a re-selected arm whose fresh
  computation bottoms rides its history. A kernel's slot CHAINS
  (`SiteAnchor`: nested prev-length words, in-loop call-site blocks)
  are semantic per-position state and survive frames as well as
  sleep; only `Drop`/truncation frees them. An arm's WAKE resumes it: a `let` that
  is a `<-` target and holds a value is not reseeded by its re-fired
  initializer (`Event::wake_init`). A producer materializes its value
  channel on its first production whatever the tag (`Bind` publishes a
  quiet first production; `CachedArgs` runs `eval` once from the
  phantom). **WAKE CATCH-UP** (`design/wake_catchup.md`, Eric
  2026-09-01, subsuming the 08-31 present-but-stale ruling): *a
  reselected arm always recomputes from the world as it stands; the
  only events it re-raises are the fires no selected reader saw,
  once, at their current value.* Three mechanisms: (1) each select
  keeps one fire bit per ARM-BODY input (free refs, refreshed at
  deselect; guards, the scrutinee and PATTERN BINDS excluded — this
  select's own AND every enclosing select's: a pattern bind is a
  facet of its arm's scrutinee delivery, which that arm's match
  consumed, so `k` beside `ev@ \`Key(k)` is never re-raised at a
  nested flip after the `ev` reader handled the key — 2026-09-02,
  `Bind::pattern`, `select_sibling_binds_spent`; a destructuring
  LET's siblings are one tracked input instead — `Bind::facet`, the
  bits keyed by the group's representative, the catch-up delivered
  to every sibling the arm reads — `let_sibling_binds_spent`), set on sound
  fires — even with no arm selected — and CONSUMED by whichever arm
  evaluation reads the input; an unconsumed bit delivers at wake as
  ONE catch-up FIRED at the current standing value (conflation;
  `queue` is lossless), injected into the event scoped to the arm's
  evaluation; (2) the first update after a node's sleep FORCES
  recompute — SLEEP STATE IS LOCAL (no ExecCtx globals — the
  parallel-compile/parallel-evaluator discipline, Eric 2026-09-01):
  every skip-owning node/Apply owns a `slept` bit its `sleep()` sets
  and its next update takes (the `dense_gate!` structs
  macro-enforced, the op macros, StringInterpolate, MapQ, Bind,
  CallSite, GXLambda, CachedArgs, Kernel itself — a kernel is a
  node — and Select, whose wake RE-MATCHES against the present
  scrutinee: a selection retained across the sleep was made against
  a value that may have moved while no reader was awake, e.g. an
  arm-local `<-` counter — `select-wake-rematch-sep2026`, sep01c
  ryouko) — and the value channel re-reaches the store (Bind quiet
  re-publish, CallSite arg refresh, GXLambda formal re-seed, MapQ
  rebuild); (3) tags stay
  honest — the wake view reads standing entries STALE (a standing
  value is a PAST event; delivering it Fired phantom-submitted the
  admin pump's password modal), and a STATELESS builtin's eval
  re-runs from the present stale slots at wake (`CachedArgs`
  consumes its own `slept` bit) while a stateful one retags — its
  resident IS its state and its edge catch-up arrives as a tracked
  fire. Kernel: the select's forced init view (`event.init` under
  `wake_init`) is what makes a kernel in a re-selected arm run
  instead of riding its resident; wire slot 0 bit 2 = WAKE (the
  arm's `wake_init` or the kernel's own slept bit), and genuine init
  = `bit0 & !bit2` gates the fastcall stale-mask suppression, so a
  wake delivers standing args STALE and the trampoline produces the
  stateless re-eval's STALE result. Kernels carry NO fire bits and
  need none — a stateful builtin never fuses, so the interp select's
  tracker, which injects THROUGH the kernel boundary, is always the
  mechanism where it matters. Things BORN at init (constants, own
  first productions) still fire; only genuine init upgrades standing
  reads; frames are excluded entirely (`wake_recompute()` is depth-0
  only — the frame-formal FIRED overlay seed survives). Companions: a
  present scrutinee with NO retained selection still routes (depth-0
  first consult; the guard-flip wake keeps its aug03 FIRED), `ByRef`
  seeds its cell stale. THE BIRTH RULE (aug31f ryouko 01): a
  labeled DEFAULT is born with the binding — the interp's bound
  dispatch seeds default args FIRED, and a config memo (`escape_fn!`)
  survives sleep; both
  engines broke together at 9b2e7231 (the metamorphic blind spot) —
  `findings/default-arg-birth-sep2026/`. sys::net level effects
  (Subscribe/Publish/PublishRpc) tear down in `sleep()` and
  re-establish from PRESENT args on their own slept bits
  (`net_{subscribe,publish}_arm_rewake`). Pins: `findings/wake-catchup-sep2026/00–06`
  (Eric's 43/2/21/62 table, shared-input spent, sequential wakers,
  conflation, `~` catch-up, the no-arm window, nested composition),
  `dyncall-arm-init-stale-aug2026` (re-adjudicated in place),
  `lib_tests/callable.rs` `arm_wake_delivers_standing_args_stale`.
  The RESTART builtins (`once`/`take`/`skip`/`uniq`/`hold`/
  `count`) clear on sleep in their own `sleep()` (stateful, so never
  fused). Pins:
  `findings/{sleep-preserves-caches-jul2026,arm-local-bind-aug2026,
  sleep-restart-gate-aug2026}/`.
- **THE QUIET FLAG**: a re-derivation inside a quiet frame
  (`frame_depth > 0 && !frame_init` — every framed pass of a tail chain
  on a non-init cycle) is NOT an init view. Only a site's first-ever
  dispatch is the forced init-view dispatch; a re-woken site is resumed,
  not re-primed; becoming-selected grants no init view in a frame. Wire
  slot 0 is a context word (bit 0 init, bit 1 quiet — set by the wrapper
  from the interp frame, by a tail-loop body for itself, inherited by
  callees; bit 2 wake). The symptom to recognize: a `let rec` chain re-derived by an input that is NOT
  consumed (read only by a structure-failed arm's guard) fires on the
  JIT every delivery and once on the interp. Pins:
  `findings/quiet-frame-init-view-aug2026/`.
- **KERNEL INTERIOR MEMORY** (`design/kernel_instance_state.md`): a
  root body claims per-INSTANCE state words (wire slot 1); a callee
  body claims per-CALL-SITE block words (wire slot 2, `SiteLayout`)
  that each caller supplies from its own storage — static words at a
  root call site, a per-slot chain leaf (`SiteAnchor`,
  `graphix_slot_state_table/blocks`) inside a scaffold loop, and for a
  self-call a lazily grown per-ACTIVATION block tree
  (`graphix_site_child_block`, one root per self-call site; unreached
  activations are reclaimed after each run — the shrink=delete twin).
  The words hold only prev-length words (exact resize detection) and
  first-call words (a callee's init view on its first call ever),
  so one compiled body gets the interp's per-slot/per-activation
  multiplicity for exactly the state that decides FIRING. Callee
  kernels define in TOPOLOGICAL order over the recorded call edges (a
  callee defined after its caller would run below a recursion with no
  interior memory). Pins:
  `findings/recursive-activation-blocks-aug2026/`.
- **Guards in kernels** tick per invocation via a PROLOGUE in
  `emit_select_arms` (the interp ticks every arm's guard every cycle);
  schedule-free guards (pure never-bottom fns of the arm's own binds)
  stay lazy in the chain.
- **Per-cycle firing (the STALE bit)**: a kernel output fires only when
  an input feeding it fired. Collection loops fire by
  `scaffold::SlotFlags`: per-slot discs fold into a slots word and a
  prev-length word gives exact resize detection — fires iff resized ∨ a
  slot fired ∨ the source fired empty; a same-length refresh with a
  quiet body does not fire. Fold included (2026-08-31): each body
  evaluation's STALE folds into the slots word (`fold_stale` — a
  mid-chain slot consuming a fired acc fires the fold even when a later
  acc-ignoring arm leaves the final carry stale) and the acc carry is
  one more firing source (it alone covers the empty-source chain);
  TAINT rides the carry only — consumption decides, an acc-ignoring
  callback recovers (`fold-midchain-fired-aug2026`). Callee bodies keep
  per-call-site state blocks (wire slot 2, `SiteLayout`) for
  first-call words and prev-len words — never select firing memory.
- **Collection HOFs** (`design/collection_intrinsics.md`): MapQ/FoldQ
  are compiler-owned nodes (`node/collection.rs`) — the canonical
  per-slot interpreters — and `GXLambda::emit_clif` inline-emits a
  collection-bodied callee as a native loop at the call site
  (`scaffold::emit_*_loop`); refusal leaves the per-slot node. List and
  Map lower through the FLATTEN boundary (`graphix_list_to_valarray`/
  `graphix_cmap_to_pairs` → the array loop → rebuild). `FoldAcc::Value`
  carries Value-shaped accumulators. `find`/`find_map` scan all slots in
  both modes (a bottom predicate after the match bottoms the find).
  Collection callbacks with labeled parameters interpret; a callback
  with only labeled parameters is a type error. Cross-kernel call sites
  force the callee's init view on the first call ever.

### Testing is differential

- `run!` (`graphix-package-core/src/testing.rs`) runs each fixture in
  `interp` and `jit` modes asserting equal values; `FuseExpect::{Jit,
  None}` asserts WHETHER it fuses (bidirectional — the harness demands
  the annotation match reality). `GRAPHIX_FUSE_AUDIT=1 cargo test --
  jit --nocapture` prints the per-fixture audit; sweep the workspace,
  the stdlib crates carry fixtures too.
- **graphix-fuzz** (`design/graphix_fuzz.md`): node-walk vs JIT with a
  per-cycle TRACE oracle; `check`/`run`/`generate [--reactive]`/`fuzz`/
  `minimize`/`regress`/`selfcheck`/`gen-check`/`detcheck`/`typemorph`.
  Programs may carry a `// schedule-v1:` header (input epochs via
  `set_many`; inputs use the `let inN = d; inN <- never(d)` contract)
  and a `callable-v1` header (the route matrix: in-language injection
  vs `compile_callable` dispatch); the metamorphic twin scan catches a
  bug that breaks every engine and route identically. `minimize` is
  typed-AST HDD (statement drop); `selfcheck` (same mode vs itself,
  100% required) gates oracle soundness; `detcheck` is the fusion-shape
  determinism gate (two fresh processes, normalized CLIF dumps must
  match). `rand::`/`sys::`/`http::`/`hold(` programs are excluded from
  divergence recording (their output depends on async timing). The
  committed `findings/` corpus is the regression gate (`regress`).
- **Soak ops**: campaigns run under `nice -n 19`, from a campaign-private
  COPY of the binary, with output OUTSIDE the repo
  (`~/tmp/target/fuzz/<campaign>/`), one corpus dir per campaign; the
  pool gives children `GRAPHIX_STACK_BUDGET=1GB`, parent-owned sandbox
  cwds, and a `BreakageWindow` backstop. **The fleet deploy is a
  script**: `graphix-fuzz/fleet.sh deploy <new> <base-seed> [old]` (steps
  `pull`/`stop`/`sync`/`launch`/`verify`/`status`; `FLEET_ONLY`/
  `FLEET_EXCLUDE` scope one box) — every step verifies a FACT (pgrep,
  content fingerprint, the campaign's own gate line with the embedded
  corpus count). Seeds are 10M apart in host-table order. Pulled
  findings go to `fuzz/pending-triage/<camp>/` (untracked); the triage
  record is its README.
- `FusionStats` (`attempted`/`fused`/`failed` with reasons, via
  `GXHandle::fusion_stats()`): read `failed` as a blocker profile, not a
  gap count.
- A stack-budget abort is `Outcome::Timeout` (containment, like the
  deadline); which limit stops a runaway first is a race between the
  engines' descent speeds, not a property of the program.

### Kernel infrastructure

- **JIT memory lifecycle**: one active JITModule + 256MB arena per
  ExecCtx (cross-kernel calls are ±2GiB PC-relative); kernels are never
  freed within a generation; on exhaustion the module RETIRES whole
  (`FusionCtx::retired_jits`, kernels stay mapped) and the region build
  retries in a fresh one — a region builds atomically in one generation
  and generations never link. The reclamation unit is the ExecCtx.
  `GRAPHIX_JIT_ARENA` shrinks the arena so gates exercise rotation.
- **Kernel ABI**: kind-grouped params (scalars, then array/tuple/struct
  pointers, then string, then 2-word variant/nullable/value) from one
  source (`KernelSig::abi_params`); any region width fuses. Recursive
  types freeze to an opaque leaf; an abstract type is an opaque 2-word
  `AbiKind::Value`.
- **Emit contracts** (`design/distributed_jit.md`): replayability ≠
  `Sync`; effects de-fuse, never silently skip (a skipped fn-formal arg
  with an effect de-fuses); first dispatch forces the init view;
  wake-ups key on `(BindId, fusion.top_id)`; clone types out of
  `with_deref` before recursing; dead statements eliminate at emit only
  when the statement subtree is effect-free, and a statement binds
  whatever its subtree binds. The Value-shape call return folds
  `tagbits` like every other shape. Kernel cache keys carry the
  instance body's catch coverage and a resolution FINGERPRINT (same
  types + different callbacks ⇒ two kernels). Sig-less modules refuse
  emission. `freeze_for_abi_normalized` never normalizes shared tvar
  cells (`check_mode_parity` pins mode-identical `--check`), and a pass
  the fusion gate owns must never change what the typechecker sees
  (`Env::seed_typedef_refs` runs in both modes). OWNED SELECT-ARM
  BINDS DROP AT EVERY ARM EXIT (2026-08-31): the non-scalar pattern
  binds (`PayloadValue`/`ListHead`/`ListTail` clones) drop on the
  value-position taken path, the guard-false, tainted-take and undet
  edges, and the guard prologue (`emit_scope_drops` before each
  truncate) — they leaked ~55-80MB/s on hot fused selects for the two
  days they existed (found by RSS probing during or-pattern P3 recon;
  `valgrind --max-threads=4096` names the class — tokio's blocking
  pool exceeds valgrind's default thread cap). Tail position was
  already safe (`emit_kernel_return`'s whole-env drop; the tail-rebind
  epilogue's above-param-mark sweep). Pinned by the `leakcheck`
  witnesses `select-payload-bind`/`select-list-binds` — run leakcheck
  whenever a change adds a new owned-local class.

### Coverage (current)

By fixture annotation (2026-08-30): 452 `FuseExpect::Jit` vs 250
`FuseExpect::None` — ~64% of the pinned corpus fuses; every bench
program fuses fully. The vocabulary: all scalar arithmetic/comparison/
logical/cast/checked-arith, every producer and accessor, `?`/`$`, all
eight array HOFs as native loops over scalar/composite/String/value
elements (HOF-of-HOF and same-HOF nesting fuse as one multi-loop kernel;
fold accumulators may be composite or string), `select` structural
destructuring with scalar leaf binds and non-scalar variant payload
binds (the slot clones out as an owned local of its ABI kind,
2026-08-30 — a recursive type's payload is an opaque value leaf),
tail loops carrying ANY kernel param kind (Value pairs and Strings
rebind via the clone/drop protocol; `structural_tail_loop` admits
every kernel-encodable carried kind, so `lfold_rec` fuses and the
hand List fold beats the intrinsic at 100k), every fast-fn builtin
and non-inline cast via the direct trampolines, cross-kernel lambda
calls (recursive self-calls: tail → rebind-and-jump, non-tail → native
recursion), trait default bodies and fn-formal forwarding/capture. (The
annotation census predates the strict flip; connects and non-fastcall
builtins node-walk by rule now.)

Fusion descends through Module/Block/Bind/CallSite/Catch/Lambda/Select
(scrutinee, guards and each arm body get their own region passes) and
ExplicitParens; not through `~`, `<-`, or operator operands (a sync
expression there fuses only as part of an enclosing region; a registry
attribute there is a loud compile error).

The correct-None denominator (principled): async/streaming builtins,
cross-cycle nodes (`~`, `Any`, `Catch`'s handler read), and non-register-
encodable types (`decimal`, `Fn`, `Ref`, unbound tvars). The missed-
fusion residue, each pinned by a `#[native]` de-fuse test or an ASPIRE
comment: select residue (whole-composite/`@`/named-rest binds, nested
patterns INSIDE a variant payload, owned scrutinees in tail position);
union-self trait dispatch and abstract patterns in select;
union-typed cross-kernel returns
(`rec_block_multi_member_collapses`); non-scalar string-interp parts;
dynamic map literals; `array::group`; ByRef/Deref; decimal arith. The
intrinsics-deletion endgame is measured in `bench/collection/README.md`.

`#[native]` asserts zero node-walk residue at a source location (a no-op
under `--no-fusion`); `#[sync]`/`#[async]`/`#[tail_recursive]` assert
analysis facts. EmitTags (per-op body tags) were retired unbuilt — they
would resurrect the GIR vocabulary tax (`node_shape.rs`).

### Design documents

**Mux-select WITHDRAWN 2026-09-04** (`design/pure_dataflow_plan.md`):
always-update impure arms and deleting sleep did not simplify the
engine (sparse delivery died on the JIT; `seq` lost exit actions).
Sleep is pause again. Keepers from that arc: `~!` (strict sample, no
bank); a pure non-recursive arm skips `sleep` (nothing to pause) and
is not updated while untaken. A `<-`, a catch, a sample, an `any`, or
a stateful/async callee is not pure — those arms still sleep. The
write rule (compiler-sampled `<-` in an arm) was withdrawn: sometimes
you want a write on every scrutinee fire and sometimes you do not,
and `~` is how you choose. Straight-line `seq` is built (2026-09-04,
`if`/loops held): AST-to-AST pc machine, `range` for the old `seq(i,j)`
builtin; `do { }` is one seq arm (lets inside, pc-sampled connects).
The admin TUI port uses surface `seq`/`do` for every multi-step
ceremony (`design/seq_blocks.md`).

`design/README.md` is the index (built / proposed / superseded). The
docs hold the rationale and the as-built records; this file holds only
the rules.

## Language features (current)

- **`~!` and skip-sleep** (2026-09-04, keepers from the withdrawn
  mux-select arc): `e ~! v` is `v` at each fire of `e` and bottom with
  no bank when `v` is bottom; `~` still banks. A connect writes when
  its RHS fires. Sample the event when a write should run on every
  delivery into the arm; a constant RHS fires when the constant does
  (init, or the arm's wake). A pure non-recursive arm skips `sleep`
  (nothing to pause) and is not updated while untaken; a `<-` is not
  pure, so a write arm still sleeps.
- **`never<T>()` is syntax** (2026-09-02, ledger 2 of the admin-TUI
  findings): `ExprKind::Never { typ, args }` → `node::Never`, typed
  bottom bare or the spelled `T` (scoped at compile), args updated and
  consumed, never produces (`NodeView::Never`, ASYNC like `any`, no
  emit). A builtin's call-site cell bound to bottom only at static
  resolution — after a select had unioned its arms in typecheck0 — so
  nested never() arms typed as free cells and forced annotations; the
  companion `union_int` fix derefs a BOUND cell to its binding. The
  connect-seed idiom (`let res = never(); res <- v`) lives in the
  binding now: an unannotated `let` over a ⊥ initializer seeds a fresh
  cell its writers refine, settled to ⊥ by `Bind::typecheck1` if
  nobody does. `never` is a reserved word; the core builtin is gone.
  Pins: `lang/select.rs` `never_*`, parser `never_parses`.
- **`seq` blocks** (`design/seq_blocks.md`, straight-line 2026-09-04;
  `if`/loops held): `seq [trigger] { stmt* [expr] }` desugars to a
  pc machine (Idle/S0..Sn, `filter` busy-drop, presence select + free
  `pc` read per step). Lets become cells; `until e` waits on a bool
  level; one catch at the top is cleanup + reset + rethrow; `?` aborts
  the run. No trigger = run at init. A bare-variable trigger is
  snapshotted. `until` is reserved and legal only as a seq step. The
  integer builtin is `range(i, j)`. `do { stmts }` is several statements
  as one arm (lets inside, connects pc-sampled). Pins: `lang/seq.rs`
  `seq_do_*`. The admin TUI port uses seq/`do` for every multi-step
  ceremony. `never()` in a step stalls the run.
- **Place references** (`design/place_references.md`, 2026-09-02,
  Eric: "not having this changed the way you wrote an API in tui;
  that qualifies as a now change"): `&a[i]`, `&s.f`, `&t.0`, `&m{k}`
  and chains over a variable (or a `*r`) are PLACES — the root
  binding plus a path — typed as a reference to the ELEMENT (the
  access's type minus its error, as `$` types). `*r` reads the root
  through the path; `*r <- v` PATCHES the root: the write is queued as
  (path, value) and resolved against the root's value AT DELIVERY
  (`push_var_event!`), so two patches to one root in one cycle land
  in order on each other's result, never on a stale whole. A dynamic
  key (`&vals[focus]`) is a moving reference: it re-fires its readers
  when the key moves and writes where it points. A missing place
  bottoms a read (warned) and drops a write (logged). Plain `&x` is
  unchanged (cell + byref chain, `Value::U64(cell)` on the wire; a
  place cell still mirrors the element so embedders keep reading it);
  `Rt::{patch_var, set_ref_path, ref_path, clear_ref_path}`,
  `node::place::{Step, Path, VarUpdate, read_path, write_path}`.
  References still de-fuse. The admin TUI's `tui::form` edits its
  focused editor through `line_edit::handle(&vals[i], e)`. Pins:
  `lang/byref.rs` `place_*`.
- **Native List, phase A** (`design/list_native.md`, 2026-08-31):
  `List<'a>` is a compiler-known constructor like `Array` —
  `Type::List`, reserved type name (in RESERVED beside `Array`/`Map`
  since 2026-08-31 — a user typedef of the name refuses at parse; the
  tui widget state type renamed `List` -> `ListBox` for it, tag
  `` `List `` and the Rust side unchanged), covariant element, no
  primitive-bit relation, `AbiKind::Value` at the kernel boundary.
  Variant TAGS are backtick-namespaced, NOT type names: a reserved
  word is a legal tag in expression, type and pattern position alike
  (the pattern parser used to refuse what the other two accepted —
  fixed 2026-08-31, pinned in `list_is_a_reserved_type_name`).
  The rep is PRIVATE to `node/collection.rs::list`: cons =
  `ValArray([head, tail])`, nil = the static EMPTY array clone,
  discriminant = length. The Collection impl lives in CORE (intrinsic
  markers; len derives from fold). TVal prints `[<1, 2>]`; the wire
  and naked echo stay structural (nested 2-arrays). Phase B: `[<1,
  2>]` literals (ExprKind::List; the JIT emits the tuple relay +
  `graphix_valarray_into_list`) and list-slice patterns as a FLAVOR
  on the slice machinery (`list: bool` on the AST, `SliceKind` on the
  node): `[<>]`, exact, `[<h, rest..>]` with rest binding the TAIL
  O(1); the SUFFIX form is refused (front is O(n)); length-ladder
  coverage carries over (array_members also collects List members).
  Grammar rules: a bare `>` immediately before `]` is the literal
  closer (never the comparison); tree-sitter spells the delimiters
  `'['`+immediate`'<'` / `'>'`+immediate`']'` (a 2-char token
  shadowed `[` in value-strings). B3: list patterns FUSE over a
  Value-kind scrutinee (`graphix_list_match` + kind-safe
  `graphix_list_get_*`/`graphix_list_tail` — the rest bind rides the
  Value tail rebind), so the `[<>]`/`[<h, t..>]` ladder is a native
  loop: `lfold_rec` beats the list intrinsic at 100k. Nested element
  patterns and `@`-binds on list arms de-fuse (coverage).

- **Or-patterns** (`design/or_patterns.md`, ruled+built 2026-08-31,
  orthodox): `p1 | p2 | …` in select arms and every bracketed element
  position; top-level or-patterns are SELECT-ARM-ONLY (`let`/lambda
  params refuse — the lambda arg list is `|`-delimited); `@`-captures
  are per-alternative (no pattern parens) and type as the UNION of
  their alternatives' narrowed types (Eric 2026-08-31 — Graphix
  narrows captures where Rust binds at the enum type, so exact
  equality refused the keymap idiom ``kk@ `Up | kk@ `Char("k")``; the
  capture is the whole matched value, the union is exact; pins
  `or_capture_union`/`or_payload_unequal_rejected`).
  Alternatives try left to right, first structural match binds; every
  alternative binds the SAME names, PAYLOAD binds at EXACTLY EQUAL
  types (BindIds are
  shared — alternative 0 allocates via `BindMode::Record`, the rest
  `Reuse` and bind nothing in the env; open cells unify at the reused
  leaf, concrete payload mismatches err, captures widen); ONE guard
  per arm covers the whole
  alternation. Coverage is per coverage ATOM (`arm_atoms`): an or-arm
  claims once per alternative against its own member of the raw
  inferred Set (`true | false` completes bool, `[] | [_, ..]` feeds
  the length ladder — the bound ladder spelling is ill-typed first by
  same-binds). Dead ALTERNATIVES are errors like dead arms (duplicate,
  post-wildcard, range-covered, type-dead vs the residual scrutinee).
  JIT (P3, same day): or-arms emit natively — `emit_or_chain` runs
  the alternatives' structure conditions left to right (each via the
  extracted `emit_structure_cond` against its member of the raw Set),
  the first match's binds forward through ONE done block's params to
  the shared BindIds (layout mismatches Err = de-fuse, never
  miscompile); the guard prologue reuses the chain with a tainted
  drop-safe placeholder feed on no-match; the arm's env mark precedes
  the chain so the arm-exit scope drops cover the chain's owned binds.
  Explicit type predicates on or-arms refuse; per-alternative residue
  = the single-arm vocabulary. Pins: `lang/select.rs` `or_*`
  (`or_native`/`or_owned_binds`/`or_guard_prologue` are the
  `#[native]` P3 pins), parser `or_patterns_parse`.
- **Nominal abstract types** (`design/nominal_abstract_types.md`):
  `type T = Abstract<rep>` (only as a whole typedef body) has identity
  `AbstractId::of(scope, name)` (a path-derived v5 UUID, minted at
  `Env::deftype`) and values `Value::Abstract(GxAbstract { id, name,
  payload })` minted only by the constructor `T(v)`; `x.0` reads the
  payload, `T(p)` destructures, `T as t` is a nominal tag test
  (parameters are not carried at runtime: `Box<i64> as b` matches a
  `Box<string>`). The three faces compile only where the definition is
  visible (`Env::abstract_reps`, gated by `AbstractRep::public` or scope
  prefix); a gxi-hidden type must be `Abstract<..>` or Rust-backed. There
  is no inside-module transparency. Rust-backed abstracts register
  path-derived UUIDs (`abstract_wrapper!`, `impl_abstract_arc!`'s
  `= "pkg::mod::Type"` form), which is what makes a type test exact and
  trait dispatch over a union of them work. Abstract patterns de-fuse
  the select (coverage). Diagnostics: `Type::Abstract` carries only the
  id, so Display consults a process-global `AbstractId → name` registry
  filled at `AbstractId::of` (2026-08-31 — errors print `Box`, not the
  word "abstract").
- **Traits v1** (`design/traits.md` §11–13): `trait T { val m: fn(self,
  ..) -> R [= default]; .. }`, `impl[<'a: C>] T for Target { let m = .. }`,
  `impl T for X;` in a gxi (the entry of record — the module's own impl
  fulfils it and consumers resolve to stable bindings across reloads),
  `'a: T + U` bounds, `fn(x: T)` ≡ a fresh bounded quantifier. Trait
  names are scoped like types, impls are global facts; `T::m`/`use T::m`
  ride the import engine. Dispatch is STATIC on the self argument's type
  (`CallSite::resolve_trait_call`); an open self type at a call is a
  compile error; a union self lowers to a synthesized select (de-fuses —
  coverage). Impl targets: an abstract type in the type's or the trait's
  package, anything else only in the trait's package, never a union, one
  impl per head. Constructor traits (`trait Collection`, the `'_` hole,
  `|c: Collection|` sugar) dispatch by decomposition on the receiver's
  outermost form. A constructor APPLICATION (`self<'b>`, `Type::App`)
  whose constructor has bound IS its filled type: `with_deref` fills it
  (`Type::app_filled`), so `is_a`, `cast`, the select coverage check,
  the typed printer and `kernel_abi` all see `Array<'b>`, never an
  `App` (2026-08-30 — before this, `select` over a trait-returned
  collection was refused as uncovered and the printer logged
  "type Array<'b: i64> does not match value"). In `contains` a cell
  bound to a reference — a filled application included — meets a
  reference on the other side BY NAME (`Type::ref_behind`) ahead of
  the expansion arm. Only an OPEN constructor stays an application,
  and consumers treat it like an open cell. Core `Eq`/`Ord`/`Display` ride the VALUE through
  netidx's abstract vtable (map keys, sort, min/max, uniq, operators,
  printers — both engines); only abstract types may implement them
  outside core; a bottoming impl resolves per key like NaN. A core-trait
  impl for a Rust-backed abstract is refused (no payload to consult). Not
  built: trait params/associated types, trait aliases, `Hash`.
- **The io traits** (`design/traits.md` §13): a stream's TYPE is its kind
  (`sys::fs::File`, `sys::tcp::TcpStream`, `sys::tls::TlsStream`,
  `sys::process::Pipe`, `sys::io::Stdio` — five Rust types over one
  `StreamKind` via `stream_kinds!`) and `Read`/`Lines`/`Write`/`Close`/
  `Seek`/`Socket` say what it can do; `read` is the only required `Read`
  method, the derived ones are Graphix over it with native overrides.
  json/toml/pack/xls parse `bytes`/`string` only. A default's accumulator
  connect must be gated on the event (`acc <- b ~ concat(acc, b)`) — the
  ungated form is the counter idiom by accident. API breaks vs 0.9.0:
  `Read::read`, `Seek::seek`, `Socket::shutdown`; `process::Stdio` →
  `process::Redirect`; `Child`'s pipe fields are `[Pipe, null]`; a TLS
  upgrade consumes the TCP handle.
- **The module system** (`design/module_system.md`): Rust-2018-style
  imports — every name arrives by a declaration, a `use` (renames,
  globs, groups, `{self, *}`), or a prelude (core's root items; package
  names as path roots). Paths lead with a package name or
  `self`/`super`/`package`, in expression and type positions. A
  submodule sees nothing of its parent implicitly; `mod`/`use` position
  carries no visibility meaning; a gxi `use` is a private import, not a
  re-export (`pub use` reserved, unbuilt). Resolution: lexical chain →
  imports → globs (two providers of a used name error at first use) →
  package prelude → core prelude; declarations shadow imports, imports
  shadow globs. `Env.names` is a global per-scope registry (exempt from
  `restore_lexical_env`) so instance-side resolution consults the
  DEFINING module's table. `use` compiles to Nop. The widget-module
  `{self, *}` idiom is the one blessed glob in exemplar code.
  DECLARATIONS ARE STATEMENT-POSITION-ONLY (2026-08-31): `use`, static
  `mod`, `type`, `trait` and `impl` refuse in value position (a `let`
  RHS, a call arg, a block's value slot, a select arm body) — they are
  ⊥-typed with a phantom value channel, and a value-position one let a
  connect route runtime values through a ⊥ binding (aug27a `use`,
  aug31e `type`). The companion typing fix: contains' (Bottom, TVar)
  arm derefs a BOUND cell (⊥ ⊇ Array refuses) instead of answering
  true — a value-position CONNECT (`let x = y <- e`) is legal by
  design, so the arm is load-bearing, not redundant
  (`bottom-connect-target-aug2026`). A dynamic `mod` stays an
  expression (real `[error, null]` value).
- **Comments** are legal only above an expression, a select arm's
  pattern, an impl method, or a struct-literal field (`parser::decorate`
  attaches them; the printers hoist them back); interior, trailing and
  dangling comments are parse errors by design. The tree-sitter grammar
  treats `#[..]` attributes and comments as extras.
- **A free union member stays free**: in `contains`' Set×Set residue arm
  an unbound rhs member is residue, never covered by a concrete lhs
  member. A select's type is the union of its arm types; a free `'b` arm
  beside an `i64` arm is not inferred to `i64` — annotate.
- **Bool literals pool per position inside composite patterns**
  (2026-09-02, ledger 14 of the admin campaign): arms whose only
  refutable leaves are bool literals are grouped by SHAPE (tuple
  arity / variant tag+arity / struct fields) in `select.rs`'s
  `LiteralPool`; a group covers the scrutinee's member of that shape
  once its literal vectors cover every assignment of the positions
  any arm tests (a bind or `_` matches both), and the dead-arm walk
  subtracts the same member — `(true, true) | (true, false) |
  (false, _)` covers `(bool, bool)`, `` `Join(true) `` + `` `Join(false) ``
  cover `` `Join(bool) ``, a wildcard behind either is dead. Two more
  from the same night: `Type::could_match` has a function arm (a
  select over `[fn(..), null]` used to refuse its bind arm — ledger
  11), and a reserved word in a name position reports itself
  ("note: at line: L, column: C: `ok` is a reserved word…",
  `grow::note_reason`, reported when the failure lies on that line or
  before it — ledger 1's reserved-word half).
- **Set coverage distributes over product heads** (2026-08-31, the
  admin-TUI panel screens): when the `(Set, t)` single-member and prim
  walks both refuse, same-shaped members (variant tag+arity / tuple
  arity / struct fields) pool ONE argument position — every candidate
  must cover every other position in full, and the pooled position's
  union must cover the member's (`contains::set_covers_by_distribution`)
  — so `` [`P(A), `P(B)] ⊇ `P([A, B]) `` and the nested-variant select
  ladder is exhaustive. Pure probe, cell-free scrutinee side only,
  commits nothing. A probe IN PROGRESS for the same scrutinee ref
  claims nothing on re-entry (`RefHist::distributing`, 2026-09-03):
  the probe re-expands a recursive alias through `lookup_ref` with no
  memo of its own, and the Ref×Ref cycle memo is dropped once the
  direct walk fails, so a mismatched `Error<ErrChain<..>>` connect
  re-asked the `cause`-field question one level deeper forever (hz0
  sep02a: hangs, a stack-budget abort, an OOM — all at compile time;
  `errchain-distribution-loop-sep2026`). The dual fix: `union`'s Variant×Variant arm merges
  component-wise only when ≤1 position differs (`union_identical` per
  slot) — the arity≥2 rectangle collapse (`` `P(A,X) ∪ `P(B,Y) `` →
  `` `P([A,B],[X,Y]) ``) invented the off-diagonal and select coverage
  accepted arm sets with a runtime hole. Pins:
  `lang/select.rs` `select_variant_union_*`,
  `select_tuple_union_member_exhausts`.

## Stdlib package notes

- **`sys::process`**: managed children live in the opaque `Proc` value
  with weak polling + `kill_on_drop`; `options` and `stdio` named-arg
  constructors; redirects are an explicit `Pipe`/`Inherit`/`Null`
  variant (default `Inherit`); the polling task is the sole reaper and
  `wait` subscribes to its watch status. Wire conversion uses
  `netidx-derive` except `SpawnOptions.env` (`immutable_chunkmap::Map`
  has no `FromValue`). Shell tests are Unix-gated with `cmd.exe` twins.
- **GUI** (`graphix-package-gui`, iced 0.14) uses the iced sub-crates
  directly; `iced_renderer` needs both `wgpu` and `wgpu-bare`. GUI/TUI
  examples are visual — test manually. `GuiWidget` has a `#[cfg(test)]
  as_any`; `GuiTestHarness::dt()` downcasts; tests fire callbacks via
  `gx.call(callable_id, args)`. Test contexts default to
  `NetConfig::Internal` (a real in-process netidx on demand); publisher
  coalescing collapses rapid updates — space them with timers.
- **Package manager** (`graphix-package`): `packages.toml` v2 is a
  `[stdlib]` table (`installed`/`removed` names; stdlib always tracks
  the shell version) plus `[packages]` for EXTERNAL packages
  (version-or-path); the old flat format migrates once on read
  (`LEGACY_REMAP`: `fs`/`net`/`time` → `sys`). `combined_map` bridges to
  the build machinery; the stdlib set at a version is enumerated from
  that shell source's `Cargo.toml` (`stdlib_packages_in_source`);
  `DEFAULT_PACKAGES` is only the bootstrap; `INTERNAL_PACKAGES =
  ["bench"]`. `update` presents a maskable change set (shell bump, new
  stdlib, external updates), prompts `[Y/e/n]`, builds BEFORE writing
  the manifest, and hard-errors on non-TTY without `--yes`. The pure
  core is unit-tested (`test::pure`); `download_source` is testable
  against a fixed released `graphix-shell` in a temp data dir.

## The admin-TUI dogfood campaign

The netidx-admin ratatui TUI (~11k lines) is being rewritten in Graphix
as `graphix-package-netidx-admin`, which lives in the NETIDX repo (the
first real external package). Design + findings log:
`../netidx/design/graphix-admin.md`, `graphix-admin-findings.md`.
**The PRIMARY objective is finding and fixing Graphix problems; the TUI
is secondary** (Eric). No workarounds: an awkward idiom, slow compile,
bad diagnostic or missing capability means stop, log a finding, fix it
here (or consciously accept it), then continue — never move decision or
presentation logic into the package's Rust layer because Graphix was
painful. Measure `--check` time at every size milestone. State: Phase D
in progress — the remote tab is complete (landing, connect, the panel
menu, seven panels, the change-password route) and the Local tab's
first slice landed 2026-09-02: `tui::panels` (the panel machinery
over any `Target`, opened on the menu or straight on the roster /
this host's permissions — the remote tab is landing + connect +
panels), `tui::local` (detection over both config roots, the action
list, the status card with the glyph, per-slot background sync and
credential probes, the in-process lifecycle ceremonies: renew,
update, backup, the four local-CA ops), `tui::app` (both tabs under
one pump; `Tab`/`q` global behind the tab's own keys; `q` needs
`tui::exit`, added the same day), and `tui::services` (this host's
activation units: list/control, create/edit through an in-TUI form
over the whole unit file, delete), and phase 4 the same evening:
install/join/add_parent/restore_stage+finish/uninstall ceremonies
(the teardown's CA chooser and the parent picker in Graphix;
`install_service` does user scope in-process), and phase E the same
night: `tui::suspend(suspended: bool)` — a LEVEL (Eric: the
dataflow-native shape; two token/run-it-yourself cuts died first):
while true the display drops its crossterm reader and restores the
terminal, the result is the display's actual state, the program runs
its child through `sys::process` with inherited stdio on the release
and clears the level with the exit; draws pause while the runner
holds a suspension and updates keep applying — a runner blocked in
its select arm would deadlock the shell loop; the runner's
stop+suspend control is one libstate entry whose suspend receiver is
parked until a display takes it; the site holds the resume signal and
its drop resumes) plus
`netidx_admin::privileged` (the become-root decision lives in a shell
wrapper the CHILD runs, so sudo's prompt lands on the released
terminal). Needs lab validation on a real terminal. The RELEASE MEASUREMENT is
done (2026-09-03, 5,434 lines): registration 93ms, app-main compile
455ms (228ms with fusion off — 324 of 1,286 kernel attempts fuse,
and a failed attempt costs a compile), first frame 584ms; a
role-menu key p50 3.2ms, a text key 0.17ms, a render 0.12ms
(`milestone_timing`/`milestone_latency`, ignored, run by hand in
both profiles). Follow-ups: a cheap fusion pre-gate for the
decidable refusals; a profile of the role-menu key. Then: full lab
validation, the parity review;
the ratatui TUI is deleted only after all of that (Eric). Seq conversion
of the port's multi-step ceremonies landed 2026-09-04 (local lifecycle,
panel actions, services, landing save/discover, remote connect); a seq
let's fire is only live in the next step — later `t ~` on it does not
write (ledger 16). Facts from
the slice: a constant-RHS connect in a
select arm fires once per selection (organic firing — Eric 2026-09-03:
that is the trigger on the selection changing, a tool, not a lint;
handlers sample the event; the select chapter's "Writing From an
Arm");
bool literals do not pool inside payload/tuple positions (nested
selects; ledger 14); a variant arm with a payload pattern never
narrowed the arms after it — `Type::diff` compared payloads with
`==` against the pattern's bind cells — fixed (`same_resolved`,
pins `variant_*payload_arm_narrows`). The
open-items ledger is the top section of
`../netidx/design/graphix-admin-findings.md`. Open prerequisites: terminal
suspend/resume for `sudo`/`$EDITOR` handoff (phase E); reserved-word
parse diagnostics at package scale.
