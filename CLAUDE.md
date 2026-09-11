# CLAUDE.md

Guidance for Claude Code in this repository. This file holds RULES ONLY:
how the project is built and tested, how the engine behaves, what the
language does. Rationale and as-built records live in `design/`
(index: `design/README.md`); history lives in `git log`. When a rule
here disagrees with the tree, the tree wins and this file is stale —
fix it in the same change. Keep it current and short: no dates, commit
hashes, campaign names or bug stories; a pointer to a design doc or a
pin is enough.

`AGENTS.md` is generated, never edited:
`cat ~/.claude/CLAUDE.md <(printf "\n") CLAUDE.md > AGENTS.md`.
Regenerate it whenever this file changes.

## What is Graphix?

A dataflow programming language for UIs and netidx network programming.
Programs compile to directed graphs: operations are nodes, edges are data
flow. The language is reactive at the language level — when a dependent
value changes the graph updates. Lexically scoped, expression-oriented,
statically typed with inference, structural types, parametric
polymorphism, algebraic data types, pattern matching, first-class
functions and closures.

## Project structure

Rust workspace:

- **graphix-compiler**: parser, compiler (`Expr` → node graph),
  typechecker, fusion/JIT. Entry point `compile()` in `lib.rs`.
- **graphix-rt**: the runtime that executes node graphs in a background
  task, driven through `GXHandle`; embedder extensions via `GXExt`.
- **graphix-package**: package manager (loading, vendoring, standalone
  builds). **graphix-derive**: proc macros (`defpackage!`).
- **graphix-shell**: REPL and CLI; the binary is `graphix`.
- **graphix-fuzz**: the differential fuzzer (`design/graphix_fuzz.md`).
- `stdlib/`: one crate per package — `core`, `array`, `map`, `str`, `re`,
  `rand`, `sys` (streams, fs, tcp, tls, netidx, timers, processes),
  `http`, `toml`, `xls`, `pack`, `tui` (ratatui), `gui` (iced); Rust in
  `src/`, Graphix in `src/graphix/*.gx`. `graphix-tests` holds the
  language and stdlib integration fixtures (a separate crate to avoid
  circular dev-deps).
- `book/`: mdbook source; `book/src/examples/` (symlinked as
  `examples/`) holds every example program (`tui/`, `gui/`, `net/`).
  `docs/` is BUILD OUTPUT — edit `book/`, then from `book/`:
  `mdbook build -d ../docs/book`.
- `design/`: design as built. `../netidx/` is expected as a sibling
  checkout; the compiler and runtime depend only on netidx's VALUE layer
  (`netidx-core`/`netidx-value`), the networking crates appear only in
  stdlib packages (`design/netidx_extraction.md`).

Workspace-level dependencies where possible; `poolshark` pools wherever
allocation can be avoided, `smallvec` where it cannot.

## Building and testing

Builds go to `~/tmp/target` (tmpfs; centrally configured — never build
elsewhere). Dev profile is `opt-level = "s"`, no debug info; release is
`opt-level = 3`, LTO, one codegen unit. Do not build release unless you
must.

```bash
cargo build                              # debug
cargo build -p graphix-shell             # one crate
cargo test                               # THE gate: whole workspace, from the root
cargo test -p graphix-tests              # one crate while iterating
cargo test --workspace --features slow-tests   # the release gate
cargo run --bin graphix -- file.gx       # run
cargo run --bin graphix -- --check file.gx     # compile + typecheck only
cargo run --bin graphix -- --expand file.gx    # check + print each seq's lowered machine
```

Tests run in parallel by design (the compiler supports many instances
per process); never rely on `--test-threads=1`. `rustfmt --edition 2024
<files>` scoped to the files you edited (`cargo fmt` follows `mod`s into
drifted files). Formatting: `rustfmt.toml`; `snake_case` / `CamelCase` /
`SCREAMING_SNAKE_CASE`; Rust edition 2024; `triomphe::Arc` unless a
`Weak` or a cycle is needed.

**slow-tests.** Tests that are slow AND cover something that rarely
moves (package builds, stack-depth guards, a network download) are
`#[cfg_attr(not(feature = "slow-tests"), ignore = "slow-tests")]`; a
plain `cargo test` skips them (they show as ignored), the release gate
runs them. Never gate a language-semantics test. A test that re-executes
its own binary must pass `--include-ignored` to the child.

**Tests exist to find bugs**, not to pass. A failure is the good case:
find out why. Never work around a failure in a test that should pass; an
off-topic failure is discussed with Eric before it is fixed.

## Architecture

**Pipeline.** Parse (`graphix-compiler/src/expr/parser/`) → `Expr` AST
with positions → compile (`node/compiler.rs`) → `Node<R, E>` graph →
typecheck (two passes, `typecheck0`/`typecheck1` on every node) → fuse
(`fusion/`, when enabled). `typecheck0` also builds `ctx.bind_to_lambda`;
`CallSite::typecheck1` pre-binds statically resolvable calls and
pre-materializes HOF callbacks.

Key types: `Expr`/`ExprKind` (immutable AST; `Expr::for_each_child` /
`map_children` are the ONE child enumeration — `fold`, the seq rewrite and
the fuzzer's preorder all ride them, so a new `ExprKind` child is added
there and nowhere else), `Node<R, E>` (a newtype over `Box<dyn Update>`;
construct with `Node::new`), `ExecCtx<R, E>` (builtins, env, runtime),
`Scope` (lexical `ModPath` + `DynScope`, the chain of error handlers a
`?` sees, following the CALL chain).

**Nodes** implement `Update` (regular nodes) or `Apply` (function
applications called by `CallSite`). `Update` requires `update`,
`delete`, `typecheck0/1`, `refs`, `sleep` (unselected arms pause), plus
`emit_clif`/`fuse` for fusion. When writing a node: store the spec
(`Arc<Expr>`) for errors, track bind ids with `Refs`, call
`ctx.set_var()` for variable writes. `wrap!(node, result)` adds
expression context to errors; `err!`/`errf!` build error values.

**Runtime.** `graphix-rt` implements `Rt`: variables, timers, spawned
tasks and watch channels (`spawn`, `spawn_var`, `watch`, `watch_var`)
that packages use to feed external events in. Event processing is
batched: all simultaneous events form one `Event` delivered in one
cycle; several writes to one variable in a cycle queue for the next.

**Session images** (`design/program_image.md`): the shell caches the
session state before any cycle (`graphix-compiler/src/image/`) under
`$XDG_CACHE_HOME/graphix/registration/<build-id>/<key>.img`: the
registration entry (the package root compiled; key = image format +
root; the build id covers the packages compiled in) and, for a script,
the program entry (the program compiled too; key adds the program
source). A warm start maps the program entry, else the registration
entry and compiles the program, else compiles both; a missing entry is
written from the runtime that compiled it. `--no-cache` disables the
cache, `--warm` writes and exits; the program key carries the compile
flags and the header the ISA. A script compiles at runtime
construction (`GXConfig::program`, `GXHandle::program`), never through
`load`. The package root compiles with fusion off. A fused region
travels as its wrapper's `BodyRecord` (`fusion/emit/record.rs`):
machine code, relocations by symbolic target and constants by recipe;
every JIT function is installed from its record through
`define_function_bytes`, cold and warm alike, and every process
address the code refers to is an imported data symbol (`BodyCx::
const_ptr`), never an immediate. A restored kernel enters no cache.
Every
imaged node kind owns an `Update::image_encode` / `image_decode` pair
in its own file; a kind without one fails the write (`image::NOT_IMAGED`,
logged) and the shell runs cold, never a partial image. Every shared
object (expressions, types, function types, origins, paths, handlers,
resolution cells, type variables, map nodes) is written once and
referenced by its file offset; a reference to an object not built yet
decodes it from there, so any part of the image decodes in any order.
Types and function types are keyed by their canonical bytes with every
shared leaf by identity (`Type::content_key`), so equal types decode to
one value; an object whose definition is in progress writes a nested
occurrence of itself as a definition (`image::ContentState`). An
expression is keyed by the first expression seen with its id and
contents (`image::expr_key`), so a node's spec shares its def body's
definition.
The writer's `ImageBuf` reports every byte to the session, and
everything a session encodes must be borrowed from the context and
root nodes for the whole session. A program image writes instance
bodies to a heap after the eager part with an instance table; a call
site keeps `Callee::Imaged` (id, resolved type, reference summary) and
decodes the body on its first dispatch, synchronously, through
`ExecCtx::image_decoder`; a builtin callee is rebuilt at decode by
static resolution's factory call with its typecheck passes replayed.
Compiler ids are `image_id!` (the compiler's `atomic_id!` plus
relocation); netidx's ids and wire format are untouched. A definition
built by Rust at runtime is `DefOrigin::Runtime` and is never imaged.
Pins: `stdlib/graphix-tests/src/lang/image.rs`.

**Module loading** is the `ModuleResolver` trait (`expr/resolver.rs`);
`VfsResolver`/`FilesResolver` are in-core, `NetidxResolver` is in
`graphix-package-sys`. `sys::net` owns its netidx through `NetState` in
`ctx.libstate`; `NetHandles` shares the raw publisher/subscriber between
the loader and `NetState`; unseeded contexts get a process-internal
netidx on demand, so programs that never touch `sys::net` have zero
network. The shell library is netidx-agnostic; the CLI is the
netidx-aware embedder (`ShellBuilder::setup_context`,
`resolver_factories`, `GXHandle::with_ctx`).

**Types** (`graphix-compiler/src/typ/`): `Type` (structural),
`TVar` (inference cells; `design/tvar_constraints.md`), `FnType`.
`contains` expands `Type::Ref` through `lookup_ref`, so bindings made
during `contains` hold the EXPANDED form — code inspecting resolved types
handles both. `TypeRef` carries a write-once resolution cell
(`design/env_independent_typerefs.md`): rebuilds share it via
`with_params`, `with_scope` makes a fresh one, never overwrite a filled
cell; `Env::seed_typedef_refs` runs right before fusion in both modes.
Format type variables with `format_with_flags(PrintFlag::DerefTVars, ..)`.

**Two-phase typecheck knot.** While an instance body typechecks, its
def is in `ctx.resolving_lambdas` (a stack per def); a site reaching the
def with the same `FnArgIdentity` (per argument, the SOURCE lambda it
resolves to) is a self-call and reuses the instance; a different
identity is a fresh instance even mid-resolution (a HOF nested under its
own callback is not recursion). An instantiation snapshots its def's
`LambdaIds`. Never special-case collection intrinsics here.

**Builtins** implement `BuiltIn<R, E>` (`NAME`, `init()`, `EFFECT`) and
register with `ExecCtx::register_builtin::<T>()`; the Graphix signature
lives in the package's `.gx` with every argument and the return type
annotated. `EFFECT` is the one classification (`effects.rs`):
`Async` (may produce later, autonomously, or never), `Sync` (same cycle
but keeps cross-invocation state, or depends on WHICH args arrived), or
`Stateless(Option<FastCall>)` (a pure function of its args; the payload
is the direct-call entry the JIT uses, `Plain` or `Typed` by the site's
resolved return type; `None` for effects and partial-delivery
producers). A wrong `Stateless` is a semantics bug (the tail-loop
collapse shares state across iterations); a wrong `Sync` only costs the
loop. Bottom never reaches builtin authors: a bottomed arg bottoms the
invocation before `eval`; raw `Apply` authors read args through
`seam_arg`/`seam_tick`/`seam_value`. Configuration a fast fn derives
from its args (a regex, a template registry) lives in a bounded
thread-local `FastMemo`, never in state.

**Collection intrinsics** (`node/collection.rs`,
`design/collection_intrinsics.md`): the Array/List/Map traversal HOFs
are compiler nodes (`MapQ`/`FoldQ`) built when a lambda body is a
reserved marker name (`'array_map`, …); they own callback
instantiation, slot identity, per-slot firing/taint/sleep and result
construction.

## The semantics both engines implement

The node-walk (`node/*.rs`) is the canonical evaluator and the universal
fallback; fusion → cranelift (`fusion/`) compiles pure sync subtrees to
native kernels, splicing on success and leaving the nodes on failure.
**A fusion bug may lose fusion, never produce a wrong answer**; the
fuzzer enforces bit-for-bit agreement, and a divergence is adjudicated
against the INTENDED semantics, never by trusting either engine. The
node graph IS the IR — there is no parallel typed IR
(`design/final_jit_architecture.md`, `design/distributed_jit.md`).

- **Strict fusion** (`design/strict_fusion.md`): fusion admits pure
  computation only. A builtin fuses iff its `Effect::Stateless` carries a
  `FastCall`; `?` fuses with or without a covering catch (a handler-ful
  raise is queued and delivered after the run through the same path
  `Qop` uses); everything else — stateful/effectful builtins, `connect`,
  `~`, `Any`, `Catch` — node-walks, transitively. A kernel's only
  cross-invocation memory is the firing boundary (prev-length words,
  first-call words, per-site/per-activation blocks); no replay caches,
  no selection memory. The runtime loans a kernel exactly `KERNEL_ABORT`,
  `KERNEL_ENV`, `QOP_RAISES` and the core-trait value hooks.
  `#[native]` asserts zero node-walk residue at a source location and is
  THE advertised performance model; `#[sync]`/`#[async]`/
  `#[tail_recursive]` assert analysis facts.
- **Bottom is dense** (`design/dense_delivery.md`,
  `design/representable_bottom.md`): `update` returns a `TagValue`
  every cycle — `Fired(v)`/`Stale(v)`/`FreshBottom`/`StaleBottom`, the
  orthogonal fired×bottom algebra. A standing bottom re-delivers
  `StaleBottom` and never re-fires consumers; bottomness ORs over
  consumed productions. In the JIT the bits ride each param's disc.
- **Organic firing** (`design/organic_firing.md`): a node fires iff a
  consumed input fires; nothing stores a previous value or selection to
  decide a tag; `uniq`/`filter`/`~` are the cadence tools. A select emits
  per fired input — scrutinee delivery, a CONSULTED guard, or the taken
  arm's own production; same-arm re-matches emit the arm's current
  value. Constants fire at init (and at an arm's wake). Kernel outputs
  fire only when an input feeding them fired; collection loops fire on
  resize, a fired slot, a fired empty source, or a fired fold carry.
- **Bottom scrutinee ⇒ bottom select.** No stored-selection ride of any
  kind; `hold` on the scrutinee is the tool. A STALE-PRESENT scrutinee
  still routes the taken arm's own fires. **Consulted-guard rule**: arms
  are consulted top-down, structure then guard; a consulted guard whose
  channel is bottom makes the selection undecidable; a never-produced
  guard is unknown, not false. `&&`/`||` are strict (`false && ⊥ = ⊥`).
- **Sleep is pause, not reset.** Value-channel state survives an arm's
  sleep (`Held` residents at the select scrutinee, pattern guard and
  `~`'s arg; `CachedVals` staging; collection slots; a `<-` target's
  value). **Wake catch-up** (`design/wake_catchup.md`): a reselected arm
  recomputes from the world as it stands, reading standing values STALE;
  the only events it re-raises are the fires no selected reader saw,
  once, at their current value (one fire bit per arm-body input per
  select, consumed by whichever arm reads it; pattern binds and a
  destructuring let's siblings are facets of one input). Sleep state is
  LOCAL: every skip-owning node owns a `slept` bit its `sleep()` sets and
  its next update takes — no ExecCtx globals (parallel compile and a
  parallel evaluator stay possible). The restart builtins
  (`once`/`take`/`skip`/`uniq`/`hold`/`count`) clear in their own
  `sleep()`. A labeled DEFAULT is born with the binding and delivers
  FIRED at a fresh callee's first dispatch. Async builtins clear their
  output on sleep (`design/async_sleep_outputs.md`). A pure non-recursive
  arm skips `sleep` and is not updated while untaken.
- **Activation state** (`design/activation_state.md`,
  `design/recursive_activations.md`, `design/atomic_recursion.md`):
  held state never decides output bottomness; activations ARE
  collection slots; non-tail recursion is an activation per level, a
  STATELESS tail loop is one activation; instances are retained
  unconditionally; shrink = delete (a depth not reached this cycle is
  deleted; re-reaching it is fresh). No depth limit; evaluation is
  atomic within a cycle; containment is the cooperative interrupt
  (`GXHandle::interrupt`, Ctrl-C, `GRAPHIX_STACK_BUDGET`). Kernel
  interior memory (`design/kernel_instance_state.md`) gives one compiled
  body the interp's per-slot/per-activation multiplicity for exactly the
  state that decides firing; the QUIET FLAG (a framed pass on a non-init
  cycle) is not an init view; only a site's first-ever dispatch is.
- **`let rec` is monomorphic-recursive**; union collapse requires strict
  tvar identity; a free union member stays free (annotate a select whose
  arms are `'b` and `i64`); float comparison is a total order (`NaN ==
  NaN`, below every number) so `Value` is map-key-able; checked arith
  (`+?` …) yields a catchable `ArithError`, unchecked wraps, integer
  div0 bottoms; indexing is bounds-checked through shared helpers on
  both backends; swallowed-error diagnostics are node-walk-only (debug
  with `--no-fusion`).
- **Emit contracts** (`design/distributed_jit.md`): effects de-fuse,
  never silently skip; owned select-arm binds drop at every arm exit
  (run `leakcheck` when adding an owned-local class); a bottom is a
  production whose STALE bit follows the same trigger fold as a value
  (`nodes::emit_bottom_placeholder` takes the governing discs); kernel
  cache keys carry catch coverage and a resolution fingerprint; a pass
  the fusion gate owns must never change what the typechecker sees.
- **JIT memory**: one JITModule + 256MB arena per ExecCtx; on exhaustion
  the module retires whole and the region rebuilds in a fresh one; the
  reclamation unit is the ExecCtx. Kernel ABI: kind-grouped params from
  `KernelSig::abi_params`; recursive types and abstract types are opaque
  2-word values (`design/unified_value_abi.md`).

Coverage today: scalar arithmetic/comparison/logic/casts, producers and
accessors, `?`/`$`, the eight array HOFs as native loops (nesting
included), structural select destructuring with scalar and variant
payload binds, `never()` arms as bottom productions of the merge
shape, or-patterns, list patterns, tail loops over any kernel param
kind, every fast-fn builtin and non-inline cast, cross-kernel lambda
calls, trait default bodies. Fusion descends through
Module/Block/Bind/CallSite/Catch/Lambda/Select/ExplicitParens; not
through `~`, `<-` or operator operands. `FusionStats.failed` is a
blocker profile, not a gap count.

## Testing is differential

- `run!` (`graphix-package-core/src/testing.rs`) runs a fixture in
  `interp` and `jit` modes asserting equal values; `FuseExpect::{Jit,
  None}` asserts WHETHER it fuses, bidirectionally.
  `GRAPHIX_FUSE_AUDIT=1 cargo test -- jit --nocapture` prints the audit.
- **graphix-fuzz** (`design/graphix_fuzz.md`): node-walk vs JIT with a
  per-cycle trace oracle; `check`/`run`/`generate`/`fuzz`/`minimize`/
  `regress`/`selfcheck`/`gen-check`/`detcheck`/`typemorph`. The
  committed `findings/` corpus is the regression gate. `rand::`/`sys::`/
  `http::`/`hold(` programs are excluded from divergence recording.
  Soaks run under `nice -n 19` from a campaign-private copy of the
  binary with output outside the repo; `graphix-fuzz/fleet.sh` deploys.
  A stack-budget abort is a `Timeout` outcome (containment).
- A semantics change is not landed until it has soaked; gates are not
  the fuzzer.

## Language features (current)

- **Sampling**: `e ~ v` is `v` at each fire of `e`, banking a trigger
  that finds `v` absent; `e ~! v` is strict (bottom, no bank). A connect
  writes when its RHS fires; a constant RHS in an arm fires once when the
  arm becomes selected (the "on entering this state" write); a handler
  that must act on every event samples it. Both are tools, not lints.
- **`never<T>()` is syntax** (`ExprKind::Never`): typed bottom or `T`;
  args stay live and are consumed. An unannotated `let` over a ⊥
  initializer takes its type from its writers.
- **Sets and coverage**: select exhaustiveness is enforced; slice-pattern
  length ladders count as coverage; bool literals pool per position
  inside composite patterns; set coverage distributes over product
  heads (`` [`P(A), `P(B)] ⊇ `P([A, B]) ``); a probe in progress for the
  same scrutinee ref claims nothing on re-entry.
- **Or-patterns** (`design/or_patterns.md`): select arms and bracketed
  element positions; alternatives bind the same names at exactly equal
  payload types; captures type as the union; one guard per arm; dead
  alternatives are errors; they fuse natively.
- **Native List** (`design/list_native.md`): `List<'a>` is a compiler
  constructor like `Array`; `[<1, 2>]` literals and `[<h, rest..>]`
  patterns (rest is the O(1) tail; the suffix form is refused); the rep
  is private to `node/collection.rs::list`.
- **Nominal abstract types** (`design/nominal_abstract_types.md`):
  `type T = Abstract<rep>`; `T(v)`, `x.0`, pattern `T(p)` only where the
  definition is visible; `T as t` is a nominal tag test anywhere.
- **Traits v1** (`design/traits.md`): static dispatch on the self
  argument's type; a union self lowers to a select; impls are global
  facts; core `Eq`/`Ord`/`Display` ride the value (map keys, sort,
  operators, printers, both engines). The io traits `Read`/`Lines`/
  `Write`/`Close`/`Seek`/`Socket` over five stream types.
- **Module system** (`design/module_system.md`): Rust-2018-style
  `use`; every name arrives by declaration, `use`, or prelude;
  `self`/`super`/`package` roots; declarations are statement-position
  only.
- **Place references** (`design/place_references.md`): `&a[i]`,
  `&s.f`, `&t.0`, `&m{k}` are root + path; writes patch the root at
  delivery; a dynamic key is a moving reference. References de-fuse.
- **`catch`** (`design/catch.md`) installs a handler for the rest of its
  block; it is not control flow.
- **`seq` / `seqq`** (`design/seq_blocks.md`): `seq [trigger] { stmt* }`
  desugars to a pc machine (one select arm per step, busy-drop, carried
  lets as cells, calls issued once per entry over an argument snapshot).
  `until`, `do { .. }`, `try { .. } with(e[: T]) { .. }` (the error
  branch; `catch` is refused in a seq body outside lambda literals).
  A step completes on a FIRED production after its entry, never on a
  standing value; a call-free step reads its level as it stands at
  entry. `seqq` queues triggers with captured values. `--expand` prints
  the machine. `range(i, j)` is the integer builtin (`` `RangeError ``).
- **Comments** are legal only above an expression, a select arm, an impl
  method or a struct-literal field; parse errors report the furthest
  point reached with the source line and a caret.

## Stack discipline

Nesting depth is attacker-controlled and overflow aborts, so it is
closed two ways: `crate::stack::ensure_sufficient` (stacker) wraps every
program-driven recursion — parser knots (`GrowStack`), `compile`,
`Display`, `fold`/`for_each_child`, type walks, pattern walks, seq
lowering, and the `Node`/`TVar`/`Expr` destructors (explicit teardown
inside the guard; `Type` is the one uncovered cycle, made unreachable by
the limit) — and `parser::DEFAULT_MAX_NESTING` (counted in parser knots;
iterative loops that fold into nested ASTs are capped at the fold).
Refusals set a thread-local (`note_refused`) because combine merges
messages. Pins: `graphix-compiler/tests/deep_drop.rs`,
`graphix-shell/tests/deep_nesting.rs` (add a case for a new recursive
construct). netidx-value has the same treatment for bracket literals.

## Debugging

`TRACE` (`set_trace`, `with_trace(enable, spec, f)`, `tdbg!`) scopes
compiler tracing to one expression — the stdlib typechecks on every
compile, so unscoped prints are gigabytes.

| env var | prints |
|---|---|
| `GRAPHIX_DBG_BIND=1` | every tvar bind, impl lookup, top-level `contains` verdict |
| `GRAPHIX_DBG_KERNELS=1` | each lambda kernel built: name, return type, ABI, state/site words |
| `GRAPHIX_DBG_INVOKE=1` | each fused-kernel invocation with per-input fired/present |
| `GRAPHIX_DBG_REGION=1` / `_FREEZE=1` | fused-region input wiring / freeze outcomes |
| `GRAPHIX_DUMP_CLIF=1` | every kernel's CLIF (`u0:N` = helper registration order in `emit_helpers.rs`) |
| `GRAPHIX_DBG_VARS=1` | runtime variable events (ref/unref, set, same-cycle notify) — graphix-rt |
| `GRAPHIX_DBG_PERF=1` | interp lazy-bind phase counters every 250ms |
| `GRAPHIX_PROFILE=1` | nested compiler phase accounting per root (`bench/profile.py` reads it; `design/jit_startup.md`) |
| `GRAPHIX_PROFILE_INSTANCES=1` | with `GRAPHIX_PROFILE`, per-instance construction/check costs (`bench/instances.py`) |
| `GRAPHIX_DBG_TVAL=1` | typed-printer render steps |
| `GRAPHIX_DBG_CYCLE_BT=1` | a backtrace at every occurs-check refusal |
| `GXDBG_TAIL=1` | every tail-loop dispatch pass |
| `GXDBG_EFFECT=1` | why a lambda classified Async |
| `GXDBG_INSTANCE_FUSION=1` | per-instance region fusion passes |
| `GXDBG_CS=1` / `GXDBG_DYNC=1` | every CallSite dispatch and result tag / every fastcall trampoline dispatch |
| `GXDBG_TYPEREF=1` | scope table dump on an "undefined type" refusal |
| `GXDBG_LETBIND=1` / `GXDBG_REF=1` | let publication decisions / read misses |
| `GXDBG_SLOT=1` | per-slot production tags and the collection fold decision |
| `GXDBG_SHALLOW=1` | each select arm's shallow discriminator |
| `GXDBG_RESOLVE=1` | static-resolution reads and index writes |
| `GXDBG_RPC=1` | the sys::net rpc path (graphix-package-sys) |

Fusion bugs: write a triggering test before adversarial review; a hung
test is a result.

## Working conventions

- Code review uses `// CR <name> for <name>: text` near the code; when
  addressed it becomes `// XCR ...`; XCRs are deleted when resolved or
  turned back into CRs with an explanation.
- PRs carry a concise summary, testing notes and related issues. Rebuild
  the book when docs or examples change.
- Examples in `book/src/examples/` are documentation and test corpus at
  once; TUI/GUI examples are tested by hand
  (`cargo run --bin graphix -- examples/tui/barchart_basic.gx`). Some
  are snippets that reference undefined names on purpose; they must stay
  syntactically valid.
- A new compiler walk must name the loss without it before it is added;
  the typechecker must stay instant (measure the GUI suite after typing
  changes); predictable fusion is a core value — push on de-fuse corner
  cases rather than accept them.
- Hot operators log and bottom on failure; rare stdlib functions return a
  catchable `Error`.

## Stdlib notes

- `sys::process`: children live in the opaque `Proc` with weak polling
  and `kill_on_drop`; redirects are `Pipe`/`Inherit`/`Null`; the polling
  task is the sole reaper. Shell tests are Unix-gated with `cmd.exe`
  twins.
- GUI (iced): uses the iced sub-crates directly; `GuiTestHarness::dt()`
  downcasts; tests fire callbacks via `gx.call(callable_id, args)`;
  test contexts default to `NetConfig::Internal`; publisher coalescing
  collapses rapid updates — space them with timers.
- Package manager: `packages.toml` v2 (`[stdlib]` tracks the shell
  version, `[packages]` for externals); `update` presents a maskable
  change set and builds before writing the manifest; hard error on
  non-TTY without `--yes`.

## The admin-TUI dogfood campaign

netidx-admin's ratatui TUI is being rewritten in Graphix as
`graphix-package-netidx-admin` in the netidx repo (the first external
package). **The primary objective is finding and fixing Graphix
problems; the TUI is secondary.** No workarounds: an awkward idiom, slow
compile, bad diagnostic or missing capability means stop, log a
finding, fix it here (or consciously accept it), then continue — never
move decision or presentation logic into the package's Rust layer
because Graphix was painful. Design and the open-items ledger:
`../netidx/design/graphix-admin.md`, `graphix-admin-findings.md`.
Measure `--check` time at every size milestone. Run its tests
(`cd ../netidx && cargo test -p graphix-package-netidx-admin`) after any
change to seq or select semantics.
