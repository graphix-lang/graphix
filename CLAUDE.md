# CLAUDE.md

Guidance for Claude Code in this repository. It is loaded into every
session and every agent, so it holds only what you must know before you
touch the code: how to build and test, the rules that bite, and where the
long form lives. **A rule's detail goes in its design doc
(`design/README.md` is the index), not here.** Add a line here only where
someone working elsewhere would otherwise get it wrong, and keep the file
under 40 KB. History lives in `git log`. No dates, commit hashes, campaign
names or bug stories. When a rule here disagrees with the tree, the tree
wins and this file is stale: fix it in the same change.

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

- **graphix-types**: the static half: syntax (parser, AST, printer,
  formatter, module resolution), types and their checker, the
  environment, the image session core. It reaches nothing of the
  compiler and carries no cranelift; `graphix-ast-pack` (every package's
  build-dependency) depends on it alone.
- **graphix-compiler**: compiler (`Expr` → node graph), elaboration,
  fusion/JIT; re-exports graphix-types' modules (`graphix_compiler::
  {expr, typ, env}`). Entry point `compile()` in `lib.rs`.
- **graphix-rt**: the runtime that executes node graphs on a thread of
  its own, driven through `GXHandle`; embedder extensions via `GXExt`.
- **graphix-package**: package manager (loading, vendoring, standalone
  builds). **graphix-derive**: proc macros (`defpackage!`).
- **graphix-shell**: REPL and CLI; the binary is `graphix`.
- **graphix-lsp**: the language server's protocol loop and queries; the
  shell's `lsp_backend.rs` is the backend (`graphix lsp`).
- **graphix-fuzz**: the differential fuzzer (`design/graphix_fuzz.md`).
- `stdlib/`: one crate per package — `core`, `array`, `list`, `map`,
  `str`, `re`, `rand`, `args`, `sys` (streams, fs, tcp, tls, netidx,
  timers, processes), `http`, `json`, `toml`, `xls`, `pack`, `hbs`, `db`,
  `sqlite`, `bench`, `tui` (ratatui), `gui` (iced); Rust in
  `src/`, Graphix in `src/graphix/*.gx`. `graphix-tests` holds the
  language and stdlib integration fixtures (a separate crate to avoid
  circular dev-deps).
- `book/`: mdbook source; `book/src/examples/` (symlinked as
  `examples/`) holds every example program (`tui/`, `gui/`, `net/`,
  `collection/`, `stdlib/`). `ide/` holds the editor support, the
  tree-sitter grammar (`ide/tree-sitter-graphix`) a workspace member.
  `docs/` is BUILD OUTPUT — edit `book/`, then from `book/`:
  `mdbook build -d ../docs/book`.
- `design/`: design as built. `../netidx/` is expected as a sibling
  checkout; the compiler and runtime depend only on netidx's VALUE layer
  (`netidx-core`/`netidx-value`), the networking crates appear only in
  stdlib packages (`design/netidx_extraction.md`).

Workspace-level dependencies where possible; `poolshark` pools wherever
allocation can be avoided, `smallvec` where it cannot.

## Building and testing

Builds go to `~/tmp/target` (centrally configured — never build
elsewhere). Dev profile is `opt-level = "s"`, no debug info; release is
`opt-level = 3`, LTO, one codegen unit, stripped; `quick` is release
with thin LTO and 16 codegen units: a third of the compile time, nearly
the speed. Use `--profile quick` (`~/tmp/target/quick/`) wherever a
release-like binary will do; build release only when you must. Dev is
the fastest way through a full `cargo test`.

```bash
cargo build                              # debug
cargo build -p graphix-shell             # one crate
cargo build --profile quick -p graphix-shell   # near-release speed, 1/3 the build
cargo xtest                              # THE gate: every test binary side by side (xtask/)
cargo test                               # the same tests, one binary at a time
cargo test -p graphix-tests              # one crate while iterating
cargo xtest --features slow-tests        # the release gate
cargo run --bin graphix -- file.gx       # run
cargo run --bin graphix -- --check file.gx     # compile + typecheck only
cargo run --bin graphix -- --expand file.gx    # check + print each seq's lowered machine
cargo run --bin graphix -- fmt file.gx         # format in place (--check, --stdout; stdin when no file)
```

Tests run in parallel by design (the compiler supports many instances
per process); never rely on `--test-threads=1`. `cargo xtest` runs the
test binaries side by side too, so a test passes under the whole suite's
load: one that fails there is fixed so each event follows from the one
before it, never serialized or given a longer wait. The whole workspace is
formatted: `cargo fmt --all` before a commit, stable toolchain.
Formatting: `rustfmt.toml`; `snake_case` / `CamelCase` /
`SCREAMING_SNAKE_CASE`; Rust edition 2024; `triomphe::Arc` unless a
`Weak` or a cycle is needed.

**slow-tests.** Tests that are slow AND cover something that rarely
moves (package builds, stack-depth guards, a network download) are
`#[cfg_attr(not(feature = "slow-tests"), ignore = "slow-tests")]`; a
plain `cargo xtest` skips them (they show as ignored), the release gate
runs them. Never gate a language-semantics test. A test that re-executes
its own binary must pass `--include-ignored` to the child.

**Integration tests share a binary.** graphix-shell's, graphix-rt's and
graphix-tests' integration tests are one target each
(`tests/shell/main.rs`, `tests/rt/main.rs`, `tests/shared/main.rs`, a
module per file), so they run side by side; a test that needs a process
of its own (it sets process-wide state: an env var, a panic hook, the
global thread pool; it re-executes its binary; it reads a child's peak
RSS, which counts the parent's at the fork; it times itself against the
wall clock) is a `tests/*.rs` target beside them. A new test file goes
into the shared one unless it needs its own.

**Tests exist to find bugs**, not to pass. A failure is the good case:
find out why. Never work around a failure in a test that should pass; an
off-topic failure is discussed with Eric before it is fixed.

### Testing is differential

- `run!` (`graphix-package-core/src/testing.rs`) runs a fixture in each
  `testing::Mode`: `interp` and `jit`, and each again with every fork
  point forked (`par`, `jit_par`). Nothing compares the modes, so a
  predicate pins the value exactly and a refusal names its message
  (`testing::refused`). `interp` also fails unless the check alone refuses
  exactly when the build does. `FuseExpect::{Jit, None}` asserts WHETHER
  anything fuses, both ways; `#[native]` asserts that an expression does.
  Details: `design/testing.md`.
- **graphix-fuzz** (`design/graphix_fuzz.md`): node-walk vs JIT with a
  per-cycle trace oracle, cold and warm images, forked against serial, and
  check against build. Its `findings/` corpus is the regression gate:
  `regress` runs every pin and compares its fused-region count and verdict
  with `fusecheck.manifest` and `outcome.manifest`; after an intended
  change, `fusecheck --bless` and `regress --bless` rewrite them (rebuild
  to embed) and the diff is reviewed like code. Typemorph's must-reject
  families each cite a checker rule (`design/must_reject.md`): a rule
  change updates its family in the same commit.
- Soaks run under `nice -n 19` from a campaign-private copy of the binary,
  deployed by `graphix-fuzz/fleet.sh`.
- A semantics change is not landed until it has soaked; gates are not
  the fuzzer.

## Architecture

The long form is `design/architecture.md`, and each subsystem's own doc.

**Pipeline.** Parse (`graphix-types/src/expr/parser/`, recursive descent
by hand) → `Expr` AST with positions → compile (`node/compiler.rs`) →
`Node<R>` graph → typecheck (`typecheck0`/`typecheck1` on every node) →
fuse (`fusion/`). The check is typecheck0 plus the settle of every call
site it recorded, drained before typecheck1 elaborates, and `--check` and
the language server run it alone. **Elaboration refusing what the check
accepted is a type-system bug** (`GRAPHIX_ELAB_AUDIT` reports it). An
instance does not check again: its types are its definition's check's,
substituted by its signature (`node::lambda::DefTable`,
`design/parallel_compile.md`). A top-level input (a REPL input, a script,
a dynamic module's load) is checked and run alone; what an earlier input
left open is one type to a later input, whose decision about it is refused
(`design/tvar_constraints.md`, Settling).

**Key types.** `Expr`/`ExprKind` (immutable AST; `Expr::for_each_child` /
`map_children` are the ONE child enumeration, so a new `ExprKind` child is
added there and nowhere else), `Node<R>` (construct with `Node::new`),
`ExecState<R>` (what an embedder owns), `ExecCtx<'a, R>` (the view
`update`, `delete`, `sleep` and image decode take), `CompileCtx` (what
compile, `typecheck0`/`1`, fusion and `BuiltIn::init` take), `Scope`
(lexical `ModPath` + `DynScope`, the chain of error handlers a `?` sees,
following the CALL chain).

**Nodes** implement `Update` (regular nodes) or `Apply` (function
applications called by `CallSite`). `Update` requires `update`,
`delete`, `typecheck0/1`, `refs`, `sleep` (unselected arms pause), plus
`emit_clif`/`fuse` for fusion. When writing a node: store the spec
(`Arc<Expr>`) for errors, track bind ids with `Refs`, call
`ctx.set_var()` for variable writes. `wrap!(node, result)` adds
expression context to errors; `err!`/`errf!` build error values.

**Compiling never reaches the runtime.** Compile, the checks, fusion and
`BuiltIn::init` take a `CompileCtx`; what they would ask of the runtime
they defer (`CompileCtx::record_ref`, `discard`, `discard_apply`,
`discard_stored`), and `ExecCtx::apply_deferred` does it. Every cycle
asserts (debug) that nothing is left.

**Runtime.** `graphix-rt` implements `Rt`: variables, timers, spawned
tasks and watch channels. Event processing is batched: all simultaneous
events form one `Event` delivered in one cycle; several writes to one
variable in a cycle queue for the next, except a level's
(`Rt::set_level`). A production is delivered through `ExecCtx::deliver`,
and a reader the walk has already passed takes a fire alone next cycle, so
none runs twice in a cycle (`design/dense_delivery.md`). Every write marks
its readers' boundaries (`design/walk_pruning.md`): publish through
`ExecCtx::publish`/`try_publish`, never `event.variables.insert` from a
node. A running program's failures are logged under
`graphix_compiler::FAILURE_TARGET`, never printed.

**Session images** (`design/program_image.md`): the shell caches the state
before any cycle under `$XDG_CACHE_HOME/graphix/registration/<build-id>/`;
`--no-cache` disables it, so pass it on every diagnostic run. Every imaged
node kind owns an `Update::image_encode`/`image_decode` pair in its own
file, and every `Apply` and `BuiltIn` a codec; a kind without one fails the
write and the shell runs cold, never a partial image. Pins:
`stdlib/graphix-tests/src/lang/image.rs`.

**Formatter** (`design/formatter.md`): parse → the one printer → reparse;
a result that differs is refused, never written. The AST stays canonical;
what the author chose rides beside it as metadata that decides nothing.
**Printing is canonical unless `PrintFlag::AsWritten` is set, and only the
formatter sets it**: printed types and expressions reach program-visible
values. After a printer change run the corpus harness over every
`.gx`/`.gxi` here and in `../netidx`: `cargo run -p graphix-compiler
--example gxfmt -- <files>`.

**Language server** (`design/language_server.md`): `--check` and the
server share one path (`GXRt::check`). A query resolves the cursor to a
target from the check's sinks, never by looking a word up. A parser site
that builds an `Expr` gives it its end (`Expr::ending`). Errors take
context by `.at(&spec)`, `wrap!` or `bailat!`, never `ErrorContext(..)` by
hand. Pins: `graphix-shell/tests/lsp/`.

**Types** (`graphix-types/src/typ/`, `design/tvar_constraints.md`): `Type`
(structural), `TVar` (inference cells), `FnType`. `contains` expands
`Type::Ref` through `lookup_ref`, so bindings made during `contains` hold
the EXPANDED form — code inspecting resolved types handles both. A
`TypeRef`'s resolution cell is write-once: never overwrite a filled one
(`design/env_independent_typerefs.md`). A typedef must be contractive and
an inferred type finite. Format type variables with
`format_with_flags(PrintFlag::DerefTVars, ..)`. Casts:
`design/abstract_cast.md`, `typ/cast.rs`.

**Builtins** (`design/builtins.md`) implement `BuiltIn<R>` (`NAME`,
`init()`, `EFFECT`) and register with `ExecCtx::register_builtin::<T>()`;
the signature lives in the package's `.gx` with every argument and the
return type annotated. `EFFECT` is the one classification (`effects.rs`):
`Async`, `Sync`, `Stateless(Option<FastCall>)` or `Io(Option<IoCall>)`. A
wrong `Stateless` is a semantics bug. Bottom never reaches builtin authors.
Raw `Apply` authors read args through `seam_arg`/`seam_tick`/`seam_value`.

**Collection intrinsics** (`node/collection.rs`,
`design/collection_intrinsics.md`): the Array/List/Map traversal HOFs are
compiler nodes built when a lambda body is a reserved marker name
(`'array_map`, …). Never special-case them in the two-phase typecheck knot
(`design/architecture.md`).

**Stack discipline** (`design/architecture.md`): nesting depth is
attacker-controlled and overflow aborts, so every program-driven recursion
is wrapped in `crate::stack::ensure_sufficient`, and
`parser::DEFAULT_MAX_NESTING` bounds AST depth. Add a case to
`graphix-shell/tests/deep_nesting.rs` for a new recursive construct.

**Parallel evaluation** (`design/parallel_eval.md`): a cycle's update pass
forks independent subtrees and merges them back in program order, so a
forked cycle computes what the serial node-walk does. The compiler never
pins threads. `GRAPHIX_PAR=off|auto|force`.

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
  computation only, and the `Effect::Io` calls gated on their trigger's
  fire. Everything else — stateful/effectful builtins, `connect`, `~`,
  `Any`, `Catch` — node-walks, transitively. `#[native]` asserts zero
  node-walk residue at a source location and is THE advertised performance
  model. `#[sync]`/`#[async]`/`#[pure]` assert analysis facts,
  `#[tail_recursive]` the loop fusion built. What fuses today is listed
  there too.
- **Bottom is dense** (`design/dense_delivery.md`,
  `design/representable_bottom.md`): `update` returns a `TagValue` every
  cycle — `Fired(v)`/`Stale(v)`/`FreshBottom`/`StaleBottom`. A standing
  bottom re-delivers `StaleBottom` and never re-fires consumers; bottomness
  ORs over consumed productions.
- **Organic firing** (`design/organic_firing.md`): a node fires iff a
  consumed input fires; nothing stores a previous value or selection to
  decide a tag; `uniq`/`filter`/`~` are the cadence tools. A constant fires
  where it is born and stands stale everywhere else.
- **Bottom scrutinee ⇒ bottom select.** No stored-selection ride of any
  kind; `hold` on the scrutinee is the tool. Arms are consulted top-down,
  structure then guard; a consulted guard whose channel is bottom makes the
  selection undecidable. `&&`/`||` are strict (`false && ⊥ = ⊥`).
- **Sleep is pause, not reset; a wake is a resume, never a rebirth**
  (`design/wake_catchup.md`). State survives an arm's sleep; a reselected
  arm recomputes from the world as it stands, reading standing values
  STALE, and re-raises only the fires no selected reader saw, once. Sleep
  state is LOCAL: every skip-owning node owns a `slept` bit, no ExecCtx
  globals. Sleep aborts an async call in flight (one not cancel-safe
  finishes, unanswered); the wake restarts nothing
  (`design/async_sleep_outputs.md`). A seq machine's run is aborted by its
  arm's sleep.
- **Activation state** (`design/activation_state.md`,
  `design/recursive_activations.md`): every call is an activation, tail or
  not; the JIT runs a STATELESS tail recursion as one native loop held to
  the activations' answers (`design/tail_calls_are_calls.md`). No depth
  limit; containment stops the runtime for good
  (`design/atomic_recursion.md`). A function value keeps the bindings it
  captures alive.
- **Emit contracts** (`design/distributed_jit.md`, `design/jit_memory.md`):
  effects de-fuse, never silently skip; owned select-arm binds drop at every
  arm exit (run `leakcheck` when adding an owned-local class); an owned
  local's words are defined only through `ValueVar::define`; a pass the
  fusion gate owns must never change what the typechecker sees. A link that
  fails is a JIT bug and panics. No JIT code runs outside a cycle's update
  pass. Fusion bugs: write a triggering test before adversarial review; a
  hung test is a result.

### Typing rules that bite

The long form is `design/tvar_constraints.md`; the rules below are the ones
a change elsewhere most often breaks.

- **`let rec` is monomorphic-recursive, and a binding something writes**
  (a `<-` target, a `&mut`'s referent) **is not generalized** (the value
  restriction, `CompileCtx::monomorphic`). A function stays generic only
  where a binding names it (`bind::names_generic`): a lambda or a read of a
  generalized binding no binding names is one instance.
- **A def's declared tvars are rigid in its body check**: none binds to a
  concrete type or ⊥, no two unify, none escapes into a cell outside the
  definition. A call instantiates them freely.
- A written function type holds no union of two or more of its own bare
  variables (`['a, 'b]`, `Type::variable_union`), and a quantified function
  type stands only as a declared signature's top (`typ::QUANTIFIED_FN`).
- **Widening:** a call's variable that only data positions hold settles to
  the widest of the decided arguments and omitted defaults, whatever the
  order; an undecided argument waits for that join and is refused, "annotate
  it", where the join does not hold it (`callsite.rs::Widening`).
- A merge takes the lower of its two cells' levels (`TVar::merge_one`), so a
  relation against a scheme instantiates it first.
- **Sound by construction:** a hole in the type system is closed by a local
  rule that refuses and names the annotation that makes the program pass
  (pin both), never by a late check chasing shapes; where the right type is
  local and cheap, it is given instead. A `--check` census of both repos
  sizes the rule: one that needs a new annotation in the stdlib, the book's
  examples or netidx-admin goes to Eric first; test fixtures alone are
  rewritten.

## Language features

The reference is the `graphix-lang` skill (`ide/skills/graphix-lang/
SKILL.md`, load it before writing Graphix) and the book; the rules as built
are in the docs named here.

- **Select**: exhaustive, no dead arms; one walk decides what reaches each
  arm (`select.rs::Reach`, `design/select_coverage.md`, with patterns inside
  a declared variable and `name@` captures). A type still undecided at a
  select's end is one unknown type, equal only to itself: an arm testing it
  is refused, naming the binding to annotate. Or-patterns:
  `design/or_patterns.md`.
- **Sampling**: `e ~ v` is `v` at each fire of `e`, banking one trigger;
  `e ~! v` is strict. A connect writes when its RHS fires; a constant RHS in
  an arm fires only at the arm's first entry
  (`design/organic_firing.md`). `never<T>()` is syntax: typed bottom.
- **`seq` / `seqq`** (`design/seq_blocks.md`): a machine node
  (`node/seq_machine.rs`), every statement a step; a step sees the world as
  it stood at its entry (`seqq`: at enqueue); `live(e)` reads live. Every
  name a lowering binds starts with `#` (`is_lowered`).
- **References** (`design/place_references.md`): `&e` is read-only and
  covariant, `&mut e` writable and invariant; references de-fuse.
- **Composite forms** (`graphix-types/src/rep.rs`,
  `design/distinct_composite_forms.md`): a tuple, struct, variant and list
  cell is an array whose element 0 is its kind's marker, an array is
  unmarked; only `rep.rs` and `list.rs` know the layout. No select arm may
  tell apart two types that share a runtime form (`Type::rep_collision`).
- **Operators** (`expr/binop.rs`, `design/tvar_constraints.md`): operands of
  EXACTLY one type; arithmetic takes `'a: Number + Singleton`, the orderings
  `Ordered`, comparison `Discernible`. Unchecked `+ - *` wrap, integer div0
  bottoms, checked forms (`+?`) yield a catchable error, floats follow IEEE.
- **Types and modules**: nominal abstract types
  (`design/nominal_abstract_types.md`), traits (`design/traits.md`), native
  `List<'a>` (`design/list_native.md`), `catch` (`design/catch.md`), modules
  (`design/module_system.md`), queued calls (`design/queue_fn.md`).

## Debugging

`TRACE` (`set_trace`, `with_trace(enable, spec, f)`, `tdbg!`) scopes
compiler tracing to one expression — the stdlib typechecks on every
compile, so unscoped prints are gigabytes. Every debug env var is in
`design/debugging.md`; the common ones are `GRAPHIX_ELAB_AUDIT=1` (a build
refusing what the check accepted), `GRAPHIX_DBG_BIND=1` (every tvar bind and
`contains` verdict), `GRAPHIX_NO_SUBST=1` (instances check again),
`GRAPHIX_PAR_AUDIT=1`, `GRAPHIX_PRUNE=audit` and `GRAPHIX_DUMP_CLIF=1`.

## Working conventions

- Code review uses `// CR <name> for <name>: [tag] text (id)` near the
  code; a live CR's text is never edited: it is X'd, noted
  (`// <date> claude: ..`), re-addressed or deleted. Load `/cr-discipline` before
  touching one.
- PRs carry a concise summary, testing notes and related issues. Rebuild
  the book when docs or examples change.
- Examples in `book/src/examples/` are documentation and test corpus at
  once: each is included by a book page, and `graphix-shell/tests/shell/
  examples_compile.rs` builds every one (check, elaboration, fusion, as
  `graphix --expand` does) in the plain gate, and checks every inline
  ```` ```graphix ```` fence as a script. REPL transcripts are checked in
  a session per page, seeded by `,repl` fences; every `-:` type line must
  match the check, with generated variable IDs renamed consistently.
  `,fragment` parses a program written against names its page
  assumes, `,sig` an interface, `,ignore` nothing; `,refused` must be
  refused, by the message a transcript after it shows. The tui package's
  `examples_test` renders every TUI example and drives some with keys,
  and the GUI examples are run by hand
  (`cargo run --bin graphix -- examples/gui/<name>.gx`).
- A new compiler walk must name the loss without it before it is added;
  the typechecker must stay instant (measure the GUI suite after typing
  changes); predictable fusion is a core value — push on de-fuse corner
  cases rather than accept them.
- Hot operators log and bottom on failure; rare stdlib functions return a
  catchable `Error`.
- A bug in a crate Graphix depends on is an entry in `UPSTREAM.md`, not a
  CR. Work around it in Graphix only when it breaks Graphix's own
  operation (a crash, a hang, a wrong answer), and say so in the entry.
- Package-specific notes (`xls`, `sys::process`, `tui`, GUI, the package
  manager) are in `design/stdlib_notes.md`.

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
change to seq or select semantics, or to typing or firing.
