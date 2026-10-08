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

- **graphix-types**: the static half: syntax (parser, AST, printer,
  formatter, module resolution), types and their checker, the
  environment, the image session core. It reaches nothing of the
  compiler and carries no cranelift; `graphix-ast-pack` (every package's
  build-dependency) depends on it alone. An image object of a type the
  core does not know rides `image::Foreign` and the session's extension
  (`image::Compiled`, `image::Restored` in the compiler).
- **graphix-compiler**: compiler (`Expr` → node graph), elaboration,
  fusion/JIT; re-exports graphix-types' modules (`graphix_compiler::
  {expr, typ, env}`). Entry point `compile()` in `lib.rs`.
- **graphix-rt**: the runtime that executes node graphs in a background
  task, driven through `GXHandle`; embedder extensions via `GXExt`.
- **graphix-package**: package manager (loading, vendoring, standalone
  builds). **graphix-derive**: proc macros (`defpackage!`).
- **graphix-shell**: REPL and CLI; the binary is `graphix`.
- **graphix-lsp**: the language server's protocol loop and queries; the
  shell's `lsp_backend.rs` is the backend (`graphix lsp`).
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
cargo test                               # THE gate: whole workspace, from the root
cargo test -p graphix-tests              # one crate while iterating
cargo test --workspace --features slow-tests   # the release gate
cargo run --bin graphix -- file.gx       # run
cargo run --bin graphix -- --check file.gx     # compile + typecheck only
cargo run --bin graphix -- --expand file.gx    # check + print each seq's lowered machine
cargo run --bin graphix -- fmt file.gx         # format in place (--check, --stdout; stdin when no file)
```

Tests run in parallel by design (the compiler supports many instances
per process); never rely on `--test-threads=1`. The whole workspace is
formatted: `cargo fmt --all` before a commit, stable toolchain.
Formatting: `rustfmt.toml`; `snake_case` / `CamelCase` /
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

**Pipeline.** Parse (`graphix-types/src/expr/parser/`) → `Expr` AST
with positions → compile (`node/compiler.rs`) → `Node<R, E>` graph →
typecheck (two passes, `typecheck0`/`typecheck1` on every node) → fuse
(`fusion/`, when enabled). `typecheck0` also builds `ctx.bind_to_lambda`;
`CallSite::typecheck1` pre-binds statically resolvable calls and
pre-materializes HOF callbacks. The check is typecheck0 plus the settle
of every call site it recorded, drained before typecheck1 elaborates:
a definition's gate keeps what its body bound as its signature (only a
vacuous ⊥ reopens), so a call's types follow from the signature, and
elaboration refusing what the check accepted is a type-system bug
(`GRAPHIX_ELAB_AUDIT` reports it). An instance does not check again, a
run-time bind's and a restored definition's included: its types are its
definition's check's (`node::lambda::DefTable`, which an image carries),
substituted by its signature, and each node's `typecheck0_instance`
does only the state part of its check (the default is the check;
`design/parallel_compile.md`). An instance the table cannot type is
refused, a compiler bug, never checked instead (`node::lambda::Typing`).
Every type name a definition
writes, a typedef body's included, must name something by the check's
end, expanded or not (`design/env_independent_typerefs.md`).

Key types: `Expr`/`ExprKind` (immutable AST; `Expr::for_each_child` /
`map_children` are the ONE child enumeration — `fold`, the seq rewrite and
the fuzzer's preorder all ride them, so a new `ExprKind` child is added
there and nowhere else), `Node<R, E>` (a newtype over `Box<dyn Update>`;
construct with `Node::new`), `ExecState<R, E>` (what an embedder owns: a
`CompileCtx` (the registry, the program's state and the compile's scratch,
which it derefs to), the runtime `rt`, `libstate`, `control` and the
cycle's `Event`), `ExecCtx<'a, R, E>` (the view `update`, `delete`,
`sleep` and image decode take, `ExecState::view`: borrows of those
fields, derefs to the `CompileCtx`; the event is `ctx.event`, and
`with_event` runs a subtree over another event),
`Scope` (lexical `ModPath` + `DynScope`, the chain of error handlers a
`?` sees, following the CALL chain).

**Nodes** implement `Update` (regular nodes) or `Apply` (function
applications called by `CallSite`). `Update` requires `update`,
`delete`, `typecheck0/1`, `refs`, `sleep` (unselected arms pause), plus
`emit_clif`/`fuse` for fusion. When writing a node: store the spec
(`Arc<Expr>`) for errors, track bind ids with `Refs`, call
`ctx.set_var()` for variable writes. `wrap!(node, result)` adds
expression context to errors; `err!`/`errf!` build error values.

**Compiling never reaches the runtime.** Compile, `typecheck0`/`1`
(`Update` and `Apply`), fusion (its state is `CompileCtx::fusion`; a
region it replaces is discarded) and `BuiltIn::init` take a
`CompileCtx`; update, delete, sleep and image decode take the `ExecCtx`. What
compiling would ask of the runtime it defers: a reference is recorded
(`CompileCtx::record_ref`), an abandoned node, application or stored
value is discarded (`discard`, `discard_apply`, `discard_stored`), and
`ExecCtx::apply_deferred` deletes the discards and then registers the
references: at the end of `check_and_fuse`, a registration read, a
runtime bind, a lazily decoded body, a dynamic module's recompile and a
core-trait hook site's build (`drop_deferred` on a failed compile);
every cycle asserts (debug) that nothing is left. `ExecCtx::unref_var`
cancels a reference not applied yet. Registration covers every node a
statement holds, selected or not: a write to a variable only a sleeping
arm reads still schedules the statement, which wake catch-up relies on.
The runtime side (`sleep`, `update`, `Deref`'s addressing) registers
directly; a builtin needing runtime state `init` cannot reach takes it
in `EvalCachedAsync::attach`. Static resolution reads only the index
(`bind_to_lambda`): a binding it lacks dispatches dynamically.

**Runtime.** `graphix-rt` implements `Rt`: variables, timers, spawned
tasks and watch channels (`spawn`, `spawn_var`, `watch`, `watch_var`)
that packages use to feed external events in. Event processing is
batched: all simultaneous events form one `Event` delivered in one
cycle; several writes to one variable in a cycle queue for the next.

**Session images** (`design/program_image.md`): the shell caches the
session state before any cycle (`graphix-compiler/src/image/` over
the session core in `graphix-types/src/image/`) under
`$XDG_CACHE_HOME/graphix/registration/<build-id>/<key>.img`: the
registration entry (the package root compiled under fixed flags; key =
image format + root; the build id covers the packages compiled in, and
an executable without one has no cache) and, for a script, the program
entry (the program compiled too; key adds the script's path or an
embedded program's text, the flags and `GRAPHIX_MODPATH`; the entry
records the sources its compile read and a load re-verifies them). The
runtime restores the first of the program entry and the registration
entry that reads, else compiles; an entry that fails to read leaves the
session untouched, and whatever was compiled is written. `--no-cache`
disables the cache, `--warm` writes and exits (failing when it cannot);
the header carries the ISA. A script compiles at runtime
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
resolution cells, type variables, map nodes, definitions' check tables)
is written once, in the
definitions area after the body and the heap (`ImageEncoder::finish`),
and every occurrence of it, the first included, is a reference to the
ordinal its first sight assigned; the trailer maps ordinals to
definition offsets, and a reference to an object not built yet decodes
it from its offset, so any part of the image decodes in any order. An
occurrence costs its reference whatever was measured or written
before it, so every `encoded_len` under a session is exact and a
derived `Pack` may frame an image object.
Types and function types are keyed by their canonical bytes with every
shared leaf by identity (`Type::content_key`), so equal types decode to
one value. An expression is keyed by the first expression seen with its
id and contents (`image::expr_key`), so a node's spec shares its def
body's definition.
Everything a session encodes must be borrowed from the context and
root nodes for the whole session. A program image writes instance
bodies to a heap after the eager part with an instance table; a call
site keeps `Callee::Imaged` (id, resolved type, reference summary) and
decodes the body on its first dispatch, synchronously, through
`ExecCtx::image_decoder`; a builtin callee travels as its own bytes:
every `Apply` owns `image_encode` and every `BuiltIn` an
`image_decode` (no defaults — a builtin without a codec does not
compile), wrapper payloads implement `graphix_package_core::ImageState`
(`unit_image_state!` / `pack_image_state!` for the common shapes), a
resident `TagValue` decodes as phantom, a bind id created with
`ctx.rt.ref_var` is re-registered at decode, and state that exists only
once a cycle ran refuses the write with `NOT_QUIESCENT`.
Compiler ids are `image_id!` (the compiler's `atomic_id!` plus
relocation); netidx's ids and wire format are untouched. A definition
built by Rust at runtime is `DefOrigin::Runtime` and is never imaged.
Pins: `stdlib/graphix-tests/src/lang/image.rs`.

**Formatter** (`graphix fmt`, `expr/format.rs`): parse → the one
printer (`PrettyDisplay`, `expr/print.rs`) → reparse; a result whose
syntax, comments, attributes or string delimiters differ from the input
is refused, never written. The AST stays canonical (struct fields, union
members and a use statement's names are held sorted); what the author
chose rides beside it as metadata that decides nothing: `WrittenAt` (a
position that is always equal, hashes to nothing, packs to nothing) on
struct pattern binds, struct type fields, variant types, every declared
name (`Name`) and every expression's end, `Expr::pos`
for struct literal fields, `Expr::str_form` for a string's delimiters,
`Comments` (always equal) for the `//` lines above an interface item or
a trait method.
**Printing is canonical unless `PrintFlag::AsWritten` is set, and only
the formatter sets it**: printed types and expressions reach
program-visible values (a cast error, a null error); `WrittenAt` and
`str_form` are not part of a session image (`Expr::pos` and comments
are), and a type is shared by content, so whose written order it carries
is incidental. A tree holding a comment,
an attribute or a doc has no single-line form. What the formatter
does normalize: `i64`/`f64` literals print bare; a run of adjacent
undecorated `use` statements merges into one per root and visibility, a
sorted tree with every shared prefix written once, where no item reads or
binds what another binds or reads (a glob merges with nothing); a blank line stands
around every file-level item that spans lines; primitives in a union
print first, in canonical order. Layout: a value follows its head
(`let x =`, `<-`, `name:`, `=>`) on the head's line when it fits or
opens with a bracket whose first line fits, else it moves under the
head; a lambda head that does not fit lists one argument to a line; a
lone argument that opens with a bracket hugs the brackets around it, in
expressions and in types alike (`f({`, `` `Tag({ ``, `` `A(`B([ ``,
`Array<{`); two arguments list. After a printer change
run the corpus harness over every `.gx`/`.gxi` here and in `../netidx`:
`cargo run -p graphix-compiler --example gxfmt -- <files>` (many files:
round trip + idempotence; one file: prints it; `GXFMT_UNCHECKED=1`
skips the reparse). The print round-trip proptests are randomized: a
printer bug can pass several runs. Width and indent are
`format::FormatConfig` (defaults 90 and 4), discovered by
`FormatConfig::discover`: the nearest `graphixfmt.json` at or above the
source file, else `dirs::config_dir()/graphix/graphixfmt.json`, else the
defaults; a malformed file is an error; `--width`/`--indent` override.
A new layout setting is a field there, never a constant in the printer
(`PrettyBuf::nested` is the one indent step). The LSP serves
`textDocument/formatting` from the same `format_source`
(`graphix-lsp/src/formatting.rs`): one whole-document edit,
no edit for a document that does not parse, an error response only for
`format::Refused` (the formatter declined its own output).

**Language server.** A ROOT is a project's root file (a `.gx` no other
file's `mod` reaches; `graphix-lsp/src/workspace.rs` scans) or an open
file outside every project. A root at `<crate>/src/graphix/mod.gx` of a
`graphix-package-<x>` crate is a package root, checked as the body of
`mod <x>` over the copy registered at startup: `Env::
unbind_scope_subtree` must drop everything a package registers, so a new
global registry is cleared there. `--check` and the server share one
path (`GXRt::check`), which runs the check alone (`CFlag::CheckOnly`:
no elaboration, no fusion, so `#[native]` and the def assertions
are verified by a build; `--expand` builds) and checks a script's file as one block, as it
runs, with its names at the root (`compile_script`); a root loads
through `RootFile::load`, open
buffers first, paired with its `.gxi`, and under buffer overrides a path
is never canonicalized (the editor's names rule). Checks are lazy and
coalesced: a change marks its roots dirty and `ServerState::flush`
checks them when the client has nothing queued; nothing is checked at
startup. The last SUCCESSFUL check of a root (`Checked`: the env and the
`Ide` sinks, `ide.rs`) answers every query, so a buffer that does not
compile still answers, stale; bind ids mean something only within one
`Checked`. A query resolves the cursor to a target (bind id, canonical
module, canonical type) from the sinks and never by looking a word up:
what the check did not record has no answer. Positions come from the
AST: a declared name is an `expr::Name` (identifier + `WrittenAt`), a
`use` item carries the position of each path segment (`WrittenPath`),
every parsed `Expr` has an `end`, and a bind, typedef, trait or `mod`
site is recorded AT THE NAME. All three decide nothing and pack to
nothing, so a name or an end out of a packed AST or an image is
`NOWHERE` (`Name::pos_or`). A parser site that builds an `Expr` gives
it its end (`Expr::ending`; `expr()`, the postfix loop, `mke` and `qop`
cover what passes through them); pin: `graphix-compiler/tests/
expr_spans.rs` (every node's `[pos, end)` parses back to the node, over
the examples and the stdlib). A package under development is not in the binary that checks it, so
under `lsp_mode` a package root's name is a `package::` root for the
check and an unknown builtin is a WARNING at its `'name`
(`node/lambda.rs::UnknownBuiltIn` stands in: typed by the declared
signature like any builtin, never produces); without `lsp_mode` it
stays an error. Warnings go through `Env::warn`: to `Ide.warnings`
under a check with a sink, else to stderr as before. The server
publishes the warnings of a root's last SUCCESSFUL check and, when the
current one failed, the error beside them. An error's position is its `ErrorSite`, the innermost wrap: contexts are
attached with `.at(&spec)` (`expr::At`), `wrap!` or `bailat!`, never
`ErrorContext(..)` by hand. Pins: `graphix-shell/tests/lsp/` (the real
server over `Connection::memory()`; positions are `"let y = |x + 1"`
markers).

**Module loading** is the `ModuleResolver` trait (`expr/resolver.rs`);
`VfsResolver`/`FilesResolver` are in-core, `NetidxResolver` is in
`graphix-package-sys`. `sys::net` owns its netidx through `NetState` in
`ctx.libstate`; `NetHandles` shares the raw publisher/subscriber between
the loader and `NetState`; unseeded contexts get a process-internal
netidx on demand, so programs that never touch `sys::net` have zero
network. The shell library is netidx-agnostic; the CLI is the
netidx-aware embedder (`ShellBuilder::setup_context`,
`resolver_factories`, `GXHandle::with_ctx`).

**Types** (`graphix-types/src/typ/`): `Type` (structural),
`TVar` (inference cells; `design/tvar_constraints.md`), `FnType`.
`contains` expands `Type::Ref` through `lookup_ref`, so bindings made
during `contains` hold the EXPANDED form — code inspecting resolved types
handles both. `TypeRef` carries a write-once resolution cell
(`design/env_independent_typerefs.md`): rebuilds share it via
`with_params`, `with_scope` makes a new one (filled when the source's
is), never overwrite a filled cell; the cell holds the definition
weakly and the `TypeDef` owns it (a recursive body reaches its own
cell), so a type outliving its definition's env entry is refused,
never re-resolved, and what outlives the entry owns the definitions
its types name (a definition's check table, a kernel's type constant:
`DefTable::typedefs`, `record::KernelType`); `Env::seed_typedef_refs` runs right
before fusion in both modes. A typedef must be contractive: every
self-reference sits under a constructor (`type T = [i64, T]` is refused
at `Env::deftype`), which is what makes the coinductive ref-pair memos
sound.
A reference is not a number: a cast whose source can hold one is
refused (`Type::holds_ref`), and where only an instance knows the
source, the cast yields its `InvalidCast` error; a reference prints as
`&ref` (its id is the session's: a warm start relocates it); a
reference widened to `Any` is the program's own business; nothing
orders references or hashes them by value (`Ordered`).
Format type variables with `format_with_flags(PrintFlag::DerefTVars, ..)`.

**Two-phase typecheck knot.** While an instance body typechecks, its
def is in `ctx.resolving_lambdas` (a stack per def); a site reaching the
def with the same `FnArgIdentity` (per argument, the SOURCE lambda it
resolves to) is a self-call and reuses the instance; a different
identity is a fresh instance even mid-resolution (a HOF nested under its
own callback is not recursion). An instantiation snapshots its def's
`LambdaIds`. Never special-case collection intrinsics here. A bind at
run time (a fresh activation, a collection slot, a dynamic call)
typechecks its instance in a settle frame of its own and settles it
when the typecheck ends (`node::with_runtime_settles`): no statement
boundary follows it, and an undrained frame pins every cell it
reaches.

**Builtins** implement `BuiltIn<R, E>` (`NAME`, `init()`, `EFFECT`) and
register with `ExecCtx::register_builtin::<T>()`; the Graphix signature
lives in the package's `.gx` with every argument and the return type
annotated. `EFFECT` is the one classification (`effects.rs`):
`Async` (may produce later, autonomously, or never), `Sync` (same cycle
but keeps cross-invocation state, or depends on WHICH args arrived), or
`Stateless(Option<FastCall>)` (a pure function of its args; the payload
is the direct-call entry the JIT uses, `Plain` or `Typed` by the site's
resolved return type; `None` for effects and partial-delivery
producers). A wrong `Stateless` is a semantics bug (the JIT's native
tail loop shares state across iterations); a wrong `Sync` is one too
where it skips a wake's recompute (`CachedArgs` re-runs only a
`Stateless` eval), and otherwise costs the loop. Bottom never reaches builtin authors: a bottomed arg bottoms the
invocation before `eval`; raw `Apply` authors read args through
`seam_arg`/`seam_tick`/`seam_value`. Configuration a fast fn derives
from its args (a regex, a template registry) lives in a bounded
thread-local `FastMemo`, never in state. A type-directed builtin
(`str::parse`, the reads) declares its target `'b: Concrete`; the checker
refuses the target where it settles open or holds a reference (data
can't be one; the run-time cast refuses one too), and the builtin's
`typecheck1` only extracts the type, never refuses
(`design/tvar_constraints.md`); a builtin that wraps a function
(`queuefn`) declares it `'a: Function` the same way. A definition's call sites settle with
the enclosing statement, the signature's cells exempt.

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
  `Qop` uses), except a handler-ful `?` under a select arm whose error
  derives from a constant: that raise fires when the arm is ENTERED and
  a kernel has no arm-entry view, so it node-walks
  (`lowering::entry_raise_blocker`); only a `never(..)` arm is a
  standing bottom (its args are still emitted and consumed), any other
  bottom-typed arm body runs; everything else — stateful/effectful builtins, `connect`,
  `~`, `Any`, `Catch` — node-walks, transitively. A kernel's only
  cross-invocation memory is the firing boundary (prev-length words,
  first-call words, per-site/per-activation blocks; in a loop each is
  the slot's, never shared by its iterations) and its loops' cost sites,
  which decide nothing a program sees; no replay caches, no selection
  memory. The runtime loans a kernel exactly `KERNEL_ABORT`,
  `KERNEL_ENV`, `QOP_RAISES`, the core-trait value hooks and the fork
  mode (`par_loop::ParLoan`): a map-family loop at a kernel's top level
  runs as chunks of its slots, in order or forked
  (`design/parallel_eval.md` §10).
  `#[native]` asserts zero node-walk residue at a source location and is
  THE advertised performance model; `#[sync]`/`#[async]`/
  `#[tail_recursive]` assert analysis facts.
- **Bottom is dense** (`design/dense_delivery.md`,
  `design/representable_bottom.md`): `update` returns a `TagValue`
  every cycle — `Fired(v)`/`Stale(v)`/`FreshBottom`/`StaleBottom`, the
  orthogonal fired×bottom algebra. A standing bottom re-delivers
  `StaleBottom` and never re-fires consumers; bottomness ORs over
  consumed productions. A third bit, WAKE, marks a fire only a woken
  arm's constants caused (and every stale tag): it ANDs as STALE does,
  and only a `<-` target's `let` reads it (`design/wake_catchup.md`). In
  the JIT the bits ride each param's disc (`STALE` masks bits 61 and 60,
  `FIRE_TEST` bit 61 alone).
- **Organic firing** (`design/organic_firing.md`): a node fires iff a
  consumed input fires; nothing stores a previous value or selection to
  decide a tag; `uniq`/`filter`/`~` are the cadence tools. A select emits
  per fired input — scrutinee delivery, a CONSULTED guard, or the taken
  arm's own production; same-arm re-matches emit the arm's current
  value. Constants fire at init (and at an arm's wake); every
  argument-less literal (`` `Tag ``, `[]`, `{}`) is a constant
  (`node::produce_constant`). Kernel outputs
  fire only when an input feeding them fired; collection loops fire on
  resize, a fired slot, a fired empty source, a fired fold carry, or a
  source back from bottom (a bottom source forgets the length).
- **Bottom scrutinee ⇒ bottom select.** No stored-selection ride of any
  kind; `hold` on the scrutinee is the tool. A STALE-PRESENT scrutinee
  still routes the taken arm's own fires. **Consulted-guard rule**: arms
  are consulted top-down, structure then guard; a consulted guard whose
  channel is bottom makes the selection undecidable; a never-produced
  guard is unknown, not false. `&&`/`||` are strict (`false && ⊥ = ⊥`).
- **Sleep is pause, not reset.** Value-channel state survives an arm's
  sleep (`Held` residents at the select scrutinee, pattern guard and
  `~`'s arg; `CachedVals` staging; collection slots; a `<-` target's
  value, which a wake's constants never overwrite and a fired input of
  its initializer does). **Wake catch-up** (`design/wake_catchup.md`): a reselected arm
  recomputes from the world as it stands, reading standing values STALE;
  the only events it re-raises are the fires no selected reader saw,
  once, at their current value (one fire bit per arm-body input per
  select, consumed by whichever arm reads it; pattern binds and a
  destructuring let's siblings are facets of one input). An input that
  went bottom during the sleep is bottom at the wake: a bottom input
  SETS a node's resident (`TagValue::set_bottom`), never rides it, and
  every wake refresh stands a stale bottom like a value. Sleep state is
  LOCAL: every skip-owning node owns a `slept` bit its `sleep()` sets and
  its next update takes — no ExecCtx globals (parallel compile and a
  parallel evaluator stay possible). The restart builtins
  (`once`/`take`/`skip`/`uniq`/`hold`/`count`) and a seq machine's `pc`
  clear in their own `sleep()`, a restart builtin's output included: a
  woken one is a fresh one; configuration (`#n`, `#rate`) survives. A labeled DEFAULT is born with the binding and delivers
  FIRED at a fresh callee's first dispatch. Async builtins clear their
  output on sleep and start again over their present arguments at the
  wake (`design/async_sleep_outputs.md`). A pure non-recursive
  arm skips `sleep` and is not updated while untaken.
- **Activation state** (`design/activation_state.md`,
  `design/recursive_activations.md`, `design/atomic_recursion.md`):
  held state never decides output bottomness; activations ARE
  collection slots; every call is an activation, tail or not: the
  node-walk has no frames, and the JIT runs a STATELESS tail recursion
  as one native loop held to the activations' answers
  (`design/tail_calls_are_calls.md`); instances are retained
  unconditionally; shrink = delete (a depth not reached this cycle is
  deleted; re-reaching it is fresh). No depth limit; evaluation is
  atomic within a cycle; containment is the cooperative interrupt
  (`GXHandle::interrupt`, Ctrl-C, `GRAPHIX_STACK_BUDGET`). Kernel
  interior memory (`design/kernel_instance_state.md`) gives one compiled
  body the interp's per-slot/per-activation multiplicity for exactly the
  state that decides firing; only a site's first-ever dispatch, and a
  loop slot's first iteration, is an init view.
- **`let rec` is monomorphic-recursive**; a def's declared tvars are
  rigid in its body check: none binds to a concrete type and no two
  unify (`contains.rs` Distinct), while a call instantiates them
  freely; a call copies only what the callee's definition owns once
  its gate is closed (or not yet open): a cell shared with the
  environment, or an open gate's, is shared (`let t = |x| x + y` is
  monomorphic in `y`'s cell; design/tvar_constraints.md,
  Generalization); a labeled default is checked at the definition
  against its parameter's type, or a declared tvar's constraints (`check_defaults`),
  and again at each omitting site by the check, where it may narrow
  that site's cells; a call's type variable that only data positions hold (never a
  function or a reference) settles to the widest argument whatever
  the order, one a callback or a reference holds to the first
  (`callsite.rs::Widening`); a formal with its own quantifiers (`f: fn<'b: C>(..)`) is
  rank-2: its argument is checked with `'b` rigid, a call copies `'b`
  generic, and an open quantifier never binds to its bound, no site's
  settle decides it (`design/tvar_constraints.md`);
  union collapse requires strict tvar identity; a free union member stays free (a type test over an
  untyped parameter binds it: annotate the parameter, not the arms); float comparison is a total order (`NaN ==
  NaN`, below every number) so `Value` is map-key-able; checked arith
  (`+?` …) yields a catchable `ArithError`, unchecked wraps, integer
  div0 and `MIN / -1` bottom; indexing is bounds-checked through shared helpers on
  both backends; `$` and handler-less `?` log a swallowed error from
  both backends; unchecked-arith diagnostics are node-walk-only (debug
  with `--no-fusion`).
- **Emit contracts** (`design/distributed_jit.md`): effects de-fuse,
  never silently skip; owned select-arm binds drop at every arm exit
  (run `leakcheck` when adding an owned-local class); a bottom is a
  production whose STALE bit follows the same trigger fold as a value
  (`nodes::emit_bottom_placeholder` takes the governing discs); kernel
  cache keys carry catch coverage and a resolution fingerprint; a pass
  the fusion gate owns must never change what the typechecker sees.
- **JIT pipeline and memory** (`fusion/emit/jit.rs`): emission names
  everything by ids of its own (`jit::Names`) and defines nothing.
  Disjoint subtrees fuse in compile tasks (`fusion::fuse_each`: the
  parts of a node that call a function); a task emits into its own
  `Emission` over a frozen copy of its parent's names and caches, and
  its join renumbers its functions after the parent's in join order, a
  lambda kernel the parent already has replacing the task's, so the
  decisions and the output are the serial walk's (`GRAPHIX_FUSE_SERIAL`
  is the A/B). A region that fails forgets the kernel signatures it
  cached with its bodies. Only the context's root emission links:
  every `LINK_BATCH` regions a batch starts compiling on threads of its
  own while emission goes on (`FusionCtx::link_batch`), and installs at
  the next batch or at the pass's link (`FusionCtx::link`), which
  compiles the rest; records are built in emission order and a batch's
  wrappers install with one finalize; a kernel has its entry only after
  its pass links. Emission accepted everything a link compiles, so a
  link that fails (a verifier error included) is a JIT bug and panics.
  One JITModule + 256MB
  arena per generation, built on the first fusion (a fusion-off context
  never builds one); an install that fails retires the generation, and
  on exhaustion the link reinstalls its records in a fresh one; a
  generation's code is freed when it and every kernel installed into it
  have dropped (each `WrappedKernel` holds its code). Pin:
  `graphix-shell/tests/jit_arena_rotation.rs`. Kernel ABI: kind-grouped params from
  `KernelSig::abi_params`; recursive types, abstract types, primitive
  unions and the primitives with no register form (varints, decimal,
  error) are opaque 2-word values (`design/unified_value_abi.md`).

Coverage today: scalar arithmetic/comparison/logic/casts, producers and
accessors, `?`/`$`, the eight array HOFs as native loops (nesting
included), structural select destructuring with scalar and variant
payload binds, owned binds of a nullable's payload, of a slice's rest,
head or whole (`[x, tail..]`, `all@ [..]`) and of non-scalar elements,
tag tests and binds over a primitive union, nested variant payload
patterns and payload literals, scalar literals over an option, a result
or a union, `never()` arms as bottom
productions of the merge shape, or-patterns, list patterns, tail loops
over any kernel param
kind, every fast-fn builtin and non-inline cast, cross-kernel lambda
calls, trait default bodies. A subtree that does not fuse whole
descends: every part that calls a function (a lambda call, a collection
operation) is tried as a region of its own, any other part only
descends (`fusion::fuse_parts`); a lambda call whose argument does not
fuse takes that argument as a node-walked feeder of its kernel
(`fusion::try_fuse_feeding_args`); a `let` bound to a lambda literal
emits nothing in a kernel, which calls it statically; a collection that
does not fuse whole fuses its prototype's callback instance, and each
slot's instance takes those kernels at its bind (`fusion/share.rs`:
by attempt ordinal, where root, inputs, return, callees and raises
agree). An attribute on a
node absorbed into a larger kernel still has its target checked
(`Attribute::check_target`). `FusionStats.failed` is a blocker profile,
not a gap count.

## Parallel evaluation

`design/parallel_eval.md`. A cycle's update pass forks independent
subtrees onto the process's evaluation pool (`branch::eval_pool`,
`GRAPHIX_EVAL_THREADS` workers): a block's runs, a call's arguments, a
constructor's fields, an operator's operands, a collection's slots and a
kernel's top-level map-family loops (chunks, `fusion/par_loop.rs`).
Branches see the runtime through branch views (`RtView`/`ForkRt`,
`Layered`, `CxView`) and merge back in program order, so a forked cycle
computes what the serial node-walk does. Siblings are independent iff
no later one reads what an earlier one publishes (`analysis::
plan_block`; a module, trait or impl statement is a run of its own and
a catch runs last) and they do not both reach an `ORDERED` builtin
(in-language state shared across nodes, `queuefn`). External effects in
parallel branches happen when they run, unordered. A seq machine runs
serially inside; runtime compiles run in compile tasks
(`branch::compile_each`). The mode is the runtime `Control`'s
(`set_par_mode`), from `GRAPHIX_PAR=off|auto|force`, `auto` by
default: `Auto` forks where `cost.rs`'s tick histograms say a side
costs the calibrated threshold (4x the pool's wake latency) and not
while every worker has a part; `Force` forks at every fork point (tests,
the fuzzer). `#[parallel]`/`#[parallel(g)]` forces the fork points
under it (a compile error where nothing forks; callees excluded),
`#[serial]` inhibits them, callees included. The stack budget is per
cycle, across workers; the compiler never pins threads.

## Testing is differential

- `run!` (`graphix-package-core/src/testing.rs`) runs a fixture in
  each `testing::Mode`: `interp` and `jit` (serial unless `GRAPHIX_PAR`
  is set), and each again with every fork point forked (`par`,
  `jit_par`), asserting its predicate in each; nothing compares the
  modes, so a predicate pins the value exactly and a refusal names its
  message (`testing::refused`). `FuseExpect::{Jit, None}` asserts
  WHETHER anything fuses, bidirectionally; `#[native]` asserts that an
  expression does.
  `GRAPHIX_FUSE_AUDIT=1 cargo test -- jit --nocapture` prints the audit.
- **graphix-fuzz** (`design/graphix_fuzz.md`): node-walk vs JIT with a
  per-cycle trace oracle, each engine also no-cache vs cold-image vs
  warm-image (`GRAPHIX_FUZZ_SESSIONS`) and forked against the serial
  node-walk (`Pair::Par`), and a program both builds refuse
  also against the check alone (`Pair::Check`: elaboration refused what
  the check passed); every run on the shell's script
  path (`GXConfig::program`), never the REPL's `rt.compile`; a corpus
  pin both engines reject is a regression unless it says
  `// expect: reject`; `check`/`run`/`generate`/`fuzz`/`minimize`/
  `regress`/`selfcheck`/`gen-check`/`detcheck`/`typemorph`; every
  typemorph subject also yields must-reject mutants
  (`design/must_reject.md`), each family citing a checker rule: a rule
  change updates its family in the same commit. The
  committed `findings/` corpus is the regression gate: `regress` runs
  every pin through `check` and also compares each pin's fused-region
  count with `graphix-fuzz/fusecheck.manifest`, so a de-fusion fails
  loud; a program whose first cycle aborts the runtime by the stack
  budget records `abort`. After an INTENDED fusion change, `fusecheck
  --bless` rewrites the manifest (rebuild to embed it) and the diff is
  reviewed like code. `regress` also compares each pin's verdict
  (`trace`/`contained`/`reject`/`excluded`/`unsure`) with
  `graphix-fuzz/outcome.manifest`: the corpus runs in parallel on a
  worker per core, and only an agreement the manifest does not vouch
  for is retried alone at 4x; `regress --bless` records the verdicts
  (every untrusted pin retried alone) when a pin's class changes on
  purpose or a pin is added. `sys::`/`http::`
  programs compare settled values per epoch (`FinalValues`); the
  `Excluded` markers (`oracle_tier`) never record a divergence.
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
  initializer takes its type from its first use, a writer or a reader;
  a later writer must fit it, and a refused one is told the declaration
  that holds both (`node/mod.rs::write_mismatch`).
- **Sets and coverage**: one walk (`select.rs::Reach`) decides what
  reaches each arm: its binds narrow to it, an arm that can match none of
  it, or that tests what an earlier unguarded arm tests, is dead, and the
  select is exhaustive when nothing reaches past the last arm. Select exhaustiveness is enforced; slice-pattern
  length ladders count as coverage, one ladder per array or list member;
  a `null` literal covers `null`; a collection type test (`Array<T> as`)
  narrows later arms only of collections whose every element it
  covers, since a mixed one that fails it may still hold a `T`;
  bool literals, variant heads
  (payload irrefutable) and or-patterns of them pool per position
  inside composite patterns, keyed by path, and an arm under a type
  test pools only a member the test holds; an or-arm narrows later arms per
  alternative; a structure that matches anything is a wildcard only over
  a scrutinee its shape covers; set coverage distributes over product
  heads (`` [`P(A), `P(B)] ⊇ `P([A, B]) ``); a probe in progress for the
  same scrutinee ref claims nothing on re-entry.
- **One runtime form** (`Type::rep_collision`): no select arm, and so no
  union trait dispatch, may tell apart two types that share a runtime
  form: a tuple, struct, list or payload variant and an array (an empty
  array and an empty list are one value), a bare variant and a string, a
  reference and a number or another reference, two function types.
  Checked per arm, after its narrowing, in the definition's check;
  members that share a constructor are told apart by their parts. A
  compared type (`==` and the orderings), a map key type and what the
  stdlib compares or hashes (`uniq`, `min`/`max`, the sorts, `dedup`,
  `map::` keys) may hold no such pair anywhere: the `'a: Discernible`
  bound (`Type::indiscernible`), which a generic definition's variable
  takes from its body, so each call checks it; what orders or hashes
  (the orderings, sorts, `min`/`max`, `dedup`, map keys) takes
  `'a: Ordered`, which also refuses a reference; a member still open at
  the statement's settle reads as any type it may yet bind
  (`PendingSettle::Discernible`, `design/tvar_constraints.md`).
  `flat_map`'s callback returns the collection (`fn(x: 'a) ->
  Array<'b>`), so nothing splices by shape.
- **`name@ pattern` captures** are typed from the SCRUTINEE: under an
  inferred predicate a capture is a type variable that
  `PatternNode::bind_narrowed` binds, after the select narrows the arm,
  to its part of the narrowed predicate (a `_` slot, the fields a
  partial struct pattern leaves out and a slice's rest carry the
  scrutinee's types; shared
  or-alternative captures union). Never type a capture from
  `infer_type_predicate`. A slice's inferred element type is a Set with
  one member per element and a cell for the rest, so each element
  narrows on its own. Pins: `lang::select::capture_*`,
  `slice_elements_typed_apart`, `slice_rest_scrutinee_type`.
- **Or-patterns** (`design/or_patterns.md`): select arms and bracketed
  element positions; each alternative is typed over the scrutinee as a
  separate arm is, alternatives bind the same names, and a shared name
  is the union of the alternatives' types; alternatives a
  structure test cannot tell apart are refused
  (`StructPatternNode::footprint`); one guard per arm; dead alternatives
  are errors; they fuse natively.
- **Native List** (`design/list_native.md`): `List<'a>` is a compiler
  constructor like `Array`; `[<1, 2>]` literals and `[<h, rest..>]`
  patterns (rest is the O(1) tail; the suffix form is refused); the rep
  is private to `graphix-types/src/list.rs`.
- **Nominal abstract types** (`design/nominal_abstract_types.md`):
  `type T = Abstract<rep>`; `T(v)`, `x.0`, pattern `T(p)` only where the
  definition is visible; `T as t` is a nominal tag test anywhere.
- **Traits v1** (`design/traits.md`): static dispatch on the self
  argument's type; a union self lowers to a select; impls are global
  facts; core `Eq`/`Ord`/`Display` ride the value (map keys, sort,
  operators, printers, both engines). A quantifier bounded by
  constructor traits alone (`'c: Collection`) is a constructor applied
  (`'c<'c#elem>`), as a trait-typed parameter (`c: Collection`) is, and
  an interface's bounds pair with the implementation's through the
  matched types, whichever form each side writes. The io traits `Read`/`Lines`/
  `Write`/`Close`/`Seek`/`Socket` over five stream types.
- **Module system** (`design/module_system.md`): Rust-2018-style
  `use`; every name arrives by declaration, `use`, or prelude;
  `self`/`super`/`package` roots (`package::` is the registered
  package, else a loaded script's own top level, else `/`);
  declarations, `let` included, are statement-position only. A module
  with an interface compiles its body with its `mod` statement and
  checks it with the statement (`Module::typecheck0`), in a compile task
  of its own; a run of them checks in parallel, after the statements
  before it. Siblings reach each other only through interfaces: an impl
  a body adds undeclared is hidden from its siblings' checks
  (`Env::hidden_impls`) and seen after them, and a module's check that
  would write a cell created outside the module is refused and the
  write not made (`tvar::decided`; annotate the binding it would
  decide; `design/parallel_compile.md`). Every cell of an interface
  `val`'s function type is generic, a constructor trait's element
  included: a call copies it.
- **References** (`design/place_references.md`): `&e` is read-only
  (`&T`, covariant); `&mut e` is writable (`&mut T`, invariant) and
  `*r <- v` requires it of every reference `r` may hold. `&mut` of a
  non-place expression is a fresh cell its uses type; an optional
  writable argument is `[&mut T, null]`. `&a[i]`, `&s.f`, `&t.0`,
  `&m{k}` are root + path; writes patch the root at delivery; a dynamic
  key is a moving reference. References de-fuse. A reference's value is
  its own cell, so `==`/`!=` compare references by what they name
  (`bind::ref_target`: the place, else the byref chain's binding, else
  the cell), rewriting each one the operand type holds
  (`Type::map_refs`) before the value comparison; such a comparison
  never fuses. References have no order (`Ordered`).
- **`catch`** (`design/catch.md`) installs a handler for the rest of its
  block; it is not control flow.
- **`seq` / `seqq`** (`design/seq_blocks.md`): `seq [trigger | let pat = trigger] { stmt* }`
  desugars to a machine node (`node/seq_machine.rs`: busy-drop, calls
  issued once per entry over an argument snapshot); every statement is
  a step and a passed step sleeps, so a `let` keeps its step's value.
  A statement starts in the first cycle its predecessor's effect can
  be seen: the next step enters in the cycle its predecessor completes
  unless it reads or writes a variable a pending write targets, by the
  post-resolution dependency summaries (`design/dependency_summaries.md`,
  `analysis::plan_machines`, per instance): a callee's writes count, a
  read or write through a reference or a call with no static target
  counts as every variable; `until` and `try` follow the same rule.
  Entering a step wakes it with `Select`'s catch-up (`node/wake.rs`).
  A `{ .. }` statement issues its statements together with local lets. `until`,
  `try { .. } with(e[: T]) { .. }` (the error branch; `catch` is refused
  in a seq body outside lambda literals). A step completes on a FIRED
  production after its entry, never on a standing value; a call-free
  step reads its level as it stands at entry. `seqq` queues triggers
  with captured values; a capture a step writes, a callee's write
  included, is live (`SeqCapture`). `seq t; abort(e) { .. }` ends the run when
  `e` fires: silent, past any `try`, and it wins the cycle it fires in
  (the compiler-only `SeqAbort` node fails the machine's guards before
  the machine updates); `e` is an initial step, asleep between runs, and
  only its fires after the entry cycle count. `flush(e)` (`seqq` only)
  also empties the queue. A machine resets to idle in its handler's
  `sleep()`: a run does not survive its arm's sleep. `--expand` prints the
  machine and each instance's step boundaries. `range(i, j)` is
  the integer builtin (`` `RangeError ``).
- **Comments** are legal only above an expression, a select arm, an impl
  or trait method, a struct-literal field or an interface item; `///`
  docs only in an interface. Parse errors report the furthest point
  reached with the source line and a caret, and a refusal its reason.
- **Operators**: `* / %` (and checked forms) bind tightest, then `+ -`,
  comparisons, `== !=`, `&&`, `||`, `~ ~!`; every binary operator is
  left-associative (`8 / 2 * 2` is 8). `BinOp` (`expr/binop.rs`) is the
  one table. Arithmetic (`fn<'a: Number + Singleton>(x: 'a, y: 'a) ->
  'a`) and comparison (`fn('a, 'a) -> bool`) take operands of EXACTLY
  one type, each containing the other's (`node/op.rs::operand_type`):
  `[i64, null] == 3`, `` [`A, `B] == `A `` and `[i64, f64] + 1` are
  refused; a ⊥ operand, or one only ⊥ was produced into
  (`TCell::bottom_fed`), takes the other's type. `Singleton` is a bound
  like `Concrete`: whatever binds the variable is no union, a primitive
  set of two or more included, so arithmetic over a type holding two
  numeric types is refused even against itself, and a generic
  definition's at the call, by the check; the open members of a union
  under it merge into one cell. Comparison takes any one type, unions
  and mixed numerics included, if it is `Discernible` (`Ordered` for the
  orderings). `OneNumber`, a written
  bound only, holds a type to at most one numeric type (`'a: [Number,
  null] + OneNumber`). An interface declares every bound its
  implementation's variables carry, a typedef parameter's bound counting
  (`FnType::sig_matches`).

## Stack discipline

Nesting depth is attacker-controlled and overflow aborts, so it is
closed two ways: `crate::stack::ensure_sufficient` (stacker) wraps every
program-driven recursion — parser knots (`GrowStack`), `compile`,
`Display`, `fold`/`for_each_child`, type walks (`Eq`/`Ord`/`Hash`
included), pattern walks, seq lowering, and the `Node`/`TVar`/`Expr`/
`Type` destructors (explicit teardown inside the guard) — and
`parser::DEFAULT_MAX_NESTING`, which bounds AST depth: it counts parser
knots and the levels each iterative fold (an operator or postfix run)
adds under them (`grow::fold_fits`). Type depth is not bounded by the
limit: `let x1 = [x0]; let x2 = [x1]; ..`, or a chain of typedefs,
builds a type as deep as the program is long.
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
| `GRAPHIX_DUMP_CLIF=1` | every linked function's CLIF, at link in join order (`u0:N` = helper registration order in `emit_helpers.rs`) |
| `GRAPHIX_DBG_VARS=1` | runtime variable events (ref/unref, set, same-cycle notify) — graphix-rt |
| `GRAPHIX_DBG_PERF=1` | interp lazy-bind phase counters every 250ms |
| `GRAPHIX_PROFILE=1` | nested compiler phase accounting per root (`bench/profile.py` reads it; `design/jit_startup.md`) |
| `GRAPHIX_PROFILE_INSTANCES=1` | with `GRAPHIX_PROFILE`, per-instance construction/check costs (`bench/instances.py`) |
| `GRAPHIX_DBG_TVAL=1` | typed-printer render steps |
| `GRAPHIX_DBG_CYCLE_BT=1` | a backtrace at every occurs-check refusal |
| `GRAPHIX_NO_SUBST=1` | every instance checks its body again instead of taking its definition's types (A/B for instances by substitution) |
| `GRAPHIX_FUSE_SERIAL=1` | fusion visits every part in order on one context instead of fusing disjoint subtrees in tasks (A/B: the decisions must agree) |
| `GRAPHIX_PAR_AUDIT=1` | a panic at a join whose right branch read what its left sibling published |
| `GRAPHIX_DBG_PAR=1` | the fork threshold's calibration and each kernel loop forked |
| `GRAPHIX_NO_OUTLINE=1` | kernel loops emitted inline, never as chunks (A/B for the outlining) |
| `GRAPHIX_TASK_AUDIT=1` | a backtrace at every write by a compile task to a cell or var an earlier task created (statement elaboration must write none) |
| `GXDBG_EFFECT=1` | why a lambda classified Async |
| `GXDBG_INSTANCE_FUSION=1` | per-instance region fusion passes |
| `GXDBG_CS=1` / `GXDBG_DYNC=1` | every CallSite dispatch and result tag / every fastcall trampoline dispatch |
| `GXDBG_CALLRET=1` | (debug builds) from inside kernels: each entry's init word (tag 4), return disc (2), scrutinee accumulator (3), tail fold and cross-kernel call result |
| `GRAPHIX_DBG_SELECT=1` | each select update: its selection, the scrutinee and the event's size |
| `GRAPHIX_DBG_BIND_BT=<id>` | a backtrace at every write to the cell with that `TVarId` |
| `GXDBG_FREEZE_RET=1` | a region whose return type does not freeze for the kernel ABI |
| `GXDBG_KERNEL_SLEEP=1` | each fused kernel put to sleep |
| `GXDBG_KPOLL=1` | each kernel's feeder poll: init, tags and presence |
| `GXDBG_NATIVE_ALL=1` | every fusion failure, not only those under a `#[native]` |
| `GXDBG_REFMISS=1` | a kernel read of a name with no local or input |
| `GXDBG_SEQPLAN=1` | each seq machine's planned steps, its step summaries and captures, and each block's opaque call |
| `GXDBG_TYPEREF=1` | scope table dump on an "undefined type" refusal |
| `GXDBG_LETBIND=1` / `GXDBG_REF=1` | let publication decisions / read misses |
| `GXDBG_SLOT=1` | per-slot production tags and the collection fold decision |
| `GXDBG_SHALLOW=1` | each select arm's shallow discriminator |
| `GXDBG_RESOLVE=1` | static-resolution reads and index writes |
| `GXDBG_RPC=1` | the sys::net rpc path (graphix-package-sys) |

Fusion bugs: write a triggering test before adversarial review; a hung
test is a result.

## Working conventions

- Code review uses `// CR <name> for <name>: [tag] text (id)` near the
  code; a live CR's text is never edited: it is X'd, noted
  (`// <date> claude: ..`), re-addressed or deleted. Load `/cr-discipline` before
  touching one.
- PRs carry a concise summary, testing notes and related issues. Rebuild
  the book when docs or examples change.
- Examples in `book/src/examples/` are documentation and test corpus at
  once: each is included by a book page, and `graphix-shell/tests/
  examples_compile.rs` typechecks every one in the plain gate; TUI/GUI
  examples are run by hand
  (`cargo run --bin graphix -- examples/tui/barchart_basic.gx`).
- A new compiler walk must name the loss without it before it is added;
  the typechecker must stay instant (measure the GUI suite after typing
  changes); predictable fusion is a core value — push on de-fuse corner
  cases rather than accept them.
- Hot operators log and bottom on failure; rare stdlib functions return a
  catchable `Error`.

## Stdlib notes

- `sys::process`: children live in the opaque `Proc` with weak polling
  and `kill_on_drop`; redirects are `Pipe`/`Inherit`/`Null`; the polling
  task is the sole reaper. Shell tests are Unix-gated; the stdout and
  wait-status ones have `cmd.exe` twins.
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
