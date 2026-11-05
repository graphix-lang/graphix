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

## Architecture

**Pipeline.** Parse (`graphix-types/src/expr/parser/`: recursive descent
by hand over one cursor, `input::Input`; a rule that fails after
consuming input commits unless `attempt`ed, and netidx-value's literals
are read by its `parser` functions through `Input::value_part`) → `Expr` AST
with positions → compile (`node/compiler.rs`) → `Node<R>` graph →
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
refused, a compiler bug, never checked instead (`node::lambda::Typing`);
a lambda or call site whose id two nodes share (a default compiled at
two sites) has no row and is checked in full, which
`GRAPHIX_ELAB_AUDIT` reports.
Every type name a definition
writes, a typedef body's included, must name something by the check's
end, expanded or not (`design/env_independent_typerefs.md`). A pattern
naming a type a later statement of its block declares binds at that
type as the block ends, and one that tests or takes such a type apart
is refused (`node::later_typedef`); a name a block declares is its type
throughout the block (an annotation, a test, a cast, an abstract
constructor, pattern or payload read), never an outer type of the name
or one a `use` brings, and a statement above the declaration that needs
it as it compiles is refused (`Lexical::block_names`); where another of
the name is in view, the definition's check warns at the name
(`Env::later_block_type_note`). A top-level input (a REPL input, each
statement of its line one; a script; a dynamic module's load) is checked
and run alone, as a compile task of its own (`Input`): at its end every
open top-level cell its bindings' and value's types reach settles, ⊥
where nothing bounds it, and is `CellFact::Settled`
(`typ::settle::settle_input`); what an earlier input left open, a
function type's own quantifier included (a call still copies that), is
one type to a later input, whose decision about it is refused and not
made, naming the bindings that hold it (`OwnWrites::enter_input`,
`tvar::in_input`); a cell below a settled ⊥ is linked to it, so a later
function's write to an earlier binding is held to its type, and a
refusal a settled ⊥ causes names the binding that holds it
(`typ::tvar::settled_note`); a load reopens nothing settled
(`design/tvar_constraints.md`, Settling).

Key types: `Expr`/`ExprKind` (immutable AST; `Expr::for_each_child` /
`map_children` are the ONE child enumeration — `fold`, the seq rewrite and
the fuzzer's preorder all ride them, so a new `ExprKind` child is added
there and nowhere else), `Node<R>` (a newtype over `Box<dyn Update>`;
construct with `Node::new`), `ExecState<R>` (what an embedder owns: a
`CompileCtx` (the registry, the program's state and the compile's scratch,
which it derefs to), the runtime `rt`, `libstate`, `control` and the
cycle's `Event`), `ExecCtx<'a, R>` (the view `update`, `delete`,
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
`ExecCtx::apply_deferred` registers the references and then deletes the
discards: at the end of `check_and_fuse`, a registration read, a
runtime bind, a lazily decoded body, a dynamic module's recompile and a
core-trait hook site's build (`drop_deferred` on a failed compile);
every cycle asserts (debug) that nothing is left. `ExecCtx::unref_var`
cancels a reference not applied yet. Registration covers every node a
statement holds, selected or not: a write to a variable only a sleeping
arm reads still schedules the statement, which wake catch-up relies on.
The runtime side (`sleep`, `update`, `Deref`'s addressing) registers
directly; a builtin needing runtime state `init` cannot reach takes it
in `EvalCachedAsync::attach`. Static resolution reads only the index
(`bind_to_lambda`): a binding it lacks dispatches dynamically. A call
bound statically keeps its definition, so a later compile's `<-` to
that binding (a REPL input's) is refused, naming the call, while the
call lives: its holders (the call site, an alias `let`, the kernel that
replaced them) hold it in `CompileCtx::static_callees` and carry their
holds through the image; a module's interface binding and its
implementation's are one variable from the module's check on
(`CompileCtx::join_interface_pair`): a `<-` marks the binding it names
(its typing stays that binding's), and a call through either binds
statically only where no name of the variable is written
(`CompileCtx::rebinds`; a woken `let` keeps a value written through
either, `var_written`); a reload's replaced body holds nothing against
its replacement, and its marks go with its binding
(`design/program_image.md`).

**Runtime.** `graphix-rt` implements `Rt`: variables, timers, spawned
tasks and watch channels (`spawn`, `spawn_var`, `watch`, `watch_var`)
that packages use to feed external events in. On a multi-thread tokio
runtime a runtime runs on a thread of its own, driven through
`Handle::block_on`, every update pass inline there, so a pass's
per-thread caches are one thread's and a wedged pass holds no tokio
worker; a task of the tokio runtime keeps the thread, and its drop at
the tokio runtime's shutdown stops the runtime (a compile in progress
runs to its end first) and waits for the thread before the timer
driver stops (`RuntimeThread`). The last handle's drop
stops it too. On a current-thread tokio runtime it is a task. Event
processing is batched: all simultaneous events form one `Event`
delivered in one cycle; several writes to one variable in a cycle queue
for the next, except a level's (`Rt::set_level`), which replaces its
pending write. A node's production to a binding is delivered through
`ExecCtx::deliver`: into the event; a fire stored as delivered, stamped
this cycle, a quiet production standing with its old stamp, so a wake
owes no fire for one, whether or not it changed the value; `Notify`
picks whether a fire schedules the roots that read it. A fire, `Roots`
or `Walk`, reaches the readers the walk has not passed in its cycle, and
each one it has (in an earlier root, or in a boundary of its own root
the walk left, `Reader::passed_by`: a reader through a function value or
a reference, an earlier REPL input or a reloaded module) alone in the
next, as a write lands (`RtView::store_fire`, `Rt::late`, `Event::late`,
`read_var_by`, a `*p <-`'s reference included; a write to the binding in
that cycle supersedes it): a reader before its producer runs a cycle
behind, and none runs twice in a cycle; one feeding its producer back
is a self-loop, a step a cycle, as `x <- x + 1` is. Order is judged by
boundary, and inside the walk's own boundary by a `Deref`'s read stamp
(`prune::ReadStamp`): one that read this cycle is passed. A wake's
re-raise, catch-up's inject, a priming from the store and a born
callee's quiet arguments are fires in the event alone, never stored; an
error raised to a catch is stored as any write is, delivered the cycle
it was raised in its handler's root, else a runtime write; a quiet formal's seed, a
reference cell's quiet seed or its bottom where its place has no
element, and a fold's quiet accumulator are written to the store alone
(`design/dense_delivery.md`). A
module's interface pair is the `val`'s one variable at run time from
its join (`branch::VarAliases`): the body's `let` publishes to it, and
every read, write, store entry and registration through the
implementing binding is the val's, in one queue; a reload drops the
writes the replaced body made through its binding and had not landed,
while one through the val lands on the new body. A
running program's failures (an unhandled `?`, a hot operator's failure)
are logged under `graphix_compiler::FAILURE_TARGET`, never printed; the
shell shows that target on stderr.

**Walk pruning** (`design/walk_pruning.md`): a block statement, a select
arm body and an instance body each sit in a `Prune` boundary
(`prune::BoundaryRef`, parent-linked), which rides its resident in a
`Cycle` view when nothing marked it this cycle. A registration names its
root and the boundary around the reader (`prune::Reader`), taken from the
traversal (`CompileCtx`/`ExecCtx::boundary`, set by every `Prune` method,
copied into the compile view by `ExecCtx`'s `deref_mut`; a discard and a
pending reference keep theirs). Every this-cycle write marks its readers'
boundaries and their ancestors: a stamped `store_insert` or a standing
`store_insert_standing`, `notify_set`,
`ExecCtx::publish`/`try_publish` (never `event.variables.insert` from a
node), the cycle's deliveries; state a node polls without a variable
marks its pollers (error handlers: `ErrorHandler::polled_by`). A map traversal may skip an awake standing lambda slot before its call
when neither source nor callback moved, its only argument is the element
reference and its body is clean; defaults and wake/step views run in full,
and audit verifies the skipped call. `Prune`
is transparent to analyses (view, children, spec, `downcast_*`);
`fusion::fuse` fuses its child inside it. `GRAPHIX_PRUNE=off|on|audit`
(on by default; `Control::set_prune_mode`): the audit walks everything
and panics where a skip would have changed a production, published or
queued a runtime call;
the fuzzer's `Pair::Prune` compares against `Mode::Unpruned`; a test
fixture prunes for real in `jit` and `par` and audits every skip in
`interp` and `jit_par` (`testing::Mode::fixture_prune`), and a test that
measures pruning sets `Mode::prune` in its setup.

**Session images** (`design/program_image.md`): the shell caches the
session state before any cycle (`graphix-compiler/src/image/` over
the session core in `graphix-types/src/image/`) under
`$XDG_CACHE_HOME/graphix/registration/<build-id>/<key>.img`: the
registration entry (the package root compiled under fixed flags; key =
image format + root; the build id covers the packages compiled in, and
an executable without one has no cache) and, for a script, the program
entry (the program compiled too; key adds the script's path or an
embedded program's text, the flags and `GRAPHIX_MODPATH`; the entry
records every place its module resolution looked, each file read and each
found absent (`expr::Probes`), and a load re-verifies them). The
runtime restores the first of the program entry and the registration
entry that reads, else compiles; an entry that fails to read leaves the
session untouched, and whatever was compiled is written, except a
program that asked for fusion where the JIT does not build. `--no-cache`
disables the cache, `--warm` writes and exits (failing when it cannot);
on Unix the graphix/registration directories, build directory and entry
must belong to the effective user and forbid group/other writes, and the
cache base must belong to that user and forbid other writes (group write,
a user private group's umask, is fine: each entry is checked when
opened). The cache base may be a symlink chain owned by that user to such
a directory; it is checked before creating anything beneath it.
Managed directories and entries refuse symlinks; new directories and
files use 0700 and 0600. A run's first refusal prints one stderr line
(`cache::report`). The collector skips untrusted paths;
the header carries the ISA, the build's fingerprint (its helpers,
libcalls and version, `build_fingerprint`), refused on a mismatch as the
ISA is, and a checksum of everything after it
(`image::seal`), so a corrupted entry is a failed read, never decoded. A script compiles at runtime
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
bodies to a heap after the eager part with an instance table, which
holds each instance's own reads and its callees by instance; a call
site keeps `Callee::Imaged` and decodes the body on its first
dispatch, synchronously, through `ExecCtx::image_decoder`; an imaged
site holds its callee's flat reads under its boundary until the callee
decodes, whose nodes then register their own; a builtin callee travels as its own bytes:
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
`Comments` and `Eol` (`Aside`: always equal) for the `//` lines above,
after and below an interface item or a trait method. Lambda formals carry
`ItemComments`, with the variadic formal and empty-list comments on
`LambdaExpr`. Use-tree comments carry their prefix depth (`UseComment`)
beside the sorted imports; a single path's comments follow its imported
name and a group's follow its prefix. Ending comments merged at one
prefix become leading lines (`design/module_system.md`). A type
definition's comments ride on it, keyed by where each field or member
was written (`TypeNotes`, which packs to nothing, printed through
`typ::Noted`), and the guard reads them in canonical order. Every
comment list above and `Decorations` (not an `Eol`, which is one line)
holds `Comment`s: a line, and whether a blank line stands between it and
the line before (`Comment::spaced`), so a run of blank lines inside a
block is one and one before a block's first line or after its last is
not the block's.
**Printing is canonical unless `PrintFlag::AsWritten` is set, and only
the formatter sets it**: printed types and expressions reach
program-visible values (a cast error, a null error), so comments print
only under it; `WrittenAt` and
`str_form` are not part of a session image (`Expr::pos` and comments
are, but not a typedef's field and member comments, `TypeNotes`), and a type is shared by content, so whose written order it carries
is incidental. A tree holding a comment,
an attribute or a doc has no single-line form. What the formatter
does normalize: `i64`/`f64` literals print bare and an `f32` in its
shortest form; a datetime prints in UTC and a duration in its largest
whole unit; a value literal's string (`error:".."`) prints as netidx
prints it, a control character with no name as `\u{hex}` and any other
character as itself, where a graphix string keeps them raw; a run of adjacent
undecorated `use` statements merges into one per root and visibility, a
sorted tree with every shared prefix written once, where no item reads or
binds what another binds or reads (a glob merges with nothing); a blank line stands
around every file-level item that spans lines; primitives in a union
print first, in canonical order, and the other members a position does
not record (all but variants and named types) next, in canonical order; an item's comments print above its
attributes, and a comment under an arm's `=>` or a field's `name:`
moves above the arm or the field. Layout: a value follows its head
(`let x =`, `<-`, `name:`, `=>`) on the head's line when it fits or
opens with a bracket whose first line fits, else it moves under the
head; a lambda head that does not fit lists one argument to a line; a
lone argument that opens with a bracket hugs the brackets around it, in
expressions, patterns and types alike (`f({`, `` `Tag({ ``, `` `A(`B([ ``,
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
`format::Refused` (the formatter declined its own output) or a malformed
`graphixfmt.json`.

**Language server.** A ROOT is a project's root file (a `.gx` no other
file's `mod` reaches; `graphix-lsp/src/workspace.rs` scans) or an open
file outside every project. A root at `<crate>/src/graphix/mod.gx` of a
`graphix-package-<x>` crate is a package root, checked as the body of
`mod <x>` over the copy registered at startup: `Env::
unbind_scope_subtree` must drop everything a package registers, so a new
global registry is cleared there (the block-type registry, shared by every
copy of the env, is not: each load names its blocks apart). `--check` and the server share one
path (`GXRt::check`), which runs the check alone (`CFlag::CheckOnly`:
no elaboration, no fusion, so `#[native]`, `#[parallel]` and the def
assertions are verified by a build; `--expand` builds) and checks a script's file as one block, as it
runs, with its names at the root (`compile_script`); a root loads
through `RootFile::load`, open
buffers first, paired with its `.gxi`: a script's interface is spliced
in and its `val`s and impls checked over the script's names, by the
run as by the check (`check_script_sig`, right after the check); and under buffer overrides a path
is never canonicalized (the editor's names rule). Checks are lazy and
coalesced: a change marks its roots dirty and `ServerState::flush`
checks them when the client has nothing queued; a request will not wait
for that quiet moment and first checks the roots of the document it
names (`ServerState::flush_document`, one check at most), and one that
names no document checks nothing new; nothing is checked at startup.
The last SUCCESSFUL check of a root (`Checked`: the env and the
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
it its end (`Expr::ending`; `expr()`, the postfix loop and
`shunting_yard`'s reduce cover what passes through them); pin: `graphix-compiler/tests/
expr_spans.rs` (every node's `[pos, end)` parses back to the node, over
the examples and the stdlib). A package under development is not in the binary that checks it, so
under `lsp_mode` a package root's name is a `package::` root for the
check and an unknown builtin is a WARNING at its `'name`
(`node/lambda.rs::UnknownBuiltIn` stands in: typed by the declared
signature like any builtin, never produces); without `lsp_mode` it
stays an error. Warnings go through `Env::warn`: to `Ide.warnings`
under a check with a sink, else to stderr as before. A check ends by
warning on each import it added that no lookup went through
(`Env::unused_imports_since`; `Env::lookup_at` marks them). The server
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
its types name (a definition's check table, a kernel's type
constant, an abstract type's registration, the block's `TypeDef` node:
`DefTable::typedefs`, `record::KernelType`, `AbstractRep::named`,
`node::TypeDef`). A typedef a block declares (a scope ending in a
generated component) stays findable after the block in `Env::
block_types`, by scope and name, held weakly and shared by every copy
of the env: a lookup at a block level no lexical map holds reads it, so
does one at a compiling block's level (or a snapshot of it) for a name
the block declares later (`Env::open_block`), and one
that meets a dead entry is refused, never continued outward; a block compiled again declares the live definition, so one
source typedef is one definition; an image carries the live entries. A
scope an expression opens is named by where it is written and its
source's load (`place_component`, `Origin::tag`), never by an id, so a
restore names a block as the cold compile did (design/program_image.md,
Scope names).
`Env::seed_typedef_refs` (block types included) runs at the check's end
(an instance elaborates under its definition's env) and again right
before fusion in both modes, and every node outside a definition has
its type's names filled then (`seed_node_types`; a definition's rows at
its check, `DefTable::record`). A bound's ref resolves the same way, its
cell holding the trait (`Resolution::Trait`), filled by the seed walk at
a definition's check. Substitution by name (`Type::replace_tvars`: a
typedef's expansion, an abstract's representation, a trait method's
signature for an impl or a method value) is capture-avoiding,
since a signature finds its quantifiers by name: under a function type
its quantifiers shadow the names substituted, and one named like a
variable a substituted type holds is renamed (`tvar::under_quantifiers`).
A trait method named as a value is its
eta-expansion (`eta_dispatcher`); a dispatcher is only a call's own
function. A typedef must be contractive: every
self-reference sits under a constructor (`type T = [i64, T]` is refused
at `Env::deftype`), which is what makes the coinductive ref-pair memos
sound. An inferred type is finite the same way: a relation that would
put a cell under a constructor of itself is refused (`b <- B([t ~
b.0])`); a lower bound's check follows its direction, and the cells
on a cycle through cells and unions alone are one type, merged
(`tvar::cycle_toward`, `contains::merge_bare_cycle`,
`design/tvar_constraints.md`). A rigid cell holds only itself: an open
cell a merge would put below it merges into it, or, where the occurs
check refuses that merge, stays a lower bound (`TVar::merge_into`).
A cast to an integer type, a type-directed read's included, is an
`InvalidCast` where the value does not fit (a float drops its fraction);
a float cast to `decimal` is the decimal its shortest text names,
rounded to 28 places, a whole one keeping its point and a zero (one
too small for 28 places included) `0.0` with its sign (`float_decimal`). A cast leaves a value's runtime class
only to parse a string into a scalar (never a collection), to print a number, bool, datetime or duration
plainly (`"42"`; a whole float, and a duration's seconds, keep the
point, `"3.0"`, as every plain print does: `typ::PlainScalar`), between a number and a datetime (Unix seconds) or a
duration (seconds), from a bool to a number (1 or 0), or to read an array of numbers as bytes; a list reads as the array of its elements and a map as the array of its `(key, value)` pairs, and an array of pairs reads as a map. A union target reads by its members past every union and name in it, its primitives, written or named, one set whose parts each go in their class (`typ/cast.rs::cast_member`, `members_by_class`, `merge_primitives`), in groups: an array the source type says is one (`cast_from`) an array member it inhabits; a member whose tag the value carries; the members of the value's own class (`in_own_class`: a container's class is its elements' too, and unmarked array data is a list's; a tuple of the data's length, then a struct, first, only where its elements stay in their classes); for text the members of the class of the scalar it parses as (`parsed_scalar`: integer text too long for every integer type is the decimal it names, else the nearest f64); the rest. Within a group a member that holds the value exactly comes first, then a real one that holds it rounded, then one that drops part of it (`Holds`, `Type::tier`; a bool is held exactly by 1 or 0), and short of a dropped part one keeping every number an integer or a real as it was before one reading it across (`Choice`); canonical order breaks a tie. An infinity a finite value overflows to is none of these: a member reads it only once every group failed short of it (`Holds::Overflowed`), so `cast<[f32, string]>(1e300)` is the string and `cast<[f32, i64]>(1e300)` f32 inf. So `cast<[f32, string]>(0.1)` is f32, `cast<[i64, string]>(3.5)` 3, `cast<[i64, f64]>("3.5")` 3.5, `cast<[duration, decimal]>("3.0")` decimal and `cast<[f32, u8]>(true)` u8 1. The JIT inlines
only the casts that always fit (`lowering::inline_cast`). A struct cast
reads a field the data omits as null wherever the field's type
admits null (`typ/cast.rs`), and a string parses as an RFC3339
datetime, so what the json and toml writers emit reads back into its
type. An abstract type crosses data only through its core `Cast` impl
(`design/abstract_cast.md`): casts and reads make it with `from_value`
(a value already of the type is held), writers write its `to_value`
(`abstract_value::to_data`), only the defining module implements it
and a method it leaves out is the representation; a type with no impl,
a Rust-backed one included, is refused by the check as a cast or read
target (`Concrete`) and as a written value (`'a: Data`), and an abstract
value decodes only under an image session. JSON and TOML write one as an
object of one entry keyed by its type's unqualified name, its data
inside, and read it only so (`core::Form::Text`, `CastInput::Text`);
the data inside is in the form its reader reads: with abstract values
in it wrapped where the representation is read, bare where a written
`from_value` casts it (`core::Form::Data`). Core's `Transparent<T>` is
written as what `T` writes without its object, and elsewhere is its
representation. They write a map as an object (a table) only under the
key type `string`, its `[key, value]` pairs under any other or none;
template data (hbs, `core::Form::Template`) writes abstract values bare
and a map whose keys are scalars that print apart as an object
(`core::map_shape`).
A reference is its own runtime form (`graphix_types::reference`, an
abstract holding its cell), never a number: a cast whose source can
hold one is refused (`Type::holds_ref`), and where only an instance
knows the source, the cast yields its `InvalidCast` error; a reference
prints as `&ref`; under `Any` a reference test is refused, since
references to different types share the form (`Type::ref_over_any`);
nothing orders references or hashes them by value (`Ordered`).
Format type variables with `format_with_flags(PrintFlag::DerefTVars, ..)`;
under it a union's members that name one cell or print alike (two cells
bound to one type) print once. A hint's declaration writes a type as a
program can (`Type::written_form`): an open cell as `'x` for the binding
`x` it waits for (`node::Holders`), else as the variable a program wrote,
else none, and the hint names no type. A refused write into a payload
an earlier use decided is told both: the payload union, then a named
recursive type for a value that reads the target; one whose own
references name the target is told the recursive type alone
(`node::write_mismatch`, `design/tvar_constraints.md`).

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

**Builtins** implement `BuiltIn<R>` (`NAME`, `init()`, `EFFECT`) and
register with `ExecCtx::register_builtin::<T>()`; the Graphix signature
lives in the package's `.gx` with every argument and the return type
annotated. `EFFECT` is the one classification (`effects.rs`):
`Async` (may produce later, autonomously, or never), `Sync` (same cycle
but keeps cross-invocation state, or depends on WHICH args arrived),
`Stateless(Option<FastCall>)` (a pure function of its args, never an
effect; the payload is the direct-call entry the JIT uses, `Plain` or
`Typed` by the site's resolved return type; `None` for partial-delivery
producers and the cached data operations listed in `design/strict_fusion.md`), or `Io(Option<IoCall>)` (stateless, but it acts on the world
at each fire of its trigger argument: the printers, `exit`; an arm
holding one sleeps; `IoCall` is its result fast fn and trigger, and a
kernel queues each call for `BuiltIn::perform`, which the wrapper runs
after the run). A builtin call's facts also join those of every function
argument it is handed (it may call any back at run time, where no
analysis sees it): a definition's own, which its check records from
its body (`analysis::def_body_facts`, carried in its `DefTable`), else
async. A wrong `Stateless` is a semantics bug (the JIT's native
tail loop shares state across iterations); a wrong `Sync` is one too
where it skips a wake's recompute (`CachedArgs` recomputes only a
`Stateless` or `Io` value at a wake, through `EvalCached::quiet`, which
for an `Io` builtin computes the value without the effect), and
otherwise costs the loop. An async call site (`CachedArgsAsync`) runs its
invocations as its `EvalCachedAsync::CALLS` says: one at a time in
order (`Fifo`, the default), the latest only, a new one aborting those in
flight, though one that completed before the abort reached it still
answers, a cycle later if need be (`Latest`: `accept`, `wait`), or each at once, answered in
completion order (`Concurrent`: the tls handshakes); `delete` and `sleep`
abort every call in flight, except that a sleep lets the calls of an eval
not `CANCEL_SAFE` (a stream read or write, a cursor read, a handshake, a
file open or seek, a db or sqlite op, a spawn, a tcp connect or listen:
whatever stopping mid-way would lose) finish; nothing answers a call after
its site's sleep, so such a reply is dropped, or settled by
`EvalCachedAsync::reclaim` (a stream read's bytes return to the stream,
and later reads of it wait for that), and a `Fifo` site's next call waits
for such calls to finish. `sys::net::call` runs its calls side
by side: each answers `(RpcId, result)` once (reply, error or
`` `Aborted ``), in arrival order, one a cycle (its site's reply id
queues them). A call under an id in flight replaces that call, which
answers `` `Aborted ``; `abort_call` aborts one from anywhere (the table
is `NetState`'s, so `call`, `call_id` and `abort_call` are `ORDERED`); a sleep
abandons the site's calls and remints its reply id, so none answers.
Remote effects are not undone. Its completed result survives sleep, and
wake alone starts no call. `sys::net::callq` is `call` (the same
`RpcSite`) with no id in or out, answering in call order (a reorder
buffer of its own, dropped at sleep), and is not `ORDERED`. Bottom never reaches builtin authors: a bottomed arg bottoms the
invocation before `eval`. Line-reader handles re-delivered after EOF,
error, or a stop generation (seek/TLS upgrade) restart at the current
position; active readers ignore the same handle. Re-delivery never
rewinds. Queued lines at an unchanged position drain before the new
reader reads (`design/async_sleep_outputs.md`). Raw `Apply` authors read args through
`seam_arg`/`seam_tick`/`seam_value`. Configuration a fast fn derives
from its args (a regex, a template registry) lives in a bounded
thread-local `FastMemo`, never in state. A type-directed builtin
(`str::parse`, the reads) declares its target `'b: Concrete`; the checker
refuses the target where it settles open or holds a reference (data
can't be one; the run-time cast refuses one too) or an abstract type
with no `Cast` impl, `json::read`'s and `toml::read`'s target is also
`'b: JsonData`/`TomlData`, and so are the json and toml writers' values
(`Type::text_refusal`: every union in the type is one member beside
null, or members each carrying a tag, a variant's or an abstract type's
name (none inside the data a `from_value` reads, where abstract values are
bare), no two the same, an error no member; `Transparent<T>` carries none
and is refused over a `T` known not to be abstract, never over a
definition's own open variable; a typedef growing its parameters
as it recurses is refused), judged of the
call's settled binding (`site_bounds`; refused at the bind, a union
target would narrow to one member), a writer declares its value `'a: Data` (no
reference, function or such abstract type; open is fine), an async
builtin's `DATA_ARGS`/`DATA_RESULT` convert a store's keys and values
both ways (the arguments keep their kind markers, so keys differ and
order as values do), and the builtin's
`typecheck1` only extracts the type, never refuses
(`design/tvar_constraints.md`); a builtin that wraps a function
(`queuefn`) declares it `'a: Function` the same way. A builtin's quantifier that a
call's result and throws do not hold, and no argument the call passes or
omits can carry a value of (no function, no open part), settles to ⊥
whatever its bounds (`CallSite::settle_unread`): `publish`'s handler is
`[fn(v: 'a) -> _, null] = null` under `'a: Concrete`. A definition's call sites settle with
the enclosing statement, the signature's cells exempt.

**Queued calls** (`design/queue_fn.md`): `queuefn` uses a counting
semaphore, initially one permit, and banks unused post-birth trigger
fires. Later invocations spend those permits without a new trigger;
a wrapper site releases at most one call per runtime cycle so its
argument deltas remain distinct. Sleep preserves credit and queued calls;
a seq rebirth resets them.

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
  computation only, and the `Effect::Io` calls gated on their trigger's
  fire. A builtin fuses iff its `Effect::Stateless` carries a `FastCall`
  or its `Effect::Io` an `IoCall`; one that performs under a select arm
  fuses there only when its trigger fires at every entry (it fires with
  the scrutinee or the arm's binds, through a nested select's scrutinee
  only and never a function's result; a performing callee performs only
  on its formals, `analysis::io_follows`; and no arm has a guard,
  `lowering::arm_io_blocker`), and a tail recursion is one native loop
  only when its Io and its handler-ful raises follow its formals
  (`lowering::site_memory`: a constant-derived raise is an
  activation's at its birth); `?` fuses with or without a covering catch (a handler-ful
  raise is queued and delivered after the run through the same path
  `Qop` uses), except a handler-ful `?` reachable under a select arm,
  callee bodies included: its raise may be owed to a fire the arm
  missed asleep or to a constant at its first entry, and a kernel remembers
  neither, so it node-walks (`lowering::arm_raise_blocker`), as does a
  guard whose handler-ful raise or performing call does not fire with
  its arm's binds (`lowering::unfollowed_guard`): a kernel runs its
  prologue guards even where unreached, with those binds bottom; the
  node-walk runs only guards the chain reaches; only a
  `never(..)` arm is a
  standing bottom (its args are still emitted and consumed), any other
  bottom-typed arm body runs; everything else — stateful/effectful builtins, `connect`,
  `~`, `Any`, `Catch` — node-walks, transitively. A kernel's only
  cross-invocation memory is the firing boundary (prev-length words,
  first-call words, per-site/per-activation blocks; in a loop each is
  the slot's, never shared by its iterations) and its loops' cost sites,
  which decide nothing a program sees; no replay caches, no selection
  memory. The runtime loans a kernel exactly `KERNEL_ABORT`,
  `KERNEL_PANIC`, `KERNEL_ENV`, the deferred raises and Io calls
  (`emit_helpers::Deferred`), the self-block counters (`SELF_BLOCK_GEN`
  and its kin), the core-trait value hooks and the fork
  mode (`par_loop::ParLoan`): a map-family loop at a kernel's top level
  runs as chunks of its slots, in order or forked
  (`design/parallel_eval.md` §10).
  `#[native]` asserts zero node-walk residue at a source location and is
  THE advertised performance model (an argument a fused call is fed by
  the node-walk is residue; bound outside the annotation it is an
  input). A registry attribute inside a definition body stays pending until
  an instance is built, including a dynamic call or a later REPL input;
  the definition alone does not require fusion to dispatch it.
  `#[sync]`/`#[async]`/`#[pure]` assert analysis facts
  (`#[pure]`: nothing that makes an arm sleep but `dbg`, which warns),
  and
  `#[tail_recursive]` analysis's gate plus, for every instance a compile
  or a run-time bind builds (a later REPL input's included), the loop
  fusion built: no call to it left in the node-walk, a dynamic call's
  included, but one dispatching an instance whose body is the loop's
  kernel (`analysis::check_tail_loops`; a bind it covers is fused until
  its failure is reported, once; unchecked where fusion does not run). A
  run-time bind fuses a collection slot's instance (its prototype's
  kernels) and one an assertion it reaches covers, no other (a function
  value called with no attribute in reach node-walks); it does so as its
  runtime does, whatever its site's flags (the package root's have fusion
  off, a site a collection or a builtin builds has none): a runtime whose
  flags disable fusion disables its context's (`FusionCtx::disable`).
- **Bottom is dense** (`design/dense_delivery.md`,
  `design/representable_bottom.md`): `update` returns a `TagValue`
  every cycle — `Fired(v)`/`Stale(v)`/`FreshBottom`/`StaleBottom`, the
  orthogonal fired×bottom algebra. A standing bottom re-delivers
  `StaleBottom` and never re-fires consumers; bottomness ORs over
  consumed productions. In the JIT the bits ride each param's disc
  (`STALE` bit 61, `TAINT` bit 62).
- **Organic firing** (`design/organic_firing.md`): a node fires iff a
  consumed input fires; nothing stores a previous value or selection to
  decide a tag; `uniq`/`filter`/`~` are the cadence tools. A select emits
  per fired input — scrutinee delivery, a CONSULTED guard, or the taken
  arm's own production; same-arm re-matches emit the arm's current
  value. A delivery to a function-valued callee fires its current result
  even for the same definition, keeping that call's instance and state.
  A constant fires where it is born (`View::born`) and stands
  stale everywhere else; every argument-less literal (`` `Tag ``, `[]`,
  `{}`) is a constant (`node::produce_constant`). Kernel outputs
  fire only when an input feeding them fired; collection loops fire on
  resize, a fired slot, a fired empty source, a fired fold carry, a
  source back from bottom, or, for filter and find, which read the
  elements, a fired source of the same length. A bottom source forgets the length and runs
  no slot: its slots pause as a deselected arm does and wake, caught up,
  when it returns; a kernel loop runs no iteration over a tainted source
  (`scaffold::slots_run`).
- **Bottom scrutinee ⇒ bottom select.** No stored-selection ride of any
  kind; `hold` on the scrutinee is the tool. A STALE-PRESENT scrutinee
  still routes the taken arm's own fires. **Consulted-guard rule**: arms
  are consulted top-down, structure then guard; a consulted guard whose
  channel is bottom makes the selection undecidable; a never-produced
  guard is unknown, not false; a guard runs only where the chain reaches
  its arm (never on a value an earlier arm claimed, or one its arm's
  structure refuses): any other sleeps, as an unselected arm does, and
  wakes caught up when the chain next reaches it. `&&`/`||` are strict (`false && ⊥ = ⊥`).
- **Sleep is pause, not reset; a wake is a resume, never a rebirth.**
  The views (`View`): `Birth` (standing values read as delivered,
  constants fire), `Wake` (a resume: standing values read stale,
  constants stand, a `<-` target keeps its value), `Step` (born into a
  world that stands: constants fire, standing values read stale), and
  nested views join (`View::join`). An arm's first entry inherits `Birth`
  from an enclosing birth and is otherwise a `Step`; every later one is
  a `Wake`. A seq step entered for a run is a `Step`. Its rebirth resets
  the entry history of selects inside it, so an arm's first entry in each
  run is born even when its scrutinee arrives later. A
  `let` re-raises, fired, a write to it that landed while it slept (a
  fire no reader saw: `Bind::reraise_writes_while_asleep`), to the
  readers that slept with it alone: its block takes it back after its
  last statement (a module, with an interface or without, leaves that to
  the block around it, whose later statements read its names); a `let`
  that re-raises marks its block (`node::Lets`), which reads the mark as
  its update ends, so an update after no re-raise visits no `let`. A
  reader outside (an escaped `&mut`) sees the write once. At the wake
  the initializer's fire by catch-up replaces such a write, whichever
  came first (a sample whose argument alone fired asleep does not fire); a
  write landing after the wake's cycle stands. State
  survives an arm's sleep (`Held` residents at the select scrutinee,
  pattern guard and `~`'s arg; `CachedVals` staging; collection slots;
  a `<-` target's value, which only a fired input of its initializer
  overwrites; the restart builtins' latches and counts; a `queue`'s
  contents and credits, a delivery it sent but had not received taken
  back). **Wake catch-up** (`design/wake_catchup.md`): a reselected arm
  recomputes from the world as it stands, reading standing values STALE;
  the only events it re-raises are the fires no selected reader saw,
  a late fire to a reader in an arm that did not run included
  (`TrackedFires::observe_late`), once, at their current value (one fire bit per input per select, shared
  by consulted guards and arm bodies and consumed by whichever arm reads
  it, an arm's guard and its own body seeing it alike; a destructuring let's
  siblings are inputs of their own; a pattern bind, of this select or
  an enclosing one, is a facet of its scrutinee's delivery: reading it
  consumes that input's bit, and it is never delivered; an arm a guard's
  flip selects binds stale). An input an arm learns as it runs (a callee
  a function value binds, a reference's new target) is caught up by its
  write stamp against the arm's falling asleep, walked or not, and, where
  a select around it tracks the input, only while the innermost such
  select holds its fire bit, which the evaluation then consumes
  (`node::wake::CatchUp`); a standing
  entry keeps its stamp (`branch::standing_stamp`). An input that
  went bottom during the sleep is bottom at the wake: a bottom input
  SETS a node's resident (`TagValue::set_bottom`), never rides it, and
  every wake refresh stands a stale bottom like a value. Sleep state is
  LOCAL: every skip-owning node owns a `slept` bit its `sleep()` sets and
  its next update takes — no ExecCtx globals (parallel compile and a
  parallel evaluator stay possible). A builtin's state starts afresh
  only where it is born again: a call site whose update after a sleep
  is a birth calls `Apply::reborn` (a seq step's next run, a call it
  issues). A labeled
  DEFAULT is born with the binding and delivers
  FIRED at a fresh callee's first dispatch. Sleep aborts an async call
  in flight (one not cancel-safe finishes, unanswered) and
  the wake restarts nothing: only an argument's fire
  starts a call, and a call's result survives the sleep; an event
  source (a timer, an idle wait, a subscription, a line reader, a
  server) pauses (a timer's run resumes at the wake, a pending idle
  wait too, a standing interest re-attaches) and its output, the last
  event, does not survive (`design/async_sleep_outputs.md`). A seq
  machine's run is aborted by its arm's sleep (`pc` to idle); a
  `seqq`'s queue keeps its requests and the aborted run's credit is
  returned at the wake. An arm with no state, no
  effect (an `Io` call included), no raise to a handler (an event, as a write is) and no recursive call
  skips `sleep` and is not updated while untaken, and is slept at its
  re-entry so it recomputes from what stands; a fused arm is judged
  by the region its kernel replaced (`analysis::RegionFacts`), so fusion
  decides nothing here.
- **Activation state** (`design/activation_state.md`,
  `design/recursive_activations.md`, `design/atomic_recursion.md`):
  held state never decides output bottomness; activations ARE
  collection slots; every call is an activation, tail or not: the
  node-walk has no frames, and the JIT runs a STATELESS tail recursion
  as one native loop held to the activations' answers
  (`design/tail_calls_are_calls.md`); instances are retained
  unconditionally; shrink = delete (a depth not reached this cycle is
  deleted; re-reaching it is fresh), except that a pure callee is kept
  asleep and reborn as a fresh bind when re-reached; each exit of the
  recursion past the kept edge (its select's chain runs and takes another
  arm) deletes its deepest kept level, or 1% of the levels it kept where
  that is more, all of them by the hundredth (`callsite::Kept`); a
  reborn select leaves the arm it held as
  a deselect does unless its chain takes it again. No depth limit;
  evaluation is atomic within a cycle; containment stops the runtime for good, busy or
  idle, never to resume (`Control::stop`, its cause read back by
  `Control::stopped_by`): the interrupt (`GXHandle::interrupt`, `Ctrl-\`),
  the stack budget (`GRAPHIX_STACK_BUDGET`) and the activation budget:
  live activations (calls, slots, kernel blocks) bound per runtime,
  unlimited by default, set by an embedder
  (`Control::set_activation_budget`) or `GRAPHIX_ACTIVATION_BUDGET`. A
  stopped runtime's handle fails every call; an embedder mints a new
  runtime, the shell's script exits 130 at Ctrl-C or `Ctrl-\` and its
  REPL's `Ctrl-\` (SIGQUIT; Ctrl-Break on Windows), idle or busy, replaces
  the runtime with a fresh one; the REPL's Ctrl-C stops nothing, leaving
  the display with the session kept. While a text display shows or an
  input runs the terminal sends both keys as signals; a TUI display reads
  them as keys. Ctrl-D exits. Kernel
  interior memory (`design/kernel_instance_state.md`) gives one compiled
  body the interp's per-slot/per-activation multiplicity for exactly the
  state that decides firing; only a site's first-ever dispatch, and a
  loop slot's first iteration, is an init view.
- **Closure lifetimes**: a function value keeps the bindings it captures alive
  after their owning slot or activation is deleted, including captures used by
  nested literals and labeled defaults. `DefTable::captures` records names;
  the emitted `LambdaDef` pins their instance ids through `reference::Captures`.
  Deletion removes the names from the current scope but retains the type records
  and stored values until the last owner drops. The initializer does not survive.
  An orphan that only orphans' values keep (a self-reference, a `let rec`
  closure) is collected (`reference::Lives::collect`, over netidx-value's
  `Value::edges`; design/place_references.md). Pins:
  `lang::functions::closure_outlives_collection_slot*`, `lang::orphans`.
- **`let rec` is monomorphic-recursive**, and a binding something writes
  (a `<-` target, a `&mut`'s referent, as the resolver names it) is not
  generalized: its uses and writers share one instance of each function
  it holds (the value restriction, `CompileCtx::monomorphic`; a name
  aliasing a generalized binding, `let g = f`, holds an instance of its
  own, as does a written name a pattern takes apart). A write compiled
  before the binding's check makes its check take one instance
  (`Bind::typecheck0`); a writer in its block, a lambda body's included
  (a body compiles at its definition's check), and a module's in it
  through `super::`, a name it imports from there or a `package::` path
  through the block's module, `self::` in a module's top block, keeps it
  from being generalized at all (`node::written_lets`: the nearest
  binding before the write; under a nested block's glob only a lambda
  body's write, the rest left to the resolver). A binding
  generalized before a write to it compiles (an earlier REPL input, or a
  function body's write the walk missed) stays generic: the
  write is held to a copy of its type with the variables rigid, a
  bound's included, each function type's apart whatever their names, and
  a `&mut` of it, or of a place whose type holds one of them, is
  refused
  (`Env::poly_binds`); a `let` nothing writes whose value
  holds a lambda or a generalized binding's value in a data literal (a
  struct, its update, a tuple, a variant, an array, a list, a map:
  `bind::holds_function`) is generalized too: each read copies the
  closed definitions' signatures its type holds (`Type::scheme_read`), so
  an annotation, a typed formal or a site's settle decides only that
  read's copy, never the definition; every read in such a holder's value
  (a call's argument, a block, a select arm, a lambda body included)
  copies through one cell map, so reads of one binding share one copy
  and a chain of holders holds one copy of its last link
  (`bind::ValueReads::Shared`, `ReadCopies`), but an annotated `let`'s or
  one binding a written name copies at each read, and a `let` forwarding a
  holder gives each name a copy of its own beside a written one; two
  lambda literals in one array or list meet, one type; a type written
  in a body (a `let`
  annotation, `never<T>`, a select arm's type test, a nested lambda's
  signature) names one
  variable per name, and a name a definition around it declares, and
  the written place does not declare itself, is that definition's
  variable, any other the written type's own: generic until a cell of
  a definition or the top level merges into it, which claims it there
  (`tvar::Level::Written`; `lambda::body_annotation`, `enclosing_tvars`; a labeled
  default's, the definitions' around its own, `LambdaDef::in_signature`;
  in an instance, the instance's types under the definition's names,
  `lambda::instance_sig`, the names its signature writes alone, never one
  a body's annotation gave a cell its check reached); a quantifier
  a definition or a written `fn<..>` declares must be held by its
  signature (a parameter, the return, the throws, a held quantifier's
  bound), else it is refused (`FnType::check_quantifiers_held`), and a
  quantifier's bound (a definition's, a written `fn<..>`'s, an impl
  head's) may not reach it, directly, under a constructor or through
  another's bound (`'a: 'a`, `'a: Array<'a>`, `'a: 'b, 'b: 'a`;
  `check_bounds_acyclic`): a recursive bound is a recursive type; a def's declared tvars are
  rigid in its body check: none binds to a concrete type or ⊥, no
  two unify (`contains.rs` Distinct) and none escapes into a cell outside
  the definition (`DefGate::check_declared_kept`; a variable the signature
  only names, undeclared, is lowered with that cell, and the definition is
  monomorphic in it), so a function type without
  `throws` (which throws nothing) refuses one that throws `'e`, while a call instantiates them
  freely; a call copies only what the callee's definition owns once
  its gate is closed (or not yet open): a cell shared with the
  environment, or an open gate's, is shared (`let t = |x| x + y` is
  monomorphic in `y`'s cell; design/tvar_constraints.md,
  Generalization); a labeled default is checked at the definition
  against its parameter's type, or a declared tvar's constraints, a
  variable nested in the type taking any instance (`check_defaults`),
  and again at each omitting site by the check, where it may narrow
  that site's cells: against the site's own view of the parameter, the
  type the instance runs with, and only where a fn type hides the
  label against the callee's parameter as that type instantiates it
  (arguments, result and throws) and the label of the function value it
  hides, which the default narrows; compiled at a bind, it takes its
  types from its definition's check, as an instance's body does (the
  definition's `DefTable` holds the defaults' rows,
  `InstanceTypes::of_default`, `DefTable::record_defaults`). A default whose type holds an enclosing
  definition's open cell requires a closed parameter type (annotate the
  factory's parameter or the defaulted parameter); an enclosing definition's
  declared variable is closed: such a default fits its parameter whole, for
  every instance of the variable (an unannotated parameter takes its type),
  and a site outside that definition judges it over a copy of the scheme
  (`callsite::Judge`). A call's type variable that only
  data positions or read-only references hold settles to the widest argument
  whatever the order, one a callback holds to the first, and one an abstract
  type, a type application or a writable reference holds is fixed there, those
  arguments checked first whatever the order the call writes (a
  transparent typedef is its expansion)
  (`callsite.rs::Widening`, `holds_invariantly`); a function type with
  type variables of its own (a written `fn<'b: C>(..)`, a trait as a
  parameter's type, `fn(c: Collection)`, a constructor-trait quantifier)
  stands only as a declared signature's top (an interface `val`'s, a
  trait method's, an impl's or a default body's method `let`, a lambda's
  own declared variables): anywhere else in a written type it is refused,
  naming the fix (`typ::QUANTIFIED_FN`: `Type::rewrite_trait_args`, and
  `refuse_quantified_fns` for a cast, a type test and a catch's
  annotation); a function stays generic only where a binding names it (a
  `let` whose value, past parentheses and a block's final expression, is
  a lambda, a generalized binding, a field or tuple read of one
  (`let j = s.g`) or a data literal holding these, a write to a
  generalized binding, a labeled default, a call's callee by name, an
  input's value: `bind::names_generic`, `holds_function`, `name_value`;
  a reference, `&f`, is not one): a lambda no binding names takes its
  enclosing level's cells at its gate's close (`Lambda::named`,
  `DefGate::close`), and a read of a generalized binding no binding names
  is one instance (`Ref::naming`, `FnType::one_instance`,
  `Type::instance_read`), so an element of a collection a call returns, a
  callback's result or a field of a call's result is one type its uses
  decide; a call copies a quantifier only where its level says so, as any
  cell, and a containment holds rigid a scheme's quantifiers alone; a
  quantifier an earlier compile
  task left open is one a later task's `contains` compares as a fresh
  copy on either side and never writes (`FnType::has_earlier_quantifier`:
  parallel elaborations share the check's cells; not in a top-level
  input's own check, to which an earlier input's is one type), and an open quantifier
  never binds to its bound, no site's settle decides it; a copy of a type
  keeps each written variable name, so a parameter merged into a declared
  quantifier still names it (`TVar::named_as`), and a name whose cell a
  merge forwarded stands for the cell at the end of the link: a copy names
  that cell's image after it, and a rigid gate holds that cell
  (`TVar::forwarded_as`, `open_rigid`; `design/tvar_constraints.md`);
  `Any` binds an open
  cell only in a contravariant position (a `|v: Any|` callback fixes its
  formal's type; `let x: Any = y` decides nothing of `y`);
  union collapse requires strict tvar identity; in a union checked against
  a union, an open member takes only the members its known siblings leave
  (`[&i64, Error<E>] ⊇ ['b, Error<E>]` gives 'b `&i64`, never `Error<E>`,
  all of them when none are left or what is left refuses it), and where two
  of its members hold open cells (`[Array<'a>, Array<'b>, null]`) the other
  union's members go in as written, else deepest first, whichever order
  places them all on copies (`Type::place_each`); a free union member stays free (a type test over an
  untyped parameter binds it: annotate the parameter, not the arms); float comparison is a total order (`NaN ==
  NaN`, below every number) so `Value` is map-key-able, and numbers of
  different types (under `Any`) order by value, exactly, ties by type,
  every number before every other kind (netidx-value's `Value::cmp`); checked arith
  (`+?` …) yields a catchable `ArithError` naming its cause, except
  over floats, which follow IEEE and never fail (inf, NaN), unchecked
  `+ - *` wrap, integer div0 and `MIN / -1` bottom, `x % -1` is 0; indexing is bounds-checked through shared helpers on
  both backends; `$` and handler-less `?` report a fired error operand,
  with no report-count or at-least-once guarantee inside a fused region:
  an unreached prologue guard may report, a constant in a late arm may
  never report, and native tail depths may share one report
  (`design/strict_fusion.md`; `tests/fusion_diagnostics.rs`).
  Unchecked-arith diagnostics, `sum`/`product`/`divide`'s
  included, are node-walk-only (debug with `--no-fusion`).
- **Emit contracts** (`design/distributed_jit.md`): effects de-fuse,
  never silently skip; owned select-arm binds drop at every arm exit
  (run `leakcheck` when adding an owned-local class); an owned local's
  words are defined only through `ValueVar::define`, which also writes
  the stack slot its drops read (`abi.rs`); a bottom is a
  production whose STALE bit follows the same trigger fold as a value
  (`nodes::emit_bottom_placeholder` takes the governing disc); a node
  produces, placeholder included, the form its type FREEZES to, which is
  how its readers take it (`BodyCx::node_kind`); a pattern reads a
  value at its arm's predicate's type only where the value is known to
  be of it (`SelectScrut::Composite`'s `exact`), else it tests the
  value's words; kernel
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
  A function past `jit::LARGE_FUNCTION` instructions compiles with
  Cranelift's single-pass register allocator: linear compile time,
  worse code, one region still.
  One JITModule + 256MB
  arena per generation (reserved, committed as code installs, on every
  host: `emit/arena.rs`), built on the first fusion (a fusion-off context
  never builds one); an install that fails retires the generation, and
  on exhaustion the link reinstalls its records in a fresh one, twice the
size while a fresh one cannot hold the batch; a
  generation's code is freed when it and every kernel installed into it
  have dropped (each `WrappedKernel`'s `KernelEntry` holds its code). A
  deleted definition's kernels and bodies, and the records no kernel
  reaches, are forgotten at the next link
  (`FusionCtx::link`); only a cached body's ids stay named. A link that
  retired a generation, or left the current one mostly dead (4MB
  committed, under a quarter live), marks a relocation: at the next
  cycle's start (`GX::do_cycle`, `FusionCtx::relocate_kernels`) every
  live kernel outside the current generation is reinstalled there from
  its record and its entry repointed, so the old module drops. No JIT
  code runs outside a cycle's update pass, and a kernel reads its entry
  at each call (`design/jit_memory.md`). Pins:
  `graphix-rt/tests/rt/jit_retention.rs`, `jit_arena_rotation.rs`. Pin:
  `graphix-shell/tests/shell/jit_arena_rotation.rs`. A two-word value never
  crosses the helper seam by value: a pointer in, an out-pointer back,
  through `HelperRefs::call` alone (`design/helper_abi_portability.md`),
  so fusion runs on every host Cranelift supports, Windows included.
  Kernel ABI (`KernelSig::abi_params`): the context words, then a
  (disc, payload) pair per param in source order (formals, then
  captures); recursive types, abstract types, primitive
  unions (a union naming primitive typedefs, `Number`, is one) and the
  primitives with no register form (varints, decimal,
  error) are opaque 2-word values (`design/unified_value_abi.md`).

Coverage today: scalar arithmetic/comparison/logic/casts, producers and
accessors, string interpolation of every part kind, `?`/`$`, a bare `null`
wherever a value stands, the eight array HOFs as native loops (nesting
included; filter_map's unwraps a `` `Keep `` result and skips a null),
structural select destructuring with scalar and variant
payload binds, owned binds of a nullable's payload, of a slice's rest,
head or whole (`[x, tail..]`, `all@ [_, ..]`) and of non-scalar elements,
tag tests and binds over a primitive union, every pattern at an
element, a payload or a list head as at the top (a binding or-pattern
included), a tuple, struct or slice pattern over a union holding one
member it may pass, scalar literals over an option, a result
or a union, `never()` arms as bottom
productions of the merge shape, or-patterns, list patterns, type tests
that hold the scrutinee's type, a written tuple, struct or array test
over a composite whose type it does not hold, any union of encoded members as one
value (`` [`A, `B, null] ``, a list beside an array) with its written
type tests, a union naming union typedefs as the flat union they name, tail loops
over any kernel param
kind, every fast-fn builtin and non-inline cast, cross-kernel lambda
calls, trait default bodies, a self-recursive instance's body, its
self-calls calling its own kernel, wherever an instance's body fuses
(`fusion::fuse_instance_body`: a compiled instance whose call does not
fuse as a region, and the loop a collection's callback, or a function
value an assertion covers, binds at run time), a collection callback's
call of the function whose body runs it inline being that function's
self-call, each loop slot rooting its own activation, and so a callback
that is that function's bare name (`array::map(xs, t)` in `t`), whose
slots make the call, the collection's own instance's included where the
function is its callback (slot-root chains, `design/kernel_instance_state.md`). A subtree that does not fuse whole
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
subtrees onto the runtime's own evaluation pool (`Control::eval_pool`,
built at its first fork, `GRAPHIX_EVAL_THREADS` workers; runtimes in
one process never run each other's parts): a block's runs, a call's arguments, a
constructor's fields, an operator's operands, a collection's slots, a
server's requests (`http::serve`, `sys::net` rpc: an instance of the
handler per request, `branch::fork_instances`, forked whenever two are
independent, whatever their cost) and a kernel's top-level map-family
loops (chunks, `fusion/par_loop.rs`).
Branches see the runtime through branch views (`RtView`/`ForkRt`,
`Layered`, `CxView`) and merge back in program order, so a forked cycle
computes what the serial node-walk does. Siblings are independent iff
neither reads what the other publishes (`analysis::plan_block`, an
earlier reader taking a later fire next cycle, through a reference or an
unresolved call only one a reference names or a lambda captures,
`analysis::escaping`, or one a call fires in a callee whose body makes a
reference or a lambda, or that has no static target; a module, trait or impl
statement is a run of its own and a catch runs last) and they do not
both reach an `ORDERED` builtin
(in-language state shared across nodes, `queuefn`; a call with no static
target counts as one, a slot site's own call excepted). External effects in
parallel branches happen when they run, unordered. A seq machine runs
serially inside; runtime compiles run in compile tasks
(`branch::compile_each`). The mode is the runtime `Control`'s
(`set_par_mode`), from `GRAPHIX_PAR=off|auto|force`, `auto` by
default: `Auto` forks where `cost.rs`'s tick histograms say a side
costs the calibrated threshold (4x the pool's wake latency) and not
while every worker has a part; a collection's slots fork only where each
half also pays what its forks took beyond their parts, and run in order
after three forks in a row no faster than in order (`SlotSite::forked`);
`Force` forks at every fork point (tests, the fuzzer). `#[parallel]`/`#[parallel(g)]` forces the fork points
under it (a compile error where nothing forks; callees excluded),
`#[serial]` inhibits them, callees included. The stack budget is per
cycle, across workers; the compiler never pins threads.

## Testing is differential

- `run!` (`graphix-package-core/src/testing.rs`) runs a fixture in
  each `testing::Mode`: `interp` and `jit` (serial unless `GRAPHIX_PAR`
  is set), and each again with every fork point forked (`par`,
  `jit_par`), asserting its predicate in each (and, in debug builds, that
  `interp` and `par` ran no fused code); nothing compares the
  modes, so a predicate pins the value exactly and a refusal names its
  message (`testing::refused`). The value must be of its static type in
  every form it carries, and the predicate sees it as data
  (`testing::observed`: markers stripped). `interp` also checks the program alone
  and fails unless the check refuses exactly when the build does
  (`; build_only` exempts a refusal only a build makes: `#[native]`,
  `#[parallel]`, the definition assertions). `FuseExpect::{Jit, None}` asserts
  WHETHER anything fuses, bidirectionally; `#[native]` asserts that an
  expression does. Every mode, `testing::init`, the `eval*` helpers,
  `compile_error` and `Fixture` restore the package root from the image
  the process compiled once per root, as a warm shell start does, and
  fail where it does not restore; a start through `testing::config`
  compiles it, and `GRAPHIX_TEST_ROOT=compile` compiles every one. A
  `run!` fixture is a `testing::RunFixture` value its four tests hand to
  `testing::run_fixture`, compiled once, never a driver per fixture.
  `GRAPHIX_FUSE_AUDIT=1 cargo test -- jit --nocapture` prints the audit,
  and in the forced modes a `PARAUDIT-NOFORK` line for each fixture that
  forked nothing (a finding where the fixture has fork points).
- **graphix-fuzz** (`design/graphix_fuzz.md`): node-walk vs JIT with a
  per-cycle trace oracle, each engine also no-cache vs cold-image vs
  warm-image (`GRAPHIX_FUZZ_SESSIONS`) and forked against the serial
  node-walk (`Pair::Par`: the node-walk at `Force`, the JIT at `Auto`
  over a fixed tiny threshold, `cost::fix_calibration`), and a program
  both builds refuse
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
  loud; the count is the program's compile alone, which runs no cycle
  (`GXConfig::compile_counted`), so a budget abort has its count too.
  After an INTENDED fusion change, `fusecheck
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
  A box in the rustc-dist build farm runs its farm profile `soak` while a
  campaign runs there and `default` after a stop leaves no survivor
  (`soak.sh`'s `farm_profile`).
  A stack-budget abort is a `Timeout` outcome (containment). Construction
  timeouts agree only with construction timeouts; beside a runtime timeout
  the construction side is retried at the slow budget and the pair cannot
  be recorded as a trusted containment (`design/graphix_fuzz.md`).
- A semantics change is not landed until it has soaked; gates are not
  the fuzzer.

## Language features (current)

- **Sampling**: `e ~ v` is `v` at each fire of `e`, banking one trigger
  however many find `v` absent, paid once when `v` arrives (the `seqq`
  lowering's own `BinOp::BankedSample` pays each); `e ~! v` is strict
  (bottom, no bank). Neither sample treats a bottom trigger as a fire: both
  keep their last output until another trigger or a banked payment. A sample
  keeps its bank through an arm's sleep, but a seq step's rebirth clears it. A connect
  writes when its RHS fires; a constant RHS in an arm fires only at the
  arm's first entry, and the definition's check warns of it (and of an
  effect call reading no variable, or a `seq` with no trigger, there:
  `select.rs::warn_birth_only`; not in a seq step, born at every run,
  `CFlag::SeqStep`); an action at every entry samples the
  event that selects the arm (`k@ .. => x <- k ~ v`), or is a seq.
- **`never<T>()` is syntax** (`ExprKind::Never`): typed bottom or `T`;
  args stay live and are consumed. An unannotated `let` over a ⊥
  initializer takes its type from its first use, a writer or a reader;
  a later writer must fit it, and a refused one is told the declaration
  that holds both (`node/mod.rs::write_mismatch`).
- **Sets and coverage**: one walk (`select.rs::Reach`) decides what
  reaches each arm: its binds narrow to it (over a union, each to the
  union of its parts over every member the arm can match, never to the
  first member's), an arm that can match none of
  it, or that tests what an earlier unguarded arm tests, is dead, and the
  select is exhaustive when nothing reaches past the last arm. Select exhaustiveness is enforced; slice-pattern
  length ladders count as coverage, one ladder per array or list member
  (a slice arm claims its lengths of each member whose element type its
  elements cover), and exact-length slice arms pool their elements' heads per length
  (`[true]` with `[false]` claims length 1); a `null` literal covers `null`, a
  constructor at a union position the members it takes, and a written test
  over a closed type the part of it its structure takes
  (`StructPatternNode::matched_part`: `` ([`A, `B], i64) as (`B, n) ``
  covers `` (`B, i64) ``); a collection type test (`Array<T> as`)
  narrows later arms only of collections whose every element it
  covers, since a mixed one that fails it may still hold a `T`;
  bool literals, variant heads
  (payload irrefutable) and or-patterns of them pool per position
  inside composite patterns, keyed by path (a partial struct pattern
  against the one member holding its fields, the rest any value; an open
  scrutinee's members are its structural bound's; a union position's one
  tuple or struct member holding no open cell is a head beside them, its
  own positions counted under that head alone; an open cell at the
  position, a declared variable included, is a head of its own that only
  a name or `_` there admits, `Head::Open`, as is a primitive member's
  value but `bool` and `null`, `Head::Rest`), and an arm under
  a type test pools only a member the test holds; a group whose combinations of
  heads pass 1024 (`MAX_POOL_COMBINATIONS`: eleven independent bool
  positions) pools nothing, so its arms cover nothing by pooling; an or-arm narrows later arms per
  alternative; a structure that matches anything is a wildcard only over
  a scrutinee its shape covers, and a written test covers a declared
  variable held rigid where it covers every member of its bound; set
  coverage distributes over product
  heads (`` [`P(A), `P(B)] ⊇ `P([A, B]) ``), and in every containment a
  rigid cell at the position it splits is its one typed bound's members,
  each also taken by a part holding the cell, else a member of its own,
  and one in that bound splits the same way, an open cell there held
  rigid while it probes (`Type::covers_by_parts`); a probe in progress for the
  same scrutinee ref claims nothing on re-entry. **A type still
  undecided at a select's end is one unknown type, equal only to itself**
  (`Select::check_undecided`): once the arms have decided what they will,
  an open cell the scrutinee held before them (never one their own
  patterns brought: a bind's, a `_`'s) in what reaches an arm (an earlier
  unguarded arm may take its member whole) that the arm tests, a
  pattern other than a name or `_` standing over its values, is refused,
  naming the binding to annotate (`StructurePattern::meets_undecided`,
  the walk the declared-variable rule below takes), and so is any open
  cell of a written test that its structure tests, and a written test
  that takes such a member only whatever the cell becomes, not with the
  cell one type of its own (`written_tests_undecided`); a name over it takes
  the open part beside the members' others (`TVar::narrow_per_member`).
  A cell an arm's pattern decided (the arms' settle, a completion, the
  narrowing, the coverage) counts as one still open there, a binding the
  decision rewrote read as it was (`Deciding`,
  `StructurePattern::meets_decided`, `written_tests_decided`), except a
  whole open scrutinee, which the arms decide together, unless an arm
  holds a partial struct pattern; a pattern there that only takes apart
  every value of the shape it decides (a tuple, a struct naming every
  field, a slice or list with a rest, an abstract payload, of names, `_`
  and such patterns: `StructurePattern::only_destructures`) decides that
  shape as an annotation would and tests nothing, except beside a member
  of that shape, where it chooses between them. An arm the coverage
  finds dead has the arms up to it judged so first, against the cells
  still open and those decided (`still_open`); any other error in an
  arm's check is that error.
  Exhaustiveness holds those cells rigid on copies first
  (`Type::contains_holding`), so a part only names and `_` meet covers
  whatever it becomes, else where the cover decides them. What reaches
  the next arm (`Type::diff`) and `could_match` unfold a pair of
  named types' definitions (arguments ignored) once on a path, a pair
  with a side that is no named type uncounted; met again, diff
  subtracts nothing and could_match answers an overlap
  (`setops::UNFOLDINGS`), as they do past `setops::WALK_PAIRS` pairs
  (either side named) in one walk: a wider residual, so fewer dead arms
  are found and a missing case may be refused that `_` fixes. What reads
  the part a difference left out (`?`'s strip and raised type) and
  `?`/`$`'s result type split the operand by its members, each name
  expanded once per parameter list (open cells by identity), where the
  budget widened it (`Strip::taken`,
  `Strip::left`). A partial
  struct pattern completes against the one member it can match
  (`StructurePattern::complete_type_predicate`); members told apart only
  by undecided cells are two members, refused unless what reaches the
  arm leaves one: annotate the cells or name every field.
  Refutability is judged
  against a type (`StructPatternNode::covers`): a constructor pattern
  covers only a position whose type is that constructor, so under a
  written type (a type test, a `let`, an abstract payload) one at a union
  position narrows to its member and is refutable; a written-union atom
  covers each member it covers; the pool reads an abstract's payload.
- **Patterns inside a declared variable**: a pattern that tests or binds
  inside a value of a type variable its definition holds rigid needs a
  written test (`` `A(i64) as `A(z) ``, a destructuring `let`'s
  annotation); only a name or `_` stands over such a value, and keeps the
  variable. At a union holding the variable beside other members a part
  meets it where it may hold one of its values, by its typed bounds, any
  value without one, inside a member the whole pattern may hold
  (`StructurePattern::meets_rigid`, judged as the select, the `let` or a
  destructuring parameter is checked, a test written over the variable
  itself as the arm compiles; an abstract pattern names the type its
  compile resolved, `CompiledAbstracts`); a name there takes the whole
  union. A written test's part over the variable must be closed: one
  holding `_` (a typedef's body included) or an undeclared variable is
  refused
  (`StructurePattern::meets_rigid_written`). Under a written test a bind
  inside the variable takes the test's part (`Type::matching_parts`); a
  name over a value holding it keeps the variable where a part of the test
  around it holds the scrutinee's part there
  (`PatternNode::type_inside_rigid`), and one that stands inside it in
  another alternative or member takes both.
- **Composite forms** (`graphix-types/src/rep.rs`,
  `design/distinct_composite_forms.md`): a tuple, struct, variant (bare
  included) and list cell is an array whose element 0 is its kind's
  marker, a Rust-registered abstract; an array is unmarked. Only `rep.rs`
  and `list.rs` know the layout. Text and netidx writers strip markers
  through `abstract_value::to_data` (`rep::strip`: a list as a flat array,
  a bare variant as its tag), and the reader's cast rebuilds the forms
  (`rep::read_*` report `Parts::Data`). Pack and db values keep composite
  kinds through `to_marked_data`; their reader, `rep::decode_marked_data`,
  admits markers only in well-formed composites, with no other abstract
  value admitted. Image reads admit markers within the image session.
  A primitive `array` holds
  arrays only; `cast<string>` of a bare variant is its tag.
- **One runtime form** (`Type::rep_collision`): no select arm, and so no
  union trait dispatch, may tell apart two types that share a runtime
  form: a reference and another reference, two function types, a
  Rust-backed abstract at two instantiations.
  Checked per arm, after its narrowing, in the definition's check;
  members that share a constructor are told apart by their parts. An open
  member there, or in a union dispatch, reads as any type it may yet bind
  (`Open::Unknown`, judged at the statement's settle, `PendingSettle::
  Told`): beside a member whose form another type may share it is refused
  ("annotate it"), a `NoError` cell sharing no error's form. A cell of
  the arm's own test, bound or open (a bind, a slice's rest: a part it
  binds, not one it tests), collides with nothing, except against a type
  bound of what else reaches the arm; an open one of what else reaches
  the arm does, even where the arm's capture shares it
  (`Type::rep_collision_tested`). Each element of a slice pattern is a
  test of its own among the whole element type, whatever the rest covers
  (`StructPatternNode::element_tests`). A
  compared type (`==` and the orderings), a map key type and what the
  stdlib compares or hashes (`uniq`, `min`/`max`, the sorts, `dedup`,
  `map::` keys) may hold no such pair anywhere: the `'a: Discernible`
  bound (`Type::indiscernible`; an abstract type without `Eq` is judged by
  its representation for equality, and one without `Ord` for ordering,
  the refusal naming the abstract and the missing trait), which a generic definition's variable
  takes from its body, so each call checks it; what orders or hashes
  (the orderings, sorts, `min`/`max`, `dedup`, map keys) takes
  `'a: Ordered`, which also refuses a reference, a function or a type holding two
  numeric types; a member still open at
  the statement's settle reads as any type it may yet bind
  (`PendingSettle::Discernible`, `design/tvar_constraints.md`).
  `flat_map`'s callback returns the collection (`fn(x: 'a) ->
  Array<'b>`), so nothing splices by shape; `filter_map`'s returns
  `` [`Keep('b), null] ``: null drops an element, `` `Keep(v) `` keeps
  `v`, null included, and the unwrap strips that one layer, so a `'b`
  that is itself a `` `Keep `` round trips.
- **`name@ pattern` captures** are typed from the SCRUTINEE (a `let`'s
  from its value, `Bind::complete_partial`): under an
  inferred predicate a capture is a type variable that
  `PatternNode::bind_narrowed` binds, after the select narrows the arm,
  to its part of the narrowed predicate (a `_` slot, the fields a
  partial struct pattern leaves out and a slice's rest carry the
  scrutinee's types; shared
  or-alternative captures union); a whole capture (`a@ (_, _)`) is
  the union of its part of each reaching member the arm matches, not
  of each position's parts, so it fits the scrutinee's own type. Never type a capture from
  `infer_type_predicate`. A slice's inferred element type is a Set with
  one member per element and a cell for the rest, so each element
  narrows on its own; its run-time test admits any member at any
  position, so a name an element binds holds its part of every member
  the element's structure admits (`hold_slice_members`). Pins:
  `lang::select::capture_*`, `slice_elements_typed_apart`,
  `slice_rest_scrutinee_type`, `slice_element_binds_hold_every_member`,
  `let_partial_struct_capture_is_the_whole_value`.
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
  definition is visible; `T as t` is a nominal tag test anywhere. Its
  parameters, and an unknown constructor's (`'c<a>`), are invariant; a
  constructed value's payload bounds its parameters from below (`TCell::
  lower`), so the type it flows into decides them (a return, an element,
  a union of instantiations, a call through a definition that constructs)
  and one nothing decides settles to the payload's type, a ⊥ produced
  beside it included; a binding of such a parameter passes the bound to
  its open parts (`commit_cell_bounds`), and a definition whose body
  binds it holds its argument to that binding, which a call's context
  then cannot widen; a trait bound or dispatch reads it, so decides it
  there. Two values compare their
  parameters, as regular trees (`typ::regular`; an application whose
  arguments grow at every level as its definition over its arguments),
  before their payloads. A pattern `T(p)` with no
  type test tests the tag and takes its parameters from the scrutinee,
  never from `p` (`StructPatternNode::narrow_payloads`).
- **Traits v1** (`design/traits.md`): static dispatch on the self
  argument's type; a union self lowers to a select; impls are global
  facts; core `Eq`/`Ord`/`Display` ride the value (map keys, sort,
  operators, printers, both engines), `Eq`'s optional `hash` the part
  `array::dedup` hashes (left out, a value hashes by its type alone), and
  core `Cast` the casts and writers (no structural default; these
  methods are never called by a program); a method
  may take `self` only in its result, and no call dispatches it. A
  method's `#[pure]`/`#[sync]` (above its `val`) is asserted of every
  body, each impl's and the default (`traits::method_decl`); core's
  methods are `#[pure]`, since a hook runs as often as its consumer
  needs. A quantifier bounded by
  constructor traits alone (`'c: Collection`) is a constructor applied
  (`'c<'c#elem>`), as a trait-typed parameter (`c: Collection`) is, and
  an interface's bounds pair with the implementation's through the
  matched types, whichever form each side writes. The io traits `Read`/`Lines`/
  `Write`/`Close`/`Seek`/`Socket` over five stream types.
- **Module system** (`design/module_system.md`): Rust-2018-style
  `use`; every name arrives by declaration, `use`, or prelude;
  `self`/`super`/`package` roots (`package::` is the registered
  package, else `/`, where a script's top level is in every mode);
  declarations, `let` included, are statement-position only. A module
  with an interface compiles its body with its `mod` statement and
  checks it with the statement (`Module::typecheck0`), in a compile task
  of its own; a run of them checks in parallel, after the statements
  before it. Siblings reach each other only through interfaces: an impl
  a body adds undeclared is hidden from its siblings' checks
  (`Env::hidden_impls`) and seen after them, a write through a sibling's
  `val` is judged against its implementation once all are checked
  (`Module::check_written_vals`), and a module's check that
  would write a cell created outside the module is refused and the
  write not made (`tvar::decided`; annotate the binding it would
  decide; `design/parallel_compile.md`); a raise of nothing joins no
  handler (`error::join_raised`), and an error the body raises to the
  function around the module, whose throws are a cell only its check
  writes, is refused: catch it inside the module. A job a compile puts on a shared pool runs
  under its spawner's `tvar::TaskContext` (task, level; the thread's
  foreign-write record set aside), never the thread's: a runtime
  compiles on its own pool (`Control::eval_pool`), and a worker blocked
  in a join runs jobs stolen from any of its compiles (`branch::Live`,
  `check_in_tasks`, `fusion::fuse_each`). Every cell of an interface
  `val`'s function type is generic, a constructor trait's element
  included: a call copies it. A `val`'s type variable stands only
  inside function types (a typedef as its expansion; an abstract
  type's parameter is outside), else it is refused: declare the val at
  one type (`Type::variable_outside_functions`). An implementation cell
  no use copies (a written `let`'s, an outer binding's, a call's
  result's) may not stand where the val, or a trait method's
  declaration, has a variable a use picks; else the declaration decides
  it, as an annotation on the `let` would (`module::hold_paired`; a
  method that is a lambda is judged by the bindings it captures, any
  other method and a val's implementation by level). A val whose type
  holds a variable, its own quantifiers and a typedef's arguments
  included, is generic (`Env::poly_binds`): each read copies it, a
  typedef's arguments as its function types (`Type::scheme_read`), and
  it is held as an earlier compile's generalized binding is above. A val
  something writes
  (`<-`, `&mut`, in any compile) must fit its implementation, whose
  variables are held rigid with their bounds
  (`CompileCtx::check_val_written`).
- **References** (`design/place_references.md`): `&e` is read-only
  (`&T`, covariant); `&mut e` is writable (`&mut T`, invariant) and
  `*r <- v` requires it of every reference `r` may hold. A read through a
  union of references has the union of their referent types; a write must
  fit every referent. Reads and writes expand typedefs through the same
  `node::reference_targets` walk. `&mut` of a
  non-place expression is a fresh cell it bounds from below, which its
  uses decide (else its type); an optional
  writable argument is `[&mut T, null]`. `&a[i]`, `&s.f`, `&t.0`,
  `&m{k}` are root + path; writes patch the root at delivery; a dynamic
  key is a moving reference, and a write while its address is bottom (a
  bottom key, a dereference of a bottom reference) does not happen,
  silently; one made before the reference first arrives lands at its
  first target. A reference keeps its cell, its place and the binding it
  names alive while any copy of it lives, past the deletion of the `&`
  that minted it or of the binding; the last copy to drop releases them,
  at the next cycle's start (`reference::Lives`,
  `ExecCtx::release_references`). References de-fuse. A reference's value is
  its own cell, so `==`/`!=` compare references by what they name
  (`bind::ref_target`: the place, else the byref chain's binding, else
  the cell), rewriting each one the operand type holds
  (`Type::map_refs`) before the value comparison. An `Any` part walks its
  runtime value; statically reference-free parts are shared untouched.
  Such a comparison never fuses. References under `Any` are refused at
  runtime by ordering operators, min/max, sorts, dedup and map-key
  operations, including singleton collections and cast-created maps;
  the operation logs a failure and bottoms (a cast returns its error).
  An abstract's own `Eq` or `Ord` controls its corresponding operation.
  References and functions have no order (`Ordered`);
  under `Any` a function orders by an id minted in run order, so an
  order over `Any`-typed functions is nondeterministic, and the fuzzer
  excludes a program that may make one (`oracle_tier`).
- **`catch`** (`design/catch.md`) installs a handler for the rest of its
  block; it is not control flow. A user handler that never reads its
  binder warns: its expression can act at birth before an error arrives.
  An ascribed catch checks each raised type at its join, while the enclosing
  definition's declared variables are rigid; the annotation must contain
  each variable's declared bound (`node::error::join_raised`).
  `?` wraps its error in an `ErrChain`
  once, extending a chain rather than nesting it, and the types agree:
  `ErrChain<ErrChain<X>>` is `ErrChain<X>`, so a generic `Result<T, 'e>`
  raises `ErrChain<'e>`. Chain-ness is the type's (`Type::
  chain_error_type`, a struct's field types included), and a site
  extends only when all it may raise is chains, never by a value's
  shape.
- **`seq` / `seqq`** (`design/seq_blocks.md`): `seq [trigger | let pat = trigger] { stmt* }`
  desugars to a machine node (`node/seq_machine.rs`: busy-drop, calls
  issued once per entry over an argument snapshot); every statement is
  a step and a passed step sleeps, so a `let` keeps its step's value;
  each later step snapshots the local lets it reads at its own entry;
  a `let` of one name to a lambda literal binds the lambda itself, so
  calls through it resolve statically (its assertions are the build's).
  A step's entry snapshot, the projection of it a read takes, a
  step's value and a `seqq` request's capture are written by nothing, so
  a read of a generalized binding through them carries its scheme
  (`seq::passes_on`, `bind::Passing`: those forms only, what the program
  wrote inside them judged as outside a seq; a capture is typed by its
  live read, `SeqCapture::scheme`), read at the step's entry or the
  request's enqueue as ever; a step's
  `let` a later step writes is written at its binding (the machine walks
  every step's items for writers); a holder with a call part is one
  instance in the step.
  A statement starts in the first cycle its predecessor's effect can
  be seen: the next step enters in the cycle its predecessor completes
  unless it reads or writes a variable a pending write targets, by the
  post-resolution dependency summaries (`design/dependency_summaries.md`,
  `analysis::plan_machines`, per instance): a callee's writes count, an
  interface pair is one variable, a
  read or write through a reference or a call with no static target
  counts as every variable; `until` and `try` follow the same rule.
  Entering a step is its birth for the run (`View::Step`: constants fire,
  the builtins in it are reborn) with `Select`'s catch-up
  (`node/wake.rs`). A call a step issues is a fresh frame at each run
  (`SeqGuard`, `wake::FreshFrame`): at its issue everything it reads that
  stands, its callee's captures and a callee it binds included, is
  delivered fired, and its callee's body, slept since the last run, is
  born again with the step (builtins reborn, an async one holding no
  answer; lets take their initializers, so one whose initializer never
  fires, and a catch's binder, which has none, keep their last value;
  selects forget their entry history); what the callee captures keeps its
  value, an event's last fire included. A bound callee whose function value is bottom then is
  born so when the function returns (`callsite.rs::Unborn`), as is a
  call in the callee meeting a bottom function at its birth. A call
  outside a seq is never born again.
  A `{ .. }` statement issues its statements together with local lets. `until`,
  `try { .. } with(e[: T]) { .. }` (the error branch; `catch` is refused
  in a seq body outside lambda literals). A step completes on a FIRED
  production after its entry, never on a standing value. A step sees the
  world as it stood at its entry: every variable the seq's own text
  reads (lambdas written in it included, a nested seq excepted, which
  takes its own) is sampled together, at the step's entry (`seq`) or at
  enqueue (`seqq`). The whole snapshot waits for every named variable,
  including untaken arms and uncalled lambdas: an absent variable delays
  reading the present ones until it arrives, as an issued call's
  arguments do; a callee's state and captures stay live (the rule is
  lexical), a read through a reference (`*r`) reads its target live,
  and `live(e)` reads `e`'s variables live (a call whose argument reads
  live, a pattern bind of a live scrutinee included, is issued again at
  each fire of that argument; a nullary call is issued on the entry clock
  as any call is): those are the ways a step waits for the world to change (`until live(ready)`; an `until` with
  no `live` part, no dereference and no call warns). `seqq` queues triggers
  with captured values. Each request retains its own trigger while
  waiting for absent captures; waiting requests share the first complete
  external snapshot and keep their arrival order. A capture a step writes, a callee's write
  included, is live (`SeqCapture`). `seq t; abort(e) { .. }` ends the run when
  `e` fires: silent, past any `try`, and it wins the cycle it fires in
  (the compiler-only `SeqAbort` node fails the machine's guards before
  the machine updates); `e` is an initial step, asleep between runs and
  born at each run's entry (`CFlag::RunEvent`), and only its fires after
  the entry cycle count. `flush(e)` (`seqq` only)
  also empties the queue: requests still waiting for a capture
  included, numbered by arrival (`queue`'s `flush_before`); its source stays active between runs and the
  queue sees its ungated fires; only its abort of the current run is gated.
  A machine resets to idle in its handler's
  `sleep()`: a run does not survive its arm's sleep, and a `seqq`'s
  handler owes its queue the aborted run's credit, paid at the wake
  (`CatchRole::Machine::clock`). `--expand` prints the
  machine and each instance's step boundaries. Every name a lowering
  binds starts with `#`, which no source can write (`is_lowered`,
  `Expr::is_lowered`): an error's chain records no frame for an
  expression a lowering built and prints no snippet naming one, and a
  write a lowering built gets no declaration hint. `range(i, j)` is
  the integer builtin (`` `RangeError ``).
- **Comments** are legal above an expression, a select arm, an impl or
  trait method, a struct-literal field, a lambda formal, a use-tree item
  or an interface item, at the end
  of a list item's line after its separator (a statement, an arm, a
  field, an element, an argument: `Decorations::eol`; an interface item
  or a trait method: `Eol`; a formal: `ItemComments`; an import:
  `UseComment`) and below a list's last item or a file's
  (`Decorations::below`, on a trailing `;`'s empty statement when there
  is one; `Sig::below`, `TraitExpr::below`, `LambdaExpr::below`, and an impl with no
  method's `ImplExpr::below`); none inside a string's
  splice or an attribute's arguments, which print on one line, and no
  `///` doc or attribute in a splice either, anywhere in what it holds
  (judged over the formatter guard's own list, `expr/ornament.rs`); `///`
  docs only in an interface. A type definition's struct fields and union
  members take all three, through the fields and members of their types
  (not a type argument, a payload or a function type); a union member
  takes one only as a variant or a named type, which print as written,
  and the lines below the last go under the last of those; anywhere
  else in a type a comment is refused (`expr/parser/typexp.rs`). A blank
  line between two lines of one comment block stays, one for a run of
  them. Parse errors report the furthest point
  reached with the source line and a caret, what was expected there,
  and a refusal its reason.
- **Operators**: `* / %` (and checked forms) bind tightest, then `+ -`,
  comparisons, `== !=`, `&&`, `||`, `~ ~!`; every binary operator is
  left-associative (`8 / 2 * 2` is 8), and a lambda may be an operator's
  last operand, its body taking the rest (`t ~ |x| x + 1`). `BinOp` (`expr/binop.rs`) is the
  one table. Arithmetic (`fn<'a: Number + Singleton>(x: 'a, y: 'a) ->
  'a`) and comparison (`fn('a, 'a) -> bool`) take operands of EXACTLY
  one type, each containing the other's (`node/op.rs::operand_type`):
  `[i64, null] == 3`, `` [`A, `B] == `A `` and `[i64, f64] + 1` are
  refused; a ⊥ operand, or one only ⊥ was produced into
  (`TVar::bottom_fed`, `CellFact::BottomFed`), takes the other's type. `Singleton` is a bound
  like `Concrete`: whatever binds the variable is no union, a primitive
  set of two or more included, so arithmetic over a type holding two
  numeric types is refused even against itself, and a generic
  definition's at the call, by the check; the open members of a union
  under it merge into one cell. Comparison takes any one type, unions
  and mixed numerics included, if it is `Discernible`; the orderings take
  `Ordered`, which refuses two numeric types. `OneNumber`, a written
  bound only, holds a type to at most one numeric type (`'a: [Number,
  null] + OneNumber`); a binding with a numeric member beside an open one
  is refused at the definition that leaves it open (annotate it). `NoError` holds a type to no member a value of
  which is an error (`Error<..>`, the primitive `error`, `Any`):
  `type Result<'r: NoError, 'e>`, so a result's error member is its only
  error (`design/tvar_constraints.md`). An interface declares every bound its
  implementation's variables carry, a typedef parameter's bound counting
  (`FnType::sig_matches`); the implementation may leave off a bound the
  interface declares.

## Stack discipline

Nesting depth is attacker-controlled and overflow aborts, so it is
closed two ways: `crate::stack::ensure_sufficient` (stacker) wraps every
program-driven recursion — parser knots (`Input::knot`), `compile`,
`Display`, `fold`/`for_each_child`, type walks (`Eq`/`Ord`/`Hash`
included), pattern walks, seq lowering, and the `Node`/`TVar`/`Expr`/
`Type` destructors (explicit teardown inside the guard) — and
`parser::DEFAULT_MAX_NESTING`, which bounds AST depth: it counts parser
knots and the levels each iterative fold (an operator or postfix run)
adds under them (`Input::fold_fits`); reaching it ends the parse, except
under a map access's probe (`Input::probe`), whose text a select's arms
read again. An expression a lookahead or a probe parsed is parsed once
where it starts, or once a depth where the limit cut its parse short
(`Input::remembered`). Type depth is not bounded by the
limit: `let x1 = [x0]; let x2 = [x1]; ..`, or a chain of typedefs,
builds a type as deep as the program is long.
Pins: `graphix-compiler/tests/deep_drop.rs`,
`graphix-shell/tests/deep_nesting.rs` (add a case for a new recursive
construct). netidx-value has the same treatment for bracket literals.
A cast reads data at most `typ::cast::MAX_DATA_DEPTH` values deep and
refuses deeper data with an `InvalidCast`.

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
| `GRAPHIX_NO_SUBST=1` | every instance checks its body again instead of taking its definition's types, a select's verdicts and a `Transparent`'s aside (A/B for instances by substitution) |
| `GRAPHIX_FUSE_SERIAL=1` | fusion visits every part in order on one context instead of fusing disjoint subtrees in tasks (A/B: the decisions must agree) |
| `GRAPHIX_ABORT_ON_NO_MATCH=1` | an abort at a select no arm matched, a coverage hole (both engines report it once; the fuzzer's children set it, so a hole is a crash finding) |
| `GRAPHIX_PAR_AUDIT=1` | a panic at a join whose right branch read what its left sibling published |
| `GRAPHIX_PRUNE=audit` | walk pruning walks every boundary and panics where a skip would have changed a production or published (`off` walks everything) |
| `GRAPHIX_PAR_T=<ticks>` | the base wake threshold fixed in place of calibration; `Auto` still accounts for measured branch overhead and unprofitable forks |
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
| `GXDBG_LETBIND=1` / `GXDBG_REF=1` | let publication decisions / every read (delivered, standing, miss) |
| `GRAPHIX_ELAB_AUDIT=1` / `=bt` | each elaboration refusal of what the check accepted (a type-system bug), with a backtrace |
| `GRAPHIX_JIT_RELOCATE=every` | every cycle's start moves every live kernel into a fresh JIT generation (stress for relocation) |
| `GRAPHIX_JIT_ARENA=<bytes>` | the arena a JIT generation reserves (256MB by default; outside a page..2GiB, fusion is unavailable) |
| `GRAPHIX_DISABLE_PACKED_AST=1` | module resolution parses source even where a packed AST is available |
| `GRAPHIX_TEST_ROOT=compile` | every test runtime compiles its package root from source instead of restoring the process's image of it (A/B for the restore) |
| `GRAPHIX_FUSION_DISCOVERY=1` | `run!` asserts no `FuseExpect` and prints each fixture's observed level (`FUSEMAP`) |
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
- A hole in the type system is closed by a local rule that refuses and
  names the annotation that makes the program pass (pin both), never by a
  late check chasing shapes; where the right type is local and cheap, it
  is given instead. A `--check` census of both repos sizes the rule: one
  that needs a new annotation in the stdlib, the book's examples or
  netidx-admin goes to Eric first; test fixtures alone are rewritten.
- Hot operators log and bottom on failure; rare stdlib functions return a
  catchable `Error`.
- A bug in a crate Graphix depends on is an entry in `UPSTREAM.md`, not a
  CR. Work around it in Graphix only when it breaks Graphix's own
  operation (a crash, a hang, a wrong answer), and say so in the entry.

## Stdlib notes

- `xls`: calamine opens every BIFF xls sheet densely, including for
  `sheets`; `MAX_CELLS` is a post-open limit for xls, not an opening
  memory bound. The ods preflight does not cover xls. This accepted
  limitation is documented in `book/src/stdlib/xls.md` and `UPSTREAM.md`.

- `sys::process`: a child lives in a task of its own (`own_child`),
  which selects on its exit against kill requests over `ctl` and reaps
  it; dropping the last `Proc` closes `ctl`, which kills a
  `kill_on_drop` child; redirects are `Pipe`/`Inherit`/`Null`. Shell tests are Unix-gated; the stdout and
  wait-status ones have `cmd.exe` twins.
- `tui`: a display holds no descriptor: what is written to stdout or
  stderr while it runs goes where the stream points, over the display on
  a terminal. It reads the terminal's events on a thread of its own whose
  every crossterm poll is bounded, and gives the terminal back once that
  thread has stopped, or after a bounded wait for a terminal that went
  away (`Events`); never crossterm's `EventStream`
  (UPSTREAM.md), whose feature the workspace leaves off.
- GUI (iced): uses the iced sub-crates directly; `GuiTestHarness::dt()`
  downcasts; updates walk every window’s widget tree, including newly
  compiled children (`design/gui_updates.md`); tests fire callbacks via `gx.call(callable_id, args)`;
  test contexts default to `NetConfig::Internal`; publisher coalescing
  collapses rapid updates — space them with timers. A control's send
  (`widgets::Echoes`) shows until echoed, and is rejected only when its
  call was answered and it is not echoed once the runtime is idle: a
  widget left waiting asks `GXHandle::notify_idle`, and the
  `GXEvent::Idle` that answers comes after every update before it, to
  the display's `CustomDisplay::idle`.
- Package manager: `packages.toml` v2 (`[stdlib]` tracks the shell
  version, `[packages]` for externals); `update` presents a maskable
  change set and builds before writing the manifest; hard error on
  non-TTY without `--yes`. Rebuilds refuse an executable beneath Cargo
  build markers (`.rustc_info.json` or a profile's `.cargo-lock`), before
  unpacking and again before replacement; binaries installed elsewhere
  may rebuild.

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
