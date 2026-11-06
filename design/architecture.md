# Architecture

Status: current principle.
Pins: the tests of each subsystem; `graphix-compiler/tests/deep_drop.rs`, `graphix-shell/tests/deep_nesting.rs`.

How a program goes from source to running nodes: the pipeline, the key types, nodes, the compile/runtime split, the runtime, the two-phase typecheck knot and stack discipline.

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
made, naming the first binding that holds each cell it would decide
(three bindings at most) and the type it would take with all of its cells
told, where none is left open (`OwnWrites::enter_input`,
`tvar::in_input`, `ForeignWrite::declared`); a cell below a settled ⊥ is linked to it, so a later
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
its join (`branch::VarAliases`; `Rt`'s store, reader and notify calls
take the `branch::Var` an id names):
the body's `let` publishes to it, and
every read, write, store entry and registration through the
implementing binding is the val's, in one queue; a reload drops the
writes the replaced body made through its binding and had not landed,
while one through the val lands on the new body. A
running program's failures (an unhandled `?`, a hot operator's failure)
are logged under `graphix_compiler::FAILURE_TARGET`, never printed; the
shell shows that target on stderr.

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
