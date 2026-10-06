# Parallel and incremental compilation

<!-- CR claude for eric: [doc-drift] The `parallel-compile` branch is merged into main,
and this doc's own sections mark module checks, fusion tasks, compile tasks and
instances by substitution BUILT. Yet this Status line says 'partly BUILT on branch' and
'the phases and the caches are the plan', two later lines say 'BUILT on the branch', and
design/README.md indexes the doc as 'the plan for instances and module bodies'. A reader
of the index concludes that substitution and parallel module checks are proposals. The
Status line should say what is built on main and what is open: the audits as fuzz
findings, the instance cache and the per-unit image caches. (x-doc-drift-06) -->
Status: partly BUILT on branch `parallel-compile` (2026-09-29): parallel
code generation, the compile context split from the runtime, statement
elaboration in parallel compile tasks, and instances typed by
substitution. The phases and the caches are the plan.
Pins: `lang::functions::same_named_tvar_in_callback_arg`,
`lang::functions::monomorphic_flat_map_twice`; the `GRAPHIX_ELAB_AUDIT`
probe (`node/lambda.rs::elab_audit`) and `GRAPHIX_TASK_AUDIT`
(`typ/tvar.rs::written`); `findings/omitted-default-check-sep2026/`.

## Goal

The compiler should be fast when code changes: `--check` and the language
server for the edit loop, and a full build for change, build, test. The two
levers are cores (independent work runs in parallel) and reuse (work whose
inputs did not change is not redone). Both need the same thing: units of
work whose inputs are known and small.

## Where the time goes

Admin TUI, `milestone_timing`, quick build, pinned to one P-core. The
profile comes from `GRAPHIX_PROFILE=1` read with
`bench/profile.py --modules` and `bench/instances.py`.

| part | ms | what |
|---|---|---|
| package module bodies | 49 | `local` 14.5, `panels` 14.3, `tui` 7, the rest small |
| app compile, fusion off | 232 | ~200 builds and checks 3343 instances of 568 definitions |
| fusion | 440 | of which 257 is cranelift code generation |

The app is one call, `app(#server: ..)`. Every instance is built and
checked under that call site, so module bodies are the small part. The
per-core bound on module bodies is ~15 ms. Developers who split their
program for speed move that bound. They do not move the instance work,
which follows the call graph. So the plan covers all three parts.

## Check vs build

Admin, quick build, pinned, `check_vs_build_timing` (netidx-admin
`test.rs`), medians, against 978f05bc (before the check split, when
`--check` was the full build):

| | before | now |
|---|---|---|
| app, the check alone | 250 ms (full build) | 0.2 ms |
| app, build without fusion | 251 ms | 255 ms |
| app, build with fusion | 680 ms | 700 ms |
| package root, check or build | 137 ms | 141 ms |

The app's check is one call site over checked definitions. The
package root has no elaboration of its own, so its check is its
build: about 100 ms wall parsing 5.4k lines (0.8 MB/s, the two
1.3k-line files ~60 ms each) and 53 ms of definition checks. The
shell's `--check` and the language server run the check alone
(`CFlag::CheckOnly`).

## Code generation (BUILT)

Fusion's decisions are all made at CLIF emission: a region that emits is
spliced, one that does not keeps node-walking. Cranelift's backend only
turns emitted functions into machine code, so it runs after the pass,
in parallel (`fusion/emit/jit.rs`):

1. Emission names bodies, thunks, wrappers, helpers and constants by ids
   of its own table (`jit::Names`) and defines nothing. Each function
   waits in the `Jit`'s queue, a body after its callees.
2. Every `LINK_BATCH` regions the queue so far starts compiling on
   threads of its own while emission goes on (`Jit::link_batch`); it
   installs when the next batch starts or at the pass's link, which
   compiles what is left on worker threads, the calling thread among
   them. Results are placed by queue index, so the output does not
   depend on the threads, and a batch installs before any later one, so
   a body's callees have their records when its relocations name them.
3. The records are built in queue order and the regions' wrappers
   install with one finalize, through the load the image path uses.
   Each kernel's entry is set then; before its pass links a kernel has
   no entry, and nothing runs it.
4. A full arena retires the generation and the link reinstalls its
   records in a fresh one without recompiling. Any other failure is a
   JIT bug past the point where a region could be refused, and it
   panics: a verifier error is our emission's bug, and a loud one is
   what the fuzzer can find (the alternative, a region that silently
   stops fusing, hid `let v: [f64, null] = 1.0`'s unwidened read).

Admin app build with fusion, quick build, `milestone_timing` medians
against b0f86a98:

| | before | after |
|---|---|---|
| one P-core | 729 ms | 650 ms |
| four P-cores | 733 ms | 459 ms |
| fusion's share on four P-cores | ~456 ms | ~187 ms |

On one core the gain is the trap stubs and per-region finalizes that
are gone. On four the link is 85 ms where the backend was 297 ms.

Discovery and emission are serial, and the batches' codegen used to
wait for them, and they for it: on four P-cores the app's fused compile
had 128 of 300 profiled ms with one busy thread, 53 of them discovery and
emission and 29 a batch's last functions compiling alone. A batch that
compiles while the next one emits takes the fused compile from 236 to
216 ms (best of five, quick build, bench mode; median 242 → 220), with
the same cycles. Batches of 64 regions measured 221 ms.

## Module checks (BUILT)

A package's registration was its module check, one module at a time:
admin's 64 of 85 ms, the definition checks of its 18 modules (each
definition's body is built inside its own check). A module with an
interface is now a unit of the check:

- its body compiles with its `mod` statement, in order, under a compile
  task of its own (`Module::task`), so the signature binding, the
  lexical exports a later sibling sees (`export_sig`) and every cell the
  body creates belong to it;
- its body's check and `check_sig` run with the statement
  (`Module::typecheck0`), after the statements before it: a child may
  read its parent's private lets;
- a run of consecutive static modules checks in parallel compile tasks
  forked before any runs, joined in order, the first error in order the
  block's (`node::typecheck0_statements`); IDE sinks fork with the task
  and append in order (`IdeMode::fork`).

Two rules make the result independent of the schedule. A body reaches
its siblings only through their interfaces: an impl a body registers
without declaring it is hidden from its siblings' checks
(`Env::hidden_impls`, read through `Env::impls_of`) and seen by the
statements after the run. And a module's check writes no cell created
outside the module: such a write (the task audit's foreign write, now
recorded for the module's task, `tvar::OwnWrites`) refuses the module, and is
not made (`tvar::decided`): every sibling that would make it is refused
in turn, none sees another's, and the first in order is the error, so an
unannotated `let x = never()` whose first use is in a child module must
be annotated. Over the gate the only foreign writes in module tasks are
the interface's own type variables, frozen at compile.

Admin registration, quick build, four P-cores, unpinned smoke runs:
85-97 -> 61-62 ms.

## Fusion tasks (BUILT)

Fusion's discovery and emission were serial: 127 ms of the admin app's
fused compile on the main thread, 53 of it CLIF emission under the
JIT's lock, ~35 discovery, 22 waiting on links. Fusion now runs on the
`CompileCtx` (its state is `CompileCtx::fusion`; a splice discards what
it replaces), and the parts of a node that call a function, disjoint
subtrees, fuse in compile tasks (`fusion::fuse_each`), each against a
fork of the context, joined in order:

- emission state (`emit::Emission`: the names functions are emitted
  against, the lambda-kernel signatures and bodies, region layouts and
  the functions waiting) is per task: a fork freezes the parent's
  layers into an `Arc` both extend; a task mints ids from the frozen
  table's next, and its join shifts its ids after the parent's (the
  callee names in its CLIF rewritten with `reset_user_func_name`), a
  kernel body the parent already has replacing the task's;
- the type memo is one `DashMap` table per pass the tasks share
  (`TypeMemo::current`/`enter`): a result is the same whichever task
  computes it;
- only the root links (`FusionCtx::unlinked` is 0 in a task).

A region that failed used to leave its lambda kernels' signatures
cached without their bodies, and a later region hitting one emitted the
body from its own instance against the first's slots (`emit_clif:
undefined local`); the failed attempt now forgets them
(`Emission::forget_attempt`). That is what made the decisions depend on
the order regions ran in. With it, serial and task modes decide the
same (admin: 959 of 2771 regions; the corpus's fusion manifest
unchanged), and `GRAPHIX_FUSE_SERIAL=1` is the A/B.

Admin app, quick build, bench mode, four P-cores, median of 10: fused
compile 222.7 -> 208.0 ms (serial mode of the same binary 223.5).

## Compile tasks (BUILT)

Compiling and type checking take a `CompileCtx`: the registry, the
program's state (the `Env` and the registries, tracked persistent maps)
and the compile's scratch. The runtime (`rt`, `libstate`, `control`,
`fusion`) is the rest of `ExecCtx`, which derefs to the `CompileCtx`.
What compiling would ask of the runtime it records and the runtime
replays at install (CLAUDE.md, "Compiling never reaches the runtime").

A `CompileCtx` forks and joins (`CompileCtx::fork`/`join`): a fork shares
every tracked map and records what it touches, and a join takes the
fork's writes. Every static bind elaborates its instance in a fork
(`CallSite::resolve_static`). A body's statements elaborate in forks of
the body's state before any runs, one per statement, on rayon
(`node::typecheck1_statements`); they join in evaluation order, and the
first error in order is the body's.

**Ownership.** Each parallel statement is a compile task with a fresh id
(`tvar::new_task`, entered by `InTask`); a nested fork runs in its
parent's task. A type cell and a type variable record the task that
created them, and no task writes a cell an earlier task created: the
check's cells, or a sibling's, which a concurrent task may be reading.
What keeps it so:

- a merge keeps the older task's cell and links the newer one to it (a
  rigid cell survives before age), never welds two earlier-task cells,
  and ORs no flags into an earlier cell (`TVar::merge_into`);
- a name alias never renames an earlier task's variable: the newer one
  takes the name, or, already named, the two merge as cells;
- a settle skips an earlier task's cell (`TVar::settle_witness`,
  `settle_bottom`): what the check left open, it decided open;
- a raise already covered by the catch's type rebinds nothing
  (`error::join_raised`);
- a builtin's per-site check rebuilds over a private copy of its
  signature (`lambda::build_builtin_check`), never reopening the
  definition's gate on shared cells.

`GRAPHIX_TASK_AUDIT=1` prints a backtrace at every write that breaks the
rule (`tvar::written`); the gate runs with none.

**The check decides every cell.** Elaboration may not settle what the
check created, so every settle that decides a type runs in the check's
drain (`PendingSettle`): a call site's terminal settle, over its
signature's cells and the cells their bindings hold; an operator's
operand settle and the arithmetic rule judged after it; a `let` over ⊥;
an omitted labeled default, compiled and checked at its site for every
definition the callee's `lambda_ids` names
(`CallSite::check_omitted_defaults`; exempt only where a definition is
not known at the check). `rand::rand(#clock: null) + 1` passed `--check`
and failed the build before the last.

Admin app, quick build, `milestone_timing` medians, against 02bf95a1:

| | before | after |
|---|---|---|
| four P-cores, fusion off | 312 ms | 196 ms |
| four P-cores, fusion on | 505 ms | 381 ms |
| one P-core, fusion on | 697 ms | 750 ms |
| GUI suite | 2.77 s | 2.82 s |

One statement holds admin's elaboration (the `app(..)` call), so the gain
is the other statements' and fusion's; the cost on one core is the
forks and the omitted defaults compiled at the check as well as at the
bind.

## Elaboration's parallelism (measured)

The instance census (`GRAPHIX_PROFILE_INSTANCES=1`, `bench/instances.py
--admin`) records each instance's parent (the instance whose
`typecheck1` built it) and its elaboration's inclusive time. An
instance's own work is that less its children's; the critical path is
the longest chain of own work from a tree root. Admin app, one P-core:

| | fusion on | fusion off |
|---|---|---|
| elaboration work | 246 ms | 238 ms |
| critical path | 36 ms | 33 ms |
| bound on the speedup | 6.9x | 7.2x |

The deepest chain is 9 instances; the widest instance builds 332. Of the
3343 instances, 1627 are distinct by definition, closed signature and
callback sources; the repeats are about a quarter of build and check.

With statements in compile tasks (one rayon thread vs four, admin app,
fusion off: ~400 ms vs ~195 ms), the chain that bounds it is
`local::local(..)` (177 ms, one statement of the app) over
`panels::panels(..)` (90 ms, one statement of `local`). Inside the
panels instance its 143 statements elaborate as tasks, 68 ms of work
with the longest 6.6 ms; the ~22 ms before them is the instance's own
graph build and check, which run statement by statement. Instance build
and check are 142 of elaboration's 212 ms, so they bound it, not the
lack of tasks: forking an expression's sibling calls into tasks as well
(`f(g(..), h(..))`, literals, select arms) measured no change.

## Against main (measured)

Admin `milestone_timing`, quick builds, medians of 10 alternating runs
at realtime priority (`chrt -f 50`), main (367582d2, with netidx
65e2962b: the same `.gx`) against the branch (76bc8f22). Registration
is the package root, "unfused" and "fused" the app's compile. Fusion
decides the same: 2772/956 kernels on main, 2771/959 here.

| cores | registration | unfused | fused |
|---|---|---|---|
| 4 P-cores, bench | 72.7 -> 62.8 | 264.5 -> 98.8 (2.7x) | 457.6 -> 221.4 (2.1x) |
| 1 P-core, bench | 74.4 -> 89.9 | 256.9 -> 184.0 (1.4x) | 642.1 -> 561.8 (1.1x) |
| all 16, bench, unpinned | 87.1 -> 85.4 | 260.3 -> 93.6 (2.8x) | 429.6 -> 189.0 (2.3x) |
| 2 P-cores, potato (400 MHz) | 781 -> 666 | 2795 -> 1204 (2.3x) | 5530 -> 3373 (1.6x) |

The one-core gain is not threads: instances take their definition's
types instead of checking again, and discovery's type memo. The
package root's check, the language server's path
(`check_vs_build_timing`), moved little: 136 -> 128 ms on four cores,
220 -> 235 on one; about 100 ms of it is parsing. Elaboration does not
scale past four cores (unfused 98.8 on four, 93.6 on sixteen): it is at
its critical path, the `local` over `panels` chain. Registration on
one core is 15 ms slower than main, the cost of the module tasks with
no second thread; on two it is already ahead.

The serial part on four P-cores (perf, frame-pointer LTO build, 10 kHz,
a millisecond with one busy thread is serial):

| | fused (220 ms) | unfused (95 ms) |
|---|---|---|
| one busy thread | 67 ms | 43 ms |
| the first cycle (`rt.compile` times it) | 16 | 9 |
| analysis | 10.4 | 10.3 |
| deleting discards (`apply_deferred`) | 8 | small |
| registering references | 2 | 2 |
| the link's tail | 8.5 | |
| the check | 1.6 | 7 |
| fork and join | 2.8 | 2.2 |
| discovery and emission | 5.5 | |

Code generation runs at 3.96 of 4 threads, elaboration at 3.8.

## What stays serial (measured)

A perf profile of the admin app on four P-cores (10 kHz, samples binned
by millisecond, a millisecond with one busy thread counted as serial)
gives, for the fused build, 235 of 400 ms serial: the check 77 ms,
fusion discovery 54, analysis 26, the link's tails 25, fork and join
14, graph compile 11, emission 7, the rest 19. The unfused build is 151
of 220 ms serial, the check 81 and analysis 27 of it. Elaboration runs
at 3.8x. The check is serial by design: a module's statements check in
order (`typecheck0`), while elaboration forks each into a task
(`node::typecheck1_statements`).

- Discovery repeats its type work: within one pass `expand_refs` ran
  6902 times over 2085 distinct types, the ABI freeze 16777 times over
  4338. `fusion::TypeMemo` holds both for the pass, keyed first by the
  allocations a type is made of (no walk, and valid for types with open
  cells, since a pass binds none), then by content for types with no
  open cell. A result is kept only when every named type in it was
  resolved before it was computed: resolution cells fill during the
  pass, and a freeze over an empty cell is `Unresolved`.
- Analysis took each instance body's `refs` to learn its own bindings,
  which only a `<-` in the body asks about; it is taken on the first,
  and without the callees' bodies (`Refs::without_callees`), which no
  `<-` in the body can name.
- `contains` took six pooled maps per call; five are taken at their
  first write (`typ::Lazy`).
- A builtin call site resolves its function by its `Ref`'s bind id,
  not by looking its name up (fusion discovery and analysis).
- A compile task's join wrote back each key it touched, once per
  write; the keys are deduplicated and written back in one
  `update_many` (`tracked.rs`).
- poolshark's pool registry hashed discriminants to their raw bits, so
  the table's tag bits were all equal and every lookup scanned its
  group.

The memo and the lazy `refs`: fused build, four P-cores, ~350 → ~315
ms; unfused ~153 → ~146 ms. Fusion decisions are unchanged (a check of
every memo hit against a fresh computation passed the workspace and the
admin package). The rest, by user instructions over the admin
milestone run: 9.39 G → 8.01 G (poolshark 4.7%, lazy maps 2%, `refs`
without callees 3.6%, bind ids 0.7%, the join 4.5%). Resolving by bind
id also fuses a trait method whose implementation is a fast builtin.

## The rule that makes it possible

BUILT on the branch: the gate keeps what the body bound, and the check's
settle drains before elaboration (`design/tvar_constraints.md`).

The definition check plus the call-site check are all of type checking.
An instance is never checked for acceptance. If an instance's check fails
where the definition and the site passed, that is a type-system bug.
Instances do elaboration:

- trait method resolution in generic bodies;
- static resolution of calls and of fn-typed parameters (HOF callbacks);
- the types that type-directed printing and type-directed builtins need;
- per-instance analysis (effects, recursion, seq step boundaries);
- fusion's concrete ABI.

`--check` stops after the definition and call-site checks. A full build
elaborates.

## Phases

1. **Interfaces**, in dependency order: every `.gxi` registers every global
   fact (typedefs, traits, declared impls, abstract reps, signature
   bindings). A module with a `.gxi` is a unit. A module without one, and a
   script, stays inside its parent's unit, because its inferred types may
   hold open cells that a consumer binds.
2. **Bodies**, in parallel: each unit compiles against a clone of the
   frozen Env and returns a delta. The deltas are keyed by fresh ids, so
   merging them is a union. The definition check and call-site checks run
   here.
3. **Elaboration**, in parallel: an instance with a closed signature and
   known callbacks is a function of (definition, signature, callbacks). It
   runs as a task. The same key also works as a cache entry that survives
   edits.
4. **Fusion**, in parallel: code generation goes to `BodyRecord` bytes.
   Installing them into the one JIT module is the only serial step.

Nothing shared decides a result under a lock. First-writer-wins
registries (impls, trait definitions) are filled in phase 1, before any
body runs. Ids come from global counters, so anything that iterates by id
must not change output. `detcheck` is the test for that.

## Units of work

A unit (a module body, a statement, an instance) compiles in a fork of
the `CompileCtx`: the `Env` and the registries are tracked maps a join
merges, and the scratch (`bind_to_lambda`, `rec_defs`, `def_gate_*`,
`resolving_lambdas`, `pending_settles`, `pending_imports`, `attr_*`,
`def_assertions`, `batch_connect_targets`) is the fork's own.
`resolving_lambdas` must stay per unit: shared, a thread reaching a
definition another thread is resolving would read it as recursion.

## Open design items

**Type-directed builtins.** BUILT on the branch as the `Concrete`
conjunct (`design/tvar_constraints.md`). About 17 builtins have an `Apply::typecheck1`
hook (`str::parse`; the json, toml, pack, sqlite, db and hbs reads). It
both learns the builtin's target type and refuses one it cannot use. When
the site is inside a lambda, the hook runs only in the instance, so
elaboration refuses programs that the checks accepted. The split:

- the refusal becomes a constraint on the type variable, a predicate the
  builtin supplies ("castable"; `hbs::render`'s partials rule);
- a variable born inside the definition is judged at the definition's own
  settle;
- a signature variable carries the constraint to each call site;
- a declared (rigid) variable must state it, like `'a: Number`;
- the hook still extracts its type during elaboration, and failing to
  extract becomes an assert.

## Instances by substitution (BUILT)

An instance's check re-derived what the definition's check settled.
Admin app, one thread, perf build: the instance check was 24% of the
compile threads' samples, containment and unification a third of it,
and it runs before an instance's statements can fork.

**The table.** At its gate's close a definition records what its check
settled, per expression id of its body (`node::lambda::DefTable`): each
node's type, each call site's signature, each lambda literal's own
table. An id two nodes share has no row. A cell of the table its gate
created, that nothing bounded and its signature does not reach, settles
to ⊥ with the check (`PendingSettle::Body`). A seq block lowers once per
expression and scope (`CompileCtx::lowered_seqs`), so every compile of a
body has the same ids.

**An instance** compiles its body as before and runs
`Update::typecheck0_instance` instead of `typecheck0`
(`node::lambda::InstanceTypes`). The definition's signature is copied
through one cell map and unified with the instance's, and every row goes
through the same map (`Type::instantiate_with`: a bound cell is
followed, an open one a closed gate owns is copied once, any other is
shared). The table owns the typedefs its rows name (`DefTable::
typedefs`): a typedef its body declares is in the env only while the
body compiles, and a row outlives the check. Every type reference a row
holds is resolved in the check's environment when the table is recorded
(`Type::seed_refs`), so no instance looks a name up by a block scope
only the check had: an image relocates the ids a block's scope is named
by, never the names. Each node kind takes its types from its row and does
only the state part of its check:
- a call site installs its signature, placeholders for omitted
  defaults, joins its raise, and pre-unifies each argument with its
  formal so a callback's parameters infer;
- a lambda literal takes its signature at its own level and generalizes
  it with no gate and no second compile of its body; its instances
  read its table through the enclosing instance's map (`Tables::outer`,
  `Type::rename_with`);
- a select reads each arm's completed predicate and its pattern binds'
  types from the table (`DefTable::aux`, `Select::aux_types`), binding
  the pattern's cells by position, realigning a struct pattern's field
  indexes and resolving an explicit predicate's names (its run-time
  type test reads them where the body's typedefs are gone); it narrows
  nothing and judges no coverage, dead arm or guard;
- a catch reads its error bind's and a `try` capture's types from the
  table (`Catch::aux_types`);
- an operator, a constructor, a field or tuple read, `?`, `$`, `&`, `*`
  and `any` take their own type from their row (`node::typed_by_row!`)
  and judge nothing: no operand rule, no field containment, no deferred
  settle; a field read still finds its index, `?` and `$` their strip,
  and `?` still joins its raise into the handler's bind;
- an array or map index or slice binds its element cell from the
  source's type as the check does, judging no index; a write (`<-`,
  `*r <-`) judges nothing, its target's type being its binding's row;
- a composite runs its children through the instance form
  (`typecheck0_with` over a `node::Child` visitor) and judges only where
  its definition's check recorded no row;
- the instance itself checks neither its arguments nor its return.

**A row binds by position** (`Type::take_row`): a node's type as the
instance compiled it and its row, the same expression's type as the
check left it read through the instance's map, have one shape; each
open cell of the node's binds to the row's part at its position. A
containment walk would not do: a slice pattern's binds share the
union of the element binds' cells (`[a, b, rest..]` types `a` and `b`
`['a, 'b]`), which containment binds one member of. Where the shapes
differ, the instance knows more than the row and its knowledge stands:
a node can be born knowing a type its definition's check widened (the
callback's `d.domain` is born `string` where the check unified its cell
with a formal's `[Array<i64>, string]`;
`lang::functions::instance_node_narrower_than_its_row`, found by the
admin TUI, not the gate). A node whose open cell found no part of its
row derives its type as the check does.

An instance judges nothing: what its definition's check accepted, it
accepts (a refusal there is a check gap; the operators' mixed-numeric
rule was one, closed by `Singleton`, `tvar_constraints.md`). It still
derives a reference's type, which copies its binding's scheme, and a
select's own type, the union of its arms: a row's union, read through
the instance's map, holds the definition's members bound separately
(`[null, 'a, 'b]`, each `i64`), which only a union normalizes, and a
fused region's freeze needs it normalized
(`lang::collection::collection_find_map_default` de-fused).

Instance construction, serial user instructions with the operators'
rule re-run in every instance, then judging nothing, then with selects
and catches read: `par_growth` (20000 slots, an arithmetic callback)
20.2G, 18.0G, 16.7G (-17%); `symbolic` (selects over an ADT) 55.8G,
49.5G at the last step (-11%); `par_wide` 42.4G, 39.5G (-7%).

The default `typecheck0_instance` is the check: the leaves, a reference
(which copies its binding's scheme), and declarations. `GRAPHIX_NO_SUBST=1`
checks every instance.

**Every instance.** An instance records how it is typed
(`node::lambda::Typing`): it is its definition's check, it substitutes
the definition's table, or it was restored from an image. It takes the
table from its definition's `init`, which shares the definition's table
cell, never from the context's registry. A dynamic bind substitutes like
a static one; a callee whose parameter list differs from the site's view
(a defaulted label the view omits) is built at a call's copy of its
definition's signature, fitted to the view as a call fits it. An image
carries every definition's table (`DefTable::image_encode`; a lambda an
instance defined is written with its rows renamed through the enclosing
instance's map), so a restored definition's instances, at run time and
in a program compiled over a restored registration, substitute too. An
instance whose definition recorded no table, or whose signature the
definition's does not hold, is refused: a compiler bug, never a check.

Where the old instance check refined an open cell of a generalized
signature (the element of an empty-array arm beside a typed one), the
substituted type keeps it open: values are the same, a few fused
regions' frozen types differ.

Admin app, `milestone_timing`, same binary, substitution off → on:

| | fusion off | fusion on |
|---|---|---|
| one thread | 395 → 281 ms | 600 → 495 ms |
| four threads | 200 → 151 ms | 390 → 355 ms |

Instance setup is now 9% of the samples, the definitions' checks 8%.

## The audit

`GRAPHIX_ELAB_AUDIT=1` reports every error an instance's check raises
outside a definition gate, and every narrowing the instance's return makes
to its static site. `=bt` adds a backtrace. Over the gate and `regress` it
reports:

- the type-directed builtin hooks above.

The unify-back at `setup_static_bind` never narrowed a site. Once the
audit is clean it becomes a finding that every fuzz lane records.

## Steps

1. Make the audit clean: the builtin constraint (`Concrete` is built);
   instances by substitution, dynamic and restored ones included, are
   BUILT (above).
2. The audits as fuzz findings, soaked. OPEN.
3. Split ExecCtx into the compile context and the runtime. BUILT.
4. Threads. Code generation and statement elaboration are BUILT
   (above). Sibling calls as tasks measured no gain (above). What
   bounds elaboration is each instance's serial build and check.
5. The instance cache: an instance with a closed signature and known
   callbacks is a function of (definition, signature, callbacks); about a
   quarter of admin's instances repeat.
6. Per-unit and per-instance caches in the image.
