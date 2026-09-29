# Parallel and incremental compilation

Status: partly BUILT on branch `parallel-compile` (2026-09-29): parallel
code generation, the compile context split from the runtime, and
statement elaboration in parallel compile tasks. The phases, per-instance
tasks and the caches are the plan.
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
2. The pass ends with a link (and links every `LINK_BATCH` regions):
   the queue compiles on scoped worker threads, the calling thread
   among them, and the results are placed by queue index, so the output
   does not depend on the threads.
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
are gone. On four the link is 85 ms where the backend was 297 ms. What
remains serial in fusion is discovery and emission, about 100 ms.

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

**Printer and builtins record types at the definition.** A type-directed
site (a print, a `parse` target) records its type in terms of the
definition's own type variables. After the definition check has settled
every other cell, each type in the body is a function of the signature's
variables. A bind, static or dynamic, then substitutes the instance
signature's bindings instead of checking again. This removes the runtime
bind's typecheck (`callsite.rs::setup_dynamic_bind`, the lazy
`typecheck1`), and with it the bugs where that recheck fails silently
(`findings/parse-bottom-member-jul2026/`).

## The audit

`GRAPHIX_ELAB_AUDIT=1` reports every error an instance's check raises
outside a definition gate, and every narrowing the instance's return makes
to its static site. `=bt` adds a backtrace. Over the gate and `regress` it
reports:

- the type-directed builtin hooks above;
- runtime-bind rechecks that fail on correct programs and are swallowed.
  They have less information than the site: `[]` in one arm leaves a
  fresh cell a rigid recheck cannot bind. These go away with the
  record-and-substitute design.

The unify-back at `setup_static_bind` never narrowed a site. Once the
audit is clean it becomes a finding that every fuzz lane records.

## Steps

1. Make the audit clean: the builtin constraint; record and substitute
   for the printer and builtins. OPEN (`Concrete` is built).
2. The audits as fuzz findings, soaked. OPEN.
3. Split ExecCtx into the compile context and the runtime. BUILT.
4. Threads. Code generation and statement elaboration are BUILT
   (above). Next: an instance's static binds as tasks of their own, the
   same ownership rule one level down; then module bodies as units.
5. The instance cache: an instance with a closed signature and known
   callbacks is a function of (definition, signature, callbacks); about a
   quarter of admin's instances repeat.
6. Per-unit and per-instance caches in the image.
