# Parallel and incremental compilation

Status: PROPOSAL on branch `parallel-compile` (2026-09-27), not built.
It enters the index when a part of it is built.
Pins: `lang::functions::same_named_tvar_in_callback_arg`; the
`GRAPHIX_ELAB_AUDIT` probe (`node/lambda.rs::elab_audit`).

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
shell's `--check` and the language server do not set `CFlag::CheckOnly`
yet: they still elaborate.

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

## What the compiler needs instead of ExecCtx

- `Env`: persistent, cloned per unit, merged by union.
- A frozen registry of builtins, attributes and tags, shared behind an
  `Arc`.
- Per-unit scratch: `bind_to_lambda`, `rec_defs`, `def_gate_*`,
  `resolving_lambdas`, `pending_settles`, `pending_imports`, `attr_*`,
  `def_assertions`, `batch_connect_targets`. `resolving_lambdas` must be
  per unit. If shared, a thread reaching a definition that another thread
  is resolving would read it as recursion.
- Runtime registrations made at compile time (`rt.ref_var` in about 15
  builtin inits and in the bind, module and error compiles; the TUI's
  `libstate` read) are recorded and replayed at install.

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
   for the printer and builtins.
2. The audit as a fuzz finding, soaked.
3. Split ExecCtx into the compile context and the runtime, still on one
   thread, with the gate green.
4. The phases, still on one thread, with `detcheck`.
5. Threads: bodies, then elaboration, then code generation.
6. Per-unit and per-instance caches in the image.
