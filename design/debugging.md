<!-- CR claude for claude: [structure/medium] Every knob in this file serves the
compiler's developers. An author whose program does nothing (a bottom, a sleeping arm, a
constant that stands, a stalled seq step) has only `dbg`, which prints when a value
fires and so is silent in exactly those cases; the per-cycle trace behind
`GXHandle::trace_start` records roots for the fuzzer and the shell does not expose it.
Add one author tool: `graphix --trace name[,name]` printing, each cycle a named binding
is updated, its tag (fired, stale, fresh bottom, stale bottom, moved) and value, and for
a seq its current step. Found by reading; not run. (a-language-08) -->
<!-- 2026-11-27 claude: Eric ruled (2026-11-27, D63): the author's tool is a proper
debugger, a REPL attached to the running program over a unix socket with debugger
commands (why, watch, pause and step, inject), not a --trace flag. Its design is
design/debugger.md, written before it is built. -->
# Debugging

Status: current principle.
Pins: none: these are tools.

## What a running program reports

Two log targets are the program's own, and the shell shows both whatever
`RUST_LOG`'s level (`graphix-shell/src/main.rs::start_logger`):

- `FAILURE_TARGET` (`graphix::failure`, error): an operator that failed
  and bottomed (a division by zero, `MIN / -1`), an error nothing
  handles, a write that was dropped, a program that spins. Both engines
  report an integer division's failure in the same words: the node-walk
  from `node/op.rs`, a kernel from the cold path of its guard
  (`fusion::emit::nodes::report_int_div`), once a fire of an operand.
- `DROPPED_TARGET` (`graphix::dropped`, warn): an error a `$` dropped.
  The author asked for the drop, and it is still the usual cause when a
  program does nothing: over the admin port's whole test suite no `$`
  drops an error, and of the 14 dropped across the tui package's tests,
  4 are one known bug in the stdlib's browser.

With no `--log-dir` the log is stderr: `RUST_LOG` alone is enough, a
program's report prints as its message with no level or module, and each
distinct report prints once however often it recurs. With `--log-dir`
everything goes to the file, every occurrence, and errors to stderr too.
`RUST_LOG=off,graphix::dropped=off` silences the drops. `graphix lsp`
logs to stderr at warn by default. Pins:
`graphix-shell/tests/shell/swallowed_error_logs.rs`.

What no log can say is why nothing happened where nothing failed: an arm
asleep, a constant that stands, a seq waiting on a step, an input that
never arrived. That is the debugger's (`design/debugger.md`).

Scoped tracing and every debug environment variable.

`TRACE` (`set_trace`, `with_trace(enable, spec, f)`, `tdbg!`) scopes
compiler tracing to one expression — the stdlib typechecks on every
compile, so unscoped prints are gigabytes.

<!-- CR claude for claude: [doc-drift/low] The table lacks `GRAPHIX_FUSE_AUDIT=1` (run!
prints FUSEAUDIT, FUSEAUDIT-BLOCKER and PARAUDIT-NOFORK lines instead of asserting
FuseExpect; only testing.md says so). `GRAPHIX_QUIET` is a dbg_flag!, so any value,
`off` included, enables the audit the row documents as `=audit`. Either say 'any value'
or read it with env_mode as GRAPHIX_PRUNE is read. Found by reading; not run.
(x-doc-drift-11) -->
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
| `GRAPHIX_DBG_CYCLE_BT=1` | a backtrace at every `AmbiguousMember` refusal |
| `GRAPHIX_FUSE_SERIAL=1` | fusion visits every part in order on one context instead of fusing disjoint subtrees in tasks (A/B: the decisions must agree) |
| `GRAPHIX_TM_SITES=1` | `graphix-fuzz typemorph <file>`: a must-reject flip's verdict also says where the refusal was placed (the expression, the positions it passed) and what the mutation reached; triage only, since the verdict text is a finding's class |
| `GRAPHIX_ABORT_ON_NO_MATCH=1` | an abort at a select no arm matched, a coverage hole (both engines report it once; the fuzzer's children set it, so a hole is a crash finding) |
| `GRAPHIX_PAR_AUDIT=1` | a panic at a join whose right branch read what its left sibling published |
| `GRAPHIX_QUIET=audit` | a panic where a reader in an ordinary cycle reads a quiet value, which no wake stood again (`Tag::moved`), that differs from its last production: a value changed under a reader nothing told to recompute (`design/bind_driven_bodies.md`, the moved mark; a seq machine's own names are exempt). The fixtures of `lib_tests::bottom` that deliver a changed value stale by hand trip it |
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
