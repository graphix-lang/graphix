# Debugging

Status: current principle.
Pins: none: these are tools.

Scoped tracing and every debug environment variable.

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
