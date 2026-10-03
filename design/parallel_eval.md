# Parallel evaluation

Status: PROPOSED on branch `parallel-eval` (2026-10-03). Nothing is
built. The rules below are the plan; open decisions are in §13.
Pins: none yet (§11 lists the ones each phase adds).

## 1. Goal

One cycle's update pass uses every core. Where two subtrees do not
communicate within the cycle, they run under `rayon::join`. Which of
the legal forks are actually taken is decided at run time from measured
cost, and the program can force or forbid them with attributes.

Three commitments shape everything else:

- **Serial equivalence.** A parallel run is observably identical to the
  serial one: the same per-cycle productions, the same order in the
  next cycle's write queue, the same order of output. The language
  already leaves independent computations unordered. Where an order is
  observable, the parallel run reproduces it. The serial node-walk then
  stays the oracle, and the fuzzer gains a pair (serial vs. forced
  parallel) instead of losing its bit-for-bit comparison.
- **The serial path pays nothing.** A program that never forks runs as
  fast as today. Forking costs only where it happens.
- **Not tuned to today's corpus.** The corpus is small and was written
  for a serial engine. Parallel evaluation will change which programs
  people write and how they write them. The cost model and the
  attributes must serve programs we do not have yet: wide data-parallel
  collections, many independent pipelines, large fan-out UIs.

Non-goals: overlapping cycles (pipelining), parallel compile (that is
`parallel_compile.md`, reused here in §8), and any change to what a
program means.

## 2. What crosses between subtrees within a cycle

A fork is legal only where the two sides do not communicate within the
cycle. The inventory below lists every channel by which one subtree's
update can affect another's in the same cycle, today, and how each is
treated under a fork. (Survey of the tree at `ecb3bb23`; C =
`graphix-compiler/src`, T = `graphix-types/src`, RT = `graphix-rt/src`,
S = `stdlib/graphix-package-*/src`.)

| channel | today | under a fork |
|---|---|---|
| `event.variables`, the overlay (C/lib.rs:156) | a `let` (`Bind::update`, C/node/bind.rs:372/384), a call's args (`publish_production`, C/node/callsite.rs:2348), pattern binds, collection element delivery (C/node/collection.rs:1258), seq `pc` (C/node/seq_machine.rs:336) insert; readers look here first (`read_var`, C/node/mod.rs:68) | per-branch delta over the parent's (§4) |
| `rt.store` stamped with the cycle | a publication also writes the store; a store entry stamped this cycle reads as Delivered (C/node/mod.rs:76) | per-branch delta, as the overlay |
| `rt.updated` via `notify_set` | a `let` schedules later roots that read it, this cycle (RT/rt.rs:215) | logged; applied at the root (§4.3) |
| `rt.var_updates`, the write queue | `<-` and `set_var`/`patch_var` queue in evaluation order; at delivery the first write per id wins the cycle, later ones re-queue (RT/gx.rs:403) | logged per branch, concatenated in serial order |
| error delivery | a `?` delivers to a same-top handler's overlay slot; first raiser wins, later ones go to `set_var` (C/node/error.rs:567) | overlay delta + the same first-wins rule at merge (§4.2) |
| `ErrorHandler` counters | `raise`/`handled` add and subtract on `raised`/`nested` atomics (C/lib.rs:1646) | commutative; the reader (a `Catch`) runs after its covered statements, never beside them |
| scoped flags `event.init`, `wake_init` | set and restored around a child (13 scopes: CallSite, MapQ, FoldQ, Catch, Module, Select, seq) | per-branch copies |
| overlay removals | `CachedArgsAsync` removes its own id (S/core/lib.rs:904); consumers `take` `event.custom` entries (S/http/lib.rs:839, S/sys/net.rs:809, S/sys/watch.rs:509) | tombstones in the delta; `custom` behind a `Mutex` (rare) |
| other `Rt` calls | `ref_var`/`unref_var`, `set_ref_path`, `set_timer`, `spawn`, `spawn_var`, `watch`, `watch_var` (RT/rt.rs:158-251) | logged; applied in order at merge. `set_ref_path` is also read back (`ref_path`), so it joins the delta |
| `libstate` | builtins `get_or_default`/`set` per-context library state (T/lib.rs:185): `NetState`, `PrintSink`, `TuiControl`, args | a builtin that touches it is `Shared` (§6) |
| shared builtin state | `queuefn`'s `Arc<Mutex<QueueState>>` between the owner and every wrapper site (S/core/queuefn.rs:96); net publish batches (S/sys/netstate.rs:420) | `Shared` (§6) |
| external output | `print`/`println`/`dbg`/`log` write synchronously (S/core/lib.rs:2508), `sys_exit` exits, `sys_net_write` writes | `Shared`; printing moves to a branch sink (§6) |
| compiles at run time | slot growth, dynamic binds, recursive activations, lazily decoded image bodies, dynamic modules and core-trait hook sites (§8) write most of `CompileCtx` | under the compile lock (§8) |
| thread-locals | kernel loans, `VALUE_HOOKS`, the interrupt scope, `RUNTIME_BIND`, `DESELECTING_ARM`, tvar `LEVEL`/`TASK` | re-entered per job by the one fork helper (§9) |

Nothing else crosses. Node state is owned by its node (sleep state is
local by rule), fused kernels keep their state in node-owned blocks
(C/fusion/kernel.rs:350), and nodes are already `Send + Sync`
(C/lib.rs:549).

## 3. Where forks are legal

### 3.1 Fork points

A fork point is a node with several children it updates in sequence.

| fork point | children | notes |
|---|---|---|
| the root loop (RT/gx.rs:475) | the roots scheduled this cycle | a root may be scheduled mid-cycle by an earlier one (`notify_set`); runs in waves (§4.3) |
| `Block` (C/node/mod.rs:1021) | statements in `evaluation_order` | a block's catches run last, after a join of what they cover |
| `CallSite::update_call` (C/node/callsite.rs:1549) | the arguments (`ArgMap`, an `IndexMap`: insertion order) | then the callee, which depends on all of them |
| constructors: struct, tuple, variant, array, list literals, string interpolation (`gather`, C/node/mod.rs:398) | fields | |
| binary operators (C/node/op.rs:173) | the two operands | |
| `MapQ` (C/node/collection.rs:918) | slots, as a range | init, map, filter, filter_map, flat_map, find, find_map; `find` keeps the first match in index order at merge |
| `FusedKernel` feeders (C/fusion/kernel.rs:311) | node-walked feeder subtrees | the kernel call depends on all of them |

Not fork points: `Select` (the arm depends on the scrutinee; guards are
consulted in order and a consulted bottom guard decides), the seq
machine (its steps are its order), `FoldQ` (slot `i+1` reads slot `i`'s
production; §12), `Any` and `Sample` (order is their meaning; `Any`
could fork with a left-first merge, but it is not worth it in v1).

### 3.2 The independence rule

Within a cycle, two siblings communicate only through what the earlier
one PUBLISHES and the later one READS: a `let`'s value, a pattern bind,
a call's synthetic argument id, an error delivered to a handler both
reach. A write (`<-`, `set_var`, `patch_var`) never makes siblings
dependent. It lands next cycle, and the merge keeps the queue in serial
order (§4.2).

> Sibling `j` depends on an earlier sibling `i` iff `reads(j)` meets
> `publishes(i)`, or both contain a `Shared` call (§6).

`reads` is the dependency summary's read set
(`dependency_summaries.md` §2), following statically resolved calls
into their instances. It has one gap: a `Deref` reads `refs`, and a
call with no static target reads `all`. Both meet any non-empty
publication set.

`publishes` is new. It is the set of bind ids a subtree delivers within
the cycle that a later sibling can name: the statement-level `let`s
and pattern binds of a block, and the error handlers both reach. A
`let` inside a statement's inner block is out of every later sibling's
scope, so scope bounds the set without alias analysis. A closure that
captured a `let` reaches a sibling only as a value: through a
publication (which `reads` sees, following the call into the
instance) or through a variable written in an earlier cycle and called
dynamically, which reads `all`.

A dynamic call that reads `all` depends on every earlier sibling with a
non-empty `publishes`. A program that wants parallelism across dynamic
calls passes values as arguments rather than through captured `let`s.
That is the idiom the book will teach.

### 3.3 Fork plans

A `join` forks two closures, so a fork point needs a series-parallel
schedule, not a DAG. For each fork point the analysis computes a
**fork plan**: the children split into a sequence of waves, where each
wave's members are independent of each other and depend only on
earlier waves. A child goes in the wave after the last wave holding
something it depends on. Within a wave, the cost model (§5) decides
which members fork and how they pair. A DAG loses a little to waves.
Waves are what `join` can express, and they are predictable.

Plans are computed per instance (as `plan_machines` plans seq steps),
after resolution, in the analysis pass. The summaries are already
computed there for seq machines. This extends that computation to
every fork point with at least two children, stores the waves in the
node, and images them with the node (as `Step::same_cycle` is). A
runtime bind plans the new instance in `analyze_bound_callee`
(C/analysis.rs:229). This is a new consumer of an existing walk, not
a new walk. The loss without it is the whole feature.

A fork point whose children are all in one wave per child (a chain)
stores nothing and never forks.

## 4. The branch context

### 4.1 Shape

`ExecCtx` today is one exclusive context: compile state, runtime,
library state, the event overlay. Under a fork, each side needs its own
view. A shared `ExecCtx` (`dashmap` for the maps, a `Mutex` around the
rest) would put a sharded lock on every variable read. The overlay is the hottest map in the engine: every `Ref` reads
it. So the design keeps `&mut` on the hot path and makes the forked
side's context a different value instead.

```rust
pub struct ExecCtx<'a, R: Rt, E: UserEvent> {
    shared: &'a Shared<R, E>,    // frozen while any branch below is live
    base: Base<'a, R, E>,        // the root's exclusive state, or nothing
    delta: Delta,                // this branch's overlay/store/ref-path delta
    log: EffectLog,              // what it would have done to the runtime
    init: bool, wake_init: bool, // the scoped flags
    serial: bool,                // under #[serial] (§7)
}
```

- **Root branch.** The cycle's root holds the runtime, library state
  and compile state exclusively (`Base::Root(&mut ..)`). It writes
  through, exactly as today. An unforked program never builds a delta
  or a log: that is the "serial path pays nothing" rule in the type.
- **Forked branch.** `join` reborrows the parent's state as `&` for
  both sides and gives each side an empty `Delta` and `EffectLog`
  (pooled, `GPooled`, since they cross threads). Reads go: own delta,
  then the parent chain (frozen for the join's duration), then the
  store. Writes go to the delta or the log.
- **Event.** `Event` dissolves into the branch: `variables` becomes the
  delta chain over the root's overlay, `init`/`wake_init` become branch
  fields, `user` and `custom` move to `Shared` (`custom` behind a
  `Mutex`; it is taken a few times a cycle). `update`'s signature loses
  its `event` argument.
- **Bounds.** Sharing the runtime's read half across threads needs
  `R: Sync` and `E: Sync`. `GXRt`'s non-`Sync` fields (the `SelectAll`
  of watch streams) are touched only through `&mut` at merge, so they
  sit in an exclusive-access wrapper.

The alternative puts the runtime behind `&` with internal locks. This
design buffers instead. A forked branch never calls a mutating `Rt`
method; it logs the call, and the root applies the logs in serial order.
`Rt` keeps `&mut self` and loses nothing on the serial path. The
runtime's interface does not grow locks.

The rename is mechanical but large: every `update`, `delete`, `sleep`
and builtin `eval` in the compiler and the stdlib. Phase 1 (§11) does it
with no fork at all, so the gate and a soak prove serial equivalence of
the refactor alone before any thread exists.

### 4.2 Merge

At a join, the right branch's results are appended after the left's,
into the parent (a delta, a log, or the root's state):

- **Delta.** Overlay, store and ref-path entries insert into the
  parent. Two branches publishing the same id is an analysis bug,
  except for error handlers. Here the serial rule is reproduced: the
  left (earlier) delivery keeps the slot, and the right's becomes a
  queued write, as `deliver_error`'s `Occupied` arm does
  (C/node/error.rs:584). A tombstone removes the key.
- **Log.** The right log concatenates after the left. At the root, the
  log replays into `Rt` in order. That covers the queue order of
  writes, `ref_var`/`unref_var` pairs, timer and task spawns, and
  `notify_set`.
- **Productions.** A fork point's own result is built after the join,
  as today, from its children's residents.

Under `GRAPHIX_PAR_AUDIT=1` each branch also records the ids it read
from outside its own delta. At a join, the right branch's reads must
not meet the left branch's publications. A miss is an analysis bug, and
the audit panics with both ids and both subtrees. That makes the
fuzzer's forced-parallel pair test the analysis itself, not only the
outputs.

### 4.3 The root loop

The root loop runs the scheduled roots in waves of the top-level fork
plan. Before each wave it takes the roots scheduled so far, including
any an earlier wave's `notify_set` scheduled. After the wave, the
merged log applies, which may schedule more. A root scheduled by a
later root in the same wave would not have run this cycle in the serial
order either: the serial loop visits roots in `IndexMap` order and only
sees marks set before its position.

The cycle enters the rayon pool (`install`) only when a fork is
expected: the cycle cost histogram (§5) says the last cycles were
expensive, or a `#[parallel]` region is live. Otherwise it runs inline
on the driver thread, as today, with no thread handoff.

## 5. The cost model

The engine measures, and the measurements choose the fork points.

**What is measured.** Each child of a fork point with a multi-member
wave carries a coarse log2 histogram of its update cost: 16 buckets of
saturating `u16` counts, 32 bytes, covering roughly 16 ns to 0.5 ms
and up. A sample is two cycle-counter reads around the child's update
and one increment of the bucket `log2(ns) - 4`, clamped to 0..15.
Collections keep one histogram for per-slot update cost and one for
slot construction (a new slot builds and checks an instance: tens of
microseconds), not one per slot. The cycle as a whole keeps one, which decides whether to enter
the pool at all. A fork point that is a chain stores nothing.

**When it is measured.** Every update while a child's decision is
unsettled; then one update in N, with N doubling while the decision
holds (to 1 in 256), and halving the counts as samples arrive so old
behavior fades. A changed decision resets N. Sampling keeps the
steady-state overhead to a branch and a counter.

**What is decided.** For a wave, the estimate per member is a quantile
of its histogram (p75 by default: a member that is usually cheap but
sometimes huge should still fork). Members are packed into a balanced
binary tree of joins by estimate, greedily largest first. A pair is
joined only if both sides' estimates exceed `T`; members below `T` are
grouped and run serially on one side. `T` is a multiple of the measured
cost of a stolen job, calibrated when the pool starts (order of a few
microseconds). An unstolen `join` costs much less, but `T` must cover
the case where the steal happens.

For a collection, `grain = ceil(T / per_slot_estimate)`. The slot range
splits in halves down to `grain`, as rayon's indexed iterators split,
but cost-weighted. Growth uses the construction histogram for the new
slots.

**Measured time is wall time.** A child that forked internally reports
less than its work, so the parent errs toward not forking it. That
direction is safe: the child is already parallel inside. True work
accounting (each branch summing its leaves' time) is possible later if
the wall-time bias turns out to cost.

**Not imaged.** Histograms are run-time state; a warm start relearns
them. Fork plans (§3.3), which are static, are imaged.

**Before there is data,** a fork point runs serially, unless an
attribute forces it.

## 6. Builtins

Every builtin gets a second classification beside `EFFECT`
(`effects.rs`): whether it touches state another node can observe
within the cycle.

- **`Local`.** All its state is its own node's (an `Apply`'s fields), or
  it has none. Every `Stateless` builtin is `Local` by rule. A `Sync`
  builtin with node-owned state (`once`, `uniq`, `count`, `hold`,
  `take`, `skip`) is `Local` once audited.
- **`Shared`.** It reads or writes `libstate`, an `Arc` shared with other
  nodes (`queuefn`), process-global state, or performs output in
  `update`. Two subtrees that both contain a `Shared` call are ordered
  (§3.2): they never run beside each other, so their effects keep their
  serial order.

The default for a builtin not yet audited is `Shared`: correctness
first, parallelism as the audit proceeds. The builtin trait gains a
constant for it, like `EFFECT`.

Printing is the exception worth engineering, because printing is how
people debug parallel code. `emit_line` (S/core/lib.rs:2508) already
writes to a `PrintSink` when one is installed. Under a fork, each
branch gets a sink buffer in its log, and the root writes the buffers
out in serial order at merge. `print`, `println`, `dbg` and `log`
become `Local`. Output appears at the end of the parallel region rather
than mid-cycle. A cycle is atomic, so nothing can tell the difference
except a wall clock.

Async builtins spawn through `rt.spawn_var`/`set_timer`/`watch`, which
the log already orders. Their completions arrive in later cycles in
whatever order the world delivers them, as today. A builtin that holds
a callback (`array_group`, `queuefn`, `opt`, net, http) builds its
callee through `CallSite::bind`, which is §8's concern. Being `Local`
says nothing about that.

`rand` uses the thread-local RNG. Its values are not reproducible today
either, so nothing changes.

## 7. Control attributes

The cost model chooses by default. Two attributes override it. Both are
compiler-reserved directives, not registry check attributes: they
change how the decorated node runs, so they are read at node
construction, like the definition assertions.

- **`#[parallel]`, optionally `#[parallel(grain: n)]`.** Every legal
  fork point lexically within the decorated expression forks whenever
  its wave has more than one member, whatever the cost model says. A
  collection splits its slot range down to `grain` slots per task
  (default: the cost model's grain when it has data, else the range
  divided evenly across the workers). On a definition
  (`#[parallel] let f = |..| ..`) it applies to the body's fork points
  in every instance. It is also an assertion, as `#[native]` is: if the
  analysis finds no wave with two members within the expression, the
  attribute is a compile error naming the dependency that serialized
  it. The programmer who asks for parallelism learns at compile time
  that there is none, and why.
- **`#[serial]`.** No fork while evaluating the decorated expression,
  callees included. It is a branch flag set and restored around the
  node's update, like `init`.

The asymmetry is deliberate. `#[parallel]` is lexical: forcing every
fork in every callee would fork tiny library functions everywhere.
`#[serial]` is dynamic: "run this serially" means the whole
computation, wherever it calls.

Process-level control: `GXConfig::parallel: Off | Auto | Force` and
`GRAPHIX_PAR=off|auto|force`. `Force` treats every fork point as
`#[parallel]` (for the fuzzer and tests). `GRAPHIX_EVAL_THREADS` sizes
the pool.

## 8. Compiling at run time

The update pass is not compile-free. Six paths build or check code
mid-cycle:

| path | entry | what it writes |
|---|---|---|
| slot growth | `MapQ`/`FoldQ` `resize` → `Slot::new` (C/node/collection.rs:483) | env binds, `pending_refs`; the slot's call binds at its first update (next row) |
| dynamic bind, recursive activation, callback-holding builtins | `CallSite::bind` (C/node/callsite.rs:1092) inside `with_runtime_settles` | instantiate + typecheck, `lambda_defs`, `bind_to_lambda`, `fn_forward_resolutions`, analysis atomics, `apply_deferred`; a slot also reuses its prototype's kernels (no codegen) |
| lazy image decode | `CallSite::materialize` (:1765) | the decoder `Mutex`, `pending_refs`, a kernel install under the JIT mutex |
| dynamic module | `Module::update` → `compile_source` (C/node/module.rs:781) | all of `CompileCtx`; its checks already run on rayon from inside `update` |
| core-trait hook sites | `take_site` → `build_site` (C/node/coretraits.rs:224) | env, `pending_refs`, `core_hook_sites`; entered from inside `Value::eq`/`cmp`/`fmt` |
| delete and sleep | `unbind_variable`, `unref_var`, `Lambda::delete` | env, `pending_refs`, `lambda_defs`, `bind_to_lambda` |

**v1: one compile lock.** `CompileCtx` lives in `Shared` behind a
`parking_lot::Mutex`. A branch that reaches one of these paths takes
the lock, compiles against the master, and releases it. Two hot read
paths do not take the lock. Select patterns and casts read types
(C/node/select.rs:905, C/node/mod.rs:1655), and fused kernels borrow an
env for typed calls (C/fusion/kernel.rs:386). These read an `Arc<Env>`
snapshot held by the branch. The snapshot is cheap: `Env` is persistent
maps. A branch's snapshot refreshes after its own compile, so new code
sees what it was compiled against. Other branches keep their snapshot:
they are independent of the compile by §3, and the types they read did
not change under them.

Deferred work stays deferred. `record_ref` and `discard` collect into
the branch's log rather than `CompileCtx`, and `apply_deferred` becomes
a log replay at the root.

**v2: slot construction in compile tasks.** The lock serializes the
expensive part of a parallel collection's growth: tens of microseconds
per slot, all under one mutex. The compile-task machinery already does this for
statement elaboration (`parallel_compile.md`). `CompileCtx::fork`/`join`
(C/lib.rs:1163), task-owned type cells (`tvar::new_task`, `decided`),
and joins in evaluation order. A collection that grows by many slots
builds them in tasks, one per chunk, and joins in index order. Under
v1, a large growth is one cycle's serial cost, as today.

**Dynamic modules.** The module recompile runs rayon from inside
`update` today (C/node/mod.rs:949). Under the eval pool, that `par_iter`
runs on the same pool, which nests correctly. A dynamic module is
`opaque` in its summary, so nothing forks beside it.

**Core-trait hooks** reach `&mut ExecCtx` through a raw pointer
published in `VALUE_HOOKS` (C/node/coretraits.rs:368) and re-enter node
updates from inside `Value::eq`/`cmp`/`fmt`. Under branches, the
pointer is the branch context, installed per job (§9), so a hook runs
in the branch that compared. Building a new hook site takes the compile
lock. The `core_hook_sites` registry moves into `Shared` behind its own
`Mutex`, and spare events become branch scratch.

**Ids.** Ids minted in parallel are unique (global atomics,
T/ids.rs:138) but their values depend on scheduling. That is acceptable
only if nothing observable depends on id order. Phase 1 audits every
iteration over an id-keyed map whose order reaches output.

## 9. Threads

There is one helper, `par::join(ctx, a, b)`, and nothing else calls
`rayon::join`. It forks the branch contexts, re-enters on the stolen
side every thread-local a job inherits nothing of, joins, merges, and
propagates a panic.

| thread-local | treatment |
|---|---|
| `CURRENT` (the interrupt `Control`, T/stack.rs:222) | installed per job; null on a worker today, which would silently stop interrupt polling there |
| `VALUE_HOOKS` | installed per job, pointing at that job's branch |
| `RUNTIME_BIND`, `DESELECTING_ARM` (C/node/mod.rs:173) | copied into the job |
| tvar `LEVEL`/`TASK` | only under a compile, which re-enters them already |
| kernel loans `KERNEL_ABORT`/`KERNEL_ENV`/`QOP_RAISES`/`KERNEL_PANIC`, `SELF_BLOCK_*` | set and consumed within one kernel call on one thread: unchanged |
| `GROWN` (stack) | per thread: correct as is |
| `FastMemo`, `LPooled` pools, scratch buffers | per-thread caches: unchanged; values freed on another thread return to that thread's pool |

**The pool.** A dedicated evaluation pool per process, shared by every
runtime in it. The compile pool's size is set for the fuzzer
(`RAYON_NUM_THREADS=2`) and must not govern evaluation. Worker stacks
are 16 MB, with `stacker` growing segments as today.

**Stack budget.** `GRAPHIX_STACK_BUDGET` is per thread today. With N
workers the worst case is N times the budget, and the fuzzer's children
run under an 8 GB address-space cap. The budget becomes per cycle: the
`Control` holds an atomic of bytes granted, every worker charges its
growth against it, and the abort fires at the total. §13 asks whether
that is the containment we want.

**Interrupt and abort.** `Control` is atomics, already shared. Every
job polls it through its own `CURRENT`. A panic in a job (a kernel's
resumed panic included) propagates out of `join` to the root as today.

## 10. Fused kernels

A kernel invocation is a leaf. Its state is node-owned (`state`/`site`
blocks), its loans are per call, and its feeders are a fork point. So
kernels need nothing for the node-level design.

Parallel loops inside kernels (§11 phase 6) are the second half. The
eight native collection loops (`fusion/emit/scaffold.rs`) keep
per-slot state in per-slot words (`kernel_instance_state.md`). The map
family (init, map, filter, filter_map, flat_map, find, find_map) can
outline the loop body into a function and call a helper,
`graphix_par_for(lo, hi, grain, body, env)`, which runs chunks under
the same `par::join`:

- The firing word (`SlotFlags`) is an OR over slots: each chunk folds
  its own word and the helper ORs them.
- Results are written by index; filter-family results concatenate
  chunks in order.
- `?` raises (`QOP_RAISES`) collect per chunk and are delivered in index
  order.
- `find` takes the lowest matching index.
- The grain comes from a per-call-site histogram, as for node-walk
  collections.

`fold` stays serial (§12).

## 11. Phases

Each phase lands on the branch with the gate green, and the
semantics-touching ones soak before the next.

| phase | content | proves |
|---|---|---|
| 1 | The branch context, serial only: `ExecCtx` split into `Shared` + branch, `Event` folded in, every rt/libstate/compile write routed through the root's exclusive state, the `custom`/tombstone/id-order audits. No delta, no log, no thread. | The refactor is serial-equivalent and costs nothing: the gate, a fleet soak, and a GUI-suite and admin-TUI timing at parity. |
| 2 | Deltas and logs with an artificial fork: under `GRAPHIX_PAR=force`, every legal fork point runs its two sides serially, but through separate branch contexts and the merge. Builtin `Local`/`Shared` classification; branch print sinks. | Merge correctness, without threads: the fuzzer pair serial vs. forced-merge; `run!` gains a `par` mode. |
| 3 | Fork plans: `publishes`, the waves, imaging the plans, `GRAPHIX_PAR_AUDIT`. | The analysis finds what serial order needed; the audit runs under the fuzzer's forced mode. |
| 4 | The pool, `par::join`, thread-locals, the per-cycle stack budget, the cost histograms and decisions, `#[parallel]`/`#[serial]`, `GRAPHIX_PAR`. | Real parallelism: a bench corpus of wide programs (§12), speedup per core count. |
| 5 | Node-walk collections: slot ranges; then slot construction in compile tasks (§8 v2). | Collection scaling, growth included. |
| 6 | Parallel loops inside kernels. | Fused collection scaling. |
| 7 | Book chapter: the independence rule, the attributes, the idioms (§3.2). | — |

Phase 1 is the bulk of the diff, and phases 2–3 are where correctness
is won. Phase 4 is small by comparison, because by then a fork is
already a merge, and the only new thing is that the two sides run at
once.

## 12. Deferred and declined

- **Parallel fold.** A fold's chain is sequential. A tree-shaped
  reduction would need the callback to be associative, which nothing
  checks. A later `#[parallel]` on a fold could assert associativity, as
  `#[native]` asserts zero residue; not in this plan.
- **Selects.** The scrutinee decides the arm, so the arm cannot start
  early. Speculative arms would break sleep-is-pause.
- **A parallel delete.** Shrinking a large collection deletes serially in
  v1; delete writes env and refs, both cheap to log, so it can follow
  phase 5 if a profile asks.
- **Benches.** The corpus has few wide programs, by its history. Phase 4
  adds a parallel bench set written for this engine: wide maps over
  heavy callbacks, many independent pipelines, a fan-out UI. These are
  measured as the intended workload, not checked against what existing
  programs do.

## 13. Open decisions

1. **`#[parallel]` as an assertion.** Refuse it where the analysis finds
   nothing to fork? Recommended: yes, as `#[native]`.
2. **`#[serial]` through calls.** Dynamic, as proposed? Recommended: yes.
3. **Stack budget per cycle** (§9) rather than per thread? Recommended:
   yes; per-thread budgets multiply with the pool.
4. **Printing at the end of a parallel region** rather than mid-cycle
   (§6). Recommended: accept; a cycle is atomic.
5. **Unaudited builtins default to `Shared`** (§6). Recommended: yes; the
   audit raises parallelism, never correctness.
6. **The quantile** the decision reads (§5): p75 to start, tuned against
   the phase 4 benches.
