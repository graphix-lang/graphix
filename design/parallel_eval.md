# Parallel evaluation

Status: PROPOSED on branch `parallel-eval` (2026-10-03). Nothing is
built. The rules below are the plan; the decisions taken are in §13.
Pins: none yet (§11 lists the ones each phase adds).

## 1. Goal

One cycle's update pass uses every core. Where two subtrees do not
communicate within the cycle, they run under `rayon::join`. Which of
the legal forks are actually taken is decided at run time from measured
cost, and the program can force or forbid them with attributes.

Three commitments shape everything else:

- **Serial equivalence in the language.** A parallel run computes what
  the serial one computes: the same per-cycle productions, the same
  order in the next cycle's write queue, the same error deliveries. The
  serial node-walk stays the oracle, and the fuzzer gains a pair
  (serial vs. forced parallel) instead of losing its bit-for-bit
  comparison. External effects are not part of this: they happen when
  they run, and independent branches' effects have no order between
  them (§6).
- **The serial path pays almost nothing.** A program that never forks
  runs nearly as fast as before; the branch views cost about 1%
  (§4.4). Forking costs only where it happens.
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
| `libstate` | builtins `get_or_default`/`set` per-context library state (T/lib.rs:185): `NetState`, `PrintSink`, `TuiControl`, args | reached through `&` in a forked branch; each value guards its own mutation (§6) |
| in-language shared state | `queuefn`'s `Arc<Mutex<QueueState>>` between the owner and every wrapper site (S/core/queuefn.rs:96): its order decides values | `Ordered` (§6) |
| external effects | `print`/`println`/`dbg`/`log` write synchronously (S/core/lib.rs:2508), `sys_exit` exits, `sys_net_write` writes, net publish batches (S/sys/netstate.rs:420) | happen when they run; no order between parallel branches (§6) |
| compiles at run time | slot growth, dynamic binds, recursive activations, lazily decoded image bodies, dynamic modules and core-trait hook sites (§8) write most of `CompileCtx` | in a compile task per branch, joined at the merge (§8) |
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
> `publishes(i)`, or both contain an `Ordered` call (§6).

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
schedule, and the merge order must stay serial order: two statements'
`<-` writes to one variable queue in the order they merge. So a block's
plan is **runs**, not waves: its statements split into contiguous runs,
and a statement starts a new run when it reads what an earlier
statement of the run publishes or both reach an ordered call
(`analysis::plan_block`). A run forks in halves and merges left to
right; runs run in order. Waves would run a later statement ahead of an
earlier independent one and reorder their writes.

- A run never spans a `catch` (catches run last, serially), and a
  module, trait or impl statement is a run of its own: what a later
  statement reads of it, a core-trait method a comparison dispatches to
  through the value hooks, no summary sees. An impl's methods capture
  only lets written before it, which are in earlier runs.
- `publishes` is each statement's bound ids (`Refs::with_bound`, callee
  bodies left out); an over-approximation, since an inner block's lets
  are out of every sibling's scope.
- A summary reads through a fused kernel's feeders and records the
  reference a `*r <- v` reads.
- The plan is made at a block's first update that may fork and kept in
  the node, not imaged: a runtime that never forks pays nothing for it.
  A callee bound later than the plan stays opaque in it (conservative).

Top-level roots are not planned: a script compiles as one block, which
is.

**`GRAPHIX_PAR_AUDIT=1`**: every forked branch records the variables it
reads (`ForkRt::note_read`, from `read_var`); at a join, a right
branch that read what its left sibling published panics. The forced
gate runs clean under it (5300 tests); its first run found the three
summary gaps listed under "owed to main" and the core-trait hook
dependency.

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

- **Root branch.** The cycle's root holds the runtime and the compile
  state exclusively (`Base::Root(&mut ..)`). It writes
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

### 4.4 As built (phase 2)

`graphix-compiler/src/branch.rs` holds the branch views:

- **`RtView`** (`ctx.rt`): the runtime at the root, or a `ForkRt`
  (store and reference-path deltas, `None` a removal, and a log of every
  other call) over its frozen parent. `Rt::store()` became the per-id
  `store_get`, and `spawn`/`spawn_var` return nothing (no caller kept
  the abort handle; async builtins drop stale results by minting a new
  id on sleep).
- **`Layered`** maps (`ctx.event.variables`, `wake_phantoms`): a
  branch's entries and removals over its parent's. Custom deliveries
  are one locked map every branch shares (`Event::take_custom`,
  `with_custom`).
- **`CxView`** (`ctx.cx`): the parent's compile state, read through,
  until the branch first writes; then a boxed `CompileCtx::fork` that
  joins at the merge. It inherits the parent's compile task: a new task
  changes how a settle treats cells of earlier tasks
  (`tvar::earlier_task`), so giving runtime compiles tasks of their own
  waits on a `GRAPHIX_TASK_AUDIT` run showing they write none (phase 4).
  The pending definition assertions are one shared list, so an
  assertion a branch's bind reaches is checked and retired once.
- **Shared, not forked**: `LibState` (behind a lock, values read out as
  clones), the core-trait hook registry (locked only while it is read
  or written; sites are built and run outside it), the image decoder (a
  `OnceLock`, set once when the registration image is read), `Control`.
- **`fork_join`** forks both sides from the same parent, runs both, then
  merges left then right. Nothing compiled may be pending when it forks:
  a collection's resize applies what it deferred (`ExecCtx::
  apply_deferred`) before its slots run, where serial evaluation used to
  leave it to the first slot's bind.
- **`MAX_FORK_DEPTH`** (16): a branch that deep runs its fork points
  serially. Every lookup walks the parent chain, so an unbounded chain
  (a forced recursion forks at every level) made lookups linear in the
  depth.

**The serial cost** of the views, measured as node-walk instructions
over the bench corpus against the pre-phase-2 build (quick profile):
+0.2% to +0.9%, and about +4% on cycles that update thousands of
collection slots (`stream_stats`, a 5000-element `array::iter`). The
rest is the dispatch at each runtime, compile-state and overlay access.
Accepted (Eric, 2026-10-03); compile-time dispatch (the branch kind as
a type parameter) would remove it at twice the generated node-walk
code.

Fork points built: binary operands, constructor fields (`gather`), call
arguments, `MapQ` slots. `ParMode` (`Off`/`Auto`/`Force`) is on
`Control`, defaulting from `GRAPHIX_PAR`; until the cost model only
`Force` forks.

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

## 6. Builtins and side effects

**External effects happen when they happen.** Printing, logging, file
and network I/O, process exit: a builtin performs its effect at the
moment it runs, as today, with no buffering. Independent branches'
effects have no order between them; within a branch they keep program
order. A program that needs an order makes it a dependency (one call
reads a value the other produces) or runs under `#[serial]`. The
fuzzer already compares a cycle's printed lines sorted
(graphix-fuzz/src/lib.rs:696), since their order within a cycle was an
evaluation-order artifact before any thread existed.

**A builtin must be thread-safe, and the types say so.** A forked
branch reaches library state only through `&`. `LibState` becomes a
map of `Send + Sync` values read through `&self`; a value that
mutates guards itself (`PrintSink` is already
`Arc<Mutex<SinkBuf>>`, S/core/lib.rs:101), and lazy creation
(`get_or_default`) locks the map, which is rare. A builtin that wants
`&mut` library state does not compile until it takes a lock. Node-owned
state (an `Apply`'s fields) needs nothing: the node is updated by one
branch.

**In-language shared state is ordered.** One class remains: state
shared between nodes whose order decides values, not output.
`queuefn`'s queue, shared by the owner and every wrapper call site
(S/core/queuefn.rs:96), is the case in the stdlib today. Two sites
pushing from parallel branches would leave the queue in either order,
and the values read from it later follow that order. Such a builtin
declares itself `Ordered` (a constant beside `EFFECT`). Two subtrees
that both contain an `Ordered` call are dependent (§3.2) and keep their
serial order. A builtin is unordered unless it declares `Ordered`: the
types already force thread safety, the class is rare and visible (state
shared across nodes, usually an `Arc` more than one node holds), and
phase 2 audits the stdlib for it. A missed case shows as run-to-run
nondeterminism, which the forced-parallel fuzzer pair can find in the
stdlib.

**The stdlib audit** (phase 2) found two ordered builtins:
`core_queuefn` and, in the admin package, `netidx_admin_answer` (two
answers to one question race for its one pending slot); both declare
`BuiltIn::ORDERED`, which the registry records (`builtin_ordered`) for
the fork plans. Judged external rather than ordered, though their order
can show in a value: `sys_net_publish`/`publish_rpc` of a path already
published (the second gets the error), `http_serve` binding one port,
`sys_exit`. Already ordered by the log: `core_buffer_decode`'s writes
through references, and every async builtin's spawns (the log replays
them in serial order, so the tasks start in serial order). In phase 4
a cycle's printed lines interleave across branches; the fuzzer sorts
them, and a test that compares a print sink's text within one cycle
has to as well.

Async builtins spawn through `rt.spawn_var`/`set_timer`/`watch`, which
the log carries to the root (§4.2). Their completions arrive in later
cycles in whatever order the world delivers them, as today. A builtin
that holds a callback (`array_group`, `queuefn`, `opt`, net, http)
builds its callee through `CallSite::bind`, which is §8's concern.

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

**Every runtime compile runs in a compile task.** Parallel evaluation
is worth little if the instances it needs are built one at a time: a
wide collection's growth is tens of microseconds per slot, nearly all
of it instance construction. So a branch that reaches one of these
paths compiles in a compile task forked from its parent's compile view.
This is the machinery statement elaboration already uses
(`parallel_compile.md`):

- `CompileCtx::fork`/`join` (C/lib.rs:1163);
- a fresh `tvar::new_task()` per task, so a task never writes a cell an
  earlier task created (`tvar::decided`, refused under `OwnWrites`);
- refs and discards deferred per task.

The fork is made at a branch's first compile, so a branch that compiles
nothing pays nothing. It joins into the parent at the branch merge, in
serial order, as compile tasks join in evaluation order today. Two
branches never see each other's compiles before the join, and §3 makes
that safe: they are independent.

- **Ownership checks turn on at run time.** Runtime code runs with
  `TASK = 0` today, so no cell-ownership check applies to a runtime
  bind. In tasks it does, and `GRAPHIX_TASK_AUDIT` covers runtime binds.
- **Collection growth is chunked.** A collection growing by many slots
  builds them in tasks, one per chunk (the grain from the construction
  histogram, §5), joined in index order.
- **Hot reads are not compiles.** Select patterns and casts read types
  (C/node/select.rs:905, C/node/mod.rs:1655) and kernels borrow an env
  for typed calls (C/fusion/kernel.rs:386). They read the branch's view:
  the parent's compile state through `&`, or the branch's task fork once
  it has one.
- **Two shared resources keep their locks:** the JIT (a kernel install,
  C/fusion/emit/jit.rs:1263) and the image decoder
  (C/node/callsite.rs:1768). Both are taken once per first use.
- **Joins touch disjoint keys.** `TrackedMap::join` keeps the last join
  per key and detects no conflict. Runtime binds write fresh ids
  (`by_id`, `bind_to_lambda` keyed by new `BindId`s) and balanced pairs
  (`resolving_lambdas`, the temporary `lambda_defs` entry), so two forks
  write disjoint keys. Under `GRAPHIX_PAR_AUDIT` the join asserts it.

**Dynamic modules** compile in their branch's task too. Their checks'
internal `par_iter` (C/node/mod.rs:949) nests in the evaluation pool. A
dynamic module is `opaque` in its summary, so nothing forks beside it.

**Core-trait hooks** reach `&mut ExecCtx` through a raw pointer
published in `VALUE_HOOKS` (C/node/coretraits.rs:368) and re-enter node
updates from inside `Value::eq`/`cmp`/`fmt`. The pointer is a loan to
one thread, made by a frame that holds the context and waits inside the
comparison, so under branches each job loans its own branch (§9) and
the aliasing is today's, once per job. The hook call sites are stateful
graphs pooled per (trait, type) in `core_hook_sites`. Two branches
comparing the same abstract type need two sites, so spare sites and
spare events become branch scratch. A site built in a branch's task
joins the registry at the merge; one that two branches both built is
kept once and the other deleted.

**Ids.** Ids minted in parallel are unique (global atomics,
T/ids.rs:138) but their values depend on scheduling. That is acceptable
only if nothing observable depends on id order or value. The phase 1
audit found the cycle loop, event delivery and every runtime map in the
compiler order-free (lookups, or iteration into sets; `GX.nodes` is an
`IndexMap` in insertion order). What does depend on ids, to fix before
phase 4 mints them in parallel:

| site | depends on | reaches |
|---|---|---|
| a reference is `Value::U64(BindId)` (C/node/bind.rs:1282) | the id's value | printing, comparison, sorting, map keys of refs |
| `LambdaDef` `Ord`/`Hash` by `LambdaId` (C/node/lambda.rs:492) | id order | sorting fn values, maps keyed by fns |
| watch and db subscription handles `Ord`/`Hash` by `BindId` (S/sys/watch.rs:218, S/db/subscribe.rs:47) | id order | the same for those handles |
| a watch stream queues one cycle's events in `IntSet` order (S/sys/watch.rs, `WatchStream::update`) | id value | the order in which events of different watches arriving in one cycle are output |
| an inferred tvar is named `'_<TVarId>` (T/typ/tvar.rs:497), synthesized names carry ids (T/expr/seq.rs, `block_component`, `#seam`, `qfn`) | id value | error text that becomes a value (a dynamic module's compile error, `QueueFnErr`) |
| `sorted_tvars` breaks name ties by `TVarId` (T/typ/fntyp.rs:284) | id order | constraint order in diagnostics |
| GUI windows iterated from an `IntMap<BindId, ..>` (S/gui/event_loop.rs:126, 407) | id value | the order of several windows' messages |

Compile-time paths sort by id for stable image bytes (C/image/registration.rs:101) and kernel input order (C/fusion/mod.rs:501); neither is observable.

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
growth against it, and the abort fires at the total (§13).

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
| 2 | Deltas, logs and compile tasks with an artificial fork: under `GRAPHIX_PAR=force`, every legal fork point runs its two sides serially, but through separate branch contexts, separate compile tasks for runtime binds, and the merge. `LibState` through `&`; the stdlib audit for in-language shared state, `Ordered` declarations. | Merge correctness, runtime compiles included, without threads: the fuzzer pair serial vs. forced-merge; `run!` gains a `par` mode. |
| 3 | Fork plans: `publishes`, the waves, imaging the plans, `GRAPHIX_PAR_AUDIT`. | The analysis finds what serial order needed; the audit runs under the fuzzer's forced mode. |
| 4 | The pool, `par::join`, thread-locals, the per-cycle stack budget, the cost histograms and decisions, `#[parallel]`/`#[serial]`, `GRAPHIX_PAR`. | Real parallelism: a bench corpus of wide programs (§12), speedup per core count. |
| 5 | Node-walk collections: slot ranges, and growth built in chunked compile tasks (§8). | Collection scaling, growth included. |
| 6 | Parallel loops inside kernels. | Fused collection scaling. |
| 7 | Book chapter: the independence rule, the attributes, the idioms (§3.2). | — |

Phase 1 is the bulk of the diff, and phases 2–3 are where correctness
is won. Phase 4 is small by comparison, because by then a fork is
already a merge, and the only new thing is that the two sides run at
once.

### Found on this branch, owed to main

- **Dependency summaries were blind to fused kernels.**
  `analysis::local_summary` walks with `fusion::for_each_node`, which
  does not descend into a `FusedKernel`, so a kernel's reads through its
  feeders were missing from every summary. Phase 3's block plans hit it
  (`graphix-shell/tests/jit_arena_rotation.rs` hung under
  `GRAPHIX_PAR=force`); the branch fixes it in `local_summary`. The seq
  machine plans its step boundaries from the same summaries
  (`analysis::plan_machines`), so on main a fused step that reads a
  variable a pending write targets may be judged same-cycle and read the
  stale value. **If this branch is not merged, investigate and fix that
  on main** (Eric, 2026-10-03).
- **A write through a reference (`*r <- v`) read nothing in its
  summary,** though it reads `r` to find the target: the walk visits
  only its right side. The block plan put `let r = &v` and `*r <- 1`
  in one run (`lang::select::select_guard_after_tainted_init` under
  `GRAPHIX_PAR_AUDIT`); the branch records the read. Owed to main on
  the same terms.

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

## 13. Decisions

Taken (Eric, 2026-10-03):

1. `#[parallel]` is a compile error where the analysis finds nothing to
   fork within it.
2. `#[serial]` is dynamic: it covers callees.
3. The stack budget is per cycle, shared by every worker (§9).
4. External effects happen when they run, unbuffered and unordered
   between parallel branches (§6). Programs that need an order say so.
5. Runtime compiles run in compile tasks, in parallel, from the start
   (§8): parallel evaluation without parallel instance construction is
   not worth having.
6. The fork decision reads the 75th percentile to start (§5), tuned
   against the phase 4 benches.
7. A builtin is parallel-safe unless it declares `Ordered` (§6); the
   stdlib is audited in phase 2.

