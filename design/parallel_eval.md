# Parallel evaluation

Status: built (phases 1-6, §11); the book chapter (phase 7) is
deferred. `Auto` is the default mode. The decisions taken are in §13.
Pins: `lang::par_attrs`, `lang::par_loops`, every `run!` fixture's
`par` and `jit_par`, `cost::tests`, the fuzzer's `Pair::Par`.

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
| `Block` (C/node/mod.rs:1021) | statements in `evaluation_order` | a block's catches run last, after a join of what they cover |
| `CallSite::update_call` (C/node/callsite.rs:1549) | the arguments (`ArgMap`, an `IndexMap`: insertion order) | then the callee, which depends on all of them |
| constructors: struct, tuple, variant, array, list literals, string interpolation (`gather`, C/node/mod.rs:398) | fields | |
| binary operators (C/node/op.rs:173) | the two operands | |
| `MapQ` (C/node/collection.rs:918) | slots, as a range | init, map, filter, filter_map, flat_map, find, find_map; `find` keeps the first match in index order at merge |

Not fork points: the root loop (top-level roots are not planned, §4.3),
a fused kernel's feeders (`FusedKernel::update` polls them in order),
`Select` (the arm depends on the scrutinee; guards are
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

Under a fork, each side needs its own view of the compile state, the
runtime and the event overlay. A shared `ExecCtx` (`dashmap` for the
maps, a `Mutex` around the rest) would put a sharded lock on every
variable read, and the overlay is the hottest map in the engine: every
`Ref` reads it. So `ExecCtx` keeps `&mut` on the hot path, and each of
its views is the root's state or a forked branch's layer over its
parent's.

```rust
pub struct ExecCtx<'a, R: Rt, E: UserEvent> {
    pub cx: branch::CxView<'a, R, E>,   // the compile state: root, or a fork's view
    pub rt: branch::RtView<'a, R>,      // the runtime: root, or a ForkRt
    pub event: &'a mut Event<E>,        // its overlay a Layered map
    pub libstate: &'a LibState,         // shared (§6)
    pub control: &'a Arc<Control>,      // shared
    fork_depth: u8, par: ParMode, fork: ForkFlags,
    ..
}
```

- **Root branch.** The cycle's root holds the runtime and the compile
  state exclusively (`RtView::Root`, `CxView::Root`) and writes
  through. An unforked program never builds a delta or a log: that is
  the "serial path pays nothing" rule in the type.
- **Forked branch.** `fork_join`/`fork_each` give each side a `ForkRt`
  (store and reference-path deltas and a log of every other runtime
  call, `RtOp`), a `ForkCx` and a forked `Event` over the parent's,
  which is frozen for the join's duration. Reads go: own layer, then
  the parent chain, then the store. Writes go to the deltas or the log.
- **Event.** The event's overlay (`variables`) is a `Layered` map; `init`/`wake_init` are copied per branch; `custom` is
  one locked map every branch shares; `user` is copied.
- **Bounds.** Sharing the parent's runtime with a forked branch needs
  `R: Sync` (`ForkRt`'s `Send`).

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

- **Deltas.** Overlay (`Layered::merge`), store and ref-path entries
  (`RtView::merge`) insert into the parent. Two branches publishing the
  same id is an analysis bug, except for error handlers. Here the
  serial rule is reproduced: the left (earlier) delivery keeps the
  slot, and the right's becomes a queued write (`fork_join`'s
  `delivered_in_both`), as `deliver_error`'s `Occupied` arm does. A
  removal removes the key.
- **Log.** The right log concatenates after the left. At the root, the
  log (`RtOp`s) replays into `Rt` in order. That covers the queue order of
  writes, `ref_var`/`unref_var` pairs, timer and task spawns, and
  `notify_set`.
- **Productions.** A fork point's own result is built after the join,
  as today, from its children's residents.

Under `GRAPHIX_PAR_AUDIT=1` each branch also records the ids it read
(`ForkRt::note_read`). At a join, the right branch's reads must not
meet what the left branch published (`branch::audit`). A miss is an
analysis bug, and the audit panics naming the id. That makes the
fuzzer's forced-parallel pair test the analysis itself, not only the
outputs.

### 4.3 The root loop

The root loop does not fork: it runs the scheduled roots in serial
order on the runtime's thread. A script compiles as one block, whose
plan forks (§3.3). No cycle-level decision enters the pool: a fork
enters it where it is made (§4.5).

### 4.4 As built (phase 2)

`graphix-compiler/src/branch.rs` holds the branch views:

- **`RtView`** (`ctx.rt`): the runtime at the root, or a `ForkRt`
  (store and reference-path deltas, `None` a removal, and a log of every
  other call) over its frozen parent. `Rt::store()` became the per-id
  `store_get`, and `spawn`/`spawn_var` return nothing (no caller kept
  the abort handle; async builtins drop stale results by minting a new
  id on sleep).
- **`Layered`** maps (`ctx.event.variables`): a
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

### 4.5 As built (phase 4)

- **Flat forks.** A fork point cuts its children into ranges up front
  (`cost::Splits::ranges`, a slot site's grain) and runs every range as
  a sibling branch one level below it (`branch::fork_each`, rayon over
  the parts), merging them in order. Binary splits nested a branch per
  level: a read walked every layer and each join copied a write one
  more time, which doubled the work of a slot-heavy cycle on one
  thread. A collision is a delivery an earlier sibling made: the later
  one is queued, as between two joined branches. `fork_join` remains
  for an operator's two operands.
- **Maps stay hash maps.** A persistent map (imhm) for the store and
  the event's deliveries made a fork an O(1) clone and every read one
  lookup, but put a trie walk on every serial read and write: the
  node-walk ran 2.4x slower with no fork at all (imhm `insert`/`find`
  31% of the profile, the store holding a binding per activation).
- **The pool is entered at the fork.** A cycle runs on the runtime's
  own thread; a fork made there installs its parallel part into the
  evaluation pool (`branch::on_pool`), and a fork already on the pool
  joins in place. The cycle's serial work stays on the runtime's thread
  (the scheduler put it on a low-power core when the whole cycle ran on
  a pooled worker), a one-shot cycle can fork, and nothing has to guess
  per cycle whether to enter the pool: a cycle-level site that did
  backed off after cycles that forked nothing and then missed the
  cycles that would have.
- **Serial cost.** Phase 4's sites and flags, measured as user-space
  instructions with `GRAPHIX_PAR=off` against the phase-4 threads
  commit (quick builds, pinned): `fold_sum` and `map_fold` node-walked
  +0.1% and +0.2%, `symbolic` +0.3%, `par_wide` node-walked +1.0% (a
  call site per activation, each with a site to consult).
- **A collection intrinsic is part of its call site.** Its body (the
  `array::map` wrapper's `MapQ`) runs under the caller's fork flags,
  where any other callee's body clears `#[parallel]`.
- **Seq machines are serial inside.** A machine's guards and the raises
  of its steps meet through handler counters (`DynNode`'s atomics) that
  no branch view isolates, in serial order: forked, a raise could land
  after a sibling's guard had read the generation
  (`lang::seq_errors::continuations` lost a block's write). A machine
  updates with `ExecCtx::serial` set; a lambda body clears it, so a
  callee forks again. Beside the machine, its `SeqAbort` and the
  machine itself are ordered in the block plan: the abort must fail the
  guards before the machine updates.

## 5. The cost model

The engine measures, and the measurements choose the fork points.

**What is measured.** Each child of a measured fork point carries a
coarse log2 histogram of its update cost (`cost::Hist`): 16 buckets of
`u16` counts. A sample is two tick reads around the child's update and
one increment of the bucket `log2(ticks) - shift`, clamped to 0..15. A
tick is the cheapest monotonic counter the platform has: `rdtsc` on
x86_64 (invariant on every current CPU, constant rate across cores and
frequency changes), `CNTVCT_EL0` on aarch64, `Instant` elsewhere.
Nothing converts ticks to time: `T` below is calibrated in ticks too,
and `shift` puts `T` in bucket 12 (`T_BUCKET`), so bucket 0 starts
between `T/8192` and `T/4096` (a cheap slot's cost, not rounded up to a
fork's) and bucket 15 at `8T`. The counter's not being serializing is lost
in the log2 buckets. Every 64 samples halve the counts, so old behavior
fades. A collection keeps one histogram for its standing slots' per-slot
cost, not one per slot, and its growth two `ProbeSite`s (below). A fork
point that is a chain stores nothing.

**When it is measured.** Every update while a site is unsettled (each
child has fewer than four samples); then one update in N, with N
doubling while the estimates hold (to 1 in 256) and reset when one
moves (`cost::Sampler`). Sampling keeps the steady-state overhead to a
branch and a counter.

**What is decided.** A child's estimate is the floor of its
histogram's p75 bucket (a child that is usually cheap but sometimes
huge should still fork). A site cuts its children into contiguous
ranges (`Splits::split`, `Splits::ranges`): a range of estimated total
`2T` or more splits at its weighted midpoint when both halves reach
`T`, and recursively; each final range is a sibling branch
(`branch::fork_each`). `T` is a multiple of the measured cost of a
stolen job (§5, as built). An unstolen `join` costs much less, but `T`
must cover the case where the steal happens.

For a collection's standing slots, the ranges are flat: slots of
`ceil(T / per_slot_estimate)` (the grain), capped as phase 5 says.
Growth is costed by two `ProbeSite`s (phase 5).

**Measured time is wall time.** A child that forked internally reports
less than its work, so the parent errs toward not forking it. That
direction is safe: the child is already parallel inside. True work
accounting (each branch summing its leaves' time) is possible later if
the wall-time bias turns out to cost.

**Not imaged.** Histograms are run-time state; a warm start relearns
them. Fork plans (§3.3) are not imaged either: a block plans at its
first update that may fork, cold or warm (`Block::image_encode`).

**Before there is data,** a fork point runs serially, unless an
attribute forces it.

**As built** (`graphix-compiler/src/cost.rs`). A `ForkSite` starts as
a probe: its first two updates time the children's total, and only a
site whose total reaches `2T` allocates histograms; any other turns
serial and probes again after 1024 updates. A measured site samples
every update until each child has four samples, then on the doubling
schedule, and turns serial when no split reaches `T` on both sides. A
`SlotSite` keeps one per-slot histogram (the measured loop's total over
its slot count) and forks in ranges of `ceil(T / estimate)` slots. `T`
is four times the lower quartile of nine latencies of handing an idle
pool a job, measured on a thread of its own at the first use (`Auto` forks nothing
until then). `GRAPHIX_DBG_PAR` prints the calibration and each kernel loop it forks.

**As built (phase 5).**

- **Growth decides in its cycle.** A collection's fresh slots are
  costed apart from its standing ones, twice: building their instances,
  and their first updates. Each is a `ProbeSite`: it runs the first
  slots of a growth in order, timing each (four until its estimate
  settles, then one per growth), and forks the rest when they are
  estimated at `2T` or more. A one-shot growth, a program's first cycle
  included, needs no history: `array::init(20000, ..)` forks in the
  cycle that builds it.
- **Ranges are capped:** at most four per worker, each at least
  `ceil(T / estimate)` slots. Each range is a branch, or a compile
  task, whose fork and merge cost grows with the writes it carries,
  not only its count.
- **A saturated pool forks nothing under `Auto`.** A fork pays for its
  branches and its merge whether or not another worker takes a part, so
  a fork made while every worker already has a part only adds that
  cost. The pool counts its live parts (`branch::saturated`); under
  `Auto` a site whose plan is to fork runs in order while they number
  the workers, and keeps measuring as it would. Without it, the
  recursion inside `par_wide`'s 16 forked slots kept forking its
  operator sites, by an amount that depended on `T`: 59 steady cycles
  took 3.7 s at `T` = 150k ticks and 3.1 s at 4.8M on four P-cores;
  with it, 2.8 s at any `T`. Disabling it costs 3.45 s against 2.83 s
  on four cores and 2.58 s against 2.30 s on twelve. Saturating at
  twice the workers measured the same as at once.
- **`T` from the lower quartile.** The wake latencies are measured
  while the cycle that first asks runs, on cores it shares; contention
  only adds to them, so the lower quartile of nine is taken, not the
  median. The median ranged 10x between runs of one program.

- **Serial cost**, user instructions with `GRAPHIX_PAR=off` against
  phase 4 (quick builds, pinned): `fold_sum` and `map_fold` node-walked
  0%, `symbolic` +0.2%, `stream_stats` +0.3%, `par_wide` +0.7% (65k
  run-time binds through the split bind), `par_growth` 0%.

**Measured, not fixed (phase 5):**

- **A branch's reads cost about 30%.** `par_wide` on a one-thread pool
  forks only its top level (the pool is saturated at once) and runs its
  steady state in 7.0 s against 5.4 s serial: every read in a branch
  walks the layered views. This is the floor under any speedup, and
  the reason unstolen forks are not free here as they are in rayon.
- **A first cycle built on many threads slows the cycles after it.**
  `par_wide` on twelve threads, P- and E-cores: its first cycle in
  1.4 s forked against 2.8 s in order, then 59 steady cycles in 2.3 s
  against 1.9 s. Same forks at the same depths, same allocator share
  (1.4%), no spinning; the steady state misses more on the nodes
  themselves (a node's first field read, its selects' tracked sets), and
  a single malloc arena, which interleaves the threads' allocations
  further, makes it 2.9 s. On four P-cores, and on a one-thread pool,
  the difference is under 2%. The net is still the forked first cycle
  (3.7 s against 4.7 s); where the nodes land is the allocator's
  business, open.
- **Hybrid cores make forced ranges sensitive to placement.** A bare
  `#[parallel]` cuts one range per worker. `par_symbolic` on twelve
  threads runs 1.40 s with the calibration started at the first fork
  site, and 1.40 s or 1.55-1.65 s (bimodal, same instructions, more of
  them on P-cores) when it started with the runtime: the threads' start
  decides where the OS places them and so which ranges wait on E-cores.
  Four ranges per worker made it 1.39 s on twelve and 2.10 s against
  1.90 s on four. Calibration starts at the first use, as before.

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

**Shared state is created atomically.** Two branches may ask for a
library state at once, so a builtin that creates one on first use
creates it inside `LibState::get_or_else`, which holds the lock across
the creation; a `get` followed by a `set` creates two. `NetState` did,
and under real threads a publish and a subscribe on two branches each
built a pump: the subscription delivered its first value twice
(`lib_tests::net::net_write1::par`, about one run in eight).

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

**As built.** `node/fork_control.rs`: a `ForkControl` node runs its
child under `ExecCtx::fork` (`branch::ForkFlags`): `#[serial]` sets
`inhibit`, which a callee's body keeps; `#[parallel]` sets `forced`,
which a callee's body clears, and makes every fork point under it fork
whenever the runtime may fork at all (`Auto` or `Force`; `Off` is off).
The grain is positional, `#[parallel(4)]`: attribute arguments are
expressions, and `grain: 4` is not one. On a `let` the
attribute moves onto the value, and on a definition onto its lambda's
body, so every instance compiles the node (`compiler::fork_on_body`). The
node is a fusion boundary: its child fuses as a region of its own, whose
depth-0 loops take the forced grain (§10). The assertion runs at the node's
`typecheck1` (`analysis::check_parallel`): a block needs a run of two
statements and is otherwise refused naming its first break
(`plan_block_explained`: the variable read, an ordered call, a module,
a catch); anything else needs a fork point with two children that are
more than a constant or a variable read, or a call into a mapping
collection. Pins: `lang::par_attrs`, `lang::image::
program_image_restores_fork_control`.

Process-level control: a runtime's mode is its `Control`'s
(`set_par_mode`), which starts from `GRAPHIX_PAR=off|auto|force`,
`auto` by default. `Force` treats every fork point as `#[parallel]`
(for the fuzzer and tests). `GRAPHIX_EVAL_THREADS` sizes the pool.

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

- `CompileCtx::fork`/`join` (C/lib.rs);
- a fresh `tvar::new_task()` per task, so a task never writes a cell an
  earlier task created (`tvar::decided`, refused under `OwnWrites`);
- refs and discards deferred per task.

The fork is made at a branch's first compile, so a branch that compiles
nothing pays nothing. It joins into the parent at the branch merge, in
serial order, as compile tasks join in evaluation order today. Two
branches never see each other's compiles before the join, and §3 makes
that safe: they are independent.

- **Ownership checks turn on at run time.** A runtime bind runs as a
  fresh compile task (`with_runtime_settles`), so the cell-ownership
  checks apply to it and `GRAPHIX_TASK_AUDIT` covers it. Phase 4's
  audited gate (every fixture's forced run included) found no runtime
  bind writing a cell it did not create.
- **Collection growth is chunked.** A collection growing by many slots
  builds them in tasks, one per chunk (the grain from the construction
  histogram, §5), joined in index order.

  As built: a slot whose callback the prototype resolved statically
  calls a constant definition, so its instance can be built before its
  first update (`CallSite::prebind`, state `Callee::Prebound`). A
  run-time bind is split in two:

  - `build_bound`, on a `CompileCtx`: instantiate, check, elaborate,
    analyze, take the slot's shared kernels;
  - `prime_bound`, at the first dispatch: prime the outer variables
    the compiled defaults read, and run the defaults under the init
    view.

  A growth whose builds are estimated to pay prebinds its fresh slots
  in compile tasks (`branch::compile_each`: a `CompileCtx::fork` per
  range on the evaluation pool, joined in order), then applies the
  deferred references and evaluates. A growth that runs in order
  prebinds nothing: building every slot before evaluating any cost the
  serial engine 6% on `par_growth` (each instance cold by the time it
  ran). A fold's chain stays serial, but its
  instances no longer are: in `par_growth` the fold's growth had been
  a tenth of the serial run, all of it on the runtime's thread.
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
  write disjoint keys. Nothing asserts it: `GRAPHIX_PAR_AUDIT` checks
  variable reads, not joins.

**Dynamic modules** compile in their branch's task too. Their checks'
internal `par_iter` (C/node/mod.rs:949) nests in the evaluation pool. A
dynamic module is `opaque` in its summary, so nothing forks beside it.

**Core-trait hooks** reach `&mut ExecCtx` through a raw pointer
published in `VALUE_HOOKS` (C/node/coretraits.rs:368) and re-enter node
updates from inside `Value::eq`/`cmp`/`fmt`. The pointer is a loan to
one thread, made by a frame that holds the context and waits inside the
comparison, so under branches each job loans its own branch (§9) and
the aliasing is today's, once per job. The hook call sites are stateful
graphs pooled per (trait, type) in `core_hook_sites`, one registry
every branch shares (§4.4). Two branches comparing the same abstract
type take two sites: a call takes a spare site from the pool or builds
one in a task of its own (`build_site`), and puts it back in the shared
pool when it returns (`return_site`); a site whose entry went stale
meanwhile is deleted.

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

`branch::fork_join` (two parts, `rayon::join`) and `branch::fork_each`
(flat, rayon over the parts) fork the branch contexts, run the parts,
merge, and propagate a panic. `branch::compile_each` runs compile
tasks the same way (§8). All three run their parallel part through
`branch::on_pool`, which installs it into the evaluation pool when the
fork is made off it (§4.5), and count their parts live until each
returns (§5, saturation).

A thread blocked in a `join` runs stolen jobs, so a stolen job sees
whatever the thread it lands on had set when it blocked, and the job's
own thread-locals must not depend on its ancestors. As built:

| thread-local | treatment |
|---|---|
| `CURRENT` (the running `Control`) | installed by every part that may run on another thread and by `on_pool`; a job may land on a thread blocked in another runtime's cycle |
| tokio runtime context | entered beside `CURRENT` (timers, `spawn`) |
| `VALUE_HOOKS` | no fork runs under a loan: the dispatch that updates a hook site suspends it first, so a stolen job sees none, as the joining thread does |
| `RUNTIME_BIND`, `DESELECTING_ARM` | set only around compiling and sleeping, neither of which forks |
| tvar `LEVEL`/`TASK` | `LEVEL` entered from the forking thread in every compile task; `TASK` entered by each run-time bind (`with_runtime_settles`) |
| kernel loans `KERNEL_ABORT`/`KERNEL_ENV`/`QOP_RAISES`/`KERNEL_PANIC`, `SELF_BLOCK_GEN`/`_REACHED`, the fork mode (`ParLoan`) | saved and restored around every kernel call: a stolen job running a kernel while its thread waits inside another kernel's dynamic call is a nested call, which these already support; a kernel loop's chunk on a worker runs under the invoking run's loans and hands back what it reported (§10) |
| `FastMemo`, `LPooled` pools, scratch buffers | per-thread caches: unchanged; values freed on another thread return to that thread's pool |

Test instruments that counted per thread (fused kernel runs, JIT
wrapper entries, live activation blocks) count on the runtime's
`Control` (`Control::invocations`, `live_self_blocks`), and so do its
forks (`forks`) and its runs of compile tasks (`build_forks`).

**The pool.** `branch::eval_pool`: one per process, shared by every
runtime in it, `GRAPHIX_EVAL_THREADS` workers (default: the cores),
16 MB stacks, `stacker` growing segments as everywhere. The compile
pool's size (`RAYON_NUM_THREADS`) does not govern evaluation; the
fuzzer sets both to two for its children.

**Stack budget.** Per cycle: the `Control` counts the grown segments
live in its cycle, on every thread (`Control::grown`), and a segment
that would pass the budget aborts the runtime. Outside a cycle the
count is per thread, as before.

**Interrupt and abort.** `Control` is atomics, shared. Every job polls
it through its own `CURRENT`.

## 10. Fused kernels

A kernel invocation is a leaf. Its state is node-owned (`state`/`site`
blocks), its loans are per call, and its feeders are polled in order,
not forked. So kernels need nothing for the node-level design.

Parallel loops inside kernels (§11 phase 6) are the second half.

**What forks.** A map-family loop (`init`, `map`, `filter`,
`filter_map`, `flat_map`, `find`, `find_map`, over arrays, lists and
maps) emitted at loop depth 0 of a kernel body, parent or callee. Its
iterations are independent by construction: a kernel is pure, and the
only memory an iteration touches is its own slot's (per-slot chains,
per-slot call-site blocks, `kernel_instance_state.md`). `fold` stays
serial (§12). A loop nested in another loop stays inline in its
enclosing loop's code.

**Outlining.** Such a loop is emitted as a function of its own, a
CHUNK, `chunk(frame, lo, hi, out)`, which runs slots `lo..hi`: the
element binds, the callback body and the push, exactly the serial
loop's iteration. The kernel keeps the loop's preheader (the source,
its length, the slot-table frame, the firing flags) and replaces the
loop with a call to `graphix_par_loop(chunk, frame, len, site, out)`.
`frame` is a stack record the kernel fills: its context word, state and
site pointers, the source and its disc, the slot tables' bases, and
every local in scope as a `(disc, payload)` pair. A chunk borrows those
locals (its abort path drops only what it bound itself). Each chunk
fills an out record: its own result buf (unfinalized) or its first
match, its TAINT (OR) and STALE (AND) accumulators. The helper merges
the records in index order: bufs concatenate, a find takes the lowest
chunk's match, flags fold. The chunk is a second function of the
kernel's record (`RecordKind::Chunk`, with its own constants;
`RelocTarget::Chunk` from the kernel), installed and imaged with it.

**Shared memory.** Everything a chunk writes outside its own frame is
indexed by its slot: no state word in a loop is shared across its
iterations (`kernel_instance_state.md`; a nested loop's prev-length
word and a call's first-call word are per slot whatever their source,
which is also the node-walk's multiplicity). What iterations share is
the chain levels at the loop's own depth, the directories its nested
loops and in-loop call sites anchor. The kernel resizes those in the
preheader, so an in-body ensure finds its level sized and only reads it
(the ensure helpers take a read-only path when nothing changes): the
loop's exit truncates are those same ensures, run once before the fork.

**Thread-local loans.** A chunk on a worker runs under the invoking
kernel's loans: `KERNEL_ENV`, the interrupt scope, the self-block
generation (reach counts sum back), and a `?` queue of its own,
appended to the kernel's in chunk order; an abort or a fast fn's panic
in any chunk aborts the kernel. A core-trait value hook loan is
exclusive to the invoking thread, so a kernel running under one does
not fork.

**The decision.** `FusedKernel::update` loans the runtime's fork mode
and forced grain (`ctx.fork_mode()`, `ctx.fork.forced`) to the run; a
callee body ignores the forced grain (`#[parallel]` excludes callees).
`Off` and runs outside a loan call the chunk once over the whole loop.
Under `Auto` the loop is a `ProbeSite` (phase 5): it runs its first
slots in order, timing each, and forks the rest in ranges of the grain
the estimate gives, at most `CHUNKS_PER_WORKER` (16) per worker: a
chunk costs a call and a buffer, so ranges are cheaper than a node-walk
fork's and more of them even out slots of uneven cost. A loop's cost is
its code's, so the site (`cost::LoopSite`) is one per compiled loop, a
constant of the kernel's record shared by every instance and thread
running it: a callee's loop called from another loop's slots would
otherwise probe afresh in every slot. Once its estimate says a loop
shorter than some length cannot pay for a fork, such loops run untimed
for `RECHECK` fork thresholds of time, which costs a hot loop two
atomic reads; a run that finds the site being decided runs in order.
`Force` forks every loop of two or more slots in ranges of the forced
grain.

**Testing.** Every `run!` fixture has a `jit_par` twin (fused, `Force`:
each slot a chunk); `lang::par_loops` pins raise order and a find's
lowest slot across chunks; `lang::par_attrs` the attributes over fused
loops. The fuzzer's `Par` pair runs the fused JIT forced
(`Mode::JitPar`) beside the forced node-walk, each against the serial
node-walk. `GRAPHIX_NO_OUTLINE=1` emits every loop inline (A/B: a
3-slot loop in a callee called 2M times costs 3% outlined under `Off`
and 10% under `Auto`).

**Measured** (`bench/par_mandel.gx`, 480000 fused pixels a cycle, quick
build): 1.51 s serial, 0.45 s on four P-cores, 0.23 s on four P- and
eight E-cores. The rest of the cycle (the fold, building and dropping
the array) is serial; the pixels run at about 92% of four cores.

## 11. Phases

Each phase lands on the branch with the gate green, and the
semantics-touching ones soak before the next.

| phase | content | proves |
|---|---|---|
| 1 | The branch context, serial only: `ExecCtx` split into `Shared` + branch, `Event` folded in, every rt/libstate/compile write routed through the root's exclusive state, the `custom`/tombstone/id-order audits. No delta, no log, no thread. | The refactor is serial-equivalent and costs nothing: the gate, a fleet soak, and a GUI-suite and admin-TUI timing at parity. |
| 2 | Deltas, logs and compile tasks with an artificial fork: under `GRAPHIX_PAR=force`, every legal fork point runs its two sides serially, but through separate branch contexts, separate compile tasks for runtime binds, and the merge. `LibState` through `&`; the stdlib audit for in-language shared state, `Ordered` declarations. | Merge correctness, runtime compiles included, without threads: the fuzzer pair serial vs. forced-merge; `run!` gains a `par` mode. |
| 3 | Fork plans: `publishes`, the runs, `GRAPHIX_PAR_AUDIT`. | The analysis finds what serial order needed; the audit runs under the fuzzer's forced mode. |
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
- **Startup freezes shared type variables.** `GRAPHIX_TASK_AUDIT`
  reports a module check during registration (`Module::compile_static`
  → `bind_sig` → `Env::deftype` → `alias_tvars` → `TVar::freeze`)
  writing vars of task 0, once per process: the first runtime to start
  freezes them and later ones find them frozen. The vars look shared
  across runtimes (the stdlib's decoded interfaces); concurrently
  starting runtimes then race on the flag. Seen on this branch, present
  on main; not investigated.

- **A loop's iterations shared two kernel state words.** A nested
  loop over a source bound outside the loop kept one prev-length word
  for every iteration of the enclosing loop, and a cross-kernel call in
  a loop one first-call word; the node-walk builds an instance per
  slot, so a growing loop's new slots fired (and raised) where the JIT
  did not. Phase 6 needed every iteration's memory to be its slot's and
  found it (5d342b87, pins `lang::functions::hof_slot_*`). Present on
  main; **port it if this branch is not merged.**
- **`Env::fork` copied every key its parent's task had written.** It
  built the fork as `{ forked maps.., ..self.clone() }`, which clones
  the tracked maps' written-key lists before dropping them, so a task
  that forks a task per instance (`CallSite::resolve_static`) pays for
  every earlier instance's writes: quadratic in a task's instances.
  The branch names the fields it clones. Main's statement compile tasks
  fork per instance the same way. **If this branch is not merged, port
  the fix and measure a statement that elaborates many instances.**

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

