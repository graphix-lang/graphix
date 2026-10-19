# Walk pruning

Status: design 2026-10-18 (Eric: boundary granularity, all boundaries
at once, atomics vs layered merges to be measured); built, see "As built",
which supersedes the sections above it where they differ. Resolves
x-perf-runtime-01 and x-alloc-01.
Pins: `stdlib/graphix-tests/src/lang/prune.rs` (boundaries walked per
cycle, `Control::boundaries()`); the fuzzer's `Pair::Prune`.

## The problem

A node-walk cycle costs the whole program, not what changed. The runtime
prunes per ROOT: `GXRt.by_ref` maps each read `BindId` to the root
expressions that read it, a delivery adds those roots to `updated`, and
`update_nodes` walks only them (`graphix-rt/src/gx.rs` `wake_readers`,
`update_nodes`). But a script is one root (`compile_script` wraps the
file in one `Block`), so any delivery walks every statement, every
instance body, every collection slot and every recursion activation.
An idle `array::map(src, |x| uniq(x) * 2)` of 2000 elements beside a
ticking counter makes each tick ~50x slower than at size 0
(design/review-2026-10-12/repro/x-perf-runtime-01a.gx; x-alloc-01.sh:
300 unrelated cycles, 0.62 s at 2000 slots, 3.95 s at 20000). Fusion
hides it only where a callback fuses whole; UI code is node-walk residue
(`~`, state, references, async calls), so every keypress pays for every
row.

## The idea

Extend the runtime's per-root scheduling to per-BOUNDARY scheduling. A
boundary is a node that wraps a subtree and can return its resident
instead of walking it. Every read is already registered (`record_ref` /
`ref_var`) against the root that holds the reader; register it against
the innermost boundary as well. A delivery marks the boundaries that
read the id and their ancestors; a boundary not marked this cycle, in
a `Cycle` view, rides its resident.

Pruning must be invisible: the same program yields the same tags and
values, cycle for cycle, with pruning on or off. Only time changes.

## Boundaries

Three kinds, each a wrapper node (`Prune`) around one child:

1. **Statements**: each child of a `Block`.
2. **Instance bodies**: the body of a bound lambda instance (the child
   `GXLambda::update` runs after publishing its formals). Recursion
   activations and collection slots are instances, so this covers them.
3. **Select arm bodies**: each arm's expression.

Nodes between boundaries always run when their enclosing boundary runs.
In particular a `CallSite` still updates its arguments every time its
boundary runs (they belong to the caller's boundary), and `MapQ`/`FoldQ`
still visit every slot's call site; a clean slot then costs its
argument refs and one stamp check instead of its callback's body.

A `Prune` holds:

- `id: PruneId`, an index into the runtime's boundary slab;
- `resident: TagValue`, the child's last production (a `Block` without
  catches returns its last child's production by reference today and
  keeps no resident: the statement boundary keeps it).

Its update:

```
if view == Cycle && !slab.dirty(id, cycle) { return resident.ride() }
let tv = child.update(ctx);
resident.set(tv)
```

`ride()` turns a FIRED resident STALE in place and keeps its payload
and bottomness, so a skipped subtree delivers exactly what a walked
subtree that nothing fed delivers under dense delivery
(`design/dense_delivery.md`).

## The slab

The boundary slab lives in the runtime beside `by_ref`:

- one entry per live boundary: `parent: Option<PruneId>`, `stamp`
  (the last cycle it was marked) and `sticky: u32`;
- allocated when a `Prune` is built (compile, instantiation, slot
  growth, image decode), freed at its `delete`; a free list reuses ids.

A boundary is dirty in cycle `c` iff `stamp == c` or `sticky > 0`.
Stamps, not bits, so a cycle needs no clear pass and a mark is one
store.

**Marking** `mark(b)`: stamp `b`, then its parent, and so on, stopping at
the first ancestor already stamped this cycle. Each mark is O(new
ancestors), so a cycle's marking is O(boundaries marked).

**Registration.** `by_ref: BindId -> [(ExprId, u32)]` becomes `BindId ->
[(Reader, u32)]` with `Reader { root: ExprId, boundary: PruneId }`.
`record_ref(id, top_id)` and `ref_var(id, top_id)` take a `Reader`.
Every node that captures `top_id` to register later (RtVar, timers,
Deref addressing, error handler binds, fs watch, db subscribe, net)
captures the `Reader` instead; the compile context carries the current
boundary as it carries `top_id` and scope. `wake_readers` adds the root
to `updated` (as today) and marks the boundary.

## When a boundary is dirty

A boundary must run in cycle `c` when:

1. **An id it reads is delivered.** A delivery is any write that
   `read_var` will see this cycle: an entry in `event.variables`, or a
   store entry stamped this cycle (`read_var` counts those Delivered,
   `node/mod.rs` `read_var`). Today these come from:
   - the runtime at cycle start: `deliver` and `deliver_custom` (timers,
     async replies, `watch_var` streams, embedder sets, GUI calls, TUI
     events, custom deliveries keyed by `BindId`);
   - the walk itself: `let` publications (`bind.rs`), `ByRef`, pattern
     binds, `ctx.deliver` (call arguments, slot elements, fold accs,
     queuefn, genn, opt, handler), the seq machine's same-cycle `pc`,
     same-top error delivery, and store-only writes (`module.rs`
     `store_production`, `bind.rs` `bottom_mirror`).
   Rule: `Event::variables` becomes private; every this-cycle write
   goes through one `ExecCtx::publish` (overlay and/or stamped store
   write) that marks the readers. The store's `store_insert` marks when
   it stamps the current cycle. `notify_set` folds into `publish`.
2. **It is new**: the view is `Birth` (a fresh instance, slot, root or
   restored image). A `Prune` never skips outside `Cycle`.
3. **It is waking**: the view is `Wake` (a reselected arm, a slot back
   from a bottom source, an instance after its function returns from
   bottom). Same rule.
4. **Shared state it polls changed.** Some nodes read state another
   node mutates without a variable:
   - error handlers: `raise`/`handled`/`abort` mutate the handler's
     atomics, which `Catch` (pending abort until receipts settle) and
     `SeqGuard` (`aborted_in`, `generation`) poll (`error.rs`);
   - `queuefn`: two sites share `Arc<Mutex>` queue state, and a pop at
     one releases the other's arguments (`queuefn.rs`);
   - `ConnectDeref`: a pending write retried across cycles.
   Rule: each such mutation marks the poller's boundary (the handler
   records its catch's and guards' `Reader`s; a queuefn site records
   its partner's), and a node that must retry next cycle holds its
   boundary `sticky` until done. The list is closed by the audit below,
   not by this paragraph.

Nothing else drives a node: `once`, `count`, `uniq`, `hold`, `take` and
`skip` fire only on an argument's tick; wall-clock reads (`now`,
`throttle`) run on a trigger; constants fire at `Birth`/`Wake`.

## Sleep and wake

A sleeping arm's subtree is not updated today, and registration covers
every node a statement holds, selected or not, which wake catch-up
relies on: a write only a sleeping arm reads still schedules the
statement, and the select's `TrackedFires::observe` records the fire.
With boundaries the same holds one level down: the sleeping arm's reads
are registered against the arm's boundary, whose ancestors include the
select's boundary, so a fire marks the select's boundary and the select
runs and observes it. The arm boundary itself is stamped and not run;
when it wakes it runs under `Wake` regardless.

## Order within a cycle

A later statement that reads what an earlier one publishes in the same
cycle runs because the publication marks it before the walk reaches it.
`notify_set` today reschedules later roots mid-cycle the same way
(`update_nodes`).

## Parallel evaluation

`design/parallel_eval.md`. Two designs, to be measured (Eric):

- **Atomic stamps.** `stamp` is an `AtomicU64` and `mark` stores
  through to the root slab from any branch. Siblings never read what
  another publishes (`analysis::plan_block`, `independent`), so a
  branch never marks a boundary a sibling is about to test; a mark
  inside a branch is visible to the branch's own later boundaries at
  once.
- **Layered marks.** A branch's `ForkRt` keeps a delta set of the
  boundaries it marked this cycle; `dirty` reads the delta, then the
  parent chain, then the root slab, as `store_get` does; the join
  merges the delta upward in program order, as `RtOp`s replay.

Both satisfy the same invariant; atomics cost a fence per mark, layered
costs a lookup chain per check. Both were built and measured; atomic
stamps won and the layered marks are deleted (see "As built").

## Fusion, images, the REPL

- **Kernels.** A fused region sits inside a boundary; its feeders
  register as any node does. A kernel already rides when no feeder
  triggered (`FusedKernel::update`); a skipped boundary does not reach
  it at all.
- **Images.** `PruneId`s are not imaged. Decoding a `Prune` allocates
  its boundary; decode-time registrations (`Callee::Imaged` held refs,
  `RtVar` decode, re-registered `ref_var` ids) take the decoding
  boundary's `Reader`. A lazily decoded instance body decodes into a
  fresh boundary under `Birth`.
- **REPL roots** are one root per statement already; each root's
  statement is its own boundary as well, harmlessly.

## Verifying it

Pruning changes no output, so it has an exact oracle.

- **`GRAPHIX_NO_PRUNE=1`** disables skipping (every `Prune` walks).
- **`GRAPHIX_PRUNE_AUDIT=1`** walks every boundary and, where pruning
  would have skipped one, asserts that the walk produced the ridden
  resident and made no write, publication, registration, spawn or
  timer (the runtime and event op counts before and after). A failure
  names the boundary's spec: a missing mark.
- **The fuzzer** gains `Pair::Prune`: the node-walk with pruning
  against the node-walk without, per-cycle trace, bit for bit; and the
  existing pairs run with pruning on.
<!-- CR claude for eric: [test-gap/medium] No gate runs the prune audit:
`testing::Mode::prune` is `PruneMode::from_env()`, which is `On` unless GRAPHIX_PRUNE is
set, and nothing in the tree sets `PruneMode::Audit`. Only the fuzzer's `Pair::Prune`
output comparison covers pruning. This section also names `GRAPHIX_NO_PRUNE`,
`GRAPHIX_PRUNE_AUDIT`, `Control::boundaries_walked` and a slab, none of which were
built. Decide whether `run!`'s interp mode should run under Audit, as designed here.
Then rewrite this section and "The slab" to the build. Found by reading; not run.
(x-doc-drift-02) -->
- **`run!`** runs pruned (the default); the audit runs in the plain
  gate for the `lang` and `lib_tests` fixtures (a test helper sets it),
  so the gate itself hunts missing marks.
- **Pins.** A runtime counter of boundaries walked and skipped
  (`Control::boundaries()`) pins the CR probes: an idle
  `array::map` of 2000 slots beside a ticking counter walks a constant
  number of boundaries per tick, not 2000; the recursion and queued
  write probes likewise.

## Costs

- A mark per delivery of a read id: one hash lookup of the readers and
  O(new ancestors) stamps. Every `let` publication pays the lookup.
- A stamp check per boundary visited, and a `TagValue` resident per
  statement.
- Slab upkeep at bind, slot growth and delete, which already walk
  `refs`.

Measured before landing: the CR probes, the collection benches
(`bench/collection`), the GUI suite's typecheck time (unchanged by
design), netidx-admin's milestone latency, and fuzzer throughput.

## Where this could be wrong

- **An unlisted non-variable input** (rule 4) skips a node that should
  have run. The audit is the net; every finding adds a mark.
- **Publication cost** on programs that are mostly hot (every statement
  dirty every cycle) could exceed the walk it saves. The bench suite
  shows it; if it does, marking can be restricted to ids whose readers
  sit in another boundary than the publisher's (a per-binding flag
  computed at registration).
- **Dynamic reads.** A `Deref` addresses its target at run time and a
  dynamic call site binds late; both register when they learn the id,
  through the `Reader` they captured. A read that learns its id only
  inside an update must register before that update's boundary could
  next be skipped, which is the case today for roots.

## As built

**Boundaries** (`graphix-compiler/src/prune.rs`, `node/prune.rs`). A
boundary is a `BoundaryRef`, an `Arc` holding its parent, an atomic stamp
(the last cycle it was marked) and a sticky count; no slab, so compiling
makes one without reaching the runtime and an id is never reused. A
`Prune` node wraps each block statement (`compile_block_children`, so
module bodies and seq steps too), each select arm body and each instance
body (`GXLambda::new_with_body`). It is transparent: its view,
`for_each_child`, spec, type, refs and `emit_clif` are its child's, and
`Node::downcast_ref`/`downcast_mut` see through it. Its `update` rides
its resident when it walked before, the view is `Cycle` and the runtime
says it is not dirty; every other method runs the child inside its
boundary.

**The current boundary is the traversal's.** `CompileCtx::boundary` and
`ExecCtx::boundary`, set by `in_boundary` around every traversal of a
`Prune`'s child; `ExecCtx`'s `deref_mut` copies it into the compile view,
so a compile the walk makes (a run-time bind, a decode) registers where it
runs. A registration is a `Reader { root, boundary }`: `record_ref` and
`ExecCtx::ref_var`/`unref_var` take the root and read the boundary from
the context, and `pending_refs` and each discard keep the boundary they
were made in (`delete_discarded` deletes under it). The runtime's
debug assertion that every unregistration matches a registration is
what catches a node traversed outside its boundary.

**Marks.** `RtView::mark_readers` marks the boundary of every reader of
an id: on a stamped `store_insert`, on `notify_set`, and through
`ExecCtx::publish`/`try_publish` for a write to the event alone (the
call site's formals, a same-root raise, a module's prime). The root's
`wake_readers` marks for the cycle's deliveries. A fork also keeps the
readers it registered this cycle, which the runtime learns only at the
join, and marks them too (a core-trait hook site built in a branch and
called again in the same cycle). A wake's catch-up injections need no
mark: they run under `Wake`.

**Shared state.** An error handler keeps the boundaries of the nodes that
poll it (`Catch`, `SeqGuard`: `ErrorHandler::polled_by`, released at
delete); `raise`, `handled` and `abort` mark them, an ancestor's too where
they move its nested count. Nothing needed `sticky` so far; the audit
decides.

**Fusion** steps into a boundary: `fusion::fuse` on a `Prune` fuses its
child inside it (`Prune::fuse_child`), so a kernel replaces the child and
the boundary stays, and everything fusion builds there registers where
it lives. A region rooted above a `Prune` emits through it and replaces
it whole; its feeders register in the outer boundary, where the kernel
lives.

**Images.** An imaged call site holds its callee's flat reads under its
own boundary until the callee materializes, then releases them: the
decoded body's nodes register their own. (Before, only the site decoded
outside every heap instance held them, for the whole tree, which would
have left the boundaries in between unmarked.)

**Modes.** `PruneMode::{Off, On, Audit}` on `Control`, from
`GRAPHIX_PRUNE` (default `on`); the audit walks every boundary and panics
where one it would have skipped delivered anything but its ridden
resident or published an entry that fires or differs from the store (a
stale republication of the store's value reads as none). `Control::boundaries()` counts
boundaries walked and skipped. The fuzzer's `Pair::Prune` compares
`Mode::Unpruned` (the node-walk walking every boundary) with the pruned
node-walk.

**Marks are atomic stamps.** A mark made in a forked branch is a store to
the boundary's stamp, visible to every branch at once; marking is
monotone, so a sibling that sees it early only walks a boundary the
serial walk would also walk or skip with the same result. Measured
against layered marks (a per-branch delta set merged at the join), on a
quick build, best of three, `--no-fusion`, 20000 map slots each
publishing two `let`s per tick over 290 ticks: serial 5.35 s both;
`auto` x4 3.41 s atomic vs 4.05 s layered; `force` x12 10.1 vs 12.3;
`auto` x12 4.07 vs 4.68. The parallel benches (`bench/par.sh`) tie, as
do pruning on and off there: they do work every cycle.

**Measured** (quick build, `chrt -f 50`, `--no-cache`, off -> on): the
review probes `design/review-2026-10-12/repro/x-perf-runtime-01a` 10.0 s
-> 5.0 ms node-walk (5.0 s -> 5.3 ms fused), `-01b` 2.03 s -> 5.4 ms,
`-01c` 149 ms -> 8.9 ms; `x-alloc-01.sh` (300 unrelated cycles, whole
run) 0.28 -> 0.10 s at 2000 slots and 2.02 -> 0.37 s at 20000. Pin:
`lang::prune::idle_map_beside_a_ticking_counter` (1000 more idle slots
cost their first walks only: 2000 walks pruned, 204400 unpruned).

**Real programs** (quick, pinned 0-3, best of three runs, off -> on):
netidx-admin `milestone_latency`: role-menu Down/Up p50 2.05 -> 0.27 ms
(p95 2.9 -> 0.4); modal text field Char p95 4.4 -> 0.37 ms, p50 and
Backspace unchanged; app build and first frame unchanged (~405 ms). The
serial bench corpus (`bench/run.sh`, `GRAPHIX_PAR=off`): `stream_stats`
node-walk 17.7 -> 0.64 s; every other program, either engine, within
noise (±2%), so a program that changes everywhere each cycle pays
nothing measurable. The walked/skipped counter (`note_boundary`, a shared
atomic) is invisible beside a no-op on the forked mark probe.
