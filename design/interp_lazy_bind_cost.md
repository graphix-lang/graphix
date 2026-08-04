# Interp lazy-bind cost — compile-per-activation is the fleet bottleneck

> Proposed 2026-08-04 (profile session on ryouko, perf + GRAPHIX_DBG_PERF,
> release+debuginfo+frame-pointer build). Reshaped same day after Eric
> flagged the collection-clone half as a `clone_rebind` resurrection — the
> phases below are ordered so the remap-free mechanism lands first and the
> cloner is a separately-gated decision carrying its ancestor's causes of
> death. Nothing here changes semantics; the node-walk stays canonical and
> the differential fuzzer arbitrates every step.

## The measurements

Per-subject CPU in the fuzz fleet's two hot classes is almost entirely the
runtime lazy-bind pipeline, not evaluation:

- **Recursion** (`fib(20)`, ~18k activations, 1.06s CPU ≈ 50µs/call):
  `bind_ms=921 setup_ms=602 tc1_ms=207 analyze_ms=92` — 100% of runtime.
  perf self-cost confirms the bind phase is typing walks: `RefHist::new`
  5.9%, `contains_dispatch` 3.5%, `would_cycle_seen` 2.0%, pooled
  `Vec<Type>`/type-map drops ~6.5%. The interp runs the compiler 18,000
  times and never meaningfully computes fib.
- **Collections** (`fold(init(50k))`, 2.0s CPU ≈ 40µs/element): **100,000
  lazy binds** — one per slot per HOF (init's callback + fold's callback) —
  `bind_ms=1263 setup_ms=994`. Fifty thousand compiles *of the same def*.
  The residual per-element dispatch cost (Phase B) is secondary.
- Reactive subjects are wait-bound (quiescence timers, 4% CPU) — out of
  scope; worker oversubscription already hides it.

These are exactly the fleet's benign-timeout classes (fib(30) ~2.7M
interp calls; init(500k)), which burn soak budget AND hand-triage
attention. Killing the constant kills the class.

## Root cause

`CallSite::bind` (node/callsite.rs:926) runs per activation:
`setup_dynamic_bind` — a **fresh compile of the whole lambda body**
(`InitFn`: Expr → Node with typecheck0's unification walks) — then
`typecheck1` over the fresh subtree and `analyze_bound_callee`. The
transient-recursion design (design/transient_recursion.md) made this
O(depth) in *memory* but left compile-per-call in *time*, noting the
follow-up: "a per-`LambdaDef` instance pool: park into a freelist,
re-bind by rebinding formals + dispatching with `event.init = true`".
Collection slots (design/collection_intrinsics.md) each own a live
CallSite whose first dispatch lazily binds its own instance of the
shared callback def — per-slot compile at collection construction.

## Phase A1 — recursion: pool the parked instances per def (FIRST)

> **Design correction (2026-08-04, pre-implementation):** the first
> draft stashed the instance on its own site (`TransientParked` gains
> the instance). That RESURRECTS THE MEMORY BOMB transient parking
> exists to kill: a retained instance's body CONTAINS its children's
> parked sites, so per-site stashes retain the full dynamic call tree
> — fib(28) = 1M instances = 9.6GB again. The correct shape is the
> per-`LambdaDef` POOL transient_recursion.md sketched all along:
> each instance pushes ITSELF at park, so the tree flattens into a
> small shared freelist (fib's sequential sibling calls: pool
> steady-state ≈ 1-2 entries, ~depth compiles total, ~18k reuses).

The pool: `ExecCtx.transient_pool: Map<LambdaId, Vec<Apply>>`, small
per-def cap (overflow deletes as today; the cap bounds idle memory
and idle wake-interest). At park: `reset_fresh` the instance and push
it — everything else about the park is UNCHANGED (the ext_refs
takeover, `TransientParked { def, ext_refs }`, the prime-deferral).
At `bind()`: when the def is active on the dispatch stack (the
existing transient pre-condition) and the pool has an instance for
`f.id`, install it directly — skipping `setup_dynamic_bind`
(compile + typecheck0), `typecheck1`, and `analyze_bound_callee`.
Soundness of cross-site reuse: `let rec` is MONOMORPHIC-recursive, so
all instances of one def in one recursion are type-identical, and
`arg_refs` are Ref nodes over the SITE's own arg ids (built by
`prepare_bind`, passed to the instance per update) — instances are
site-portable by construction. A parked site's existing `arg_refs`
remain valid (they never reference instance formals); a descent
site's first bind builds them via `prepare_bind` as today (cheap —
Ref construction; defaults still compile per site).

**Why this is first: it is remap-free.** The reused instance keeps
its own BindIds because it IS the same instance — none of
`clone_rebind`'s failure modes (remap aliasing, env mutation,
under-typechecked templates, clone↔delete accounting) can exist.
Blast radius is only transient-gated bodies (Sync, STATELESS builtins,
no connect/ByRef), and it kills the fib class alone. Pool lifecycle:
entries purge (delete) when the def dies (the `lambda_defs` removal
hook) and at context teardown; pooled instances keep their wake
registrations while idle (unref-on-push would double-unref at the
eventual delete), so a capture event can wake the top spuriously —
bounded by the cap, and the parked sites' quiet-cycle check already
handles content-free wakes.

The reset is the new work. It is neither `sleep` (preserves value
caches per the sleep-is-pause ruling) nor `reset_replay` (clears replay
caches, deliberately KEEPS `selected` and async-builtin state): a
reused instance must be indistinguishable from a **fresh compile** —
`selected = None`, empty `Cached` residents, empty `CachedArgs` slots,
cleared collection slot state. The transient gate already proves the
body holds nothing else, which is what makes "reset to fresh"
well-defined at all — the gate's guarantee is precisely that
delete-and-reinit is unobservable, and reset-to-fresh is
delete-and-reinit minus the allocator.

Mechanics: a required `reset_fresh` method on `Update`/`Apply` (the
reset_replay pattern — no default, the compiler forces the per-node
decision; most impls are `reset_replay` + clear-the-semantic-residue).
Wake registrations are NOT touched (the park's ext_refs takeover
already owns them). The stashed instance dies with the site
(`CallSite::delete` path unchanged) or when the def dies.

Expected: fib-class subjects ~10-20x (bind 50µs → reset walk ~2-3µs);
the entry instance (retained, non-transient) is untouched; non-Sync
recursion untouched.

Risks: a `reset_fresh` impl that misses a state kind = a stale-state
bug — the misfiling minefield sleep-preserves-caches mapped, in
reverse. Mitigations: required-method (every node author decides
explicitly), the transient gate bounding what state can exist at all,
the regress corpus (heavy recursion/select/taint pins), a dedicated
soak round before anything lands on top.

## Phase B — collection dispatch micro-costs (re-measure after A1)

The per-element residue beyond the bind (fold50k perf): formal inserts
into `event.variables` (`HashMap::insert<BindId, TagValue>` ~12%),
`Ref::update` reads + `ref_var` interest churn ~9%, per-element
`rt.cached` publishes of callsite args (`immutable_chunkmap` COW
`make_mut`+drop ~8.5%), `CallSite` update/delete plumbing ~7%.
Candidate levers, deliberately NOT designed in detail until A1 changes
the denominators:

- Frame-style formal delivery for slot dispatch (the replay_frames
  private-map precedent) — needs care: per-slot wake routing reads
  `event.variables` through ordinary Refs, and per-slot interior cache
  identity is SEMANTIC (the per-slot selection-memory ruling).
- Skipping the `rt.cached` publish for slot-internal arg ids whose
  only reader is the slot's own seed path.
- Batching the chunkmap COW writes per cycle instead of per slot.

B may move the fold class enough that Phase C below is unnecessary —
that is part of why C is gated rather than scheduled.

## Phase C — GATED PROPOSAL: clone the collection prototype

> This is a **resurrection of `clone_rebind`** (deleted 6317216d, P4
> final: ~40 structural clone impls + `RebindMap`/`remint_bind_id`)
> and does not proceed without an explicit ruling. Its ancestor's
> causes of death, and what would be different, are recorded here so
> the decision is made with the history in view.

Slot instances all coexist (prefix retention), so A1's freelist can't
serve the initial N-slot materialization — only making instantiation
cheap can. The proposal: slot instantiation becomes a structural clone
of the collection's **fully-typechecked, lazily-bound prototype
instance**, re-minting interior BindIds through a remap table.

What killed `clone_rebind`, and the delta:

1. **Under-typechecked templates.** The old machinery cloned
   fusion-path templates ("unchecked dynamic instances" — the jul10a
   IIFE divergence). DELTA: C clones only the prototype produced by
   the ordinary lazy-bind pipeline — same typecheck0/1 + analysis a
   real call site gets. This cause of death does not apply.
2. **The recompile-from-spec fallback.** Env/reference nodes
   (`TryCatch`, `Module`, `ByRef`, `Sample`, `Lambda`…) fell back to
   compiling at runtime, mutating shared env state (`alias_variable`
   pollution) with uncharacterized side effects. DELTA: **no fallback,
   ever** — a shape that can't clone structurally REFUSES and the slot
   takes today's lazy bind (a perf miss, never a correctness path).
3. **Unvalidated accounting** (clone↔delete `by_id` symmetry) and
   **aggregate-only coverage** of the structural impls. DELTA: the
   unexecuted plan in design/clone_rebind_testing.md becomes the
   PREREQUISITE — the per-shape clone-equivalence matrix and the
   `env_stats` grow/shrink invariant hook land and pass BEFORE the
   cloner is wired into slot instantiation.
4. The jul08m per-slot-lift aliasing class — the remap surface. DELTA:
   a debug assertion that no un-remapped interior id survives a clone,
   plus the fuzzer, which has caught every aliasing bug of this shape.

Scope if approved: cloning exists ONLY inside the collection
intrinsic's slot instantiation (bounded shape set — what callbacks
actually contain), not as a general lambda-instantiation mechanism.

## What this does NOT touch

- Kernel/fusion semantics: zero changes. The JIT twin constrains
  nothing here; agreement is enforced by the existing oracle.
- Compile-time paths: the typechecker-must-be-instant guardrail is
  unaffected (we run LESS typing at runtime, none extra at compile
  time). GUI wedge measured anyway after each phase.
- The transient gate itself, prime/replay, parking order (jul22b) —
  A1 swaps only delete-vs-reset at the park and compile-vs-reuse at
  the bind.

## Acceptance

Per phase: fib20/fold50k microbench + PERF counters before/after;
regress + workspace + detcheck + benches (the node-walk bench columns
should visibly DROP — a first); GUI wedge; then a full soak round on
the fixed binary before the next phase lands. Fleet-level success =
the fib/init benign-timeout classes stop appearing at current scales.
