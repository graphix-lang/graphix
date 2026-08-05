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

## Phase A1 — BUILT (abd4339a, 2026-08-04) then REMOVED (2026-08-05,
## Eric's ruling)

A1 was built as a per-`LambdaDef` transient instance POOL (the
per-site stash drafted first would have retained the full call tree —
the 9.6GB bomb — so instances pushed themselves to a shared per-def
freelist, cap 256), with a required `reset_fresh` method on
`Update`/`Apply` (~140 impls) restoring fresh-compile state at the
park. It delivered its numbers (fib(25) 220k binds → 24 compiles,
symbolic bench 63x) and survived gates and one soak round.

**Removed a day later.** The aug05a soak (katana) found a settled
depth-trip bottom RE-IGNITING under pool reuse: a growing-argument
non-tail recursion (`f(n + f(n-1))`) that pre-A1 settled in 513
dispatches (one trip, 0.08s) churned 2.9M shallow re-trips under the
pool. Root category: A1's soundness argument ("reset-to-fresh =
delete-and-reinit minus the allocator") missed that delete-and-reinit
also provided **id sterility** — a fresh compile mints fresh BindIds,
so a rebound instance was sterile w.r.t. every id-keyed channel
OUTSIDE the body (`rt.cached`, wake registrations, event residue).
`reset_fresh` cannot restore sterility; identity is what the pool
preserves. The depth-trip settle protocol was load-bearing on it
(tainted productions never enter `rt.cached`, so parked sites could
only re-arm from genuinely new deliveries; id reuse laundered clean
previous-life values back in). The final mechanism link (why the
era-identical wake check + prime + replay settle fresh but redescend
pooled) was never fully established — two candidate channels were
disproven — and Eric ruled the machinery out rather than deeper in:
interp performance on recursion doesn't matter enough (the soak
profile below shows the fleet is compiler-dominated; the user-facing
win was real but narrow). `reset_fresh` went with it (its only
caller). Pinned: findings/depth-trip-settle-aug2026/ guards the
settle behavior itself.

Lesson recorded for any future instance-reuse design (including the
gated Phase C): **reset-to-fresh ≠ fresh identity.** Any reuse
mechanism must either re-mint identity (clone_rebind's remap, with
its own graveyard) or prove every id-keyed runtime channel taint/
staleness-correct under reuse — the body-purity gate alone is not
sufficient.

## The actual-soak profile (post-A1, 2026-08-04)

Recorded on LIVE aug04f lanes on ryouko (perf on the running fuzz and
generate workers, ~660k samples each, frame-pointer build of
c3be4170+A1). This answers "did we profile the soak or just example
programs" — and the answer redirects the fleet-throughput question
away from this doc's interp levers:

| bucket                  | fuzz lane | gen lane |
|-------------------------|-----------|----------|
| typecheck+types         | ~25%      | ~24%     |
| fusion+JIT compile      | ~9%       | ~19%     |
| compile (Expr→Node)     | ~5%       | ~5%      |
| parse (combine)         | ~3%       | ~3%      |
| env chunkmap (by_id)    | ~6.4%     | ~6.6%    |
| interp-exec             | ~11%      | ~2.4%    |
| allocator (mimalloc)    | ~7%       | ~7%      |
| pool take/drop residue  | ~6.5%     | ~6.5%    |

The profile is FLAT (top symbol 2.7%) and compile-dominated: the top
two symbols in BOTH lanes are `Type::scope_refs_int` (2.4-2.7%) and
`normalize_int` (1.7-1.9%) — the per-subject STDLIB typecheck, run in
a fresh child per subject, in both engines. Direct measurement: a
trivial subject (`i64:1`) and a 1KB corpus subject both cost ~60-120ms
through the campaign binary — the subject's own work is noise against
the child constant. Fleet throughput ≈ cores / constant.

What this says about the phases here:

- **A1 verified in the real workload**: the pre-A1 bind pipeline
  (`RefHist::new`, `contains_dispatch`, `would_cycle_seen`) is GONE
  from the top of the profile. Interp-exec residue is dispatch +
  delivery churn (`CallSite::update` 2.3%, `GXLambda::update` 1.9%,
  `LPooled<HashMap<BindId,TagValue>>` take/drop ~2.8%, `Value::clone`
  1.1%, `ref_var` 0.7%) — exactly Phase B's target list, at ~11% of
  the fuzz lane and ~2% of the gen lane.
- **Phase B is a user-facing lever, not a fleet lever.** It stays on
  the docket for the fold class, but even zeroing interp-exec buys the
  fleet <11%.
- **The fleet levers are elsewhere** (recorded here, owned elsewhere):
  1. HARNESS subject batching — **BUILT 2026-08-05** (`check-batch`,
     Eric-approved shared-instance model): one warmed runtime pair per
     child runs K=16 Exact-tier subjects sequentially (subject-unique
     module names; `CompRes` drop deletes each subject's graph; a
     `SwapResolver` swaps the per-subject VFS into the long-lived
     ctx). Only AGREEMENT is trusted from a batch — any other
     verdict, a poisoned batch (Timeout/RuntimeErr wedges the shared
     runtime → abort + withhold the tail), or a dead child falls back
     to the individual `check_isolated` gold path, so every finding
     still derives from a fresh single-subject process with the full
     escalation ladder. Measured: 2.6-3.9x fleet throughput (261
     subjects/s at PAR=24 on ryouko); K=32/64 are WORSE (poisoned-
     batch blast radius), K=16 is the default (`GRAPHIX_FUZZ_BATCH`,
     1 disables). detcheck/selfcheck/minimize stay per-process.
  2. Stdlib compile cost — `scope_refs_int` (re-scoping walk that
     re-mints TVar cells and rebuilds Ref params per use site) +
     `normalize_int` + `collect_tvars`/`resolve_tvars_seen` ≈ 8%,
     plus the `env.by_id` chunkmap `bind_variable`/`Bind` clone COW
     churn ≈ 6.5%, plus combine's `easy::Error` eq/merge
     accumulation ≈ 1.3% on trusted (stdlib) sources. These also cut
     REAL user compile latency — typechecker-must-be-instant adjacent.
  3. gen lane only: fusion+JIT compile is ~19% (`ResolveCx::node_frame`
     1.5%, cranelift verifier+regalloc ~1.1%) — generated programs
     skew whole-program-fusable. The cranelift verifier could be
     disabled outside debug builds if we ever want the ~0.6%.

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
