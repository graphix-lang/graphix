# graphix-fuzz — differential model-checking harness for the fusion/JIT backend (design)

Status: design, pre-build. Crate to be added: `graphix-fuzz`.

## 1. The idea

Graphix has two evaluators of the *same* compiled `Expr → Node` graph:

- the **node-walk interpreter** (`CFlag::FusionDisabled`) — simple, and the
  more-trusted model;
- the **fusion + cranelift-JIT backend** — vastly more complex, the system
  under test.

For any deterministic program the two must produce identical observable
behavior. **A divergence proves a bug exists.** It is *probably* in fusion+JIT
(it is far more complex), but the node-walk is the more-trusted model, **not an
infallible oracle** — we do not pre-assign blame. On every divergence we reason
from the language semantics about what the program *should* do, then determine
what each model actually does and which one is wrong.

This is dynamic translation validation: we don't prove `compiled ≡ source`, we
*run* both and compare. The oracle already exists — the three-mode
`run!`/`assert_modes_agree` harness does exactly this on ~550 fixed fixtures.
The missing ingredient is a firehose of **valid, deterministic,
semantically-rich** programs that exercise the fusion machinery (nested HOFs
with captures, composite-element destructure callbacks, `?` on Results, select
arm bindings, value-shape arithmetic) — where every real bug of the last month
(#162–#170) actually lived.

We have a structural advantage most compiler-fuzzing efforts lack: a trusted
reference implementation that shares the *entire front-end* with the SUT. The
Csmith/C-compiler world must run N compilers and take a majority vote because
there is no reference; we have one model that is usually right and a second that
is usually the suspect. That collapses the cross-checking problem to a single
A-vs-B comparison plus human reasoning on the rare divergence.

## 2. One pipeline, pluggable sources

The framework is a single pipeline; everything in front of the oracle is a
swappable **source**, everything behind it is shared:

```
  SOURCES                            SHARED PIPELINE
  ┌──────────────────────────┐
  │ A fixture-mutation        │\
  │ B agent harvest (haiku)   │ \  text   ┌───────────────────────────┐
  │ C type-directed gen       │  ──────────▶│ ORACLE check(code)         │
  │ D corpus crossover+swarm  │ /  (--check │  interp(FusionDisabled)    │
  │ E adversarial agents      │/   gate)    │   vs jit                   │──▶ agree:
  └──────────────────────────┘             │  (fused as failure bisect) │     read coverage,
            ▲          ▲                     │  staged: snapshot → trace  │     keep iff new bits
            │          │ (E calls the oracle └─────────────┬─────────────┘
            │          │  directly, iterates)              │ divergence
            │ coverage feedback                            ▼
            │ (GirOp tags, FUSEBAIL,            ┌────────────────────┐
            │  interaction pairs)               │ COARSE DEDUP        │── seen K×? drop
            │                                   │ (kind, panic/tag)   │
            └──────── corpus ◀─────┐            └─────────┬──────────┘ novel ↓
                                   │            ┌─────────▼──────────┐
                                   │            │ MINIMIZE (typed HDD)│ same-bucket-persists
                                   │            └─────────┬──────────┘
                                   │            ┌─────────▼──────────┐
                                   └────────────│ PRECISE BUCKET +    │
                                                │ emit agreement test │──▶ fuzz/crashes/<bucket>/
                                                └────────────────────┘    + #[ignore] regression
```

The five sources are interchangeable front-ends; the oracle, divergence
detection, minimizer, dedup, and corpus are written once. B and E are the two
*agent* sources, and they search differently: B gives **breadth** (cheap, high
volume, idiomatic feature combinations from problem-solving); E gives **depth**
(few, powerful, adversarial, frontier-seeking, with the oracle in its own loop).

### Source A — fixture mutation (v1 bug-finder)

Seed = the ~550 existing `run!` fixtures (known-valid, type-diverse,
fusion-relevant). Mutation = "replace a random in-scope, typed subexpression
with another in-scope ref or a small generated calc of the same type," applied
1–5×. The recent bugs all lived in *interactions* no single fixture covers but a
mash-up of two does, so this finds real divergences with near-zero generator
sophistication.

**Mutate-deep-before-discarding** (the valley-crossing lever): mutations stay
type-directed so most steps keep compiling, but a step that breaks compilation
is *not* immediately discarded — allow up to `K` further mutations to recover
(or wander to a different valid program) before throwing the whole chain away.
This reaches coordinated multi-edit programs that no single valid-preserving
mutation can. Keeping the per-step mutations type-directed is what stops the
chain from wandering off into the (enormous) non-compiling void; the `K`-deep
recovery loop only crosses *small* valleys.

### Source B — LLM-harvest (early seed-corpus bootstrap + idiomatic diversity)

Run many cheap (haiku) agents, each given: the language spec, a problem to
solve, and the compiler with fusion **off** (`graphix --check` + run under
`FusionDisabled`). Each agent iterates until its program compiles and runs and
produces the intended result under the trusted model. Output: idiomatic,
semantically-meaningful programs that combine features the way real code does —
driven by *problem structure*, not coverage bits, so they hit combinations a
random generator never assembles.

Two payoffs: (1) every harvested program is immediately a test case (run it
through the oracle; any divergence is a find); (2) idiomatic programs are
far better **crossover donors** and **mutation seeds** than random subtrees, and
they bootstrap a rich corpus *fast, before the type-directed generator exists.*
This is a batch/periodic harvest, not a search loop — it feeds the corpus that
the search loop then exploits.

### Source C — type-directed generation (the scalable engine)

Carry a `GenCtx` shadowing the compiler's `Env` (lexically-scoped
`(name, Type, lambda_depth)`, typedefs, in-scope tvars). Core primitive
`gen_expr(ctx, target: Type, budget) -> Expr` folds over the `Type` enum (the
target type says which constructors are legal). Dual primitive
`find_producers(ctx, target)` enumerates every way to make `target` from what's
in scope (refs, struct/tuple/array projections, applications of in-scope and
stdlib functions) — this is the bug engine: it manufactures internal data
dependencies and is the highest-yield path to type-correctness.

**Type-correct by construction, never generate-then-filter** — graphix's
structural HM inference means a random parse-valid program typechecks ~never
(this is why the existing `expr/test.rs` proptest is a *parser* round-trip test,
not a semantic generator). `--check` is a *health metric* (a rising reject rate
means the generator's type model has drifted from the compiler's), not the
validity mechanism.

The user's original "90% constrained / 10% recursive do-block" weighting maps
directly: the constrained 90% = `find_producers`; the recursive 10% = full
`gen_expr` recursion.

### Source D — corpus crossover + swarm (diversity layer)

Coverage-guided search stalls when new coverage needs a coordinated
multi-construct change. Two defenses: **type-aware subtree crossover** (splice a
well-typed subtree from one corpus parent into a type-compatible hole in
another — `Expr.typ` makes the slot-matching feasible) and **swarm testing**
(randomly *ban* a feature set per batch — e.g. ban integer arithmetic to *force*
the generator to wire HOFs and composites together, reaching interaction buckets
a maximal generator dilutes away). Swarm is nearly free and is the
evidence-based defense against generator coverage-skew.

### Source E — adversarial agent bug-hunters (the productionized hand-search)

Give a *powerful* agent (Sonnet/Opus, not Haiku) the language spec, **CLAUDE.md**
(the fusion architecture + the recent-bug taxonomy #162–#170), and the oracle as
a CLI (`graphix-fuzz check <file>`), with one instruction: *write a valid program
that makes the two compilers disagree* (`interp ≠ jit`, a backend panic, or an
asymmetric hang). This is adversarial fuzzing by a real programmer — and it is
not speculative: **the entire #162–#170 cascade was exactly this loop, run by
hand.** Every one of those nine bugs was found by a capable agent reasoning
"what edge case would break this," writing a tiny program, and running it under
both modes. Source E productionizes the process that already produced the bugs.

Two things distinguish E from B (the Haiku harvest):

- **The oracle is in E's own loop.** B just writes valid programs; E *runs the
  differential check itself* and iterates — write → check → see the divergence
  (or not) → refine toward the frontier. That feedback loop is the human
  bug-hunting loop, automated.
- **It is armed with the bug taxonomy.** CLAUDE.md documents every known class;
  E's job is a *new* class, or a variant of a known one that slipped the
  regression net. That focuses the (expensive) search on the frontier instead of
  re-finding fixed bugs.

E feeds the pipeline two ways: a *found* divergence goes straight to
minimize/triage; and even when E *fails* to break the compiler, its candidate
programs are high-value **corpus seeds** (frontier-by-construction → high
coverage). Better still, a successful E run's *reasoning trace describes a new
bug class*, which becomes a new mutation bias for C/D and a new problem prompt
for B — so the expensive agents **discover frontiers** and the cheap mechanical
loop **saturates** them.

Honest caveat: agent sources (B and E) are nondeterministic, rate-limited, and
costly relative to the mechanical generator. They are the **discovery** layer,
not the overnight-millions **saturation** layer. The combination is the design:
agents find the frontier, the mechanical fuzzer saturates it. E is also the
*cheapest source to stand up* — it needs only the `check` CLI, no generator
infrastructure, so it can run the day V1 ships.

#### Worked example: the class the differential oracle CANNOT catch

While setting up the float edge tests, hand-probing turned up two real bugs that
together prove why E is structurally necessary, not just nice-to-have:

- **`+?`/`-?`/`*?` never detected integer overflow** (silently wrapped). Both
  the node-walk AND the fused path wrap *identically* — they **agree on the
  wrong answer**. The differential oracle (interp vs jit) is **blind** to this:
  no disagreement, no signal. Only an oracle that knows `+?` is *supposed* to
  error on overflow catches it — i.e. a hand-written spec test, or a Source-E
  agent reasoning about intended semantics.
- **`NaN != NaN` diverges** (node-walk total-equality `false` vs fused IEEE
  `true`). Here the two models *disagree*, so the differential oracle catches it
  directly — no semantics knowledge required.

The lesson for the design: the mechanical differential oracle (Source A/C/D →
interp-vs-jit) catches the *disagreement* class only. The *both-models-
consistently-wrong* class — semantic bugs that live identically in both paths —
is invisible to it and requires either a spec oracle or the semantics-aware
agent sources (B/E). Build both halves; neither subsumes the other.

## 3. Serialization boundary

All sources emit text; the oracle compiles from text. Generate/mutate the
`Expr` AST → `to_string()` (pretty-printer) → re-parse → compile. This is
mandatory, not cosmetic: the typechecker mutates `Expr.typ` (the
`Arc<OnceLock<Type>>` cell) and TVars *in place*, so reusing a typed `Expr`
across compiles cross-contaminates the generator's type bookkeeping with the
compiler's. Bonus: every test case is human-readable, which matters enormously
for triage.

## 4. The oracle

### Comparison, staged

- **V1 — single-snapshot on a pure-sync, terminating sublanguage.** Reuse the
  existing `eval` (which returns on the first `Updated` of the result). This is
  *sound here* because a pure-sync program produces exactly one value and stops;
  the single-snapshot limitation is invisible when there is only one snapshot.
  No new runtime code.
- **V2+ — per-cycle trace.** Add a `DriveToQuiescence` control message (the
  `ToGX::{MatchShape,DescribeShape,EnvStats}` pattern shows how; `cycle_ready()`
  already exists as the principled quiescence signal). Step `do_cycle` until
  `!cycle_ready()` or `max_cycles`, snapshotting the observable per cycle →
  `Vec<Option<Value>>` where `None` = "emitted nothing this cycle" (strictly
  stronger than value equality — catches `?`/`$`/`never` drops). Same
  `(source, schedule)` to both modes → identical cycle indices → comparable
  traces. Reactive generation (driver-controlled `tick` input via `GXHandle::set`
  on a designated BindId; bounded `<-` loops) ships *with* the trace oracle, not
  before it.

Reactive is complex; we are not claiming to test everything. V1 tests something
worth testing (pure-sync, where most of the GirOp/JIT surface lives) and we
expand the horizon as we learn.

### Two modes hot, the third as a free bisector

Run `interp` vs `jit` per case (highest signal). On divergence, auto-re-run
`fused` (`JitDisabled`): `interp == fused ≠ jit` → cranelift codegen bug;
`interp ≠ fused` → GIR/emit bug. This trisection is exactly how #162–#170 were
diagnosed ("reproduced under JitDisabled ⇒ GIR not JIT"). Don't pay the third
mode per-case; pay it only on failure, where it's a free bisection.

### Minimal canonicalization

Every canonicalization hides a potential bug, so the list is deliberately tiny:

- **NaN → one sentinel** (the only mandatory one; `NaN ≠ NaN` would create false
  divergence — both models legitimately produce NaN). Floats otherwise compare
  **exactly**; a last-bit difference between cranelift and the node-walk's scalar
  Rust is either a real finding or a one-line cranelift-settings pin (FMA
  contraction is the usual culprit) — never a tolerance.
- Errors: **tag-strict, message-lenient** (messages embed positions that differ
  trivially across fused/unfused routes; downgrade message-only divergence to a
  logged finding until proven real).
- Maps/structs are canonical (sorted) by construction — *assert* it; a reorder
  is a real bug.

### Determinism is a property of the generated sublanguage

The oracle dissolves under nondeterminism (can't tell "models disagree" from
"two runs saw different randomness"). **Exclude from generation:** `rand::*`,
`sys::time::{timer,now}`, `sys::net`, `sys::fs`; treat `print`/`dbg`/`log` as
value-irrelevant. This costs almost nothing in *fusion* coverage — those are
exactly the `EffectKind::Async` builtins fusion bails on anyway, so the
**deterministic sublanguage and the fusable sublanguage are nearly the same
set.** Detect leakage by double-running `interp` and quarantining any program
that disagrees with itself (a generator defect, not a SUT bug).

### Per-program verdict

`PASS` (modes agree) · `FAIL(divergence: program, all-mode outputs, which agree)`
· `DISCARD(reason: didn't compile / both-hang / self-nondeterministic)`. An
**asymmetric hang** (one mode finishes, the other doesn't) is a top-tier `FAIL`,
not a discard — that's fusion adding/removing nontermination (the #162–#170 hang
signature). Only *both*-hang is a discard.

## 5. Selection — coverage-guided, not scalar fitness

Keep a candidate iff it adds a **new bit** to the union of three cheap maps read
straight off existing instrumentation (`GirOpTag`/`visit_ops` in
`node_shape.rs`; `FUSION_INVOCATIONS`/`JIT_INVOCATIONS`/`record_fuse_bail` in
`gir_jit_helpers.rs`). AFL's `has_new_bits`, graphix-shaped. No score, no
weights, no tuning treadmill.

1. **Fused-op coverage** (primary): set of `GirOpTag`s that ran *in a kernel
   that actually fired* (gate on `FUSION_INVOCATIONS > 0`). Bucket at the
   resolution where danger lives — `ArrayMap{scalar}`/`{composite}`/`{destructure}`
   distinct; integer `Bin` collapsed.
2. **Fusion-bail coverage** (frontier): the `FUSEBAIL` tag set. A program hitting
   exactly one bail sits *on* the fusion frontier — where the next bug and the
   next cliff both are.
3. **Risky interaction pairs** (coarse): `(parent_op, child_op)` in executed
   kernels, restricted to pairs touching a risky tag (HOF ops, `QopUnwrap`,
   `DynCall`, `Call`, composite producers, `ValueArith`). Every recent bug was a
   2–3-feature interaction, not a single op.

Plus one coarse `dataflow_depth ∈ {0,1,2,3,4+}` bucket as a backstop so a
structurally-deep program counts as new coverage.

The original graded-fitness intuitions survive, relocated: "reward machinery
use, saturating" *is* a coverage set (each feature counted once, automatically,
self-maintaining as we lower cliffs); "reward internal data dependencies"
becomes the generator's **in-scope-ref bias** (prefer a ref to a recent binding
over a fresh literal), which *produces* connectivity by construction — which
matters precisely because fusion *is* dependency-subgraph analysis.

## 6. Minimization — typed-AST Hierarchical Delta Debugging

Non-optional and first-class: every recent fusion bug was diagnosed from a *tiny*
hand-reduced program. Text reduction (creduce-style) produces parse errors
~always under graphix's structural rules; proptest integrated shrinking ties to
a `Strategy` tape the mutational generator doesn't have. HDD on the typed `Expr`
is the fit.

Highest-value operator: **replace any subtree with the simplest literal of its
type** (collapses whole computations while staying typed). Then: inline-let,
drop-unused-let, drop-select-arm-if-still-exhaustive (self-correcting — the
exhaustiveness checker rejects unsound drops at re-compile), collapse-block-to-
tail, shrink-collections, numeric-shrink-toward-boundary.

Two guards make it trustworthy: (a) **re-typecheck every step** (`--check`); (b)
**re-run both models and require the divergence persists with the same bucket
key** — prevents reduction "succeeding" by turning bug A into bug B (a different
B becomes its own seed). Budget-cap (~2000 oracle calls / 60s); ship the
fixpoint reached. Accept partial minima — a 40-node reproducer beats 200 lines.

## 7. Triage, dedup, and closing the loop

**Dedup before minimizing** (minimization is the expensive step; a fuzzer
rediscovers a bug thousands of times/hour). Coarse key on the raw find:
`(divergence_kind, primary_signal)` where `primary_signal` is, in priority:
normalized panic-template (a `gir_interp` `panic!` or cranelift codegen panic —
most precise, and most recent bugs produce one) → differing `FUSEBAIL` set →
executed-kernel `GirOpTag` set. Seen ≥K times ⇒ drop unminimized. After
minimization compute the full key including an `ast_skeleton_hash` (minimized
tree, literals erased to type, bind names canonicalized) so "400 variants of the
same select-arm-binding bug" collapse to one skeleton.

**Persistence:** one RON file per record under `fuzz/crashes/<bucket>/`
(git-tracked; the dir *is* the database): bucket key, raw + minimized program,
all-mode outputs, generator seed + mutation log (deterministic replay), GirOp
tags / bails / panic template.

**Closing the loop — regression emission respects "node-walk isn't infallible":**
the minimized program emits as a `run!`-style fixture asserting **cross-mode
agreement** (`assert_modes_agree` — runs all modes, asserts equal, commits to
*no specific value*). That guard fails while the bug exists and goes green when
fixed, with zero assumption about which model was right. A second, stronger tier
— assert a specific value — is added only after a human confirms the intended
semantics (and it additionally guards against both models drifting to the *same*
wrong answer). `#[ignore]`'d with the bucket as the doc comment until fixed —
exactly the repo's existing idiom (#162 landed `#[ignore]`'d as a live doc;
#167's `shadow_arm_binding_*` are permanent guards).

## 8. Risk register

1. **Type-correct generation yield falls off with depth.** Mitigation:
   type-directed generation-from-context is the architecture, not an
   optimization; watch `--check` reject rate as a drift canary; seed from
   fixtures + LLM-harvest so early phases don't depend on reaching depth from
   scratch.
2. **Reactive-oracle soundness — single-snapshot misses the newest bug class.**
   Mitigation: staged. V1 generates only pure-sync terminating programs (single
   snapshot is exactly correct there). V2 adds `DriveToQuiescence` + trace; don't
   ship reactive generation before the trace oracle — they're coupled.
3. **Nondeterminism masquerading as a backend bug.** Mitigation: exclude
   nondeterministic/IO builtins by construction (cheap — they don't fuse);
   canonicalize NaN; quarantine self-disagreeing programs.
4. **Native hangs / JIT crashes wedge the harness.** A sync infinite loop in
   JIT'd code can't be preempted by a tokio timeout (no yield points); cranelift
   bugs SIGILL/SIGSEGV. Mitigation: V2 = coordinator + worker *processes*,
   heartbeat + OS SIGKILL watchdog, worker writes `current_program` before each
   `check`; a crash/asymmetric-hang *is* a divergence. V1 accepts in-process risk
   under a shell `timeout` + iteration cap + pre-check logging.
5. **Thread-local counters force single-threaded oracles.**
   `FUSION_INVOCATIONS`/`JIT_INVOCATIONS` are thread-local `Cell`s — reset-run-read
   on one thread. Mitigation: parallelism is N worker *processes* (one/core),
   each single-threaded internally; aligns with isolation (risk 4). The JIT is
   per-`ExecCtx` (fresh per program — the harness already does this), so distinct
   runs don't corrupt each other's JIT *except* the documented cranelift
   `func_ctx`/`builder_ctx` poisoning class — if a divergence vanishes in process
   isolation, suspect poisoning, not codegen.
6. **Minimization cost + same-bug-different-minimum.** Mitigation: coarse-dedup
   before minimizing; same-bucket-persists guard during reduction; accept partial
   minima and minor bucket impurity (a human spots a mis-merge in 5 programs
   instantly — don't over-engineer perfect dedup).

## 9. V1 and roadmap

**V1 (~4 days; single-process, single-threaded, in-process, pure-sync,
single-snapshot). Cuts: type-directed generator, coverage-guided selector,
reactive trace oracle, process isolation, skeleton bucketing.**

1. `graphix-fuzz` bin crate. `run_once(code, flags, timeout) -> Run` +
   `check(code) -> Option<Divergence>` (copy `run_with_flags` from `testing.rs`,
   three flag sets, compare the single `eval` value). (½ d)
2. **Source A only:** fixtures + the one in-scope-typed-subexpr-replacement
   mutation, applied 1–5×, with the mutate-deep-before-discard loop. (1 d)
3. Oracle + hang guard: 10s async timeout per `check`; run under shell
   `timeout 8h` + `--max-iterations`; log `current_program` before each check.
   (½ d)
4. Typed-AST minimizer, four highest-value operators, re-typecheck +
   re-diverge + same-bucket-persists each step. (1.5 d)
5. Coarse dedup (`divergence_kind` + panic-template), persist to
   `fuzz/crashes/`, `--emit-fixtures` printing an `assert_modes_agree` line.
   (½ d)

- **V1.5 — adversarial agents (Source E).** The day the `check` CLI exists,
  point Sonnet/Opus agents at it with the spec + CLAUDE.md + "break this
  compiler." No generator infrastructure required — it's the cheapest source and
  it's the productionized form of the loop that already found #162–#170. Found
  divergences flow through the same minimize/triage; candidate programs seed the
  corpus.
- **V2 — isolation + reactive oracle.** Coordinator/worker processes,
  heartbeat + SIGKILL, crash/asymmetric-hang as divergence (unlocks unattended
  overnight running, matching the existing parser-proptest cadence). Add
  `DriveToQuiescence` + per-cycle trace; driver-`tick` reactive generation (the
  two reactive pieces ship together).
- **V2.5 — agent harvest (Source B).** Batch Haiku agents solving problems →
  self-validated idiomatic corpus; feed as oracle input *and* as seed/crossover
  donors. (B is breadth/volume; E from V1.5 is depth/targeting — they coexist.)
- **V3 — type-directed generator (Source C).** `gen_expr` + `find_producers`
  from a harvested stdlib `.gxi` signature catalog; one bias rule per historical
  bug class (capture pressure / nested HOF / destructure / `?`-on-Result /
  shadowing-arm).
- **V4 — coverage-guided selection + crossover + swarm (Source D + selector).**
  The three coverage maps + `has_new_bits` corpus; type-aware crossover; swarm
  batching; precise bucketing (GirOp-set + skeleton hash); nightly cron
  regression replay of the crash corpus.

## 10. Open questions (mostly resolved)

- **Reactive scope:** v1 pure-sync / single-snapshot, reactive deferred to v2 —
  **resolved (accepted).**
- **Fitness:** coverage-guided, not scalar — **resolved (accepted).**
- **Build profile:** fuzz the **dev profile** (`debug_assertions` on) — we *want*
  the malformed-GIR `panic!`s and the invocation counters; release would silence
  exactly the asserts that caught #162. **Default.**
- **Float strictness:** exact, NaN-canonicalized; pin cranelift rather than add
  tolerance. Worth a one-time check of whether node-walk and cranelift float ops
  can already diverge (FMA contraction) before scaling on float-heavy programs.
- **`DriveToQuiescence` default `max_cycles`** (for v2): a 50-cycle prefix as the
  starting horizon for stateful-kernel bugs? — open.
- **Crash-corpus persistence:** git-track `fuzz/crashes/` to start; revisit if it
  grows large. — default, revisit.
