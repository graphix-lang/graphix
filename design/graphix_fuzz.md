# graphix-fuzz: the differential fuzzer

Status: built 2026-06, extended through 2026-08
Pins: `graphix-fuzz/findings/` (the `regress` gate — every directory is a pinned finding), `graphix-fuzz/findings/callable-route-aug2026/`, `graphix-fuzz/src/lib.rs` tests (`breakage_window_trips_only_on_sustained_majority`), `graphix-fuzz/src/typemorph.rs` tests

## 1. The idea

Graphix has two evaluators of the same compiled `Expr → Node` graph:
the node-walk (`CFlag::FusionDisabled`, the canonical model) and
fusion + cranelift JIT (the system under test). For any deterministic
program the two must produce identical observable behavior, so **a
divergence proves a bug exists**. It is probably in fusion+JIT, but the
node-walk is the canonical model, not an infallible oracle: on every
divergence we reason from the language semantics about what the
program *should* do, then determine which engine is wrong. Several
divergences have been node-walk bugs.

This is dynamic translation validation — run both, compare — with a
structural advantage most compiler fuzzing lacks: a reference
implementation that shares the entire front-end with the SUT, so the
cross-checking problem collapses to one A-vs-B comparison plus human
reasoning on the rare divergence.

The differential oracle sees only the DISAGREEMENT class. A bug that
lives identically in both engines (checked arith that never detected
overflow; a `*st <- v` through a `&` param dropped in a sleeping arm)
agrees on the wrong answer. Those need an oracle that knows the
intended semantics: hand-written fixtures, the in-program twin
invariant (§6), the stdout baseline, or the acceptance-plane lane (§8).
Build both halves; neither subsumes the other.

## 2. The pipeline

Everything in front of the oracle is a swappable source; everything
behind it is shared: `check` (the oracle), divergence classification,
dedup, the minimizer, the findings corpus.

Subcommands: `check`/`run`/`minimize`/`typemorph <file>`; `generate
[--reactive]`; `fuzz` (a campaign); `regress` (the findings corpus);
`selfcheck`, `detcheck`, `gen-check`, `reactive-check`, `leakcheck`,
`typemorph-scan` (gates). `check-one`/`minimize-one`/`selfcheck-one`/
`typemorph-one` are the hidden child-process forms.

Sources: **corpus mutation** (seeds = the hand seeds plus every `run!`
fixture harvested at build time by `build.rs`; a type-directed
in-scope subexpression replacement applied 1–5×, with type-aware
subtree transplant between seeds), **generation** (`generate/`: a
`GenCtx` shadowing the compiler's `Env`; `gen_expr(target: Type)` and
its dual `find_producers` — type-correct by construction, since under
structural HM inference a random parse-valid program typechecks
essentially never; `--check` rejection is a drift canary, not the
validity mechanism), and **reactive generation** (`generate/reactive.rs`:
counter/accumulator/cross-cycle/sample-chain/slept-arm/dynamic-reload
templates with an injection schedule, plus `seq`/`seqq` ceremonies:
the trigger is an injected input or a burst counter restarted per
injection, so a second trigger lands while a run is busy and `seq`
drops it where `seqq` queues it; the body draws `let`, issued calls,
connects, `do`, `until`, `try … with`, an abort into a catch sink and
`println` steps, and every run is observable through an accumulator,
a run counter and the last block value, all present from init; a
body that can stall or abort stays out of the always-firing tail set). Statement slots may embed a whole generated
subprogram as a typed block (`subprogram_depth`, default 2; 50/50
sharing the outer scope vs closed), and slot counts are a geometric
draw so long dataflow chains appear organically — composition adds
depth with zero cross-seam type obligations.

**Serialization boundary:** every source emits text; the oracle
compiles from text (`Expr` → pretty-printer → re-parse). The
typechecker mutates `Expr.typ` and TVars in place, so reusing a typed
`Expr` across compiles would cross-contaminate the generator's
bookkeeping with the compiler's. Every test case is also human-readable
for triage.

**The campaign is one process** (`soak.sh`): the three work sources
share a single pool that divides the box by measured CPU
(`fuzz:generate:reactive` shares). Three lane processes could only
divide a box through the OS scheduler, and equal worker counts bought
13/19/66 CPU splits. **The evolutionary ring**: agreeing mutants where
both modes produced runtime traces and whose AST shape signature is
novel (an order-independent multiset hash over (node-kind, arity) with
a triviality floor) join a bounded FIFO ring of mutation seeds sampled
50/50 against the base seeds — an outward random walk from the curated
seeds, bounded by the mix and eviction. `FuzzStats::novel` is the
exploration metric. Ring trajectories are not seed-reproducible;
findings embed their program text, and the seed-replay gates never use
the ring. mimalloc is the fuzz binary's global allocator (~12% of
subject CPU was glibc malloc cold-start).

## 3. The oracle

**Per-cycle traces.** Recording is runtime-side, because host-side
batch counting is timing-fragile: `ToGX::TraceStart` arms a
`TraceState` hook at the `result_watch` site, a `Compiled` marker
anchors epoch 0 (killing the compile-vs-first-cycle race), and
`ToGX::TraceWaitIdle` resolves at the idle check with a
`TraceSegment`. `trace.rs` folds segments into
`Trace{epochs: Vec<Epoch{events: Vec<(cycle-offset, Value)>, capped}}`
— offsets relative to each epoch's anchor, because mid-burst relative
pacing is deterministic while absolute cycle numbers are not.
`first_difference()` classifies `EpochCount | ValueMismatch |
MissingFire | ExtraFire | Pacing | CapMismatch`, and the discriminant
participates in the dedup bucket so the minimizer cannot morph a firing
bug into a value bug. A bottom program is an instant empty-trace
agreement.

**The cycle budget is a deadline over WORKED cycles** — cycles where the
graph was handed program events, including eventless internal churn;
control-only cycles are excluded so recording is a pure function of the
program's own stream. `trace_wait_idle` always resolves, so a
never-idle spinner is an exact differential subject at the same
program-driven cycle count in both modes. Caps are schedule DATA,
identical in both modes; a cap mismatch (interp capped vs jit idle) is
a RECORDED divergence, because prefix agreement cannot distinguish
"interp merely slow" from "the JIT produced a value the interp never
emits".

**Injection schedules** ride a comment header: `// schedule-v1: cap=64
events=512; in0=i64:3 in1=f64:1.5; in0=i64:4` (`;`-separated epochs,
space-separated simultaneous sets). Inputs follow the contract
`let inN: T = <default>` plus `inN <- never(<default>)` at top level —
the connect makes the binding unstable so fusion binds it as a region
input instead of const-folding it. Epoch delivery is atomic
(`GXHandle::set_many`: one message = one batch = one cycle); separate
`Set`s batch nondeterministically. `// file-v1:` headers carry module
sections for multi-file subjects.

**Oracle tiers** (`OracleTier`, matched on code with comments
stripped): `Exact` — pure programs, exact per-cycle trace agreement,
with captured print output compared as well; `FinalValues` —
value-deterministic async (IO pacing races quiescence, but each
epoch's settled value is deterministic, so compare per-epoch finals);
`Excluded` — `rand::`/`sys::`/`http::`/`hold(`: no value comparison is
sound at any strength, the shapes still run so crashes surface, and
divergences never record. The Excluded list is empirical and
`selfcheck` polices it: a missing marker shows up as a finals-strength
flake.

**`Timeout` narrowly means a wedged evaluator** — or a runaway the
stack budget aborted: a `RuntimeErr` from a runtime whose `Control`
reports `budget_aborted()` maps to `Timeout`, because the deadline and
the budget are the same containment and which one stops an unbounded
descent first is a race between the engines' descent speeds, not a
property of the program. Timeout==Timeout is AGREE; read a bench
timeout as an unexplained failure, not a pass. An asymmetric hang is a
top-tier finding: fusion adding or removing nontermination.

**Minimal canonicalization**, because every canonicalization hides a
bug: NaN is one sentinel (both engines legitimately produce NaN);
floats otherwise compare exactly — a last-bit difference is a real
finding or a one-line cranelift-settings pin, never a tolerance; errors
are tag-strict; `normalize_diag` strips process-global abstract ids
from compile errors. Zero relaxations are encoded in
`Trace::agrees_with`: the triage policy is fix, don't whitelist.

**`selfcheck` is the oracle-soundness gate**: interp-vs-interp and
jit-vs-jit trace equality over generated + corpus programs, 100%
required before any interp-vs-jit finding is trusted. **`detcheck`** is
the fusion-shape determinism gate: two fresh processes, normalized CLIF
dumps must match. **`stdout-baseline.sh`** diffs absolute stdout against
a recorded baseline under a chosen mode — the gate for a semantics
change that moves both engines (the print-firing ruling).

## 4. Process isolation and campaign hygiene

Campaign checks run in CHILD processes (`check-one`: program on stdin,
one `VERDICT` line out, `kill_on_drop`, an outer deadline). A JIT'd
sync infinite loop cannot be preempted by a tokio timeout, cranelift
bugs SIGSEGV, and a node-walk overflow SIGABRTs — an in-process
campaign cannot converge, since every such mutant kills the run and
loses whatever was being minimized. Signal death records a
`crash_NNNNNN.gx` with the wait status and a stderr tail (the triage
signal separating a known overflow class from a silent SIGSEGV in JIT
frames). A crash finding is never promoted to `findings/` until fixed,
because the embedded regression corpus runs in-process. DIVERGE makes
the parent re-check the same (proven non-crashing) program in-process
for the full record. Minimization is also isolated (`minimize-one`): a
reduction of a benign divergence can itself be a crasher (dropping a
recursive function's base case), and minimizer child death records
the unminimized mutant instead. The child re-exec path is
`/proc/self/exe` (a rebuild under a live soak otherwise ENOENTs every
spawn); a spawn IO error hard-aborts the campaign rather than recording
garbage. `GRAPHIX_FUZZ_INPROC=1` opts back in-process for debugging.
Throughput cost is negligible — per-subject resolver spin-up dominates,
not process spawn.

Three rules learned from a campaign that died of its own exhaust:

- **Parent-owned sandboxes** (`sandbox_cwd`): the spawning side creates
  the tempdir, sets it as the child's cwd, and drops it after the child
  exits — the parent survives even a SIGSEGV'd child, so cleanup is
  unconditional (children exit via `process::exit`, so a self-owned
  tempdir leaked one inode per subject until the tmpfs's inodes ran out
  — invisible to `df -h`). `GRAPHIX_FUZZ_SANDBOXED` tells the child to
  skip its self-sandbox.
- **`BreakageWindow`**: the pool aborts the campaign (`FATAL`, exit 2)
  when a MAJORITY of a full 200-subject window are findings. The worst
  real bug classes hit well under 0.1%, so a sustained majority means
  the environment or the build is broken and every further "finding"
  is noise (an ENOSPC flood dedups nowhere, since the key varies with
  program text). Excluded-tier hangs do not count; finding-write
  failures are fatal too.
- **Campaign output lives OUTSIDE the repo** (`~/tmp/target/fuzz/`,
  `GRAPHIX_FUZZ_CORPUS` overrides); the repo's corpus dir syncs across
  machines. Children get `GRAPHIX_STACK_BUDGET` and an `RLIMIT_AS` (48GB — address space, not RSS; a healthy batch child passes 8GB).

The fleet deploy is a script (`fleet.sh deploy`), every step of which
verifies a fact (pgrep, content fingerprint, the campaign's own gate
line with the embedded corpus count).

## 5. Minimization: typed-AST hierarchical delta debugging

Non-optional: every fusion bug has been diagnosed from a tiny reduced
program. Text reduction produces parse errors almost always under
graphix's structural rules, and proptest shrinking ties to a `Strategy`
tape the mutational generator lacks; HDD on the `Expr` is the fit
(`mutate.rs`).

Operators: **statement drop** (the workhorse — generated programs are
long runs of interdependent `let`s, and the only thing constant-collapse
can do to a run is replace the block with one statement, a candidate
that essentially never survives; drops are keyed by the STATEMENT's
preorder index, not the block's, so a whole round can apply together),
replace-subtree-with-simplest-literal-of-its-type, hoist, and module
section internals (`parse_items`/`render_items`: a section is an item
sequence, so it shrinks as a `Do` and renders back bare — otherwise a
divergence that needs `m0::f` keeps every unrelated binding in `m0.gx`).

**Rounds, not restarts** (`shrink`): one scan tries every reduction
widest-extent-first, skips anything inside an accepted extent (so the
accepted set is pairwise disjoint), and applies them all at once. Each
was verified alone, so the composite nearly always holds; when it does
not, halving recovers a prefix, and a round that found anything always
makes progress. The restart-on-first-accept design spent a whole budget
re-testing the head of a large finding. Body and module sections cycle
with an equal slice of the remaining budget per lap.

Two guards make it trustworthy: re-typecheck every step, and re-run
both engines requiring the divergence persists WITH THE SAME BUCKET
KEY — reduction must not "succeed" by turning bug A into bug B (a
different B becomes its own seed). Budgets are oracle checks:
`CAMPAIGN_MINIMIZE_BUDGET` (80) buys a legible reproducer per finding;
`minimize <file> [budget]` defaults to 4000 because the last bytes cost
the most. Accept partial minima — a 40-node reproducer beats 200 lines.

## 6. Route equivalence and metamorphic twins

**callable-v1 (the route matrix).** The embedder dispatch path
(`GXHandle::compile_callable` + `Callable::call`, how every GUI/TUI
handler is driven) had no fuzz coverage. A `// callable-v1:
handler=m0::handler; cx0=i64:7; …` header (`callable.rs`) names a
handler in a `file-v1` module; the runner synthesizes the driver — arg
decls per the input contract plus an in-language call with each arg
`skip(#n:1, …)`-gated so the init never dispatches — and drives the one
text artifact on two routes: **in-language** (dispatch epochs as
`set_many` injections) and **dispatch** (`compile_callable` + `call` per
epoch, with gap compiles first — the embedder timeline has cycles
between building a handler and the first event). `check` runs the 2×2
matrix: engine pairs per route (the dispatch pair at finals strength),
then the route pair. `Divergence` carries which pair
(`Pair::{Engine, EngineDispatch, Route, Twin}`). Callable programs never
batch and never enter the mutation ring.

**Twins (the symmetric-bug oracle).** The route matrix could not catch
the bug that motivated it: a `&`-param write dropped in a sleeping arm
broke both routes identically, and every pairwise comparison agreed on
the wrong answer. The oracle that sees a symmetric bug is the program
itself: `generate/twin.rs` emits handler modules whose state is written
through SEVERAL equivalent routes (`&`-param, capture, `&`-param through
a nested call) from the same dispatch cycle, with an in-program verdict
that settles on the reserved `` `TwinDiverged `` tag when they
disagree. `check` scans every run's per-epoch FINALS for the tag
(transient intra-cycle skew never reaches a final); a violation is a
single-run finding (`Pair::Twin`), recorded after one confirming rerun.
Twins ride the reactive generation lane, half schedule-form, half
callable-form. What was missing was never vocabulary alone — the
pure-language face of the bug was generator-reachable all along — it
was an oracle that survives symmetry.

## 7. Triage, dedup, and the findings corpus

Dedup before minimizing (a fuzzer rediscovers a bug thousands of times
an hour): the coarse key is the divergence kind plus the primary
signal — a normalized panic template, else the trace difference class.
After minimization a finding is a `.gx` file in a directory under
`graphix-fuzz/findings/<class-date>/` with a README recording the
adjudication; the directory IS the regression gate. `regress` runs the
whole corpus through `check` in-process; the corpus is embedded in the
binary, so a soak's startup gate carries the count of pins it verified.
A finding's pin commits to cross-mode agreement, not to a value, until
a human confirms the intended semantics — the node-walk isn't
infallible, and only a value pin also guards against both engines
drifting to the same wrong answer. Pulled fleet findings go to
`fuzz/pending-triage/<campaign>/` (untracked); the triage record is
that directory's README.

## 8. The acceptance plane: `typemorph`

The differential oracle cannot see typechecker bugs on the acceptance
plane: a program wrongly rejected never runs, and both engines agree it
was rejected — a vacuous agreement. A week of inference bugs, all
order-sensitivity of unification, all found by hand, motivated a lane
that finds the family by machine.

**No decision oracle.** "Should this arbitrary program typecheck?" is a
parallel typechecker — a second implementation of the same semantics
that drifts. The lane never decides an arbitrary program; it tests
ACCEPTANCE ONLY (`--check` semantics: compile + typecheck + analyze,
never execute), so a transform may freely change runtime semantics and
remain a good typing probe, and throughput is `--check`-bounded. The
soundness direction (wrongly accepted programs) needs no new machinery:
a type lie that runs goes wrong in a type-shaped way in the runtime
lanes, and `abi_kind`/`freeze_for_abi` crash rather than shrug.

**Metamorphic transforms** (`typemorph.rs`): take a program the checker
accepts, apply an acceptance-preserving `Expr → Expr` transform, print
it back, check again; accept→reject is a flip. Transforms are graded,
and the grade is the triage default: parens-wrap is SOUND (a flip is a
compiler bug); block-wrap (`e` → `{ let __t = e; __t }`, not on direct
lambda-literal arguments), let-extract (`f(.., |x| body)` → `let __c =
|x| body; f(.., __c)` — THE order probe: declared-param push vs
body-first inference), let-inline (the reverse), stmt-permute (adjacent
independent binds — tvar allocation order) and alias-swap (hoist an
annotation's structural spelling into a typedef, and the reverse — the
Ref-vs-expansion channels) are EXPECTED (a flip files for triage:
compiler bug, transform-precondition bug, or a language rule that
needed words — "a free union member stays free" is the kind of rule
this lane forces into the open, witnesses attached). union-permute is
AST-invisible (the parser sorts unions) and eta-expand needs arity
knowledge; both are deferred. A candidate the printer cannot round-trip
is DROPPED and counted (`noparse`) — a printer-fidelity signal, never an
inference finding.

**Mechanics.** `typemorph_subject` runs one subject's probes against a
single warmed runtime through `GXHandle::check_with_resolvers` (a check
never executes and the env is restored per call, so the stdlib init is
paid once, not per probe); probes run only when the base accepts. The
child (`typemorph-one`) writes its verdict to a FILE, never stdout,
since the checked program can own the process streams. Site indices
live in `mutate`'s preorder space and transforms are deterministic
functions of the body text, so a `(kind, site)` id re-derives the
identical candidate in a fresh process — every flip is CONFIRMED in a
fresh process before it is reported, and a program whose own
acceptance flaps across processes is a determinism finding, never a
flip. Rejection heads are normalized (digit runs collapsed) so
positions and fresh-counter ids do not split buckets. `typemorph
<file>` is the triage tool; `typemorph-scan [n] [seed]` scans the
findings corpus plus generated subjects — a hand-run gate, re-run when
the transform catalog or the typechecker changes.

Not built: the reflective oracle (re-insert the checker's own printed
inferred types as annotations — blocked on the open ruling that
body-annotation tvars are fresh, not the signature's), type-stress
generation (strip annotations the generator certifies inferable — legal
only where the generator constrains shapes, since on arbitrary programs
an annotation may be load-bearing by language rule), and must-reject
mutation.

## 9. Ruled out

- **Scalar fitness / coverage-guided selection:** the corpus grows by
  shape novelty (the ring) and by pinned findings, not by a tuned
  score; the coverage maps a selector would read were tied to the
  deleted GIR vocabulary and were never rebuilt.
- **A third bisection mode:** the `fused` (GIR-interpreter) mode that
  once split "emit bug" from "codegen bug" is gone with the GIR; with
  one fusion flag a divergence is diagnosed by semantic reasoning and
  the subprocess re-check.
- **Whitelisting a divergence class** in the trace comparison: every
  relaxation hides the next bug of that class; the answer to a flaky
  pin is a tier (`FinalValues`/`Excluded`), fixed once per marker and
  policed by `selfcheck`.
- **Release-profile fuzzing:** the dev profile keeps
  `debug_assertions` on, and the asserts are what caught the earliest
  JIT classes.
