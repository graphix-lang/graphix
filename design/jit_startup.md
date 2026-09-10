# JIT startup cost

Status: built 2026-09-09
Pins: `stdlib/graphix-tests/src/lang/fusion.rs`, `bench/startup.py`, netidx's `graphix-package-netidx-admin::test::milestone_timing`

## Measurement

The admin application is the primary workload. Its `milestone_timing`
test compiles the same app with fusion enabled and disabled, separately
times package registration, and reports attempted and successful regions.
This measures compilation, not rendering or time to the first frame.

Run from the sibling netidx checkout:

```bash
cargo test --locked --release -p graphix-package-netidx-admin milestone_timing -- --ignored --nocapture
cargo test --locked --release -p graphix-package-netidx-admin milestone_timing -- --ignored --nocapture
```

The profile is `opt-level=3`, LTO, one codegen unit, without debug info.
Build artifacts remain in the centrally configured tmpfs. Keep copies of
the before/after test executables, run them directly in alternating order,
and wait for builds and other measurements to finish before comparing
wall times. Run timing comparisons without `GRAPHIX_PROFILE`.

Fusion phase times come from `GRAPHIX_PROFILE` (the `ReturnType`,
`Inputs`, `Builtins`, `Callees` and `Emit` phases; see full compiler
profiling below). `Callees` includes the callee's own builtin discovery.
`Emit` includes Graphix analysis such as block liveness as well as CLIF
emission, Cranelift compilation and finalization; it is not a
measurement of Cranelift alone.

Representative admin profiles on an Intel Core Ultra X7 358H, measured
separately from the uninstrumented timing comparison below:

| phase | before, ms | after, ms |
|---|---:|---:|
| whole fusion pass | 381.6 | 283.3 |
| return types | 65.4 | 59.7 |
| inputs | 26.5 | 12.4 |
| builtins | 19.5 | 10.0 |
| callees | 4.4 | 2.2 |
| emission and code generation | 258.5 | 193.3 |

There were 2,815 attempted regions and 541 successful regions. A sampled
release run also showed substantial reference-set insertion and type
clone/drop work. Inspection of the reference-set paths found the repeated
suffix scans in block emission described below.

Nine alternating release runs, following one warm-up per binary and with
builds and tests finished, produced these medians:

| measurement | before, ms | after, ms |
|---|---:|---:|
| package registration | 115.0 | 99.3 |
| app compilation, fusion enabled | 620.4 | 526.7 |
| app compilation, fusion disabled | 234.0 | 231.0 |
| enabled minus disabled, per run | 388.9 | 297.0 |

App compilation improved by 15.1%; the additional cost of enabling
fusion fell by 23.6%. Enabled compilation ranged from 616.3–625.1 ms
before and 524.8–535.5 ms after. Both binaries attempted 2,815 regions
and fused 541: the improvement preserves coverage and makes refusals
cheaper. The baseline was repository revision `95480428`.

The generated-block harness, using dev-profile shell binaries and nine
alternating runs, measured whole-process check time as follows. These
include package registration, so they also reflect the reduced cost of
rejecting function return types during registration.

| bindings | before, ms | after, ms |
|---|---:|---:|
| 64 | 123.7 | 81.6 |
| 128 | 137.5 | 99.9 |
| 256 | 179.3 | 145.0 |
| 512 | 259.9 | 207.4 |

Validation: the workspace gate passed 3,168 tests (11 slow tests ignored),
the admin release suite passed 22 (two manual benchmarks ignored), and
the differential fuzzer reported zero regressions over 477 corpus
programs and zero divergences or crashes in a 500-program mixed soak.

## Admission

Builtin discovery rejects a registered builtin without a fast-call entry
before resolving and freezing its argument types. The same walk rejects
every node `fusion::effect_blocker` names: connects, catch installation,
sequence guards, modules and impls. That one classification is also what
block emission consults before discarding a dead statement, so a node
discovery rejects anywhere is never one emission could have dropped.
Region input collection and emission run only after discovery succeeds.
Callee signature construction also performs discovery before
constructing its parameters and captures.

Sampling, partial delivery, and reference nodes reject immediately when
they are the region root. They cannot reject an arbitrary ancestor:
an unused, effect-free statement containing one can be discarded by
block emission. The `native_block_discards_unused_reference` regression
protects this distinction.

Rejection preserves the ordinary descent into child regions and records
the blocker at its source, so `#[native]` can still identify the operation
that prevents fusion. `FusionStats.rejected_before_emit` counts attempts
that end during discovery. Passing discovery is not a promise of fusion;
the emitter remains responsible for supported shapes.

A return type the ABI cannot represent at all (a function, a reference,
a wide primitive union) is refused before normalization or environment
expansion; only a non-canonical type is normalized and only an
unresolved named type is expanded.

## Block liveness

For each non-tail statement, emission needs to know whether a later
statement reads or writes any binding it introduces. Scanning the entire
suffix for each statement repeats reference collection quadratically in
the number of statements.

A backward pass now accumulates later reads and connect targets in a
pooled set and stores one liveness bit per statement in a pooled vector.
It collects each statement's references once. The effect-free predicate
and emission order are unchanged. References in discarded statements
still conservatively contribute to the needed set, matching the previous
rule rather than introducing more dead-code elimination.

`bench/startup.py` generates blocks with 64, 128, 256, and 512 bindings,
requires their calls to fuse, and measures whole-process check latency.
Other compiler phases and code generation still contribute to these
times; the linearity claim is specifically about reference collection
within a single block emission.

## Further profiling

Node markings need to describe the emitted computation at each instance,
including statement elimination and inline collection callbacks. The
existing sync/stateless facts serve activation and tail-loop semantics:
for example, a stateless builtin may deliberately have no fast-call entry.
Reusing those facts as a complete fusion predicate would conflate the two
contracts.

The current admission check deliberately reuses discovery instead of
adding another whole-program pass or persistent per-node metadata. The
full compiler profile below separates the remaining type-processing,
emission, and backend costs.


## Full compiler profiling

`GRAPHIX_PROFILE=1` enables per-thread, nested phase accounting. Each
outermost span reports its start timestamp, wall duration, and phase
counts. `self_ns` excludes nested spans; the self times sum exactly to
the root duration. `total_ns` includes nested spans and must not be
summed. `failed_ns` is the inclusive time of marked failed calls, a
subset of `total_ns`.

The probes cover source parsing, packed-AST decoding, graph construction,
both typecheck passes, deferred settles, call-graph/effect/recursion
analysis, typedef seeding, fusion discovery, ABI freezing and fallbacks,
CLIF construction, backend body/wrapper/stub/spill compilation, and
finalization. `Clif` includes body setup and Graphix analysis performed
while constructing CLIF. `JitBuild` failures include their cleanup.
`BackendStub` measures backend compilation of abandoned declarations.

Run the built release test executable directly, with builds and other
test runs finished:

```bash
GRAPHIX_PROFILE=1 /path/to/admin-test milestone_timing --ignored --nocapture > profile.log 2>&1
python3 bench/profile.py profile.log
python3 bench/profile.py --json profile.log > profile.json
```

The log reader validates exclusive-time accounting for every root. Keep
profiling and latency comparisons separate: clock reads and profile
output add overhead. The existing `GRAPHIX_DBG_PERF` counters are
independent; leave that flag unset for this profile. No compilation or
fusion policy changes when profiling is enabled.


### Measured breakdown

Seven runs of the refined release instrumentation followed warm-ups and
alternated with both the preceding binary and the new binary with
profiling disabled. Builds and tests had finished. The unchanged preceding
binary measured **580.7 ms** for the app request in this session
(573.1–613.2 ms), versus 526.7 ms in the earlier measurement session.
The new binary measured 580.5 ms with profiling disabled and 583.1 ms
with profiling enabled. These are profiling results under the current
machine load, not a new speedup or regression claim.

The instrumented app request includes a median 23.1 ms outside
`compile_stmt` (22.2 ms without fusion). The runtime sends the compile
reply before `do_cycle`, but the current-thread test's waiting task may
resume after initial evaluation. Request timing therefore includes
scheduling/initial-update work and profiling output in addition to
compilation; it is not time to a rendered frame. Whole-process CPU
samples also cover initial evaluation and teardown.

Median compiler phase times:

| phase | fusion on, ms | fusion off, ms |
|---|---:|---:|
| whole app `compile_stmt` | 560.2 | 247.9 |
| top-level typecheck1, including instantiation | 195.9 | 188.8 |
| function-property analysis | 59.6 | 58.7 |
| typedef seeding | 0.3 | 0.3 |
| fusion | 302.1 | — |

Top-level graph construction, typecheck0 and deferred settles together
are below 0.1 ms for the app call. Most graph construction and first-pass
typechecking occur *inside instantiation during typecheck1*. Its exclusive
costs with fusion enabled are:

| work | calls | self time, ms |
|---|---:|---:|
| construct instance bodies | 2,513 | 67.2 |
| check instance bodies and argument/return types | 2,513 | 75.2 |
| remaining static-binding setup | 2,914 | 33.2 |
| remaining typecheck1 work | 1 | 21.1 |
| finalize definition checks | 3,164 | 0.3 |

Effect inference accounts for 55.0 ms of function-property analysis.
**Reference collection and local-binding sets take 52.8 ms**, about 96%:
252 body-reference walks across three rounds (84 bodies per round).
The rest of the fixed-point rounds take only 2.3 ms. Graph collection,
resolved-site discovery, recursion marking and cleanup account for the
remaining analysis time.

Fusion breaks down as follows (inclusive phase times):

| work | calls | time, ms |
|---|---:|---:|
| return-type processing | 6,134 | 63.6 |
| builtin discovery / early rejection | 2,778 | 10.4 |
| external inputs | 1,710 | 13.2 |
| callees | 1,710 | 2.3 |
| emission pipeline | 1,710 | 206.1 |

Across fusion, ABI freezing itself takes 9.1 ms over 15,862 calls;
normalization retries take 32.2 ms over 2,768 calls, and environment-based
reference expansion takes 30.0 ms over 3,411 calls. Those costs are
already inside the rows above, mostly return types and inputs.

The emission pipeline contains:

| work | calls | time, ms |
|---|---:|---:|
| CLIF construction and body setup | 1,720 | 45.2 |
| Cranelift body compilation | 536 | 108.0 |
| Cranelift wrapper compilation | 526 | 19.3 |
| Cranelift abandoned-body stubs | 1,264 | 17.2 |
| finalization | 526 | 2.0 |

**1,184 of 1,710 JIT builds fail** (69.2%), consuming 56.2 ms including
cleanup. Failed CLIF construction accounts for 29.2 ms of that; stub
compilation accounts for another 17.2 ms. These are subsets of the
emission table, not additional costs. There are more stubs than failures
because an abandoned attempt can leave both parent and callee symbols
undefined. The module still needs definitions for those symbols before
finalization; admission must move earlier to avoid paying this cost.

Registration is separate from app compilation: its instrumented request
median is 108.7 ms. It decodes 96 packed AST/interface blobs (9.6 ms of
aggregate worker time), constructs 339 definition-check bodies (31.2 ms)
and checks them (32.9 ms). Other graph construction takes 15.8 ms,
module checks 8.8 ms, and interface checking 1.5 ms. Registration builds
15 fused regions; the app builds another 526, preserving the cumulative
541 regions and 2,815 attempts of the preceding binary. Source parsing
is below 0.4 ms across registration and the app; JIT initialization is
below 0.2 ms. Neither is a priority for this packed-package workload.

Each table uses independently computed medians, so rounded rows need
not sum exactly. Nested inclusive columns must not be added together.
The log reader verifies exact exclusive-time accounting per individual
root. Runs that overlapped validation builds were excluded.

### CPU samples

Seven release CPU profiles used `cpu-clock:u` at 3,000 Hz, with DWARF
stacks retained and realtime timestamps to match the phase intervals:

```bash
perf record -B -N -e cpu-clock:u -F 3000 -k CLOCK_REALTIME \
    --call-graph dwarf -o startup.perf -- \
    env GRAPHIX_PROFILE=1 /path/to/admin-test \
    milestone_timing --ignored --nocapture > startup.log 2>&1
```

The samples below are flat/self CPU samples, not inclusive stack costs.
They were attributed to the compiler phase intervals. There were no lost
records; nearly all CPU samples came from the test/runtime thread.
Unresolved libc addresses and samples without a known mapping stay
unattributed rather than being assigned to a neighboring symbol. The
local `perf report` mapping issue required reading the sample records
and symbolizing them against ELF load segments and `nm` symbol ranges.

| sampled phase | mean CPU time per run, ms | prominent self costs |
|---|---:|---|
| app typecheck1, fusion on | 176.9 | type clone/drop, type operations, path handling, environment COW, scratch-map management |
| app analysis, fusion on | 56.4 | BindId-set insertion 21.6 ms; reference walks; effect inference |
| app fusion | 296.3 | register allocation, ABI handling, verification, type processing, allocation |
| app typecheck1, fusion off | 174.3 | the same frontend costs |
| app analysis, fusion off | 52.8 | BindId-set insertion 20.8 ms; reference walks |

This confirms that the effect-analysis cost exists independently of the
JIT. `body_facts` rebuilds references and the local-binding set for each
lambda on every fixed-point round. `CallSite::refs` follows its resolved
callee, so a caller's reference collection walks compiled callee bodies
as well. These traversals are repeated while propagating facts that do
not change the binding sets.

A separate concrete issue is `netidx-core/src/path.rs::PATH_ESC`: it is
a `const LazyLock<Escape>`. Accesses materialize fresh locks instead of
sharing an initialized escaper. In app typecheck1, the escaper initializer
alone sampled 4.1 ms and the Escape-specific lazy-lock helper 3.5 ms;
path separator search sampled another 4.9 ms. These exclude generic
`Once`, allocation and cleanup costs that cannot be uniquely attributed
from flat samples. A shared static is the first small fix to measure.
No speedup from changing it has yet been measured.


### Optimization order

1. Make the netidx path escaper a shared static and measure the whole
   workload again. This is a small, directly observed initialization cost.
2. Compute local binding/effect facts once per body, then propagate callee
   facts without rebuilding reference sets on every round. This targets
   almost all of the roughly 55 ms effect-inference phase in both modes.
3. Reject unsupported emission shapes before declaring JIT functions.
   The failed-build timer gives a 56 ms cost to compare against; preserving
   the same fused regions remains the coverage requirement.
4. Distinguish definitive ABI refusal from a type needing normalization
   or resolution, and avoid repeated fallbacks for the former. Actual
   freezing is much cheaper than its recovery paths.
5. Investigate reusable instantiation/typechecking results with keys that
   include higher-order argument identity and the resolved environment.
   The 2,513 body constructions and checks dominate the ordinary compiler.

A kernel-name census also found 553 defined bodies but only 442 distinct
names across registration and app compilation, including two source
regions each compiled 14 times. This is a candidate for further cache
analysis, not proof that their signatures, captures or activation layouts
are interchangeable. Successful backend compilation and wrappers cost
about 127 ms; reuse and region granularity are more promising questions
there than assuming all remaining emission time is Graphix analysis.

Validation: the workspace gate passed 3,168 tests (11 ignored slow tests).
The two existing effect-rejection tests also passed with profiling enabled;
the reader verified 356 roots across 67 threads. All repeated admin runs
passed and retained identical fusion counts. This change adds measurement
probes and reporting; optimization candidates above are not yet applied.

## Startup optimizations after full profiling

The next pass applies the first four candidates above:

- `netidx-core::path::PATH_ESC` is a `static LazyLock`, so path operations
  share one initialized escaper. A `const LazyLock` created a new lazy
  value at each use.
- Effect inference collects each definition's local binding set once,
  before the fixed point. The set is pooled and retained alongside the
  body until analysis finishes. Each round still reads current callee
  facts; only the binding set is reused.
- The fusion driver skips roots whose node representation cannot emit a
  value: bindings, declarations, modules and existing fused kernels. It
  still follows the node's normal fusion traversal, so a binding remains
  live and its initializer can fuse. The same shapes inside an otherwise
  eligible block remain available to statement emission. A root `never`
  is rejected during discovery; an unused `never` statement can still be
  discarded by block liveness.
- ABI freezing distinguishes an unsupported type from one that needs
  normalization or name resolution. Unsupported children of represented
  composites stop the attempt; opaque map/error payloads retain their
  existing treatment. Unions and constructor applications can still need
  normalization, and unresolved named types can still need the environment.
  Normalization continues to operate on a detached copy of the type cells.

The full emission-failure trace contained 1,206 failures across registration
and app compilation. Of these, 831 were attempts at a binding root and 27
at a `never` root. These are actual attempts, rather than the diagnostic
census grouped by source location. The root gate also avoids return-type
preparation for structural roots that previously failed before emission.

For comparison, three release executables are retained: the unchanged
profiled compiler, the path-escaper plus effect-set changes, and the complete
pass. The admin workload source is unchanged across these builds, including
an existing local edit in the admin checkout. Timings alternate the three
executables after warmup, with builds, tests and fuzzing idle. Each process compiles
with fusion enabled and disabled; profiling is measured in separate runs.

### Results

Nine alternating repetitions after warmup, profiling disabled, milliseconds:

| Measurement | Baseline | Escaper + effect sets | Complete pass |
|---|---:|---:|---:|
| App compile request, fusion on | 533.1 | 483.1 | 390.6 |
| App compile request, fusion off | 235.2 | 189.5 | 188.7 |
| Package registration, fusion on | 103.7 | 91.3 | 89.5 |
| Registration + app request, fusion on | 636.4 | 574.5 | 482.8 |
| Fusion attempts, cumulative | 2,815 | 2,815 | 1,802 |
| Fused regions, cumulative | 541 | 541 | 541 |

The complete pass saves 142.5 ms (26.7%) on the app request and 46.4 ms
(19.8%) with fusion disabled. These are comparisons from one measurement
session; the earlier 527 ms result used a different session. Registration
is separate from the app request, and neither measure includes first-frame
rendering. The fused-region input-count histogram is also identical:
234 zero-input, 237 one-input, 56 two-input, 8 three-input and 6 five-input
regions. ExprIds vary across runs because source decoding is concurrent.

Five additional alternating repetitions with profiling enabled give these
app-only medians. Phase times are inclusive and overlap:

| Phase | Baseline ms | Complete pass ms |
|---|---:|---:|
| Typecheck1 | 185.1 | 167.7 |
| Effect inference | 43.3 | 16.7 |
| Binding-set collection | 41.9 | 15.6 |
| Fusion, total | 284.2 | 188.5 |
| Return-type preparation | 59.8 | 11.8 |
| Normalization within fusion | 30.1 | 4.5 |
| Named-type expansion within fusion | 28.0 | 6.4 |
| Failed JIT builds | 52.7 | 18.8 |
| Abandoned-function stubs | 16.2 | 5.6 |
| Backend body compilation | 102.1 | 99.6 |
| Backend wrapper compilation | 18.3 | 17.9 |

Binding-set collection falls from 252 scans to 84. App JIT builds fall from
1,710 to 862, including a reduction from 1,184 failed builds to 336. Root
filtering and selective type retries together account for the return-type
improvement; these measurements do not separate their individual shares.
The first-stage binary leaves fusion time essentially unchanged, while
reducing Typecheck1 and effect inference.

The remaining large costs are ordinary instance construction/typechecking
and successful backend compilation. Failed builds now account for about
19 ms, so they are a smaller target than those two phases.

`bench/profile.py` validates the exclusive accounting of every profiled
root. The escaper change is `netidx-core::path::PATH_ESC`.

### Validation

- `cargo test --locked`: 3,172 passed, 11 ignored.
- The ABI property test compares selective retries with the previous
  unconditional fallback over 512 generated type trees. Separate tests
  preserve shared type cells, normalize constructor unions, and verify
  that a fused initializer still publishes its binding while an unused
  `never` statement is discarded.
- Fuzzer: all 477 corpus programs passed; 500 further mutation, generation
  and reactive cases produced no divergences or crashes.
- `netidx-core` path API doctests: 12 passed.
- Admin suite: 22 passed, two manual tests ignored, on idle runs of both
  binaries. An initial optimized run overlapping workspace validation
  passed 21 tests and failed the landing test: it waits for an empty
  registry but rendered saved, unreachable-domain state. Admin fixtures
  share a process-wide temporary XDG configuration directory, and other
  tests can populate its bookmark file. The landing test also passed in
  isolation with both binaries. The initial failure is retained in
  `admin-tests.log`; the successful rerun is `admin-idle-tests.log`.

## Repeated instance census

`GRAPHIX_PROFILE=1 GRAPHIX_PROFILE_INSTANCES=1` records per-instance
construction and checking costs. Read the admin timing test's output with
`python3 bench/instances.py --admin LOG`; `--json` preserves the full
breakdown. The census is diagnostic only and does not reuse bodies.

The report retains the first completed construction in each group and
sums the other instances' exclusive `InstanceGraph` and `InstanceCheck`
time. Nested profiled phases, including nested instances of the same
phase, are excluded. The reader verifies both call counts and nanoseconds
against the phase totals. Binding setup, argument-pattern construction,
the remainder of typecheck1, later analysis and fusion are outside these
costs. They must not be added speculatively to the measured opportunity.

Three increasingly specific groupings are available:

- The same `LambdaId`, regardless of specialization.
- The same definition and closed signature, compared with `FnType`'s
  equality and hashing after resolving cells into a detached snapshot.
  Open signatures remain separate. Named types are not expanded and
  equivalent alternative spellings are not normalized.
- The preceding group plus the call site's `FnArgIdentity` vector: source
  expression IDs for resolved function arguments. Missing vectors remain
  separate, but a present vector can contain unknown entries. Definition
  gate checks have no call-site vector.

These are workload groupings, not sufficient cache keys. Function type
equality omits lambda provenance; callback source identity does not
identify captured values or the captured environment. Matching rows can
still differ in binding identity, wake roots, exception coverage, state
ownership and activation context. Registration roots are grouped
separately. Distinct definitions created from the same source are also
kept separate.

The `clone_rebind` history is a concrete warning against interpreting
these groups as permission to copy graphs. Commit `77f4bc0d` fixed capture
lookup resolving to an unrelated name in a clone's destination scope;
`c586f3bf` fixed async work waking an analysis top that was never driven.
`6317216d` removed the cloning/template machinery. See also
`design/collection_intrinsics.md` for the state and ownership issues.

Signature snapshots, interning and labels run in `InstanceCensus`, outside
the measured build/check phases. Per-instance rows are printed after the
root timer stops. Instrumented wall times still include diagnostic work
and output, and are not startup improvements. Compare timings with both
profiling variables absent; use separate profile-only runs to assess
measurement disturbance.

### Census results

Five warmed release runs of the unchanged admin workload, with fusion
enabled, give these medians. Time is the sum of exclusive graph construction
and checking on instances after the first in each group:

| App grouping | Repeated instances | Build ms | Check ms | Combined ms |
|---|---:|---:|---:|---:|
| Definition only | 2,131 | 23.8 | 43.7 | 67.5 |
| Definition + closed signature | 1,028 | 10.8 | 16.3 | 27.1 |
| Also match callback source vectors | 1,024 | 4.7 | 9.0 | 13.7 |

The app constructs 2,513 instances of 382 definitions. Only 1,061
signatures are closed at the measurement point; the other 1,452 are
unmerged in the refined rows. Of the closed instances, 1,055 have a
call-site callback vector. Thus the last row is not a bound on all possible
reuse, and it still does not prove safe reuse within its groups. The
definition-only row deliberately conflates different specializations.
All app instance construction/checking totals about 121 ms in census runs.

Registration constructs 339 instances of 335 definitions. Its four
repetitions are `tui::style`, costing only 0.042 ms. Most registration
construction/checking is therefore first-time work under this grouping.
The roughly 90 ms registration cost is not substantially explained by
repeating the same definition inside a compilation root.

The largest repetitions surviving the callback grouping are `tui::line`
(363 repetitions, 2.56 ms), `line_edit::view` (8, 2.47 ms), and
`block::block` (58, 1.61 ms). These are their own measured build/check
costs; nested helper costs are accounted separately. In the broad group,
`netidx_admin::questions` accounts for about 12.0 ms over 49 repetitions,
but its signatures are open at this point. `panels::panels` accounts for
another 12.0 ms: its two instances have equal closed signatures but
different callback vectors for `on_close`. It drops out of the last row.
Source locations are netidx's `src/graphix/mod.gx:12` and
`src/graphix/tui/panels.gx:265` under `graphix-package-netidx-admin`.

The census itself adds 6.1 ms of explicit app metadata work. Compared
with five alternating profile-only runs, graph time is similar
(46.3 versus 46.0 ms), while checking is somewhat higher (74.9 versus
69.9 ms). The numbers above retain this measurement disturbance instead
of presenting an adjusted figure as a measured saving. Output also makes
the external census timing unsuitable as a startup baseline.

Nine alternating runs with profiling disabled give app request medians
of 389.9 ms before instrumentation and 390.3 ms after; registration plus
app is 478.9 versus 480.1 ms. Every run retains 1,802 attempts and 541
fused regions. These measurements reinforce that eliminating the work in
the broad repeated-instance group alone is far short of the roughly
71 ms fast-machine budget implied by the 500 ms / 7 target. Other work
that might disappear with a different compiler design has not been
measured as part of this estimate.

Validation: the whole workspace gate passed 3,172 tests, with 11 ignored.
The existing recursive higher-order callback test also passed with both
profiling flags enabled. The reader reconciled all instance counts and
exclusive times in that test and in all five admin census runs.
