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
GRAPHIX_DBG_PERF=1 cargo test --locked --release -p graphix-package-netidx-admin milestone_timing -- --ignored --nocapture
```

The profile is `opt-level=3`, LTO, one codegen unit, without debug info.
Build artifacts remain in the centrally configured tmpfs. Keep copies of
the before/after test executables, run them directly in alternating order,
and wait for builds and other measurements to finish before comparing
wall times. Run timing comparisons without `GRAPHIX_DBG_PERF`.

`GRAPHIX_DBG_PERF` reports a `FUSION` line after each fusion pass:

| field | measured work |
|---|---|
| `total_ms` | the whole fusion pass, including child recursion |
| `return_ms` | return-type freezing and its normalization/expansion fallbacks |
| `inputs_ms` | collecting and classifying external references |
| `builtins_ms` | builtin/cast discovery and early effect rejection |
| `callees_ms` | callee discovery and kernel signature derivation |
| `emit_ms` | CLIF emission, Cranelift compilation, and finalization |

These are process-wide counters; isolate the workload when profiling.
Callee time includes its own builtin discovery. Emission time also includes
Graphix analysis such as block liveness; it is not a measurement of
Cranelift alone. Signature assembly, diagnostics, feeder construction,
and node replacement account for work outside the individual counters.

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
before resolving and freezing its argument types. The same existing walk
rejects connects, catch installation, and sequence guards. Region input
collection and emission run only after discovery succeeds. Callee
signature construction also performs discovery before constructing its
parameters and captures.

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

Function and reference return types are rejected before normalization or
environment expansion. Neither operation can give those outer type
constructors a kernel ABI representation.

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
root. Final logs and summary data are in
`/tmp/graphix-jit-startup/detail/`; the earlier `detail-under-load/` runs
overlapped validation builds and were excluded from these measurements.

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

Raw profiles, logs, sample extraction code, and the aggregated symbol
counts from this session are under `/tmp/graphix-jit-startup/`:
`full-{0..6}.perf`, `full-sample-{0..6}.log`, `profile_samples.py`, and
`full-samples.json`. They are local profiling artifacts, not required
build inputs.


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
