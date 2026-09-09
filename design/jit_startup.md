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
remaining return-type fallbacks and the split between CLIF construction
and Cranelift compilation are the next useful measurements.
