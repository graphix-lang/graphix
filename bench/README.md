# Self-timed benchmark corpus

A small corpus comparing the two graphix execution modes on pure
computation:

- **JIT** — fusion on (the default): sync subtrees compile to native
  code via cranelift.
- **node-walk** — `--no-fusion`: the canonical `Box<dyn Update>`
  reactive interpreter.

## Self-timing

Each program brackets *only its computation* with `sys::time::now`, so
parse / typecheck / kernel-compile (all of which happen before the first
reactive cycle) are excluded:

```
let t0 = sys::time::now(0);       // fires once at startup
let seed = cast<i64>(t0)$;        // makes the compute depend on t0
let result = <compute using seed>;
let t1 = sys::time::now(result);  // sampled when result is ready
let elapsed = cast<f64>(t1)$ - cast<f64>(t0)$;
println("... elapsed_s=[elapsed]");
sys::exit(elapsed ~ 0)            // exit only after elapsed is computed
```

`sys::time::now(x)` is already triggered by its argument, so `now(result)`
fires when `result` is ready — no `result ~ now(result)` gate needed. The
only load-bearing `~` is on `sys::exit`: its argument `0` is a constant
that would otherwise fire at startup, so it's gated on `elapsed` to delay
the exit until the computation finishes.

Notes / gotchas baked into the structure (each cost real debugging):

- **Start the clock after the setup, by dataflow.** A program that
  builds its input first writes `let t0 = sys::time::now(src)`, so the
  build is ordered before `t0` by what `now` reads, not by statement
  order: a forked first cycle runs independent siblings side by side,
  and `now(0)` would start the clock beside the build.

- **Seed the computation with `cast<i64>(t0)`.** This makes the result
  depend on a runtime value, which (a) forces the `t0 -> compute -> t1`
  dataflow ordering so the timer brackets the work, and (b) defeats any
  constant-folding. The seed is the epoch *second*, constant within a
  run, so re-runs differ only by whole seconds (you'll see the result
  drift by ~1 between runs).

## Running

Compile startup has a separate comparison harness:

```bash
python3 bench/startup.py /path/to/baseline/graphix /path/to/candidate/graphix
```

Use binaries built with the same profile. The harness alternates the two
binaries, reports medians, and times `--expand --no-cache`: the whole
compile (parse, check, elaboration, fusion), which refuses a generated
`#[native]` call that does not fuse. For the admin application and phase profiling,
see [`design/jit_startup.md`](../design/jit_startup.md).

```
cargo build --profile quick -p graphix-shell
bench/run.sh [iterations] [graphix-binary] [corpus-dir]
```

The binary defaults to the quick build in cargo's target directory and
the corpus to `bench/` (`bench/collection/run.sh` runs this script over
its own directory); the `par_*` programs are `par.sh`'s and are skipped.
`run.sh` runs each program a few times per mode, keeps the best (min)
time to cut scheduler noise, and prints the node-walk / JIT ratio. A
node-walk cell of `fail` means the program produced no timing line and
`timeout` means it exceeded the per-run limit.

Every bench but two fuses its whole computation and says so with
`#[native]`, which a build refuses where anything node-walks (the
examples test builds every bench): the folds and maps over arrays and
lists, `composite_seams`, the tail loops, `mandelbrot` and `symbolic`.
The other two probe the edge of fusion coverage: `stream_stats` and
`netidx_stream` have an async event spine that can never fuse (only the
per-event sync compute does).

The two scalar tail-loop benches (`tail_sum`, `leibniz_pi`) run 100k
iterations: the node-walk keeps every activation of a tail loop
(`design/tail_calls_are_calls.md`), about 16 KB each, so at 10M they
outgrew the machine. The JIT runs them as one native loop.

## Results

Quick build, `bench` CPU mode, best of 3 per mode (2026-10-21). `run.sh`
runs under the default fork mode, `auto`; the last column is the JIT
with `GRAPHIX_PAR=off`:

| bench             | jit      | node-walk | speedup | jit, serial |
|-------------------|----------|-----------|---------|-------------|
| `tail_sum`        | 0.16 ms  | 2.47 s    | 15368x  | 0.16 ms     |
| `mandelbrot`      | 0.52 ms  | 5.73 s    | 11025x  | 0.80 ms     |
| `leibniz_pi`      | 0.60 ms  | 3.56 s    | 5931x   | 0.49 ms     |
| `fold_floatmath`  | 2.20 ms  | 3.27 s    | 1487x   | 2.20 ms     |
| `filter_fold`     | 3.17 ms  | 1.98 s    | 626x    | 3.32 ms     |
| `fold_sum`        | 2.19 ms  | 1.29 s    | 592x    | 2.74 ms     |
| `symbolic`        | 16.8 ms  | 8.26 s    | 491x    | 9.79 ms     |
| `map_fold`        | 4.29 ms  | 2.05 s    | 479x    | 3.98 ms     |
| `composite_seams` | 15.8 ms  | 4.38 s    | 278x    | 21.0 ms     |
| `list_map_fold`   | 35.1 ms  | 2.07 s    | 59x     | 35.8 ms     |
| `list_fold_sum`   | 30.7 ms  | 1.33 s    | 43x     | 29.1 ms     |
| `stream_stats`    | 53.5 ms  | 1.06 s    | 20x     | 53.9 ms     |
| `netidx_stream`   | 198 ms   | 0.92 s    | 4.6x    | 187 ms      |

Under `auto` a loop forks only once the fork threshold is calibrated,
on a thread the runtime starts with it; a loop before that runs in
order, and nothing waits for it. The list rows pay the flatten boundary
(the list's cells to an array and back) around each native loop.
`stream_stats` shows what fusing only the per-event compute buys on a
stream; `netidx_stream` shows the same compute behind a real network
round trip; `symbolic` shows a recursive-ADT workload.
Absolute times vary with machine load, the ratios less so.

## Benches

| file               | shape                                            | fuses cleanly |
|--------------------|--------------------------------------------------|---------------|
| `fold_sum`         | int `fold (+)` over 100k                         | yes           |
| `fold_floatmath`   | heavy f64 math `fold` over 100k                  | yes           |
| `map_fold`         | `map (*3)` then `fold (+)`, 100k                 | yes (1 kernel)|
| `filter_fold`      | `filter (even)` then `fold (+)`, 100k            | yes (1 kernel)|
| `list_fold_sum`    | `fold (+)` over a 100k list                      | yes           |
| `list_map_fold`    | `map (*3)` then `fold (+)` over a 100k list      | yes (1 kernel)|
| `composite_seams`  | tuple elements through a map, a cross-kernel call per element | yes |
| `tail_sum`         | scalar int tail loop, 100k                       | yes           |
| `leibniz_pi`       | scalar f64 tail loop, 100k                       | yes           |
| `mandelbrot`       | per-pixel escape-time, 100x75                    | yes           |
| `stream_stats`     | per-tick sliding-window stats, 5000 events       | per-event compute only (async spine node-walks) |
| `netidx_stream`    | same stats, each tick a real netidx round trip   | per-event compute only (round trip dominates) |
| `symbolic`         | build/deriv/simplify/eval over a recursive ADT   | yes           |

`mandelbrot`'s per-pixel `iterate` is a recursive function called inside
`array::init`'s callback (a nested recursive lambda); the whole hot loop
fuses.

`stream_stats` delivers n simulated market ticks one per reactive cycle
via `array::iter`; each tick recomputes sum/mean/variance/min over a
64-element sliding window plus an EMA. The async boundaries (`iter`,
`~`, connect scheduling) can never fuse.

`netidx_stream` is the same per-tick pipeline, but every tick makes a
real netidx round trip (publish → subscribe, self-clocked so coalescing
can't drop ticks).

`symbolic` builds, differentiates, simplifies, and evaluates expression
trees over a recursive ADT (`` `Num/`Var/`Add/`Mul ``). A recursive
ADT is an opaque two-word value in a kernel, so all four functions
fuse, `simplify`'s tuple-of-variant patterns (`` (`Num(x), `Num(y)) ``)
included.

## Parallel evaluation

`bench/par.sh [iterations] [graphix]` runs the `par_*.gx` benches
serially and under `GRAPHIX_PAR=auto`, pinned with taskset (serial and 4
threads on the P-cores, 12 on the P- and E-cores; never the low-power
cores), at normal priority. `par_symbolic` is a fused map over 32 heavy
slots, each folding 64 of `symbolic`'s trees, which the cost model
decides to split unforced; `par_wide` is a node-walked map whose slots
the cost model learns to fork, timed over its 60 cycles after the first;
`par_growth` builds 20000 slots in one cycle, nearly all instance
construction (both run `--no-fusion`: fused, each takes milliseconds);
`par_mandel` is a fused `array::init` over 480000 pixels whose kernel
loop forks as chunks, timed over the 10 cycles after the first. `par_chain`
is a node-walked chain of 1000 distinct functions timed through its
first cycle, where nothing pays for a fork: Auto must cost what serial
does. Quick
build, `bench` mode, best of three (2026-10-21):

| bench          | serial | auto P x4 | auto P+E x12 |
|----------------|--------|-----------|--------------|
| `par_growth`   | 2.02 s | 1.08 s    | 1.03 s       |
| `par_mandel`   | 1.30 s | 0.42 s    | 0.24 s       |
| `par_symbolic` | 0.69 s | 0.50 s    | 0.47 s       |
| `par_wide`     | 7.65 s | 3.03 s    | 2.22 s       |

Of `par_mandel`'s cycle, the fold over the grid, building its array and
dropping the last one stay serial (8 ms of the 4-thread cycle's 45); the
pixels run at about 92% of four cores.

`par_symbolic` under `auto` runs half its slots in order: the loop
probes 16 of the 32 slots as one timed run before it forks the rest, so
forced (`#[parallel]`) its four-thread run takes 0.29 s.

Hybrid cores make these noisy: which parts land on E-cores is the OS's
choice (`design/parallel_eval.md` §5, measured not fixed), and
`par_symbolic` seeds itself from the clock, so each run is a different
workload. Compare builds interleaved, several runs each.

For compiler startup profiling, run a workload with `GRAPHIX_PROFILE=1`,
capture stderr, then use `python3 bench/profile.py profile.log` (or
`--json`). The reader reports call counts, exclusive and inclusive phase
times, and failed-emission costs, and verifies the exclusive totals.
See [the startup measurements](../design/jit_startup.md#full-compiler-profiling)
for the admin workload and measurement boundaries.
