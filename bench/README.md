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

- **Seed the computation with `cast<i64>(t0)`.** This makes the result
  depend on a runtime value, which (a) forces the `t0 -> compute -> t1`
  dataflow ordering so the timer brackets the work, and (b) defeats any
  constant-folding. The seed is the epoch *second*, constant within a
  run, so re-runs differ only by whole seconds (you'll see the result
  drift by ~1 between runs).
- **Trigger `now` with a constant (`0`), not `once(null)`.**
  `once(null)` does not fire `now`'s trigger, so the whole timing chain
  goes dead and the program hangs.
- **Gate `sys::exit` on `elapsed`, not on `println`'s return.**
  `println`'s return value does not reliably propagate as an event, so
  `exit(printed ~ 0)` can hang; `exit(elapsed ~ 0)` is robust.

Sub-second `cast<f64>(datetime)` precision depends on the fix to the
`datetime -> f64` cast in `netidx-value` (it previously returned
`2 * whole_seconds` with the sub-second part discarded).

## Running

```
cargo build --release -p graphix-shell
bench/run.sh [iterations] [graphix-binary]
```

`run.sh` runs each program a few times per mode, keeps the best (min)
time to cut scheduler noise, and prints the node-walk / JIT ratio. A
node-walk cell of `fail` means the program produced no timing line and
`timeout` means it exceeded the per-run limit.

The corpus is no longer all-fused: the seven pure-compute benches
(`fold_*`, `map_fold`, `filter_fold`, `tail_sum`, `leibniz_pi`,
`mandelbrot`) fuse fully, so their ratios reflect native code vs the
interpreter. The three newer benches deliberately probe workload
classes at the edge of fusion coverage — `stream_stats` and
`netidx_stream` have an async event spine that can never fuse (only the
per-event sync compute does), and `symbolic` works over a recursive ADT
that has no fixed kernel ABI, so its hot path node-walks even with
fusion on. Their ratios measure fusion *coverage* as much as codegen
quality, which is the point.

The two scalar tail-loop benches (`tail_sum`, `leibniz_pi`) are the
case where the interpreter does *best* relative to the JIT among the
fully-fused programs: it handles tail self-calls iteratively (no
per-element node-graph overhead), so the ratio there is the floor
(~50x). The HOF benches are where the interpreter's per-element node
graph hurts most (~1400-2600x). (Non-tail recursion — e.g. fib — would
overflow the interpreter's native stack; that's a separate limitation,
not exercised here.)

## Results

Release build, best-of-3 per mode (2026-07-02):

| bench            | jit       | node-walk | speedup |
|------------------|-----------|-----------|---------|
| `fold_floatmath` | 2.2 ms    | 5.80 s    | 2597x   |
| `fold_sum`       | 2.2 ms    | 4.71 s    | 2129x   |
| `map_fold`       | 4.6 ms    | 7.10 s    | 1532x   |
| `filter_fold`    | 3.8 ms    | 5.20 s    | 1383x   |
| `mandelbrot`     | 80.0 ms   | 7.09 s    | 89x     |
| `tail_sum`       | 35.8 ms   | 1.93 s    | 54x     |
| `leibniz_pi`     | 74.3 ms   | 3.64 s    | 49x     |
| `stream_stats`   | 0.65 s    | 20.8 s    | 32x     |
| `netidx_stream`  | 0.68 s    | 0.69 s    | 1.0x    |
| `symbolic`       | 18.5 s    | 12.2 s    | 0.66x   |

(`run.sh` rounds the ratio to a whole number, so it prints `symbolic`
as `1x`; the honest ratio is 0.66x — with fusion *on* it runs ~1.5x
slower than the node-walk. See the `symbolic` notes below.)

The HOF benches show the largest gap (node-walk builds a per-element node
graph); the fully-fused scalar loops (`mandelbrot`, `tail_sum`,
`leibniz_pi`) show the smallest (node-walk runs tail self-calls as a
tight iterative loop). `stream_stats` shows what fusing only the
per-event compute buys on a stream (32x end-to-end); `netidx_stream`
shows the same compute hidden behind a real network round trip (~1x);
`symbolic` shows the recursive-ADT class the JIT cannot touch. Re-run
with `run.sh` to reproduce; absolute times vary with machine load, the
ratios less so.

## Benches

| file               | shape                                            | fuses cleanly |
|--------------------|--------------------------------------------------|---------------|
| `fold_sum`         | int `fold (+)` over 100k                         | yes           |
| `fold_floatmath`   | heavy f64 math `fold` over 100k                  | yes           |
| `map_fold`         | `map (*3)` then `fold (+)`, 100k                 | yes (1 kernel)|
| `filter_fold`      | `filter (even)` then `fold (+)`, 100k            | yes (1 kernel)|
| `tail_sum`         | scalar int tail loop, 10M                        | yes           |
| `leibniz_pi`       | scalar f64 tail loop, 10M                        | yes           |
| `mandelbrot`       | per-pixel escape-time, 100x75                    | yes           |
| `stream_stats`     | per-tick sliding-window stats, 5000 events       | per-event compute only (async spine node-walks) |
| `netidx_stream`    | same stats, each tick a real netidx round trip   | per-event compute only (round trip dominates) |
| `symbolic`         | build/deriv/simplify/eval over a recursive ADT   | no (recursive ADT — no kernel ABI) |

`mandelbrot`'s per-pixel `iterate` is a recursive function called inside
`array::init`'s callback (a nested recursive lambda). That case (#203)
now fully fuses, so the whole hot loop JITs to native code like the flat
fold/map benches.

`stream_stats` delivers n simulated market ticks one per reactive cycle
via `array::iter`; each tick recomputes sum/mean/variance/min over a
64-element sliding window plus an EMA. The async boundaries (`iter`,
`~`, connect scheduling) can never fuse — the 32x speedup is what
fusing just the per-event window folds buys end-to-end.

`netidx_stream` is the same per-tick pipeline, but every tick makes a
real netidx round trip (publish → subscribe, self-clocked so coalescing
can't drop ticks). The round trip dominates per-event cost, so the two
modes tie (~1x): fusion doesn't help a workload whose bottleneck is the
network, and the bench documents that honestly.

`symbolic` builds, differentiates, simplifies, and evaluates expression
trees over a recursive ADT (`` `Num/`Var/`Add/`Mul ``). Recursive ADTs
have no fixed kernel ABI, so the hot path node-walks even with fusion
on — with fusion enabled the program runs a little *slower* than plain
node-walk (partial fusion of tiny scalar fragments inside a
node-walked recursive region costs more than it saves). The bench also
found a real fusion value divergence on its first run (the checksum
came out exactly 2x under fusion — a name-keyed clone_rebind bug,
fixed 2026-07-02; see graphix-fuzz/findings/audit-jul2026/03), which
is exactly the kind of finding it exists to surface.
