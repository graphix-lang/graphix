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
let t0 = sys::time::now(i64:0);   // fires once at startup
let seed = cast<i64>(t0)$;        // separate binding — see note below
let result = <compute using seed>;
let t1 = result ~ sys::time::now(result);
let elapsed = t1 ~ (cast<f64>(t1)$ - cast<f64>(t0)$);
println("... elapsed_s=[elapsed]");
sys::exit(elapsed ~ i64:0)        // exit only after elapsed is computed
```

Notes / gotchas baked into the structure (each cost real debugging):

- **Seed the computation with `cast<i64>(t0)`.** This makes the result
  depend on a runtime value, which (a) forces the `t0 -> compute -> t1`
  dataflow ordering so the timer brackets the work, and (b) defeats any
  constant-folding. The seed is the epoch *second*, constant within a
  run, so re-runs differ only by whole seconds (you'll see the result
  drift by ~1 between runs).
- **Bind the seed to its own `let`.** Inlining `cast<i64>(t0)$` directly
  in a HOF argument position (e.g. as a `fold` accumulator) currently
  blocks fusion of that HOF, so it silently node-walks — a fusion gap,
  not a correctness bug (both modes agree on the value).
- **Trigger `now` with a constant (`i64:0`), not `once(null)`.**
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
bench/selftimed/run.sh [iterations] [graphix-binary]
```

`run.sh` runs each program a few times per mode, keeps the best (min)
time to cut scheduler noise, and prints the node-walk / JIT ratio. A
node-walk cell of `fail` means the program produced no timing line and
`timeout` means it exceeded the per-run limit (neither is expected for
this corpus — all six complete in both modes).

The two scalar tail-loop benches (`tail_sum`, `leibniz_pi`) are the
case where the interpreter does *best* relative to the JIT: it handles
tail self-calls iteratively (no per-element node-graph overhead), so the
ratio there is the floor (~100x). The HOF benches are where the
interpreter's per-element node graph hurts most (~1000-3000x). (Non-tail
recursion — e.g. fib — would overflow the interpreter's native stack;
that's a separate limitation, not exercised here.)

## Results

Release build, best-of-4 per mode, quiet machine (load ~1.7):

| bench            | jit       | node-walk | speedup |
|------------------|-----------|-----------|---------|
| `fold_floatmath` | 2.4 ms    | 6.58 s    | 2714x   |
| `fold_sum`       | 2.4 ms    | 5.02 s    | 2108x   |
| `map_fold`       | 4.8 ms    | 6.38 s    | 1332x   |
| `filter_fold`    | 3.4 ms    | 4.12 s    | 1197x   |
| `leibniz_pi`     | 27.6 ms   | 4.46 s    | 162x    |
| `tail_sum`       | 21.3 ms   | 2.70 s    | 127x    |

The HOF benches show the largest gap (node-walk builds a per-element node
graph); the scalar tail loops show the smallest (node-walk runs them as a
tight iterative loop). Re-run with `run.sh` to reproduce; absolute times
vary with machine load, the ratios less so.

## Benches

| file               | shape                                  | fuses cleanly |
|--------------------|----------------------------------------|---------------|
| `fold_sum`         | int `fold (+)` over 100k               | yes           |
| `fold_floatmath`   | heavy f64 math `fold` over 100k        | yes           |
| `map_fold`         | `map (*3)` then `fold (+)`, 100k       | yes (1 kernel)|
| `filter_fold`      | `filter (even)` then `fold (+)`, 100k  | yes (1 kernel)|
| `tail_sum`         | scalar int tail loop, 10M              | yes           |
| `leibniz_pi`       | scalar f64 tail loop, 10M              | yes           |
