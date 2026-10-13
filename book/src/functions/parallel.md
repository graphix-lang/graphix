# Parallel Evaluation

A cycle's work runs on more than one core where it can. When parts of
an expression are independent, the runtime may evaluate them at the
same time on a pool of worker threads, and merge their results back in
program order, so a parallel cycle computes exactly what a serial one
does. The places it can fork are:

- the statements of a block,
- the arguments of a call and the operands of an operator,
- the fields of a struct, tuple or array literal,
- the slots of a collection operation (`array::map`, `array::filter`, …),
- the requests a server handles (`http::serve`, a `sys::net` rpc), and
- the chunks of a fused native loop over a large array.

Two siblings are independent when no later one reads what an earlier
one writes, and when they do not both reach a builtin whose state is
shared across calls (`queuefn`). A `seq` machine runs its steps in
order, though the work inside a step may still fork.

## When it forks

By default (`GRAPHIX_PAR=auto`) the runtime measures what each fork
point costs and forks only where a side costs enough to repay handing
it to another thread. `GRAPHIX_PAR=off` runs everything on one thread;
`GRAPHIX_PAR=force` forks at every fork point (useful for testing).
`GRAPHIX_EVAL_THREADS` sets the number of worker threads.

Two attributes override the measurement for the code under them:

```graphix
let n = 1000;
let fast = #[parallel] array::map(array::init(n, |i| i), |i| i * i);
let slow = #[serial] array::map(array::init(n, |i| i), |i| i + 1)
```

`#[parallel]` forks every fork point under it (a compile error where
nothing can fork; the bodies of the functions it calls are not
included). `#[serial]` keeps everything under it on one thread, the
functions it calls included.

## Effects

Values merge in program order, but an external effect (printing,
writing a file, publishing to netidx) in a forked branch happens when
that branch runs. Effects in two parallel branches are therefore
unordered with respect to each other; use `#[serial]`, or a data
dependency between them, where the order matters.
