# Atomic Recursion — giving up the no-wedge property

Status: RULED by Eric 2026-08-15. The engines already behaved this way;
this doc records the DECISION (it was previously an undocumented
consequence of JIT loops, and CLAUDE.md still called the JIT half an
"accepted artifact"). Built the same day: the containment path the
ruling depends on was armed in the shell (Ctrl-C → `interrupt()`, and
`abort()` on the way out) — before that, a wedged program made the
process unkillable by Ctrl-C.

Occasioned by `fuzz/pending-triage/connect_in_call_arg_nontermination`
(aug14f), where a `<-` inside a recursive call's argument makes the
program legally non-terminating, and both engines spin inside one
cycle.

## The rule

**Function evaluation is ATOMIC within a cycle.** A derivation runs to
completion; nothing pauses it partway and resumes it on a later cycle.
Cycles are the REACTIVE layer (`<-`, `~`, event propagation), not a
scheduling quantum for evaluation.

**Therefore a program may spin forever inside one cycle**, and the
engine does not bound it. The constant-stack, bounded-memory case is
the infinite tail recursion; non-tail recursion hits the call-depth
limit and settles on the whole-derivation bottom instead (that limit is
about STACK, not time, and is unaffected by this ruling).

## Why — it follows from the recursion ruling, not from performance

The headline of the recursion ruling (2026-08-13) is that **recursion
fires like the hand-inlined chain of distinct functions**. A hand-inlined
chain is just a big expression. Nobody proposes evaluating half an
expression this cycle and the rest next cycle, so once recursion is
semantically indistinguishable from inlining, atomic evaluation is
forced. The wedge then follows from Turing-completeness: the infinite
case can't be detected, so it can't be special-cased.

Performance is the bonus, not the justification: a fused kernel walking
a list or grinding a mandelbrot pixel would be crippled by advancing one
step per cycle.

## What was given up

The old model evaluated one step of a recursive function per cycle. That
made **wedges impossible** — a nice property, and a uniform rule rather
than an accident. It cost two things:

1. **Recursion was observable.** The inlined twin completed in one
   cycle while the recursive one took N. That leaks the implementation
   strategy of calls into program meaning — the same objection organic
   firing settled on the firing plane.
2. **JIT loops were capped at one step per cycle**, which is not a
   viable execution model for the loops the JIT exists to make fast.

The change was already made when kernels landed; it was user-observable
beyond the wedge case, and undocumented until now.

## Why not iteration credits

The obvious salvage — give each derivation a budget, settle to bottom
when it runs out — is **the worst of both worlds** (Eric):

- **Unpredictable semantics.** Whether a recursive call finishes in one
  cycle or spreads over many would depend on its INPUT SIZE, with
  observable consequences either way. A program that works on a
  100-element list behaves differently on a 10,000-element one, for
  reasons the source doesn't show.
- **A differential hazard.** Any credit accounting must be replicated
  bit-identically in the node-walk and in emitted CLIF, or every long
  loop becomes a trace divergence. The fuzzer would spend its life
  relitigating pacing, and the constant would be load-bearing for
  correctness rather than for scheduling.

There is also precedent for accepting the cost: the retention ruling
("let the user run out of memory; you can't fix stupid", 2026-08-13)
already established that the engine does not semantically bound
resources. Infinite time is the same decision as infinite space.

## What replaces it: containment, outside the language

The interrupt is the right shape precisely because it is NOT semantics:

- `GXHandle::interrupt()` sets a flag; in-flight loops abort to bottom
  and the runtime keeps running. `abort()` additionally shuts down.
- It is polled by the interp's tail driver (`node/lambda.rs`, once per
  pass) and at every emitted loop head (`emit_interrupt_check` — the
  tail rebind-and-jump head in `emit/lower.rs`, plus all eight HOF
  scaffolds in `emit/scaffold.rs`).
- **No program can observe it**, because nothing arms it except a human
  or an embedder — which is exactly what distinguishes it from credits.
  Credits change what CORRECT programs observe; an interrupt only
  changes what BROKEN programs experience, loudly. It is the browser's
  slow-script dialog, not a scheduler.
- Abort ≠ bottom: the aborted cycle rides its last result and re-fires
  next cycle.

Three consequences, all in place as of 2026-08-15:

1. **The shell arms Ctrl-C.** The signal task is armed BEFORE the first
   cycle (a top-level infinite loop wedges inside `load_env`, long
   before the input loop exists), and the run loop `abort()`s on the way
   out. Without that, the tokio runtime's shutdown waits forever on the
   `block_in_place` section `do_cycle` runs in — Ctrl-C could not exit
   the process, only SIGKILL could.
2. **An embedder watchdog is buildable today** — arm `interrupt()` on a
   wall-clock timer, log loudly. Deliberately not built into the engine:
   a default timeout would be a credit system wearing a hat.
3. **Every native loop head polls.** Audited 2026-08-15: the tail loop
   and all eight scaffolds do.

## Pins

- `stdlib/graphix-tests/src/lib_tests/interrupt.rs` — a wedged tail loop
  recovers on `interrupt()` in BOTH engines, and `abort()` unblocks
  pending commands.
- `graphix-shell/tests/interrupt_wedge.rs` — the real binary on a
  first-cycle wedge and a later-cycle wedge, both engines: SIGINT still
  frees the process.

## The worked example (RULED 2026-08-15 — nothing open)

The occasioning witness, `connect_in_call_arg_nontermination` (aug14f
katana fuzz):

```graphix
{let x = array::iter([i64:1, i64:2, i64:3, i64:4]);
 let m = x / i64:3;
 let rec f = |n: i64| -> i64 select n {
   i64:0 => i64:0,
   _ => f({let s = i64:0; s <- array::fold([i64:1, i64:2, i64:3], i64:0, |a, e| a + e); s})
 };
 f(m)}
```

Eric ruled the second question the same day: **a `<-` target inside a
call ARGUMENT is not reseeded per call** — seed-applies-once
(`findings/arm-local-bind-aug2026`) holds there like everywhere else.
So `s` advances 0 → 6 once, `f(6)` tail-calls itself forever, and the
program is LEGALLY NON-TERMINATING. Both engines spin; both Timeout;
the oracle scores that an agreement. There is nothing to fix, and the
witness is not in the regress corpus — a never-terminating program
would burn the per-program budget (and, since 78b9003e, its sequential
retry) on every gate run. It lives here instead.

This is the ruling's shape in miniature: the program is well-typed, its
semantics are fully determined, it never produces, and getting your
terminal back is the interrupt's job rather than the language's.
