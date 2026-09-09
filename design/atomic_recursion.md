# Atomic recursion

Status: current principle (ruled 2026-08-15)
Pins: `stdlib/graphix-tests/src/lib_tests/interrupt.rs`,
`graphix-shell/tests/interrupt_wedge.rs`

## The rule

**Function evaluation is ATOMIC within a cycle.** A derivation runs to
completion; nothing pauses it partway and resumes it on a later cycle.
Cycles are the reactive layer (`<-`, `~`, event propagation), not a
scheduling quantum for evaluation.

Therefore a program may spin forever inside one cycle, and the engine
does not bound it. An infinite tail recursion is the constant-stack,
bounded-memory case; an infinite non-tail recursion is bounded by
memory (`recursive_activations.md` §4).

## Why

Recursion fires like the hand-inlined chain of distinct functions
(`organic_firing.md`). A hand-inlined chain is just a big expression,
and nobody proposes evaluating half an expression this cycle and the
rest next cycle; once recursion is semantically indistinguishable from
inlining, atomic evaluation is forced. The wedge then follows from
Turing-completeness: the infinite case cannot be detected, so it
cannot be special-cased.

Performance is the bonus, not the justification: a fused kernel
walking a list or grinding a mandelbrot pixel would be crippled by
advancing one step per cycle.

A consequence worth naming: a `<-` target inside a recursive call's
ARGUMENT is not reseeded per call (seed-applies-once,
`findings/arm-local-bind-aug2026`), so `f({let s = 0; s <- 6; s})`
advances `s` once and then tail-calls `f(6)` forever. That program is
well-typed, fully determined, legally non-terminating, and both engines
spin on it; the oracle scores the pair of timeouts as agreement.

## Rejected: one step per cycle

The earlier model evaluated one step of a recursive function per
cycle, which made wedges impossible. It cost two things: recursion was
OBSERVABLE (the inlined twin completed in one cycle while the recursive
one took N — the implementation strategy of calls leaked into program
meaning, the same objection organic firing settled on the firing
plane), and JIT loops were capped at one step per cycle, which is not
a viable execution model for the loops the JIT exists to make fast.

## Rejected: iteration credits

Giving each derivation a budget and settling to bottom when it runs
out is the worst of both worlds (Eric):

- **Unpredictable semantics.** Whether a call finishes in one cycle or
  spreads over many would depend on its input size, with observable
  consequences either way — a program that works on a 100-element list
  behaves differently on a 10,000-element one, for reasons the source
  does not show.
- **A differential hazard.** The credit accounting would have to be
  replicated bit-identically in the node-walk and in emitted CLIF, or
  every long loop becomes a trace divergence; the constant becomes
  load-bearing for correctness rather than scheduling.

The retention ruling ("let the user run out of memory; you can't fix
stupid") already established that the engine does not semantically
bound resources. Infinite time is the same decision as infinite space.

## Containment lives outside the language

The interrupt is the right shape precisely because it is NOT
semantics:

- `GXHandle::interrupt()` sets a flag (`CtlFlag::Interrupt` on the
  runtime's `Control`); in-flight loops abort and the runtime keeps
  running. `abort()` additionally shuts down.
- It is polled by the interp's tail driver (`node/lambda.rs`, once per
  pass) and at every emitted loop head (`emit_interrupt_check`: the
  tail rebind-and-jump head and every HOF scaffold loop).
- No program can observe it, because nothing arms it except a human or
  an embedder. Credits change what CORRECT programs observe; an
  interrupt only changes what BROKEN programs experience, loudly. It is
  the browser's slow-script dialog, not a scheduler.
- Abort is not bottom: the aborted cycle rides its last result and
  re-fires next cycle.

The shell arms Ctrl-C before the first cycle (a top-level infinite loop
wedges inside `load_env`, before the input loop exists) and `abort()`s
on the way out; without the abort the tokio runtime's shutdown waits
forever on the `block_in_place` section the cycle runs in, and only
SIGKILL could end the process. An embedder watchdog is buildable today
(arm `interrupt()` on a wall-clock timer); it is deliberately not built
into the engine, because a default timeout would be a credit system
wearing a hat.

The memory twin is the stack budget (`GRAPHIX_STACK_BUDGET` /
`set_stack_budget`, unlimited by default): a recursion that would
exceed it aborts the runtime through the same `Control`
(`CtlFlag::Budget`), on both engines, and the fuzz harness reads that
abort as a timeout — containment, like the deadline, not a program
outcome.
