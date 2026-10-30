# Atomic recursion

Status: current principle (ruled 2026-08-15; the interrupt final,
2026-10-29)
Pins: `stdlib/graphix-tests/src/lib_tests/interrupt.rs`,
`graphix-shell/tests/interrupt_wedge.rs`,
`graphix-shell/tests/repl_interrupt.rs`

## The rule

**Function evaluation is ATOMIC within a cycle.** A derivation runs to
completion; nothing pauses it partway and resumes it on a later cycle.
Cycles are the reactive layer (`<-`, `~`, event propagation), not a
scheduling quantum for evaluation.

Therefore a program may spin forever inside one cycle, and the engine
does not bound it. A tail call is an activation like any other call
(`tail_calls_are_calls.md`), so an infinite recursion, tail or not,
grows stack and memory with its depth; only a stateless tail recursion
the JIT runs as one native loop is constant-stack and bounded-memory.
What contains the rest is the activation budget, the stack budget
(`GRAPHIX_STACK_BUDGET`) and the interrupt (`recursive_activations.md`
§4).

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

- `GXHandle::interrupt()` stops the runtime for good, busy or idle
  (`Control::stop`: `CtlFlag::Interrupt` beside `Abort` on the runtime's
  `Control`): in-flight loops abort, the run loop ends and every call on
  its handle fails, naming the interrupt. `abort()` is the same stop
  with no cause, the embedder's own shutdown.
- The node-walk polls it at each lambda dispatch (`node/lambda.rs`: an
  interrupted dispatch rides its last result, it is not a bottom) and
  before each collection slot (`node/collection.rs`); a kernel polls it
  every 64th iteration of each emitted loop (`emit_interrupt_check_every`:
  the tail rebind-and-jump head and every HOF scaffold loop) and takes
  its abort path.
- No program can observe it, because nothing arms it except a human or
  an embedder. Credits change what CORRECT programs observe; an
  interrupt only changes what BROKEN programs experience, loudly. It is
  the browser's slow-script dialog, not a scheduler.
- Nothing resumes: stopping a calculation at an arbitrary point and
  resuming it later would need every builtin, timer and reply a cut
  cycle left half-delivered to be made whole again, so the stopped
  runtime is dropped and a new one minted.

The shell arms its keys before the first cycle (a top-level infinite
loop wedges inside `load_env`, before the input loop exists). The
interrupt is `Ctrl-\` (SIGQUIT; Ctrl-Break on Windows): a script ends at it
or at Ctrl-C, after the bounded teardown, with status 130; the REPL's
interrupt, idle or busy, replaces the runtime with a fresh empty one and
says so, an interrupt during that start starting again. The REPL's Ctrl-C
stops nothing: it leaves the value display (or the line) and keeps the
session, and while an input waits on a busy runtime it names the
interrupt. The shell reads no key while a display shows or an input runs:
the terminal sends both keys as signals then, and at its prompt the line
editor, as a TUI display does, reads them as keys. Ctrl-D exits. The shell `abort()`s on the way out;
without the stop the tokio runtime's shutdown waits forever on the
`block_in_place` section the cycle runs in, and only SIGKILL could end
the process. An embedder watchdog is buildable today (arm `interrupt()`
on a wall-clock timer, then start a new runtime); it is deliberately not
built into the engine, because a default timeout would be a credit
system wearing a hat.

The memory twin is the stack budget (`GRAPHIX_STACK_BUDGET` /
`set_stack_budget`, unlimited by default): a recursion that would
exceed it stops the runtime through the same `Control`
(`Control::stop`: `CtlFlag::StackBudget` beside `Abort`, read back by
`Control::stopped_by`), on both engines, and the fuzz harness reads that
stop as a timeout — containment, like the deadline, not a program
outcome.

The activation budget counts what a runaway recursion actually grows:
live activations, each a call's instance, a collection slot's or a
kernel's per-activation block, charged where a cycle binds one and
released at its delete, against the runtime's `Control`
(`Control::set_activation_budget`, `set_activation_budget`,
`GRAPHIX_ACTIVATION_BUDGET`; unlimited by default, so an embedder
chooses). A node-walked activation holds about 20 KB of heap, so memory
runs out long before stack, and a count follows the program where stack
bytes follow the thread and the fork schedule; past the budget the
runtime stops the same way (`CtlFlag::ActivationBudget` beside
`Abort`). The fuzzer's children
run at 50,000, below where their 1 GiB stack budget would trip. A JIT
tail loop binds nothing and is not counted; the interrupt stops it.
