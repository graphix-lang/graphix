# Seq and netidx-admin review — 2026-09-04

Reviewed Graphix `9d4e587a^..1a5a88b3` and netidx
`0abfc009^..72342ae8`, including the starting commits. Implementation
unchanged during the review; CR comments mark the outstanding findings.

The direction makes sense: preserving reactive loops and the JIT keeps
useful expressiveness, while seq can centralize the sampling and error
discipline needed for sequential effects. The main implementation gap is
that the lowering does not yet distinguish issuing an effect, waiting for
a fresh asynchronous completion, and reading an already-present value.
Those are different rules in the design's R2/R3, but currently all use a
presence-select. Error reset also needs to govern transitions atomically.
These findings do not require changing ordinary select/sleep semantics.

## Validation

The existing Graphix seq, select, and error tests passed: 343 tests across
the interpreter and JIT configurations. The small programs below were
also run with both configurations against the current debug shell. Their
failures occur with both settings; differential agreement alone does not
establish the intended seq behavior.

The netidx-admin suite also passed: 28 tests, with its two measurement
tests ignored. Cargo confirmed the test binary was current before it
ran. Its fixture uses temporary configuration and local test servers.

Run a snippet saved as a `.gx` file with:

```sh
timeout 2 /home/eric/tmp/target/debug/graphix -n -i repro.gx
timeout 2 /home/eric/tmp/target/debug/graphix -n -i --no-fusion repro.gx
```

The timeout ends the shell after observation; it is not itself evidence of
a bug. Expected and observed productions are given for each case. Timer
measurements are approximate. No actual privileged command was run.

## F1 — P1: value-less effects stall the sequence

Location: [seq.rs:464](../graphix-compiler/src/expr/seq.rs#L464),
`lower_do_stmts`, and the same expression fallback in `step_arm`.
These select the expression's result before evaluating the continuation.

```graphix
let result = seq { do { println("before"); 42 } };
println(result)
```

Expected: prints `before`, then `42`. Observed: only `before`.
The same failure occurs without `do`.

`println` produces no value, so the generated select never takes its arm.
R3 explicitly says same-cycle effects complete when issued. The compiler
needs that distinction; treating every bottom as completion would break
intentional waits such as `never()`.

Disposition, 2026-09-05: the completion-at-issue proposal above was
rejected. Even after receiving arguments, a builtin may need later
invocations to finish; bottom cannot establish completion. R3 now
requires an explicit completion production from an effectful call.
`print`, `println`, and `log` emit a fresh `null` after processing each
message. Their interfaces and the book document this convention.
Regression tests in `stdlib/graphix-tests/src/lang/printing.rs` cover
repeated identical messages, delayed inputs, and ordinary and `do`
seq steps in both engines. All 2,482 Graphix tests and 28 netidx-admin
tests pass after this change (the latter's two measurement tests remain
ignored). The book rebuild passes. The Windows check is blocked by a
missing `x86_64-w64-mingw32-gcc`. The nested-connect case below remains open.

This blocks the netidx port's privileged handoff in
[local.gx:774](/home/eric/proj/netidx/graphix-package-netidx-admin/src/graphix/tui/local.gx:774): the do block starts
with println after setting `suspended` true and waiting for terminal
release. Neither process spawn nor the subsequent resume can execute.
The non-escalation branch of the uninstall outcome also ends in a nested
connect (`_ => refresh <- r`), leaving that sequence busy after its first
such result under the current lowering.

## F2 — P1: an async step accepts the previous run's reply

Location: [seq.rs:235](../graphix-compiler/src/expr/seq.rs#L235),
`step_arm`'s Bind arm, the presence-select around the rewritten
initializer; the same mechanism is used in `lower_do_stmts`.

```graphix
let go = sys::time::timer(duration:150.ms, 2)?;
let done = seq go {
  let t = sys::time::after_idle(duration:60.ms, go);
  t
};
println(done ~ (done == go, sys::time::diff(sys::time::now(done), go)))
```

Expected: two `true` results, each after roughly 60 ms.
Observed: `(true, ~62ms)`, then `(false, ~0.36ms)`.

Async nodes retain their standing output through sleep. On the second
entry, the operation restarts but that standing value immediately passes
the presence-select. The sequence consumes the first run's reply and
leaves the step before its new reply arrives. A timer variant reproduces
the same early completion and cancellation of the new wait.

An issued async operation needs a fresh completion from that invocation;
an ordinary read of a standing value still needs to complete immediately.
This distinction also matters for repeated process spawn/wait and file
operations in the admin port.

Follow-up: the builtin audit in [async_sleep_outputs.md](async_sleep_outputs.md)
resets outputs for restarted operations. This fixes the direct timer,
file-read, network-call, and iterator cases without changing seq lowering.
F2 remains open for surrounding expressions that retain their own output:
projecting a map iterator's tuple inside the awaited expression still
accepts the previous run's projected value. The failing regression is
preserved as `seq_projected_iterator` (explicitly ignored).

## F3 — P1: changing inputs reissue a waiting operation

Location: [seq.rs:213](../graphix-compiler/src/expr/seq.rs#L213),
`step_arm` and `lower_do_stmts`, where `rewrite` leaves call inputs live
rather than applying the entry sampling rule.

```graphix
let tick = count(sys::time::timer(duration:20.ms, 8)?);
let x = tick;
let go = select tick { 1 => true, _ => never() };
let began = sys::time::now(go);
let done = seq go {
  let value = sys::time::after_idle(duration:60.ms, x);
  value
};
println(done ~ (done, sys::time::diff(sys::time::now(done), began)))
```

Expected by R2: sample `x = 1` when the step starts and return it after
roughly 60 ms. Observed: returns `8` after roughly 208 ms. Each update to
`x` re-arms the operation while the same step is active. For operations
with external effects, this means duplicate requests rather than merely
a postponed timer. Sample the operation's inputs once after they become
present; keep explicit level waits such as `until` reactive.

Follow-up: `seqq` queues a tuple of the trigger and captured inputs, held
for the duration of each run. It retains last-produced capture values,
keeps `until` and directly written state live, and leaves ordinary seq's
busy-drop behavior unchanged. See [seqq.md](seqq.md). General step-entry
sampling for ordinary seq remains separate from this opt-in queued form.

## F4 — P1: an error does not suppress an already-queued transition

Location: [seq.rs:192](../graphix-compiler/src/expr/seq.rs#L192), the
`trans` expression in `step_arm` and `lower_do_stmts`, and the catch's
separate write of `Idle`.

```graphix
let step = 0;
step <- select step { s if s < 12 => s + 1, _ => never() };
let n = 0;
let errors = 0;
{
  catch(e) errors <- e ~ errors + 1;
  seq {
    { error(`Oops)?; 0 };
    n <- n + 1
  }
};
println(select step { 12 => (n, errors), _ => never() })
```

Expected by R7: `(0, 1)`. Observed: `(1, 1)`.

The ordinary block used as the first step both throws and produces `0`.
Its continuation queues the next pc value; the catch then queues Idle.
The next step executes before the later reset. This can execute a success
action after failure. The design explicitly identifies this race in
section 7.8, but no transition guard or generation check was implemented.

Follow-up: [sequence completion guards](seq_error_guards.md) now reject
a step's value when its handler receives a raise. This gates ordinary
steps, connects, `until`, and each `do` continuation before success writes
are scheduled. It also prevents a failed final step from publishing a
result or returning an extra seqq queue credit. The interpreter and JIT
mark raises before deferred error delivery; ordinary block behavior is
unchanged. Regression cases live in `lang::seq_errors`.

## F5 — P1: throws through calls bypass reset or rethrow

Location: [seq.rs:109](../graphix-compiler/src/expr/seq.rs#L109),
`may_throw` and `expr_may_throw`.

```graphix
let step = 0;
step <- select step { s if s < 24 => s + 1, _ => never() };
let go = select step { 1 | 12 => step, _ => never() };
let n = 0;
let errors = 0;
let f = |x: i64| -> i64 select x { 1 => error(`Oops)?, _ => x };
{
  catch(e) errors <- e ~ errors + 1;
  seq go { f(go); n <- n + 1 }
};
println(select step { 24 => (n, errors), _ => never() })
```

Expected: `(1, 1)`: the first run aborts, the second succeeds.
Observed: `(0, 1)`: the first error reaches the outer handler, but seq has
no reset handler and remains busy, dropping the second trigger.

The syntax walk only sees Qop nodes inside the statements; it cannot see
`f`'s throws. Adding a user catch to the seq resets it, but produces the
other failure: `may_throw` is still false, so the generated handler omits
the rethrow. With a cleanup counter, the observed tuple
`(successes, outer_errors, cleanups)` is `(1, 0, 1)` rather than `(1, 1, 1)`.
Handler installation/rethrow must account for callee throws, including
dynamic calls, rather than depend on a syntactic Qop search.

Follow-up: installing a handler for every sequence is part of the
completion-guard implementation. Both seq forms now use the typed
compiler-generated rethrow, so calls-only failures reset and rethrow
without giving nonthrowing sequences a phantom throws type. Regression
cases cover calls with and without a user cleanup and successful requests
following an aborted run.

## F6 — P2: a trailing semicolon makes a do block stay busy

Location: [seq.rs:383](../graphix-compiler/src/expr/seq.rs#L383),
`lower_do_stmts`, before splitting the statement list.

```graphix
let go = sys::time::timer(duration:100.ms, 2)?;
let n = 0;
seq go { do { n <- n + 1; } };
println(n)
```

Expected: `0`, `1`, `2`. Observed: `0`, `1`.
Removing the trailing semicolon produces `0`, `1`, `2`.

The parser appends NoOp for the trailing separator. The outer seq filters
NoOp, but do lowering turns it into a presence-select over a value that
never arrives. The pc never resets, so subsequent triggers are dropped.
Discard separator NoOps consistently before lowering steps.

Follow-up: `lower_do_stmts` trims the trailing separator NoOp from its
borrowed slice before identifying the final statement. No runtime change
or allocation is needed. `lang::seq::do_trailing_semicolon` covers values,
lets, connects, subsequent steps, repeated runs, and queued requests with
and without the separator in both engines. Explicit `never()` and ordinary
blocks' trailing-semicolon behavior remain unchanged.

## F7 — P2: shadowed lets share the same carried cell

Location: [seq.rs:300](../graphix-compiler/src/expr/seq.rs#L300),
`collect_step_binds`, `collect_do_bind`, and `fill_this_let`.

```graphix
let result = seq {
  let x = 1;
  let a = &x;
  let x = 2;
  *a
};
println(result)
```

Expected: `1`, as in an ordinary block. Observed: `2`.
Changing the second binding to a string instead causes a type error at
the generated connect, although changing a shadowed variable's type is
legal.

The collection pass stores one cell per spelling for the entire body.
The second `x` overwrites the first entry before either step is lowered;
`fill_this_let` consequently points both declarations at the second cell.
Carry bindings by declaration identity and advance the visible mapping
in source order, preserving earlier bindings captured by references or
closures.

Follow-up: carried cells are keyed by the binding expression's `ExprId`
and pattern name, with one collector for ordinary steps and `do`
statements. Lowering writes each declaration to its own cell and exposes
it to subsequent steps only after rewriting its initializer. The trigger
cell is tracked separately, so shadowing the trigger cannot remove its
initial update. `lang::seq_shadow` covers references, closures,
type-changing shadowing, initializers, destructuring, direct and indirect
writes, and trigger shadowing over repeated and queued runs in both
engines. No runtime change is needed.

The regression work also exposed a stall without shadowing:
`seq request { let x = request; let f = |v| v ~ x; f(request) }`
produces nothing for spaced requests in either engine. A closure using
`v + x` and called with `0` works. The sampled-closure case is retained as
a separate failing regression pending investigation; it is not addressed
by changing carried-binding identity.
