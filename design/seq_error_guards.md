# Sequence error boundaries

`seq` and `seqq` require two facts before accepting a step: its value is
present, and no error has escaped to this sequence's handler. An ordinary
reactive expression can raise an error and still produce a value. That
value must not advance a failed sequence.

## Handler ownership

Each installed dynamic handler owns an error-generation counter within
its existing allocation. `ErrorHandler` is a shared handle to that node,
retained by the handler's `?` sites and sequence guards. The counter is
local to this handler, not global to the runtime or thread. A nested
ordinary catch can consume its own error without aborting its enclosing
sequence. A rethrow advances the receiving handler's counter.

Each handler also counts errors pending in descendant handlers. Raising
an error increments that count along the dynamic parent chain; processing
its catch delivery decrements it. A rethrow raises its new delivery before
the original delivery is acknowledged, so the containing sequence never
sees a false gap while errors are moving outward. This works through both
the sequence's generated handler and ordinary catches that may swallow or rethrow.

The interpreter marks a raise before calling `deliver_error`. A compiled
`?` marks it before appending to the kernel's delivery queue. Delivery
does not mark it again. Recursive-frame outboxes and cross-top scheduling
can delay the error value reaching the catch; they cannot delay the guard
observing the raise.

## Completion guards

Lowering wraps each step expression, let initializer, connect RHS, and
`until` condition in a compiler-only `SeqGuard`. Each statement within a
`do` has the same boundary, before its nested continuation.

The guard captures the handler generation when activated. It checks that
generation before evaluating its child and again before returning the
child's value. A change produces bottom and latches failure until sleep;
the failed child stops evaluating unless nested catches still have pending
errors. In that case it continues updating to drain those handlers, but
its output remains suppressed. Sleep and activation replay reset the guard
for a new activation.

A step also waits for its nested catches to process their pending errors
before accepting a value. If they swallow all their errors, the step can
complete; if they rethrow to the sequence, the guard latches failure.

Consequently, a failed step cannot schedule the next PC, write its carried
let values, issue a generated connect, or publish the block result. A
failed seqq request does not return a success credit. Reset and
rethrow remain in the generated handler.

The guard is a fusion boundary; its child still fuses normally. The
continuation cannot execute inside the child's kernel before the error
check. There is no rollback of effects already performed by the child:
ordinary blocks retain their reactive behavior.

## Handler installation and types

Both sequence forms install their own handler unconditionally. Syntactic
inspection cannot establish whether a function call throws. The internal
`Rethrow` forwards the catch variable's inferred type and permits bottom
when the region cannot throw. Its input is already a caught error, so its
type is forwarded without adding another `ErrChain`.
Ordinary `?` checks and warnings are unchanged.

## Regression coverage

`stdlib/graphix-tests/src/lang/seq_errors.rs` exercises ordinary and do
continuations, connect RHSs, until conditions, final outputs, queue credits,
calls-only failures, delayed failures, recursive calls, nested sequences,
handler isolation, `try … with` recovery and cleanup, and restart after failure in both engines (`seq_try.rs` holds the try pins).
Multiple-error cases check ordering, identical errors, payloads, captured
request identity, recursive-frame delivery, nested rethrows, local
swallowing, sleep/restart, and later queued requests.

## Multiple errors from one step

Rethrow runs for every error that reaches the sequence's handler. PC reset and the seqq abort credit
run only after the sequence's handler has received all its raised errors.
For requests 1, 2, and 3, the function below reports both errors, fails
request 1, and allows requests 2 and 3 to complete:

```graphix
let bad = |v| select v {
    1 => { error(`First)?; error(`Second)? },
    _ => v
}
```

Lowering gives the generated catch a separate compiler-only `seq_abort`
expression. The catch counts fresh deliveries, once per runtime cycle;
standing values and activation replay are not deliveries. The first error
marks an abort pending. Once the received count matches the handler's raise
generation and its nested-error count is zero, the catch clears that flag
and activates `seq_abort`. No new run can start before reset, so this
produces exactly one abort.
The counters span the handler's lifetime; sleeping does not turn old
deliveries into a fresh abort.

The nested-error count keeps the outer handler from declaring its run
drained after only the first inner rethrow. Guards keep the nested
handler subtree live while those deliveries remain outstanding. The
outer request and its captures stay pinned until every error has been
offered to the sequence's handler and forwarded. An enclosing catch
remains an ordinary reactive expression: this does not wait for
arbitrary asynchronous work it starts.

The extra accounting uses the existing handler allocation and inline
catch state, not per-error allocations or a second error queue. Ordinary
catches retain their existing behavior.
