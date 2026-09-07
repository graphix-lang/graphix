# Queued seq blocks

`seqq [trigger] { steps }` uses the ordinary seq state machine behind one
FIFO queue. Queue entries contain the trigger and the body's captured
value inputs. The compiler emits the queue and input projections in a
preamble, rewrites captured reads, and lowers the body through the same
step machinery as seq. There is no separate seqq evaluator or JIT path.

Every trigger samples one tuple, not separate queues of independently
updating captures. One initial release credit starts the first request;
each successful block output returns one credit. An abort resets the
machine and returns one credit after all the run's errors have reached
the sequence's handler and been rethrown. Multiple errors cannot release extra requests;
captures remain pinned throughout their delivery. Neither a standing
output nor an intermediate step releases a request.

The generated catch forwards its inferred error type through a compiler-only
`Rethrow` expression using the existing `?` node. A region that throws nothing
leaves that input bottom; it neither invents a throws type nor raises an
unhandled-error warning.

Captures hold their last non-bottom productions. Before all captures have
produced, the tuple is absent and the sample accumulates request debt.
The first complete tuple can satisfy several pending requests with the
same snapshot. This deliberately inherits ordinary sample behavior;
exact arrival-time snapshots require initialized inputs.

An activation samples standing captures into their latches and grants one
initial queue credit. The completion variable is separate from that
activation, so sleeping and waking cannot suppress the initial credit by
preserving a connect target's old value.

Free reads are identified using the current environment and a scoped
rewrite, respecting nested binds, patterns, and lambda parameters.
Qualified names participate. Trait method dispatchers remain static call
targets. Captured references and function handles retain their ordinary,
shallow semantics.

`until` conditions and address-taking stay live. Directly written external
variables also stay live for reads, preserving read-modify-write state
across requests. Direct write targets are never redirected into queued
snapshots; a dereferenced write uses the queued reference handle.

Sequence completion guards suppress success when the same step raises
an error, so a failed request returns only the abort credit. See
[seq_error_guards.md](seq_error_guards.md).

`seqq` is opt-in. It does not implement seq's still-outstanding general
step-entry sampling or resolve the remaining projected-output F2 case.
Those remain separate compiler issues.
