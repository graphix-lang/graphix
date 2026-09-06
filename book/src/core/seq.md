# Seq and Seqq

`seq trigger { ... }` runs steps in order. A trigger arriving during a run
is dropped. `seqq trigger { ... }` queues requests instead, with one run
active at a time. Without a trigger, either form runs once at initialization.

```graphix
let request = count(sys::time::timer(duration:20.ms, 3)?);
let label = "job";
seqq request {
    let reply = sys::time::after_idle(duration:60.ms, "[label] [request]");
    println(reply);
    reply
}
```

This processes all three requests in order. The block's last step supplies
its output; each new output releases the next queued request, even when the
output equals the previous one. Effectful calls must produce a completion
value: `println`, for example, returns `null` after printing. `never()`
stalls the current run and therefore stops the queue from advancing.

A trailing semicolon in `do { ... }` does not add a step or discard the
last statement's value. That statement supplies the `do`'s completion and,
when it is the sequence's last step, its output. Ordinary `{ ... }` blocks
retain their usual trailing-semicolon behavior.

A later `let x = ...` creates a new binding, even inside `do`. Its
initializer sees the preceding `x`; earlier references and closures
continue to refer to that preceding binding. The new binding can have a
different type.

## Call inputs

In both forms, each call waits until all its explicit arguments are
present, then samples them together with `~!` and issues once. This gives
standing values a fresh call event, including values carried from an
earlier step:

```graphix
seq request {
    let x = request;
    let f = |v| v ~ x;
    f(request)
}
```

If an argument is bottom at entry, the call waits; it cannot reuse a
previous run's snapshot. After issuance, later argument changes or bottom
do not change the snapshot or interrupt the pending result. Nested calls
get their own snapshots. Calls without explicit arguments retain their
ordinary activation behavior.

This clocks the call site, not the function body: callbacks and captured
state inside a function remain reactive. References are sampled as
handles, not copies of their contents. Calls inside `until` conditions and
`catch` handlers retain their ordinary reactive behavior.

## Captured inputs

`seqq` captures the external values read by its body. At each trigger it
samples them together into one tuple and queues that tuple. Later changes
to those inputs do not change a queued request or reissue a waiting step.
Bindings declared inside the block retain their ordinary scope.

A capture uses its last produced value; it need not produce on the same
cycle as the request. If its source becomes bottom, its previous value is
still usable. These snapshots can therefore contain stale data.

Until every capture has produced at least once, there is no complete tuple
to sample. Requests wait for that first tuple, and those early requests can
share the first available snapshot, including its latest trigger value.
Initialize required inputs before issuing requests if each request must
carry its exact arrival-time values.

Capture is shallow: references retain their identity and remain live when
dereferenced. Capturing a function does not snapshot the external state
read inside that function.

A write through a captured reference uses that queued handle, even if the
source reference variable subsequently points somewhere else.

## Live state and waits

`until ready` observes the live condition, rather than a queued copy of it.
Connect destinations remain the original variables. An external variable
written directly by the block also remains live when read, so queued
`count <- count + 1` operations can accumulate rather than overwrite one
another with an old count. Taking `&state` refers to the original state.

Keep persistent effects such as subscriptions and `tui::suspend` outside
the block; steps can update their inputs and wait for their responses.

A thrown error aborts the current request, runs the block's `catch` handler
if present, resets the sequence, and releases the next request. The error
is rethrown to the enclosing handler; it is not a successful block output.
If a run raises several errors, each reaches cleanup and is rethrown. The
sequence resets and releases the queue only once, after those deliveries,
keeping the failed request's captured inputs in place for cleanup. This
does not wait for asynchronous work started by the cleanup handler.
Like an ordinary `catch`, cleanup remains reactive. Sample its side-effect
arguments on the error, for example `println(e ~ "failed [request]")`,
so later captured-value changes do not reissue the effect.

An error reaching the sequence's handler takes precedence over a value
produced by the same step. The failing step cannot publish a result or
start subsequent statements, including subsequent statements within `do`.
This is not rollback: effects already performed inside an ordinary
expression remain performed. An error handled by a nested ordinary
`catch`, without rethrowing to the sequence, does not abort the run.

The queue is unbounded. A producer faster than the block can consume will
grow it; a permanently stalled run can retain all subsequent requests.
Sleeping the enclosing expression discards pending queued work, following
the ordinary `queue` sleep behavior.
