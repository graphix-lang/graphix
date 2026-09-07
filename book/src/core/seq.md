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
a `with` body's steps are steps like any other.

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

## Errors

A thrown error aborts the current request, resets the sequence, and
rethrows to the enclosing handler. It is not a successful block output.
If a run raises several errors, each is rethrown. The sequence resets
and releases the queue only once, after those deliveries, keeping the
failed request's captured inputs in place. This does not wait for
asynchronous work started by an enclosing handler. Sample that
handler's side-effect arguments on the error, for example
`println(e ~ "failed [request]")`, so later captured-value changes do
not reissue the effect.

`catch` is not allowed anywhere inside a seq body. A catch is an
install: it can observe an error but it cannot produce the value the
next step is waiting for, so inside a sequence it could only rethrow
or stall the run. (A lambda literal is its own scope and may contain
one: inside a function you are back in ordinary Graphix.) Error
handling inside a sequence is control flow:

```graphix
seq req {
  let code = try {
    let child = sys::process::spawn(options(req))?;
    let status = sys::process::wait(child.proc)?;
    status.code
  } with(e) {
    toast <- failed(e);
    -1
  };
  report(code)
}
```

`try { steps } with(e) { steps }` is a seq statement. An error raised
anywhere in the try body, by a `?`, by a function the body calls, or
inside a fused region, transfers control to the with body's first step
with `e` bound to the first error of the failure (a step that raises
several errors in one cycle delivers only the first; the rest are
consumed). The with body's last step continues to the statement after
the `try`. The statement's value is whichever body ran, so
`let x = try { .. } with(e) { .. }`, `x <- try { .. } with(_) { .. }`
and a bare `try` as a statement or as the block's last expression all
work. The with body's value must fit the try body's type; annotate the
let to recover into a wider one, `let x: [i64, null] = try { f()? }
with(_) { null }`.

An error raised in the with body goes to the enclosing `try`, else to
the sequence, which aborts as usual. Cleanup that still fails the run
is therefore `with(e) { cleanup; e? }`, and the error reaches the
handler around the seq exactly once. A let bound in the try body is not
visible after the `try`; `e` is visible only in the with body;
`with(e: T)` ascribes `T` to `e` and requires it to cover everything the
body can throw. `try` is a seq statement: it is refused inside `do`
and outside a seq.

An error reaching the sequence's handler takes precedence over a value
produced by the same step. The failing step cannot publish a result or
start subsequent statements, including subsequent statements within `do`.
This is not rollback: effects already performed inside an ordinary
expression remain performed.

The queue is unbounded. A producer faster than the block can consume will
grow it; a permanently stalled run can retain all subsequent requests.
Sleeping the enclosing expression discards pending queued work, following
the ordinary `queue` sleep behavior.
