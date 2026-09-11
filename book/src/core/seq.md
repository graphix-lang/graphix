# Seq and Seqq

## Why sequences

Sometimes you just want to run some steps one after another. Graphix
could always express that, but only verbosely: a state variable, a
`select` over it, `~` guards on every input so a step waits for the one
before it, and a `<-` to advance the state. The dataflow was correct and
unreadable.

Within a `seq` you write statements the way you would in an ordinary
language, and the compiler transforms them into that state machine for
you.

```graphix
seq trigger {
    do_step_1(x);
    do_step_2(y);
    do_step_3(z)
}
```

In ordinary Graphix `do_step_1`, `do_step_2` and `do_step_3` would all
run concurrently, each firing whenever its own inputs fire. Inside a
`seq` only one statement is running at a time. Each statement runs until
it produces a value; then it stops and the next one starts. A statement
that has completed does not fire again later in the run, however its
inputs move. The run is started by the trigger; without one, the block
runs once at initialization.

A `seq` is still a Graphix expression. Its value is the value of its last
step, produced once per completed run, so you can bind it, connect it
to a variable, or feed it to another expression.

## `seq` and `seqq`

The two forms differ in what happens when the trigger fires while a run
is in progress:

- `seq trigger { ... }` drops the trigger. Use it for things that should
  not pile up, such as a refresh button.
- `seqq trigger { ... }` queues the trigger, with one run active at a
  time. Use it when every request must be served, such as a stream of
  jobs. The queue is unbounded; see [Captured inputs](#captured-inputs)
  for what a queued request carries.

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

## Statements

A seq body is straight-line: a list of statements, each of which is one
step. There is no `if` or looping inside a seq; branch with `select`
inside a step, or call a function. The statement kinds are:

**An expression.** `f(x);` is a step that completes when the expression
produces a value. The last expression of the block is its output.

**`let x = e;`** is a step like any other, and additionally carries the
value it produced into every later step. A later `let x = ...` creates a
new binding, even inside `do`. Its initializer sees the preceding `x`;
earlier references and closures continue to refer to that preceding
binding. The new binding can have a different type.

**`x <- e;`** connects, as it does anywhere. The destination is the
original variable outside the block, and the step completes when `e`
produces.

**`until cond;`** waits until the boolean `cond` is true, reading it live
rather than as a queued copy. It has no value, so it cannot be the last
statement of a body whose value is used.

**`do { s1; s2; ... }`** groups several statements into one step. At
the seq level every `;` is a cycle boundary: a statement completes, and
the machine moves to the next statement on the following cycle, so
`a <- x; b <- y;` writes `a` one cycle and `b` the next. Inside a `do`
there is no such boundary. Everything that can produce in the step's
entry cycle produces then, so the two writes in
`do { a <- x; b <- y }` land together. Dependencies still hold: a `let`
inside binds before the statements that read it, and a statement that
has to wait for an asynchronous result delays the ones after it. The
`do`'s value is the value of its last statement, so a `do` can end a
seq or initialize a `let`. A trailing semicolon in `do { ... }` does not
add a step or discard the last statement's value. `until` and `try` are
refused inside `do`.

A `do` is a seq construct: its connects are clocked to the step and its
lets are seq lets. To run ordinary reactive Graphix inside a step, use
an ordinary `{ ... }` block as the value of the step,
`let x = { a; b };`. Its contents are live expressions, and the step
completes when the block produces. A bare `{ ... }` as a statement is
refused; ordinary blocks retain their usual trailing-semicolon behavior.

**`try { steps } with(e) { steps }`** is the sequence's error handling.
An error raised in the try body transfers control to the with body; see
[Errors](#errors).

`graphix --expand file.gx` checks the file and prints each sequence's
lowered program: the step variable, one select arm per step, and the
cells that carry `let` values between steps. It is the tool for seeing
exactly which event a step is waiting on.

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
get their own snapshots. A call without arguments is read at entry like a
level.

A step completes when it produces a new value after its entry, never on a
value left standing from an earlier run. A call's answer is what the call
produces after it is issued; a step that reads a level (`let y = x`,
`until flag`) takes the level as it stands at entry, and waits for it if
it is absent. One consequence: a function that returns a level it does not
derive from its argument (`|v| k`) produces nothing new when called
again, so a step calling it never completes after the first run. Sample
the level on the argument instead (`|v| v ~ k`).

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
