# Seq and Seqq

## Why sequences

Sometimes you just want to run some steps one after another. Graphix
could always express that, but only verbosely: a state variable, a
`select` over it, `~` guards on every input so a step waits for the one
before it, and a `<-` to advance the state. The dataflow was correct
but difficult to read.

Within a `seq` you write statements the way you would in an ordinary
language, and the compiler transforms them into that state machine for
you.

```graphix,fragment
seq trigger {
    do_step_1(x);
    do_step_2(y);
    do_step_3(z)
}
```

In ordinary Graphix `do_step_1`, `do_step_2` and `do_step_3` would all run
concurrently, each firing whenever its own inputs fire. Inside a `seq` only
one statement is running at a time. Each statement runs until it produces a
value; then it stops and the next one starts. The run is started by the trigger;
without one, the block runs once at initialization (or once per outer run for a
nested block).

A `seq` is still a Graphix expression. Its value is the value of its last
step, produced once per completed run, so you can bind it, connect it
to a variable, or feed it to another expression.

`seq let name = expr { ... }` names the trigger's value for the body,
and nothing outside the block sees the name. Like `seq name { ... }`
over a variable, the name is the value the run started with, and the
pattern may destructure (`seq let {x, y} = point { ... }`):

```graphix,fragment
seq let session = (*connected)$ {
    known <- upsert(known, session.domain, session.server, session.fingerprint);
    save <- session
}
```

A `seq` can also be a step of another `seq`. Written without a trigger
it runs every time its statement is reached, and the enclosing step
waits for that run to complete:

```graphix,fragment
seq request {
    let cfg = seq { let raw = read_config(request); parse(raw) };
    apply(cfg)
}
```

An error inside the inner `seq` aborts the inner run and, unless a
`try` in the outer body takes it, the outer run as well. A nested
`seq` costs two cycles more than the same statements written inline.

## `seq` and `seqq`

The two forms differ in what happens when the trigger fires while a run
is in progress:

- `seq trigger { ... }` drops the trigger. Use it for things that should
  not pile up, such as a refresh button.
- `seqq trigger { ... }` queues the trigger, with one run active at a
  time. Use it when every request must be served, such as a stream of
  jobs. The queue is unbounded; see [Captured inputs](#captured-inputs)
  for what a queued request carries.

The trigger is any expression. A burst of triggers that should cost one
run, such as a key held down, is debounced at the trigger:
`seqq sys::time::after_idle(duration:250.ms, key) { ... }` runs once,
250ms after the last press.

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
value it produced into every later step. Once its step has passed, `x`
keeps that value: a later change of `e`'s inputs while a later step
waits is not seen. A later `let x = ...` creates a
new binding. Its initializer sees the preceding `x`;
earlier references and closures continue to refer to that preceding
binding. The new binding can have a different type.

**`x <- e;`** connects, as it does anywhere. The destination is the
original variable outside the block, and the step completes when `e`
produces. A later statement that reads `x` sees the write.

**`until cond;`** waits until the boolean `cond` is true. A step sees
the world as it stood when it started (see [What a step
sees](#what-a-step-sees)), so the variable it waits on is written
`live`: `until live(ready);`. An `until` whose condition reads nothing
live and calls nothing could never change after it starts, and the
compiler warns of it. It has no value, so it cannot be the last
statement of a body whose value is used.

**`{ s1; s2; ... }`** runs several statements together as one step.
Every statement in the block is issued when the step starts, so two
asynchronous calls in one block are in flight at once, where at the seq
level the second would wait for the first (see below). A `let` inside a
block is local to it; a later statement in the block that uses it waits
for it, as ordinary dataflow does. Connects and calls in a block are
still clocked to the step: they issue once, when the block is entered,
not again when their inputs move. The block completes when every
statement in it has produced, with the value of its last statement, so
a block can end a seq or initialize a `let`. Inside a block you are
back in ordinary Graphix: a statement that reads a variable a sibling
wrote sees the old value, and an error in one statement does not
retract a write a sibling already issued. `until` and `try` are refused
inside a block.

A declaration (`use`, `type`, `trait`, `impl`, `mod`) is not a step: write
it in a block with the code that uses it, or outside the seq. An
attribute on a statement annotates the statement's computation, as it
would outside a seq (`#[native] let y = x * 2;`); a `try` statement takes
none.

**`try { steps } with(e) { steps }`** is the sequence's error handling.
An error raised in the try body transfers control to the with body; see
[Errors](#errors).

## When the next statement starts

A statement starts in the first cycle where the effect of the one
before it can be seen. For a `let` or a call the effect is its value,
and dataflow carries it at once, so `let a = f(); let b = g(a)` issues
`g` in the cycle `f` produced. For a connect the effect is its write,
which lands the next cycle, so a statement that reads a variable an
earlier statement wrote starts the next cycle and reads the new value:
`n <- n + 1; publish(n)` publishes the new `n`. Connects that nothing
in between reads land together: `a <- x; b <- y; let s = a + b` writes
`a` and `b` in one cycle and binds `s` the next.

The compiler decides this after it has resolved every call, following
each call into the function it reaches. A write a function makes counts
like one the statement makes itself: `put(5); let s = b`, where `let put
= |v| { b <- v; v }` writes the `b` it captured, reads the new `b`. (A
step must produce a value to complete: written `|v| b <- v`, `put`'s
value would be a connect's, which never produces, and the run would
stall.) A call to a function
that does not read `a` does not wait for `a <- ..` to land. Two things
count as touching every variable, because the compiler cannot name what
they touch: a read or write through a reference (`*r`), and a call
whose function is only known at run time (one stored in a struct or an
array). A statement after one of those that reads anything outside the
seq starts a cycle later. `until` and `try` follow the same rule. When
you know two calls are independent, put them in a block: `{ a <- f(x);
b <- g(y) }` issues both at once.

`graphix --expand file.gx` checks the file and prints each sequence's
lowered program: the step variable and one step per statement, then,
for each instance, which steps start in the cycle the one before them
completes (`S0 -> S1 same`) and which wait a cycle (`S1 -> S2 next`).
It is the tool for seeing exactly which event a step is waiting on and
which statements share a cycle.

## What a step sees

A step sees the world as it stood when the step started. Every variable
the seq's own text reads, lambdas written in it included, is read once,
when the step is entered (under `seqq`, when the request is queued: see
[Captured inputs](#captured-inputs)), and a variable that has no value
yet is read when it first gets one. It does not change while the step
runs:

```graphix,ignore
seq { select x > 2 { true => x, false => never() } }
```

stalls if `x` was 2 when the step started, however `x` moves later. A
step waits for the world to change only where it says so, with
`live(e)`, whose variables are read live:

```graphix,ignore
seq { select live(x) > 2 { true => live(x), false => never() } }
```

A call reads live what its arguments read live: `big(live(x))`, or
`big(v)` where `v` is bound by a pattern over `live(x)`, is called again
each time `x` changes, and the step completes on the call's first value
after the step began. A function that answers only in some cases gates
the step on them.

The rule is lexical. A function the step calls runs on its own terms:
its own state and the variables it captures are live, as an asynchronous
builtin's subscription or socket is. So what a run must agree on, it
passes as arguments. A run sees its own effects: a statement is entered
after the writes before it have landed, so it reads them, a function's
writes included. `live` means nothing outside a seq and is refused
there.

## Call inputs

In both forms, each call waits until all its explicit arguments are
present, then samples them together with `~!` and issues once. This gives
standing values a fresh call event, including values carried from an
earlier step:

```graphix,fragment
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

A step snapshots every variable its text names together, including variables
read only by an untaken select arm or an uncalled lambda. It waits until
all of them are present before taking that whole snapshot:

```graphix
let mode: [`A, `B] = `A;
let a_val = 1;
let b_val = never<i64>();
let go = 0;
seq go { select mode { `A => a_val, `B => b_val } }
```

This step waits for `b_val` even though it selects arm `A`. It also delays
reading `a_val`: if `a_val` becomes 5 before `b_val` arrives, the step
returns 5. It does not retain the 1 that was present at entry. Initialize
all named inputs before triggering the sequence when their entry-time
values must be retained.

A `let` defined by an earlier step keeps its completed value. Each later
step that reads it takes a new entry snapshot, so sampling on that value
can fire in each later step. This does not depend on which steps the
compiler can run in the same cycle.

A step completes when it produces a new value after its entry, never on a
value left standing from an earlier run. A call's answer is what the call
produces after it is issued; a step that reads a level (`let y = x`)
takes the level as it stands at entry, or when the whole step snapshot
is ready if any named input is absent.
A call a step makes is a fresh call at every run, as in an ordinary
language: the function's body starts again, reading its arguments and
the variables it captures as they stand. So `let load = ||
sys::fs::read_all(path)` reads the file again at each run, though `path`
never changed, where outside a seq a function that returns a level it
does not derive from its argument (`|v| k`) produces nothing new when
called again. State inside the function starts afresh too, so a
function whose answer starts from a constant completes the step on that
constant: `let counter = |t| { let n = 0; n <- t ~ n + 1; n }` makes
`let a = counter(t)` 0 at every run, its count arriving a cycle later.
What the function captures from outside keeps its value: keep a count
that must last across runs outside the function.

This clocks the call site, not the function body: callbacks and captured
state inside a function remain reactive. References are sampled as
handles, not copies of their contents. Calls inside `until` conditions and
a `with` body's steps are steps like any other.

## Captured inputs

`seqq` captures the external values read by its body. At each trigger it
samples them together into one tuple and queues that tuple. Later changes
to those inputs do not change a queued request or reissue a waiting step.
Bindings declared inside the block retain their ordinary scope. A nested
`seq` takes its own step snapshots when it enters, so it sees external
variables as they stand then, rather than the enclosing `seqq`'s queued
copies. The enclosing request's trigger value and local bindings still
belong to that request.

A capture uses its last produced value; it need not produce on the same
cycle as the request. If its source becomes bottom, its previous value is
still usable. These snapshots can therefore contain stale data.

Until every capture has produced at least once, there is no complete tuple
to sample. Each request keeps its own trigger value while it waits.
Those early requests share the first available snapshot of the external
captures, and run in arrival order. Initialize required inputs before
issuing requests if their external values must be captured at arrival.

Capture is shallow: references retain their identity and remain live when
dereferenced. Capturing a function does not snapshot the external state
read inside that function.

A write through a captured reference uses that queued handle, even if the
source reference variable subsequently points somewhere else.

## Live state and waits

`until` reads the queued copy like any statement, so `until live(ready)`
observes the live condition. The trigger's name is the request this run
is serving: `seqq request { until live(served) == request; .. }` waits on
it, as the same block under `seq` does.
Connect destinations remain the original variables. An external variable
the block writes, directly or through a function it calls, also remains
live when read, so queued `count <- count + 1` operations can accumulate
rather than overwrite one another with an old count. Taking `&state`
refers to the original state. A write through a reference does not make
anything live: the reference's target stays what it was captured as. So a
body that writes through a reference and reads the target by name,
`*r <- a + 1` with `r = &mut a`, reads `a` from each request's snapshot,
and queued requests overwrite one another. Read through the reference
too: `*r <- *r + 1` reads the target live and accumulates.

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

```graphix,fragment
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
body can throw. `try` is a seq statement: it is refused inside a block
and outside a seq.

An error reaching the sequence's handler takes precedence over a value
produced by the same step. The failing step cannot publish a result or
start the statements after it. This is not rollback: effects already
performed inside an ordinary expression remain performed, and the
statements of a block are issued together, so a sibling's write is not
retracted.

## Aborting a run

`abort(event)` in the head, separated from the trigger by `;`, ends the
run in progress when `event` fires:

```graphix,fragment
seq go; abort(cancel) {
  let child = sys::process::spawn(options(cmd))?;
  let status = sys::process::wait(child.proc)?;
  report(status.code)
}
```

The run stops where it is. The block produces no value, no error is
raised, and a `try` around the current step is not taken: an abort is
not a failure to recover from. The sequence is idle and the next
trigger starts a fresh run. The step that was waiting drops its work as
any sleeping expression does (a pending timer is cancelled, a request
in flight is forgotten), so nothing from the aborted run arrives in the
next one.

The event is evaluated as the run's first step. It is asleep between
runs and wakes when a run starts, so a timer in it times the run, and
an event that fired while nothing was running aborts nothing:

```graphix,ignore
// a ten second budget for each run
seq go; abort(sys::time::timer(duration:10.s, false)) { ... }

// the trigger's name is this run's trigger
seqq let job = jobs; abort(sys::time::after_idle(job.budget, job)) { ... }
```

Only fires after the run has started count; a value the event already
holds when the run starts is not a fire.

Nothing is undone. Writes the run already made stay made, and a value
bound by a `let` stays alive until the next run replaces it, a spawned
process included. Undo what needs undoing outside the block, on the
same event: `busy <- cancel ~ false`. A child that must die with the
run is written to a variable outside the block rather than bound with
`let`.

Under `seqq`, `abort` ends the current run and the next queued request
starts. `flush(event)` also empties the queue:

```graphix,ignore
seqq request; abort(skip); flush(cancel_all) { ... }
```

`flush` follows `abort` when both are present, and only `seqq` accepts
it. The flush source stays active between runs, so it empties queued
requests even during the handoff between short runs. A request already
dispatched for the next run is no longer queued. A request that arrives
with the flush or after it is kept. Only the abort of the running
body is gated to fires after that run's entry. Unlike an `abort` source,
a timer written in `flush` keeps running between requests; it is not a
per-run timer.

## Sleeping

A sequence inside a `select` arm, or inside a function called from one,
sleeps with the arm. A run in progress does not survive that: the
sequence is idle when the arm wakes, since a wake resumes the arm and
starts nothing. A `seq` without a trigger in an arm runs only at the
arm's first entry, and the check warns of it: to run at every entry,
trigger it on the event that selects the arm. A `seq` nested in a step
of an outer run that was aborted is idle too; a triggerless one starts
again when a later run enters its step.

A sample (`~`) inside a step starts each run with no banked trigger. An
unpaid trigger from an aborted run cannot complete a later run when the
sampled value arrives. An ordinary arm's sleep and wake preserves its bank;
entering a step for a new run is a birth. A select inside the step also
starts a new entry history: each arm's first selection in that run is
its birth, even if an asynchronous scrutinee arrives after step entry.
Constant writes in such an arm can therefore fire again in every run.

The queue is unbounded. A producer faster than the block can consume will
grow it; a permanently stalled run can retain all subsequent requests.
Sleeping the enclosing expression keeps the queued requests, as `queue`
keeps its contents: they are served after the wake, and the credit of
the run the sleep aborted is returned then.
