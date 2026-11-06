# `seq` and `seqq`

Beyond the syntax and the rules in `SKILL.md`.

The trigger is any expression. `seqq sys::time::after_idle(duration:250.ms,
key) { .. }` runs once per burst of presses, not once per press.
`seq let c = e { .. }` names the trigger's value for the body and nothing
after it; `seq let {x, y} = pt { .. }` destructures. Under `seqq` it is
the value that queued the run, including requests waiting for absent
captures. Each request retains its trigger; waiting requests share the
first complete external snapshot and run in arrival order.
`seq let c = (*r)$ { .. c.f .. }`
is how a nullable reference is consumed; `(*r).f` is refused.

`seq go; abort(cancel) { .. }` ends the run in progress when `cancel`
fires: silently (no value, no error, a `try` is not taken), and the next
trigger starts fresh. The event is the run's first step, asleep between
runs: `abort(sys::time::timer(duration:10.s, false))` is a budget per
run, and a fire while idle aborts nothing. It reads the trigger's name as
this run's trigger (`seq let c = go; abort(sys::time::after_idle(d, c))`).
Nothing is undone: clean up outside on the same event (`busy <- cancel ~
false`), and write a child that must die with the run to an outer
variable, not a `let`. Under `seqq`, `abort` starts the next queued
request and `flush(e)` (after `abort`, `seqq` only) also empties the
queue. Its source stays active between runs and clears the queue even
between short runs; only its abort of a running body is run-gated. A timer
in `flush` therefore keeps running between requests. A seq whose select
arm sleeps mid-run is idle when it wakes.
A sample inside a step clears its banked triggers when the step is born
for a new run; a trigger from an aborted run cannot complete the next run.
A select in that step also starts fresh entry history: its arms are born
on their first selection in each run, even after an asynchronous wait.

A nested `seq` inside `seqq` takes its own entry snapshots of external
variables. It retains the enclosing request's trigger and local bindings.

A busy flag set on a trigger, a call sampled on that trigger, and the
flag cleared on the result is a seq written by hand, three statements
apart:

```graphix
busy <- go ~ true;  let r = f(go ~ x);  busy <- r ~ false      // never this
let r = seqq go { busy <- true; let r = f(x); busy <- false; r }   // this
```

`seqq` when the result must track the latest input (a dropped trigger
would leave `r` aligned with a stale `x`); `seq` when a re-trigger during
a run is noise. Debounce the trigger when a burst should cost one run.

An effect that follows a change takes the change as its trigger. A
state write and a separate "go" pulse are two statements someone must
keep paired, and the effect reads the state later, not the value that
caused it:

```graphix
book <- upsert(book, x); save <- ev       // never this, at every writer
let edit = never();  book <- edit;        // this: writers do `edit <- upsert(book, x)`
seqq let b = edit { write(path, b)? }
```

To gate a seq on a nullable, put it in the trigger, sampled by the
event: `seqq let (dir, b) = edit ~ (d$, edit) { .. }` runs once per
edit, and an edit made while `d` is null is banked and paid when `d`
arrives. The bare tuple `(d$, edit)` also runs at every change of `d`,
re-saving the last edit (a tuple fires when either part does). A `d$`
in a step stalls the run instead, and a stalled `seqq` queues every
later trigger forever.

A statement starts in the first cycle its predecessor's effect can be
seen: `let a = f(); let b = g(a)` issues `g` the cycle `f` produced;
`n <- n + 1; publish(n)` publishes the NEW `n` (a cycle later);
`a <- x; b <- y; let s = a + b` lands both writes in one cycle. The
compiler follows calls into the functions they reach: a call waits for
an earlier write only when its function reads it, and `put(5); let s =
b`, where `let put = |v| { b <- v; v }` writes the `b` it captured,
reads the new `b`. A read
or write through a reference (`*r`), or a call through a function value
the compiler cannot resolve, counts as touching everything and costs a
cycle; a block is the override. `until` and `try` follow the same rule.
A passed step is asleep: a `let` keeps the value its step produced. A
step completes when it produces a NEW value after its entry, never on a
value standing from an earlier run; a step that reads a level takes it
as it stands at entry (under `seq`; `seqq` captured it with the
request) and waits for it if absent. Each later step snapshots the
seq-local `let` values it reads, so `v ~ x` can fire at each step's
entry regardless of whether the planner placed it in a later cycle. A run whose last write targets its own trigger ends
before the write lands, so the write starts the next run.
A step whose value never fires stalls the run: `never()`, a select
whose arms are connects (`select err { `Admin(m) => note <- m }`), a
call to a function whose body ends in a connect (`|v| b <- v`). Put the
connect outside, `note <- select err { `Admin(m) => m }`, or end the
function with a value, `|v| { b <- v; v }`. The two
branches of a `try .. with` must have one type; end both with `null`
when they are only effects. A seq inside a step is a step: with
no trigger it runs at every entry of its statement and the outer step
waits for it (two cycles over inline). `graphix --expand` shows the machine.
`range(i, j)` is the integer sequence builtin (throws `` `RangeError ``).
