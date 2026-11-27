# On Blocks

## Why a clock

An event handler usually means "when this happens, do these things with
the state as it is now". Written with `~`, every statement has to say so
for itself:

```graphix,fragment
let keys = |k| select k.code {
    `Left => { sel <- k ~ max(0, sel - 1); menu_sel <- k ~ 0; `Stop },
    `Right => { sel <- k ~ sel + 1; menu_sel <- k ~ 0; `Stop },
    _ => `Continue
}
```

Leave one `k ~` out and that write no longer waits for the key: `sel <-
sel + 1` reads and writes `sel`, so it feeds itself and counts as fast
as it can, and `menu_sel <- 0` is a constant, which acts once and never
again.

An `on` block states the clock once for everything in it:

```graphix,fragment
let keys = |k| on k {
    select k.code {
        `Left => { sel <- max(0, sel - 1); menu_sel <- 0; `Stop },
        `Right => { sel <- sel + 1; menu_sel <- 0; `Stop },
        _ => `Continue
    }
}
```

## What it means

Think of every binding outside the block as a latch. Values arrive
whenever they arrive and each latch keeps the last one. When the event
fires, the block releases the latched values into its body, and the body
runs once on them. `on x { y <- z }` is `y <- x ~! z`.

Three rules follow from that picture.

**A read is the value at the event.** Whatever the block reads from
outside is taken as it stands when the event fires. A latch that is
still empty releases nothing: what reads it does not happen, and it is
not made up later when the value arrives. The rest of the block is not
held back.

**A constant acts once an event.** `menu_sel <- 0` writes at every key,
`` `Stop `` is answered at every key, and a call whose arguments are all
constants is made at every event.

**A write lands after the event.** `<-` takes effect in the next cycle
as always, so every read in the block sees the value from before the
event. That is what makes a counter work:

```graphix
let clicks = 0;
let press = |c| on c { clicks <- clicks + 1 };
press(1);
clicks
```

The event's own value is the event: inside `on k { .. }` a read of `k`
is the value that fired. `on let` names it when the event is an
expression:

```graphix,fragment
on let (x, y) = mouse::position() { cursor <- (x, y) }
```

## State inside the block

The block's body only ever sees the released values, so anything in it
that keeps state advances once an event:

```graphix,fragment
// counts every x, read at k
let seen = k ~ count(x);
// counts the events
let pulses = on k { count(x) }
```

Keep an accumulation that should run between events outside the block,
and read it inside.

A latch holds one value. One that came and went between two events is
never seen by the block; if every value matters, queue them with
[`seqq`](./seq.md).

## What it is not

An `on` block does not wait and does not order its statements: they all
run in the event's cycle, which is why a handler can answer its event. A
call the block starts may answer later, and what it returns flows on
from there, off the clock. To run steps one after another, use a
[`seq`](./seq.md).

The rule for what counts as the block's body is the seq's. A function
you call by name reads its own captures as they stand, so pass it what
must agree as arguments. A lambda written in the block reads the
released values. `live(e)` reads `e` where it stands. A nested `on` or
`seq` block has its own clock.

A `catch` goes around the block, not in it: its handler runs when an
error is raised, not at the event, and a `?` inside the block raises to
it. A call that takes no argument has nothing for the event to fire;
give it the event.
