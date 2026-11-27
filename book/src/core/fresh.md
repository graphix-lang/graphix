# Fresh Blocks

## Why start over

A select arm that is not selected sleeps, and when it is selected again it
resumes: whatever state it held is still there, and nothing fired while
it slept is delivered to it. That is right for a cursor or a scroll
position. It is wrong for a dialog that should open blank, a timer that
should start when a screen opens, or a view that shows data through
something that remembers:

```graphix,fragment
let view = select tab {
    `A => "A shows [uniq(data)]",
    `B => "B"
}
```

Leave tab A, let `data` change, come back: A still shows the old value,
because `uniq` gives nothing until `data` fires again. The check warns
of this.

A `fresh` block starts over each time an event fires:

```graphix,fragment
let view = select uniq(tab) {
    t@ `A => fresh t { "A shows [uniq(data)]" },
    `B => "B"
}
```

## What it means

`fresh e { .. }` is nothing until `e` first fires. At each fire of `e` the
block starts over, the way a program starts:

**Its state is new.** Every stateful thing in it holds nothing, every
`let` takes its initializer, and a call still waiting for an answer is
dropped.

**Its constants fire.** A write of a constant, a timer over constants
and a call over constants all act at each start.

**What it reads fires with the value it has now.** The block starts from
the world as it stands, so `uniq(data)` gives the current `data` at once.

**Between two fires it is ordinary live code.** It follows what it reads
as any expression does.

```graphix
let n = 0;
n <- select n { i if i < 5 => i + 1, _ => never() };
let k = select n { 2 => null, 4 => null, _ => never() };
let ticks = fresh k { count(n) };
ticks
```

`ticks` counts 1, 2 from the fire at 2, then 1, 2 again from the fire at
4.

`fresh let` names the event's value:

```graphix,fragment
fresh let record = selected { editor(record) }
```

In a select arm the event is the arm's bind, as in the example above.
Put `uniq` on the scrutinee when selecting the same arm again should not
start it over.

## Events come in as parameters

Because a start fires everything the block reads, the block treats what
it reads from outside as a level: a value that is right to start from.
An event that is held as a value, such as the last key pressed, would be
delivered again at every start. Let an event reach the block as the
parameter of a function the block makes, which is how widgets deliver
them:

```graphix,fragment
fresh opened {
    let text = "";
    let keys = |k| on k { text <- "[text][k]" };
    input(#on_key: keys, text)
}
```

## `on` and `fresh`

The two answer different questions. `on` says when a block reads the
world: at the event, and not between. `fresh` says whether the block
remembers anything from before the event.

| | Remembers | Starts over at the event |
|---|---|---|
| Reads live | plain code | `fresh t { .. }` |
| Reads at the event | `on t { .. }` | `fresh t { on t { .. } }` |

`on t { count(t) }` counts the events. `fresh t { count(t) }` is always
1. The useful nesting is two different events: `fresh opened { on key {
.. } }` keeps its state for one opening.

## What it costs

Starting over builds the block again, which costs about what the first
call of a function costs: a fraction of a millisecond for a few lines,
more for more code. It is meant for things a person causes, such as
opening a dialog, switching a tab or picking a record. Keep a block that
starts over many times a second small.

Reads in the block are live, so a write that reads its own target
feeds itself, here as anywhere. Clock it: `fresh k { on k { n <- n + 1 }
}`.
