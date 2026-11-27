# `fresh e { .. }`: a block that starts over at an event

Status: built 2026-11-27 (Eric's rulings of that day, D60 in the CR
discussion file; CR `a-language-03`, item 3 of the language design
review).
Pins: `stdlib/graphix-tests/src/lang/fresh_blocks.rs`, the unit tests of
`graphix-types/src/expr/seq/fresh.rs`, `expr::parser::test::fresh_parses`.

## Why

Entering a select arm is not an event (`bind_driven_bodies.md`), so an
arm can only resume: its state is what it was when the arm slept, and an
event-consuming node in it shows what it last gave until an input fires.
What the right behaviour is at a re-entry differs per node (a cursor is
kept, a level shown is refreshed, a dialog or a timer starts over), and a
level and an event look the same to the runtime, so no rule for arms is
right for all of them. Three were tried and removed: a wake as an init,
catch-up, and constants that fire at a first entry.

What existed before this block:

| Written as | At re-entry | While shown | State |
|---|---|---|---|
| an arm | stale | follows | kept |
| `on t { .. }` in the arm | right | frozen until the next entry | kept |
| `uniq(any(d, t ~ d))`, the idiom | right | follows | kept |
| `seq t { screen(t) }` in the arm | the old value for a cycle, then right | frozen once the step completes | new |

Nothing gave new state that then runs live. Lustre has it as `reset ..
every`, SCADE as a restart transition.

## The rule

```graphix,ignore
fresh t { editor(record) }
fresh let (x, y) = click { .. }
```

**The block starts over at each fire of its event and runs live
between.** Starting over is a birth, as a program's first cycle and a seq
run are:

1. **Its state is new.** Everything in the block is built again: a
   stateful builtin holds nothing, a `let` takes its initializer, a call
   in flight is aborted, and an arm inside the block that slept through
   the start is not the arm of the block's last life.
2. **Its constants fire**, so `out <- 7`, a timer over constants and a
   call over constants act at each start.
3. **What it reads fires with the value that stands**, so the block
   starts from the world as it is: `uniq(data)` gives `data`, and a call
   on a standing argument is issued.
4. **Between two fires it is ordinary live code.** Nothing is sampled
   and nothing is clocked.

Before its event first fires the block is nothing and does nothing. An
event that goes bottom starts nothing over, and the block keeps running.
The event's own value is the event: `fresh let` names it.

In a select arm the event is the arm's bind:

```graphix,ignore
let view = select uniq(tab) {
    t@ `Edit => fresh t { editor(record) },
    `List => list_view
}
```

Each visit starts the editor over, right in the cycle of the entry. An
entry with no event (an outer arm waking around it) starts nothing over:
an outer resume resumes what is inside.

## A capture is a level

Rule 3 fires every capture, and the runtime cannot tell a level from an
event. So in a fresh block a capture is read as a level: an event that
stands as a value (the last key, held in a binding or a pattern bind) is
delivered again at each start. An `on` block inside clocked by it, a
`count` of it and a call given it all act once at the start, with no new
event. An event reaches the block safely as a parameter of a function the
block makes and something outside calls (`let keys = |e| on e { .. }`,
which is how widgets deliver them: the call site is outside the block),
or as a value that is bottom between its fires. Pin:
`a_capture_that_stands_fires_at_the_start`.

No exemption is made for `on`'s clock or the left side of `~`: it would
cover those two and not `count(key)` or `once(key)`.

## `on` and `fresh`

They are independent: `on` decides when reads are taken, `fresh` whether
state survives the event.

| | State kept | State starts over at the event |
|---|---|---|
| Reads live | plain code | `fresh t { .. }` |
| Reads taken at the event | `on t { .. }` | `fresh t { on t { .. } }` |

`fresh t { count(t) }` is always 1 and `on t { count(t) }` counts the
events. `fresh t { on t { .. } }` is `on t { .. }` with nothing kept
between events, which is the same thing for a stateless body; what it
adds is that a start aborts what the last event left in flight. The
useful nesting is two events: `fresh opened { on key { .. } }` keeps its
state for one opening. A fresh block in an `on` block or a seq step is
its own, as any nested block is: the outer block samples nothing in it,
and its body runs live.

## What is refused

`until`, `try .. with`, `live(..)` and the `abort` and `flush` clauses are
a seq's, and the block has none of them. `fresh` is a reserved word. A
`catch` in the block is allowed: the block is ordinary code.

A write that feeds itself is a self-loop here as anywhere, since reads
are live: `fresh k { n <- n + 1 }` spins. Clock it, `fresh k { on k { n
<- n + 1 } }`, or write `n <- k ~ n + 1`.

## How it is built

`fresh` is a kind of the seq syntax node (`SeqKind::Fresh`), so the
parser, the printer, the formatter and every walk take it as they take a
seq. `expr::seq::fresh::desugar` lowers it to a call of the block as a
function of its event, marked `ExprKind::Fresh`:

```graphix,ignore
fresh let (a, b) = e { a + b }
// lowers to
(|#frv| { let (a, b) = #frv; a + b })(e)
```

The call site carries the rest (`CallSite::fresh`, `node/callsite.rs`).
It never binds statically. It holds no instance until its argument fires
with a value, and at each such fire it binds its function again
(`rebind` under `renew`), which deletes the instance it held and builds
a new one; the new instance's first update runs under `View::Birth`
whatever view the site is updated in (`ExecCtx::born`), so a block in a
waking arm is born too. Building the instance again is what makes rule 1
hold by construction: no node of the last life is left to reset. The
site is never inside a kernel (`fusion::effect_blocker`); the instance's
body fuses as any run-time bind's does.

A start costs what a first call of a function costs, the build of its
instance: about 0.15 ms for a three-line body on an optimized build
(`~/ptmp/arm-restart/c1.gx`), more for more code, and memory does not
grow over 100,000 starts. It is meant for a dialog, a tab, a record: an
event a person causes. A body that must start over many times a second
should be small.

## The warning beside it

The check warns where a select arm calls a builtin that consumes events
(`Effect::Sync` or `Effect::Async`) and entering the arm fires nothing
the call reads (`select.rs::warn_stale_consumers`): `uniq(data)` in a
tab's arm shows what it showed before the arm slept. What an entry fires
is the scrutinee's reads and whatever the arm or an arm around it binds,
a `let` in it included. The outermost select walks the arms nested in it
(`CFlag::InArm`), and a seq step's select, a lambda's body and a fresh
block are not walked. The message names the three ways out: feed the
call the arm's bind (`any(v, k ~ v)`), move it out of the arm, or start
the arm's body over (`k@ .. => fresh k { .. }`). It is first-order: a
`let` in the arm made only of captures is taken as fired, and a call of
a function that holds such a builtin is not seen. Pin:
`lang::select::stale_consumer_warning`.
