# `on e { .. }`: a block clocked to one event

Status: built 2026-11-27 (Eric's rulings of that day, D58 in the CR
discussion file; CR `a-language-04`).
Pins: `stdlib/graphix-tests/src/lang/on_blocks.rs`, the unit tests of
`graphix-types/src/expr/seq/on.rs`, `expr::parser::test::on_parses`.

## The rule

```graphix,ignore
on k { n <- n + 1; menu_sel <- 0; `Stop }
on let (x, y) = click { pos <- (x, y) }
```

Everything latches as it arrives, and the block releases the values on
the clock's pulse. `on x { y <- z }` is `y <- x ~! z`.

1. **A read is its value at the event.** Every binding made outside the
   block that the block's text reads is taken as it stands when the event
   fires, each alone. A read with no value yet stops what reads it and
   nothing else, and is never paid later (`~!`, not `~`). A read through
   a reference (`*r`) follows its target, so it is taken at the event
   too.
2. **A constant acts once an event.** A connect's right side, a `let`'s
   value, a select's scrutinee and a block's value that read no variable
   are fired by the event, and a call none of whose arguments fires is
   issued by it (each argument is fired).
3. **What the block makes is left alone.** A `let` in the block, a
   pattern bind and the result of an async call the block starts derive
   from 1 and 2, or arrive later, off the clock.

It follows that:

- a binding the block reads and writes is read as it stood before the
  event, since a write lands in the next cycle: `on k { n <- n + 1 }`
  counts the events and does not feed itself;
- the block answers in the event's cycle, which a seq cannot (its value
  comes a cycle after its trigger): a key handler is `|k| on k { .. ;
  `Stop }`;
- a call with several inputs is issued once, on the event, with all of
  them: the whole-input rule holds by construction;
- state inside the block is in the clock's domain: `on k { count(x) }`
  counts the events, where `k ~ count(x)` counts every `x`. An
  accumulation that should run between events is written outside the
  block and read in it.

## What converting real code showed

The admin port, the stdlib's Graphix and the book's examples were
rewritten with `on` when it was built (540 hand-written samples, about
170 left). What bit, each a consequence of the rule:

- **A hand sample inside the block does not bank.** `on k { b <- k ~
  late }` lowers to `b <- k ~ (k ~! late)`: the read is the event's
  value like any other, so a value absent at the event is not paid when
  it arrives. `live(late)` keeps the hand sample's meaning. Wrapping a
  handler that leaves one `k ~ v` in place changes that sample silently.
- **A constant is fired by the event, not by a value the block
  computes.** `{ let r = (k ~ sel)$; open <- r ~ true }` opens only when
  a row is selected; `on k { let r = sel$; open <- true }` opens at every
  key. Gate the constant on the value (`open <- r ~ true`), or put it in
  an arm of a select over it, which a bottom scrutinee never takes.
- **A handler with a guard answers once an event.** A guard written by
  hand reads live state, so the handler answered again when its own
  write landed; in the block the guard reads the event's value. The
  states are the same and the answers are fewer (one example went from
  135 answers to 70 for 70 keys).
- **Where the old code banked without meaning to**, a key that arrives
  before the sampled value has ever existed is dropped, where it was
  paid when the value arrived (the browser's cursor before its first
  listing).
- **A call through a field is no call by name.** `pan.handle(e)` reads
  `pan`, so the struct is sampled and the call is made once an event,
  where the hand-written dispatcher was re-issued when the struct fired.
- **A bare constant arm is not fired by the event** and needs nothing: a
  select fires with its scrutinee and gives the arm's standing value.
- **`on` as a word** was a variable in four places in the port and about
  150 lines of test fixtures (a flag that is on or off), all renamed.

## What is and is not the block's text

The seq rule (`design/seq_blocks.md` §5) decides it, and the lowering
shares the seq's rewrite:

- a lambda written in the block reads the event's values too, so nothing
  in the block moves between two events;
- a function called by name is never sampled: the call resolves it where
  it stands, and what the callee reads is its own business;
- a write's target, a `&x` and a `&mut x` reach the variable itself;
- `live(e)` reads `e` where it stands;
- a nested `on` or `seq` block is its own: the outer block samples
  nothing in it, its event and trigger included;
- the event itself is read as it fires: in `on k { .. }` a read of `k`
  is not sampled, and `on let pat = e { .. }` names the event's value.

## What is refused

- `catch` in the block: its handler runs when an error is raised, not at
  the event. It is written around the block, where a `?` in the block
  raises to it.
- A dynamic module in the block, as in a seq.
- `until`, `try .. with` and the `abort` and `flush` clauses: they are a
  seq's.
- `on` is a reserved word.

A call with no argument has nothing for the event to fire, so it is not
issued by it: it runs once, as anywhere. Give it the event.

## Lowering

`on` is a kind of the seq syntax node (`SeqKind::On`), so every walk of
the tree, the formatter and the fuzzer's transforms take it as they take
a seq. `expr::seq::on::desugar` lowers it to nodes that exist; there is
no machine and no new runtime:

```graphix,ignore
on k { sel <- max(0, sel - 1); menu_sel <- 0; `Stop }
// lowers to
{
    let #onk = k;
    let #ons_sel = #onk ~! sel;
    sel <- max(0, #ons_sel - 1);
    menu_sel <- #onk ~! 0;
    #onk ~! `Stop
}
```

One sample is made for each outer binding the block reads, shared by its
reads; a name every read of which the block shadows takes none. `graphix
--expand` prints the lowered block.

## Why each read alone

A seq step samples what it reads together and waits for an absent input.
An on block cannot wait (rule 1: nothing is paid later), so one sample of
every read would cancel every statement while any one read anywhere in
the block had no value, and a handler would still answer its event: the
key is swallowed. Each read alone gives the same values, since all are
taken in the event's cycle, and an absent read stops only what reads it.
Probe: `~/ptmp/on-block/run.sh` (the admin port's key handler in
miniature, as written and lowered both ways).
