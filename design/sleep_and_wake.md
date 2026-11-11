# Sleep and wake

Sleep is pause, not reset, and a wake is a resume, never a rebirth. What a body reads and what fires in
it when it resumes is `bind_driven_bodies.md`; this is what sleep and wake do to state.

## Who sleeps

A deselected select arm, a guard whose pattern does not admit the scrutinee's value, the slots of a
collection whose source went bottom, and the instance of a call whose function went bottom. Each is put
to sleep by the node that holds it (`Update::sleep`) and resumes on its next update. `sleep()` reaches
every node of the body, through a `CallSite` into its instantiated callee. An arm sleeps at its deselect
when it holds state, an effect (an `Io` call included), a raise to a handler or a recursive call; a fused
arm is judged by the region its kernel replaced (`analysis::RegionFacts`), so fusion decides nothing
here. A pure arm, in which nothing runs while it is untaken, is slept when it is next entered instead
(`analysis::arm_sleeps_on_deselect`, `wake::asleep_since`). A deselect deletes a recursive callee inside
the arm (`node::deselecting_arm`, `CallSite::sleep`): each depth is an activation, and a shrink is a
delete (`recursive_activations.md`).

## Sleep state is local

A node skips recomputation when no consumed input fires. That is sound for a node that stays awake: a
value changes only by firing, and it saw every fire. A node that slept did not, so its first update
after the sleep recomputes from the present values instead of riding its resident.

State lives in nodes, never in an `ExecCtx` field: nothing may end up behind a lock, since compilation
is parallel and the evaluator forks. Every node or `Apply` that can skip work owns a `slept: WakeBit`
its own `sleep()` sets and its next update takes: the `dense_gate!` structs (the macro takes
`$self.slept`, so the field is macro-enforced), the op macros, `StringInterpolate`, `MapQ`, `Bind`,
`CallSite`, `GXLambda`, `CachedArgs`, `FusedKernel` and `Select`. `Node` stays a bare 16-byte newtype; the
bits hide in struct padding. Nodes that recompute unconditionally need none. `~` rides: it is edge
state. `Any` rides too, except that it stands bottom at a wake where the child its resident came from is
bottom now. `uniq` is bottom while its input is, and a value back from bottom is new to it. A `Constant`
fires where it is born and stands at a resume.

A woken `Select` re-matches against the present scrutinee: the selection it held was made against a
value that may have changed while it slept.

## Views

`View` says how an evaluation reads what stands (`View::join` nests them):

| View | Standing values | Constants | Where |
|---|---|---|---|
| `Cycle` | as they stand | stand | an ordinary cycle |
| `Birth` | read as delivered | fire | a root's first cycle |
| `Wake` | stale, marked moved | stand | a body's entry and its resume (`ExecCtx::enter_body`) |
| `Step` | stale | fire | a seq step's run; a birth nested in a resume |

## The value channel re-reaches the store

What a resume recomputes is quiet, and a quiet value must still reach the readers past a publish seam.
`Bind::update` re-publishes a quiet production after a sleep, and one that moved
(`bind_driven_bodies.md`, the moved mark). `CallSite` refreshes its argument ids' standing entries,
`GXLambda` re-seeds its formals, `MapQ` rebuilds its collection from the refreshed slots. A quiet bottom
is republished the same way: an input that went bottom during the sleep delivers a stale bottom at the
wake, and the seam stands a stale bottom in the store, or its readers would see the value from before
the sleep. For the same reason a node whose input is bottom sets its resident (`TagValue::set_bottom`)
and never rides it. Pins: `findings/wake-stale-bottom-sep2026/`.

A binding something writes keeps the value it holds at a wake (`Bind::update` holds a `<-` target back
from a production that did not fire), and so does a target born `never()`, whose initializer's standing
bottom is no write. Pins: `findings/wake-connect-target-sep2026/`.

## Builtins

- **`CachedArgs`** (package-core): the argument slots are refreshed to the present values on every
  delivery. On the first update after a sleep with an all-stale production, and whenever an argument
  moved, a `Stateless` builtin runs `eval` again from the slots and its result is stale and moved. A
  stateful one retags its resident, which is its state. A configuration memo (`FastMemo`) survives
  sleep: it is a pure function of the config argument.
- **Restart builtins and queues keep their state** across a sleep (`once`, `take`, `skip`, `uniq`,
  `hold`, `count`). `queue` and `iterq` take back what they sent but had not received (`InFlight`) and
  send it again from the wake. `filter` keeps its pending value.
- **Rebirth is a seq step's.** `Apply::reborn` (a no-op by default) starts a builtin's state afresh; a
  call site calls it before an update that follows its sleep under a born view
  (`CallSite::update_call`), which only a seq step's next run is. `BuiltInLambda` forwards it.
- **Async.** Sleep aborts the call in flight and the wake restarts nothing; a call's result survives the
  sleep (`CachedArgsAsync`, rpc `call`). `net::write` re-attaches to its path at the wake and writes only
  on a value fire. Event sources (timers, idle waits, line readers, subscriptions, watches, servers)
  pause: a timer's run and a pending idle wait resume at the wake, a standing interest re-attaches;
  their output, the last event, does not survive (`async_sleep_outputs.md`).

## Seq machines

A run is aborted by its arm's sleep (`pc` to idle). A `seqq`'s queue keeps its requests, and its machine
handler owes the aborted run's credit, paid at the wake (`CatchRole::Machine::clock`). A write to a
machine's own `#` name that landed while its arm slept (the reset of `pc`) is raised again at the wake
(`Bind::reraise_writes_while_asleep`), for the machine's names alone; a block takes the delivery back
after its last statement (`node::take_back_reraised`, `node::Lets`). A `let` entered under a born view
does not raise such a write again: it is the previous run's.

## Pins

`lang::async_restart`, `lang::dense_deltas::no_arm_window_pauses`, `lang::seqq::{sleep_restarts,
aborted_run_returns_its_credit, a_rerun_keeps_every_request,
a_write_a_run_left_does_not_fire_in_the_next}`, `lang::seq_abort::sleep_resets_the_machine`,
`lib_tests::db::db_op_not_repeated_at_wake`, `lib_tests::net::{net_call_not_repeated_after_wake,
net_write_not_repeated_after_wake}`.
