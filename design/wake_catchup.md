# Wake catch-up

Status: built 2026-09-01; no partial rebirth 2026-10-18 (below)
Pins: `graphix-fuzz/findings/wake-catchup-sep2026/`, `dyncall-arm-init-stale-aug2026/`, `default-arg-birth-sep2026/`, `select-wake-rematch-sep2026/`; `stdlib/graphix-tests/src/lib_tests/callable.rs` (`arm_entry_and_wake_deliver_standing_args_stale`, `callable_body_flip_reads_standing_key_stale`, `alias_read_consumes_the_formals_fire`, `banked_bind_does_not_consume_the_level`); `stdlib/graphix-tests/src/lang/select.rs` (`select_sibling_binds_spent`, `let_siblings_catch_up_apart`)
Supersedes: `pure_select.md`, `levels_and_events.md`, `pure_dataflow_plan.md` part A (the sleep-free select proposals, withdrawn — sleep is pause)

## The hole

Sleep breaks the ride-skip's invariant. Under dense delivery a node
skips recomputation when no consumed input triggers (R1), which is
sound because an AWAKE node's stale input cannot differ from what its
resident was computed from — values change only by firing, and an
awake node saw every fire. A sleeping arm did not. While it sleeps its
inputs drift behind its back; its first update after wake delivers
present values under stale tags, the skip takes, and the arm surfaces a
product of a world that no longer exists.

The two obvious fixes are each half right. Delivering everything FIRED
at wake gives correct values but re-raises consumed events (a modal arm
re-submitting the Enter that opened it). Delivering everything stale
re-raises nothing and shows stale values (`select cond { true => in0 +
1, false => in0 + 42 }` showing 43 after `in0` moved to 20). The rule
separates the two channels.

## The rule (Eric, 2026-09-01)

1. **Every select tracks one fire bit per input** — the union of
   its arm bodies' and guards' free refs (binds defined outside them). Bits are
   set when a tracked input fires soundly, whatever the selection
   state, including no-arm windows (bottom scrutinee, undecidable
   guards). Bits OR-accumulate: N fires during a sleep conflate to ONE
   catch-up at the latest standing value (`queue` is the lossless
   tool).
2. **Fire state is consumed by evaluation.** Whichever guard or arm evaluation
   reads an input clears its bit — the live selected arm same-cycle as
   the degenerate case, a woken arm at catch-up. Delivery is
   consumption whether or not the arm's interior does anything with it.
   An arm that does not read an input leaves its bit for a future
   waker; two sleeping readers waking in sequence → the first consumes,
   the second reads stale. At most once per select.
3. **A woken node's first update recomputes from present values.**
   `sleep()` reaches every node in the arm (through `CallSite` into
   instantiated callee bodies); each node sets a local `slept` bit
   there and its next update refuses the ride-skip. The bit is
   co-extensive with the hole: a node that never sleeps never drifts
   and never pays.
4. **Tags stay honest.** A tracked bit delivers FIRED at the current
   standing value; everything else delivers present-but-stale;
   recomputed products tag by the join of their inputs. Edge consumers
   (`~`, connects, accumulators, callback dispatch) tick exactly for
   the events no selected reader saw, once, and nothing consumed is
   ever re-raised.

The invariant: **a select's output is always a function of the present
world; events influence it exactly once.**

Two other sites pause a body and wake it the same way, as a one-arm
select: a collection whose source turns bottom pauses its slots
(`collection::SourceSeen::Paused`), and a call whose function value
turns bottom pauses its instance (`callsite::Absent`). Each tracks the
fires of what the paused body reads and, when the source or function
returns, runs it under the wake view with those fires caught up.

A guard the chain does not reach sleeps as an unselected arm does and,
when the chain next reaches it, catches up the fires its inputs had
while it slept, through a consumer slot in the select's one tracker. A
guard spends the same fire bit an arm body would; neither catches up a
fire the other already consumed. Pins:
`lang::select::{guard_below_the_claim_sleeps, guard_catches_up_a_direct_input,
guard_and_arm_share_missed_fire}`.

Outside the mechanism: the scrutinee (its own consult machinery);
pattern binds of THIS select and of every enclosing select — a pattern
bind is a facet of its arm's scrutinee delivery, which that
arm's match consumed, so `k` beside ``ev@ `Key(k)`` is never re-raised
at a nested flip after the `ev` reader handled the key. The facet cuts
the other way too (2026-09-17): a pattern bind carries the inputs
whose fires reach its scrutinee (`Bind::pattern`, the scrutinee's
triggering refs closed over enclosing pattern binds), and an arm that
reads the facet consumes those inputs' bits. So `ev` read by the
landing arm consumes `e`, and a panels arm that takes `e` itself does
not catch the key up at the flip — the same key was handled once,
through its alias. Consumption follows the alias; delivery never does.
A level under a sample's right side is not an input of the binds:
`select kk ~ (toast, sel) { (t, i) => .. }` binds facets of `kk` alone,
the tuple was banked, so a read of `i` leaves `toast`'s bit for the
sibling arm that watches `toast` itself. A tuple or struct literal
scrutinee stays joined: every fire of any member rebinds every leaf
FIRED, so each leaf is a facet of all of them. A destructuring
`let`'s siblings are inputs of their own, as separate `let`s are: a
`let` bind is a real input, delivered, whose catch-up is wanted.
Nesting needs nothing extra: an outer arm's ref set includes everything
under it, so an inner select's missed window is covered by the outer
bits; the wake evaluation carries the tags down and the inner select
routes and consumes with them.

## Inputs learned as an arm runs

The bits cover the inputs an arm names before it runs. Two it learns
only as it runs: the body of a callee a function value selects, bound
at its first dispatch, and a reference's new target. For those a write
stamps its variable with the cycle it landed in (the store's stamp); a
standing entry (a `let`'s quiet republication at its wake, a formal
re-seeded standing) is no write and keeps the stamp it had
(`branch::standing_stamp`). Each arm records the first cycle whose
writes it did not see (`node::wake::ArmClock`): a run makes it the
next cycle, and the arm's falling asleep (its deselect, a guard the
chain passes by, the select's or the machine's own sleep while it is
selected) makes it the cycle it fell asleep in, so walk pruning, which
skips a selected arm nothing marked, changes nothing; until the arm
runs it is the tracker's first cycle, which the arm never saw. An arm
evaluated under a wake or step view opens a catch-up scope
(`node::wake::CatchUp`): that cycle, no later than the enclosing
scope's (an arm sleeps whenever the arm around it does), the select's
tracked inputs and fire bits, and the scopes around it. A callee bound
fresh inside the scope is delivered, FIRED for its update, each input
of its body written from that cycle on; a `Deref` reads such a target
FIRED. An input a select around it tracks is owed only while the
innermost such select holds its fire bit, and the evaluation that is
owed it consumes the bit when it ends, so a fire another arm consumed
is not raised twice and one no arm read is raised once; consumption of
an input no arm names is not tracked, so two arms reading one only
through function values may each catch it up. Pins:
`lang::select::first_entry_{static,dynamic}_callee_catches_up`,
`lang::select::build_cycle_{static,dynamic}_callee_catches_up`,
`lang::select::caught_up_once_through_a_fresh_callee`,
`lang::select::quiet_republish_is_owed_no_fire`,
`lang::prune::{catch_up_counts_from_the_deselect,
catch_up_counts_from_the_enclosing_sleep, guard_catch_up_counts_from_the_pass,
guard_catch_up_counts_from_the_enclosing_sleep,
skipped_statement_owes_its_awake_fires_nothing}`.

## Worked examples

`select cond { true => in0 + 1, false => in0 + 42 }`, `in0` init 1,
`cond` init false:

| event | woken arm sees | output |
|---|---|---|
| init | everything fires (genuine init) | 43 |
| cond→true | `in0` bit consumed at init by the false arm → stale 1; recompute | 2 |
| in0→20 | live fired delivery | 21 |
| cond→false | bit consumed by the true arm → stale 20; recompute | 62, never 43 |

Step 2 emits although nothing in the arm fired — organically, because
the consulted scrutinee fired.

**The modal**: `e = "x"` fired while `` `A `` was selected and `` `A ``
reads `e` — bit consumed. `` `B `` wakes with `e` stale, `t ~ (submitted
+ 1)` does not tick, no phantom submit.

**The fork**: `in1` fired while the OTHER arm, which does not read it,
was selected — the bit survives, so the woken arm catches up with a
genuine fire.

**Shared-input effect** — `select cond { true => publish(p1, v), false
=> publish(p2, v) }`: both arms read `v`, so its fires are always
consumed by whichever arm is awake and the woken arm gets ZERO catch-up
fires. Value correctness cannot come from fires here — tracking alone
is insufficient; the forced recompute republishes `p2` at the present
`v`, and a pure function in place of `publish` recomputes identically.

## Rejected alternatives

- **Ride is the rule** (uniform stale, no recompute): user-visible
  staleness — 43 instead of 62, a `subscribe` stuck on a path the graph
  moved past.
- **Fire everything at wake**: re-raises consumed events, or demands a
  distinguishable "soft fire" every edge consumer and every future
  builtin author must learn to ignore — an open-set tax against the
  closed set of skip sites we own.
- **Value-diff gating** (re-eval iff a stale value differs from the
  cached copy): an invisible `uniq` at every wake seam; "the same value
  fired can carry just as much meaning as a differing value."
- **Stateless-only re-eval**: `subscribe` is stateful and stays on the
  wrong path; statelessness is the wrong axis. The question is which
  fires the arm missed, and that cannot be reconstructed at wake, so it
  must be tracked.
- **Per-arm independent tracking**: re-phantoms the modal — `e` fired
  during `` `B ``'s sleep, so `` `B `` would catch it up although `` `A ``
  consumed it. Consumption is per select.
- **Always-computing dataflow** (no firing; consumers pull): a much
  simpler compiler, tolerable on input, a nightmare on output — the
  naive `print(42)` prints forever and the graph is a busy loop.
- **No sleep at all** (the pure-select and mux-select proposals,
  2026-09-03/04: pure arms lazy, impure arms always on, effects keyed
  on presence): sparse delivery died on the JIT and `seq` lost its exit
  actions; the engine did not get simpler. Sleep is pause. The keeper:
  a pure non-recursive arm skips `sleep` (nothing to pause) and is not
  updated while untaken (`LazyArmFacts::sleep_on_deselect`); a `<-`, a catch,
  a sample, an `any`, a `?` that raises to a handler (its raise is an
  event; re-entered, the arm reads a standing error stale and raises
  nothing new), or a stateful/async callee makes an arm impure, and
  impure arms still sleep. A fused arm is judged by the region its kernel
  replaced (`analysis::RegionFacts`).

## Mechanics

### The tracker (`node/wake.rs`, `TrackedFires`)

Select and the seq machine share it.

- `per_arm`: each consumer's free refs (`Refs` referenced minus bound
  within it, minus pattern binds). A select assigns bodies the first N
  slots and guards the next N slots; a guard's slot is empty where the
  arm has no guard. A select without guards allocates only the body slots.
- `consumes`: per arm, the inputs the pattern binds its body reads are
  facets of (`Env::pattern_inputs`, marked at `Select::compile` for
  each arm's structure-predicate ids: the inputs that reach the
  scrutinee). Reading a pattern bind consumes its inputs' fires; the
  bind itself is never delivered.
  Computed from compile-time refs at first update and REFRESHED AT
  EACH DESELECT — compile-time refs cannot see through a lambda literal
  into an instantiated body; deselect-time refs can.
- `pending`: sound fires no arm evaluation has consumed. `observe`
  records this cycle's fires before any routing or early return;
  `deliver` injects the unconsumed bits an arm reads into
  `event.variables` as FIRED entries at the standing value, scoped to
  exactly that arm's evaluation (`restore` afterwards), and clears
  them. Semantic state: survives sleep. Each recursion depth is an
  activation with its own selects, so each depth owns its bits
  (`tail_calls_are_calls.md`). The bits record only fires that HAPPENED in
  the awake graph; a paused async producer produces nothing to catch
  up and resumes on wake.

### Sleep state is local

State lives in nodes, never in an `ExecCtx` field (parallel
module-level compilation is coming and a parallel evaluator must stay
possible, so nothing may end up behind a lock). Every skip-owning
node/Apply owns a `slept: WakeBit` its own `sleep()` sets and its next
depth-0 update takes: the `dense_gate!` structs (the macro takes
`$self.slept`, so the field is macro-enforced), the op macros,
StringInterpolate, MapQ, Bind, CallSite, GXLambda, `CachedArgs`,
`FusedKernel`, and Select. `Node` stays a bare 16-byte
newtype; the bits hide in struct padding. Nodes that recompute
unconditionally need none; `~` rides correctly — it IS edge state;
`Any` rides too, except that it stands bottom at a wake where the
child its resident came from is bottom now; `uniq` is bottom while its
input is, and a value back from bottom is new to it; a `Constant` fires
where it is born and stands at a resume.

A woken Select RE-MATCHES against the present scrutinee: a selection
retained across the sleep was made against a value that may have moved
while no reader was awake (an arm-local `<-` counter has no tracked
fire, since arm locals are not arm-body inputs).

### The value channel re-reaches the store

A wake's recomputed STALE values must be republished or readers
downstream of a publish seam still ride: `Bind::update` re-publishes a
quiet wake production (`<-` targets holding a value are still held
back — sleep is pause — and so is a target born `never()`, whose
initializer's standing bottom is no write: pins
`findings/wake-connect-target-sep2026/`). A target is held back from the
initializer's fire only: a resume fires nothing but the inputs it catches
up (its constants stand), so a `let` under a wake view whose production
did not fire leaves its `<-` targets' last writes (`Bind::update`). A
collection loop's resize or a source's return from bottom is a fire
whatever the source's tag, on both engines. Pins:
`lang::select::wake_input_fire_reaches_target`,
`wake_constant_keeps_target`, `wake_fire_keeps_targets`,
`wake_unreached_input_keeps_target`, `wake_fire_caught_up_late`. `CallSite` refreshes its arg ids' standing
entries, `GXLambda` re-seeds its formals, MapQ rebuilds its collection
from the refreshed slots. A quiet BOTTOM is republished the same way: an
input that went bottom during the sleep delivers STALE bottom at the
wake, and the seam must stand a stale bottom in the store or its readers
see the pre-sleep value. For the same reason a node whose input is
bottom sets its resident (`TagValue::set_bottom`) and never rides it:
the resident of a node that slept through the fresh bottom is a value.
Pins: `findings/wake-stale-bottom-sep2026/`.

### The builtin wrapper (`CachedArgs`, package-core)

The slots are refreshed to the present values on every delivery. On
the first post-sleep update with an all-stale production a `Stateless`
builtin re-runs `eval` from the slots, result STALE (the phantom
first-production value rule extended to wake); a stateful one retags —
its resident IS its state, already the correct present value, and its
edge catch-up arrives separately as a tracked FIRED through the normal
path, added exactly once. A configuration memo (`FastMemo`) survives
sleep: it is a pure function of the config argument, and clearing it
would demand a fired re-delivery no wake provides.

### Kernels

A kernel carries no fire bits and needs none: a stateful builtin never
fuses, so every edge-consuming arm interior sits under an interpreter
select whose tracker injects THROUGH the kernel boundary (an
arm-position kernel's params read the injected fires). Pure arms
recompute anyway — kernels compute always. What a kernel needs is the
view: wire slot 0 bit 0 = born (`View::born`: constants fire), bit 1 =
standing values read stale (`View::wake` or the kernel's own `slept`
bit); genuine init is `bit0 & !bit1` and is what gates the fastcall
stale-mask suppression. A resume runs the kernel with bit 0 clear,
exactly as an ordinary cycle; a seq step's entry (`View::Step`) sets
both.

### The birth rule

A LABELED DEFAULT is born with the binding: its one arrival is the
fresh instance's first dispatch, not a past event some reader consumed.
Present-but-stale alone left defaults permanently dark for instances
born at a becoming-selected dispatch (a default reaches the callee only
as a standing read, and a wake does not upgrade those): a fired-gated
config channel never configured and the arm emitted nothing, on both
engines identically — the metamorphic blind spot. The interpreter's
bound dispatch seeds default args FIRED.

### What stays

Present-but-stale for untracked standing reads; genuine init upgrades;
constants fire where they are born; the guard rules; `ByRef`'s stale seed;
residents survive sleep and are REFRESHED at wake rather than surfaced;
shrink-is-delete for recursion (a fresh activation has phantom
residents and takes the ordinary first-production path).

## No partial rebirth (2026-10-18)

Eric's ruling: a wake is a resume, never a rebirth. Before it, a wake
was part rebirth: an arm's constants fired again (under a WAKE tag bit
that kept them from rewriting `<-` targets), the restart builtins
(`once`, `take`, `skip`, `uniq`, `hold`, `count`) and `queue` cleared
in `sleep`, and every async builtin restarted over its standing
arguments, so a woken arm repeated `sqlite::exec`, `db::pop_min`, an rpc
call or a `net::write` with no trigger
(`design/review-2026-10-12/repro/http-sqlite-db1-02.gx`, `db2-02.gx`).

As built:

- **Views** (`View`, joined by `View::join`): `Birth` (standing values
  read as delivered, constants fire), `Wake` (a resume: standing values
  read stale, constants stand, `<-` targets keep their values), `Step`
  (born into a world that stands: constants fire, standing values read
  stale). A select arm's first entry is a `Step`, every later one a
  `Wake` (`LazyArmFacts::entered`); a pure arm resumes as the others do, slept at its re-entry (it is not slept when
  deselected, since nothing in it runs) so its nodes recompute from what
  stands, as a kernel's do; a seq step entered for a run is a `Step`; a
  birth nested in a resume is a `Step`. The WAKE bit is deleted, from the
  tag and the JIT.
- **A write that landed while asleep is a missed fire.** An arm local
  is no tracked input, yet a write can land on it while the arm sleeps
  (a write queued in its last awake cycle, a seq machine's reset of
  `pc`): its `let` re-raises it, fired, at its wake
  (`Bind::reraise_writes_while_asleep`). The reactive fuzzer found the
  gap (`findings/wake-resume-seq-abort-oct2026/`): the constants that
  used to fire at a wake had papered over it.
- **Rebirth is a birth's.** `Apply::reborn` (no-op by default) holds
  what `sleep` used to clear; a call site calls it before an update that
  follows its sleep under a born view (`CallSite::update_call`), so a seq
  step's next run starts its builtins fresh while a resumed arm's keep
  their state. `BuiltInLambda` forwards it.
- **Restart builtins and queues keep their state** across a sleep.
  `queue` and `iterq` take back what they sent but had not received
  (`InFlight`) and send it again from the wake. `filter` keeps its
  pending value.
- **Async**: sleep aborts the call in flight and the wake restarts
  nothing; a call's result survives the sleep (`CachedArgsAsync`, rpc
  `call`). `net::write` re-attaches to its path at the wake but writes
  only on a value fire. Event sources (timers, idle waits, line readers,
  subscriptions, watches, servers) pause: a timer's run and a pending
  idle wait resume at the wake, a standing interest re-attaches; their
  output, the last event, does not survive, since a select re-emits its
  arm's value at a re-entry (`design/async_sleep_outputs.md`).
- **Seq**: a run is aborted by its arm's sleep (`pc` to idle); a `seqq`'s
  queue keeps its requests, and its machine handler owes the aborted
  run's credit, paid at the wake (`CatchRole::Machine::clock`).
- **The warning**: the select's definition check warns where an arm
  acts on constants alone (a `<-` or an effectful builtin call reading
  no variable) or holds a `seq` with no trigger: those act at the arm's
  first entry only (`select.rs::warn_birth_only`,
  `warn_triggerless_seq`); not in a seq step, which is born at every
  run (`CFlag::SeqStep`, set on a machine's steps and dropped at a
  lambda), and not in a seq machine's generated handler action.

Pins: `lang::async_restart::{wake_keeps_a_standing_count,
wake_resumes_a_count, wake_resumes_quietly, wake_resumes_standing_timers}`,
`lang::select::arm_const_writes_at_its_birth`,
`lang::printing::{arm_entry_prints_a_constant,
arm_entry_prints_every_time}`, `lang::dense_deltas::no_arm_window_pauses`,
`lang::seqq::{sleep_restarts, aborted_run_returns_its_credit}`,
`lang::seq_abort::sleep_resets_the_machine`,
`lib_tests::db::db_op_not_repeated_at_wake`,
`lib_tests::net::{net_call_not_repeated_after_wake,
net_write_not_repeated_after_wake}`, and the LSP's
`birth_only_arm_actions_are_warnings`.
