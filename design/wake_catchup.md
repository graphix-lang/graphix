# Wake catch-up

Status: built 2026-09-01
Pins: `graphix-fuzz/findings/wake-catchup-sep2026/`, `dyncall-arm-init-stale-aug2026/`, `default-arg-birth-sep2026/`, `select-wake-rematch-sep2026/`; `stdlib/graphix-tests/src/lib_tests/callable.rs` (`arm_wake_delivers_standing_args_stale`, `callable_body_flip_reads_standing_key_stale`); `stdlib/graphix-tests/src/lang/select.rs` (`select_sibling_binds_spent`, `let_sibling_binds_spent`)
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

1. **Every select tracks one fire bit per arm input** — the union of
   its arm bodies' free refs (binds defined outside the arm). Bits are
   set when a tracked input fires soundly, whatever the selection
   state, including no-arm windows (bottom scrutinee, undecidable
   guards). Bits OR-accumulate: N fires during a sleep conflate to ONE
   catch-up at the latest standing value (`queue` is the lossless
   tool).
2. **Fire state is consumed by evaluation.** Whichever arm evaluation
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

Outside the mechanism: the scrutinee and guards (their own consult
machinery); pattern binds of THIS select and of every enclosing select
— a pattern bind is a facet of its arm's scrutinee delivery, which that
arm's match consumed, so `k` beside ``ev@ `Key(k)`` is never re-raised
at a nested flip after the `ev` reader handled the key. A destructuring
`let`'s siblings ARE tracked, as one input: a `let` bind is a real
input whose catch-up is wanted, but its siblings are one delivery.
Nesting needs nothing extra: an outer arm's ref set includes everything
under it, so an inner select's missed window is covered by the outer
bits; the wake evaluation carries the tags down and the inner select
routes and consumes with them.

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
  a sample, an `any`, or a stateful/async callee makes an arm impure,
  and impure arms still sleep.

## Mechanics

### The tracker (`node/select.rs`, `TrackedFires`)

- `per_arm`: each arm body's free refs (`Refs` referenced minus bound
  within the arm, minus pattern binds — `Env::is_pattern_bind`, marked
  at `Select::compile` for each arm's structure-predicate ids), keyed
  by the input they are tracked under (`Env::facet_of` maps a
  destructuring `let`'s siblings to the group's representative).
  Computed from compile-time refs at first update and REFRESHED AT
  EACH DESELECT — compile-time refs cannot see through a lambda literal
  into an instantiated body; deselect-time refs can.
- `pending`: sound fires no arm evaluation has consumed. `observe`
  records this cycle's fires before any routing or early return;
  `deliver` injects the unconsumed bits an arm reads into
  `event.variables` as FIRED entries at the standing value, scoped to
  exactly that arm's evaluation (`restore` afterwards), and clears
  them. Semantic state: survives sleep and `reset_replay`; frames are
  excluded (a framed pass runs against private maps — loop plumbing,
  not the reactive world). The bits record only fires that HAPPENED in
  the awake graph; a paused async producer produces nothing to catch
  up and resumes on wake.

### Sleep state is local

State lives in nodes, never in an `ExecCtx` field (parallel
module-level compilation is coming and a parallel evaluator must stay
possible, so nothing may end up behind a lock). Every skip-owning
node/Apply owns a `slept: bool` its own `sleep()` sets and its next
depth-0 update takes: the `dense_gate!` structs (the macro takes
`$self.slept`, so the field is macro-enforced), the op macros,
StringInterpolate, MapQ, Bind, CallSite, GXLambda, `CachedArgs`,
`Kernel` (a kernel is a node), and Select. `Node` stays a bare 16-byte
newtype; the bools hide in struct padding. Nodes that recompute
unconditionally need none; `Any` and `~` ride correctly — they ARE
edge state; `Constant` fires at wake as at init.

A woken Select RE-MATCHES against the present scrutinee: a selection
retained across the sleep was made against a value that may have moved
while no reader was awake (an arm-local `<-` counter has no tracked
fire, since arm locals are not arm-body inputs).

### The value channel re-reaches the store

A wake's recomputed STALE values must be republished or readers
downstream of a publish seam still ride: `Bind::update` re-publishes a
quiet wake production (`<-` targets holding a value are still held
back — sleep is pause), `CallSite` refreshes its arg ids' standing
entries, `GXLambda` re-seeds its formals, MapQ rebuilds its collection
from the refreshed slots.

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
wake VIEW: wire slot 0 bit 2 = WAKE (the arm's `event.wake_init` or the
kernel's own `slept` bit, depth 0 only); genuine init is `bit0 &
!bit2` and is what gates the fastcall stale-mask suppression, so a wake
delivers standing args STALE and the trampoline produces the stateless
re-eval's STALE result. Frames are excluded on both engines: every wake
predicate is depth-0 only, which keeps the frame-formal init-view seed
intact.

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
constants fire at init only; the guard rules; `ByRef`'s stale seed;
residents survive sleep and are REFRESHED at wake rather than surfaced;
shrink-is-delete for recursion (a fresh activation has phantom
residents and takes the ordinary first-production path).
