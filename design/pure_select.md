# Pure select — arms are pure, sleep disappears

Status: proposed, 2026-09-03. Eric's alternative to the kind model of
`levels_and_events.md`:

> Allow only pure code in select arms. Sleep disappears. Select
> becomes a pure "selection" mechanism, deciding what data flows. It
> greatly simplifies a lot of things; I've toyed with it and was
> worried that it was too limiting, but it's worth exploring.

This document explores it. The conclusion up front: it is the right
cut, it removes more than the kind model did, and the "too limiting"
worry is answered by DEFINING impure code in an arm as sugar for the
pure form — the surface keeps today's convenience, the semantics is
the pure one, and the corner cases that were confusing are exactly the
ones whose meaning changes. The kind (occurrence vs level) survives in
a small, load-bearing role: it is what makes the sugar correct for
event samples.

## 1. The rule

A select arm is pure computation over the arm's inputs. Nothing in an
arm has state across cycles, issues an effect, or changes cadence.
Consequences:

- **Evaluating an unselected arm is unobservable**, so the runtime does
  not do it. That is not sleep: nothing is paused, resumed, retained
  or caught up, because a pure arm has no history. When the selection
  moves, the newly taken arm is computed from its inputs as they stand.
  This is also what keeps recursion finite (`select n { 0 => 1, n => n
  * f(n - 1) }` never demands `f(-1)`), so laziness is required, not an
  optimization — and it is free to require because it cannot be seen.
- **A select is a mux.** Its output is the taken arm's value; it fires
  when a consumed input fires (the scrutinee, a consulted guard, the
  taken arm's inputs) — organic firing, unchanged. Bottom scrutinee ⇒
  bottom select, and the consulted-guard rule, are unchanged.
- **Absence is how a select turns something off.** An arm that yields
  `never()` flows absence downstream; an effect fed by that absence is
  off (§4). Where sleep used to pause a subscription, the arm now stops
  supplying its path.
- **Effects, state, `~` and `<-` live at block level**, fed by selects
  and feeding them.

## 2. What "pure" is

The line already exists: `Effect::Stateless` — the strict-fusion
vocabulary. In an arm: operators, casts, accessors, `?` (a raise onto
the delivery queue is how the kernel already treats it), literals,
constructors, `never()`, references and widget values (a widget tree
is data; its callbacks are lambda values, invoked by the runner at
callable level), pure lambdas (`lambda_is_stateless`), nested selects,
and the benign effects strict fusion already tolerates (`print`,
`log`: emitted once whichever instance runs them).

Not in an arm: `Sync` builtins (`count`, `sum`, `once`, `uniq`,
`window`, …), `Async` builtins (`subscribe`, `timer`, `spawn`, rpc,
http, …), `~`, `<-`, `catch`, and any lambda that reaches one of
those. Pure is not the same as fusable: references and decimal are
pure and still node-walk. Every select is pure; every pure select
whose vocabulary the emitter covers is a kernel.

## 3. The sugar: impure code in an arm hoists

Without this, the port's handler layer (`local.gx` alone has 114
connects, most inside arms) would be a rewrite into actions-as-data.
With it, the same source compiles, and its meaning is the hoisted
form.

**The gate.** For a select `S` and its arm `i`, `taken_i` is the level
"arm `i` is the selection" (absent when the selection is undecidable).
For an expression `a` written in arm `i`, `gate_i(a)` is `a`
restricted to `taken_i`: equal to `a` while the arm is taken, absent
otherwise, firing when `a` fires while taken. **Gating a level births
it at entry** — when `taken_i` becomes true, `gate_i(a)` fires once
with `a`'s present value. **Gating an occurrence does not**: an event
(a callback parameter, a pattern bind of an event scrutinee, a tick, a
stream item) gated by an arm fires only at its own occurrences while
the arm is taken. This is the one place the kind is load-bearing, and
it is the same fact the tracker's `Bind::pattern` exclusion encodes by
hand today.

**The hoists**, each to the nearest enclosing non-arm block, with the
composed gate when selects nest (a gated gate is a gate on the
conjunction of takens):

| written in arm `i` | means |
|---|---|
| `x <- rhs` | `x <- gate_i(rhs)` at block level |
| `f(args)`, `f` impure | `let t = f(gate_i(args))` at block level; `t` read in the arm |
| `e ~ v` | `gate_i(e) ~ gate_i(v)` |
| `count(x)` (any `Sync` builtin) | `count(gate_i(x))` |
| `catch(e) h` | `catch(e) h` at block level, its dynamic scope kept over the arm's `?` sites |
| `let x = init; x <- …` (arm-local state) | the binding moves to block level under a hidden name; same lifetime as today (state survived sleep) |

Hoisting is recursive: an impure subexpression inside a pure one
hoists first (`(k ~ selected) - 1` becomes `let t = gate(k) ~
gate(selected)` and the arm reads `t - 1`). `--expand` prints the
result, as it will for `seq` — the legibility tool for "what does my
select mean".

**What the sugar preserves, by the gate rule:**

- `screen <- \`Pick` in an arm: `gate(\`Pick)` fires at entry only —
  today's "constant RHS fires once per selection", the tool Eric kept
  (ledger 3+13), now the general rule for every level in an arm rather
  than a fact about constants firing at init.
- `selected <- (k ~ selected) - 1` with `k` a key: `gate(k)` is an
  occurrence, no entry birth, so the write happens on each Up while
  the arm is taken and never at entry. No phantom, no tracker.
- `x <- n` with `n` a payload bind of a level scrutinee: fires on each
  delivery of `n` while taken, as today.
- `select p { null => 42, p => subscribe(p) }`: `let s =
  subscribe(gate_2(p))` at block level; the arm reads `s`. The
  subscription exists while the second arm is taken (§4), which is
  what sleep-teardown did.
- The counter `{ let x = 0; select x { n if n < 10 => x <- n ~ x + 1,
  _ => never() }; x }`: `x <- gate(n) ~ gate(x + 1)`, which is `x <-
  x ~ (x + 1)` while `x < 10` — the loop idiom, one connect, outside.

## 4. Presence-driven effects

Sleep is what tears down a level effect today: `Timer::sleep` unrefs
the timer, the six sys::net hooks drop subscriptions and publications,
and `net_{subscribe,publish}_arm_rewake` re-establish from present
args. Without sleep, the same contract rides presence: **a level
effect exists while its inputs are present and is torn down when they
go absent.** `subscribe(⊥)` unsubscribes; `timer(⊥, _)` stops;
`publish(⊥, _)`/`publish(_, ⊥)` unpublishes; `tui::suspend(⊥)`
resumes. The wrapper today bottoms an invocation without calling
`eval` on a bottomed arg (bottom never reaches builtin authors); a
level effect needs the seam to see the transition to absent — a
`sleep()`-shaped hook renamed to what it is. The nine `fn sleep`
implementations in sys::net and sys::time become nine "on absent"
hooks; the `Apply` trait loses `sleep`. Event effects (`spawn`, an rpc
call, `write`) issue when their gated args fire: at entry for level
args (the birth), at each occurrence for event args. This settles the
open question `wake_catchup.md` left: a sleeping arm's standing
publication is no longer standing, because the arm no longer supplies
the value.

## 5. What disappears, what stays

Disappears — the concept of sleep and everything that exists to
reconcile it:

- 115 `fn sleep` implementations across the compiler, runtime and
  stdlib; the `Update::sleep` and `Apply::sleep` methods; `Node`'s
  guarded sleep funnel.
- Wake catch-up entire: `TrackedFires`, `Bind::pattern`,
  `Bind::facet`, the conflation rule, the at-most-once consumption,
  the `slept` bits in the `dense_gate!` structs, `Event::wake_init`,
  the wire's wake bit and the fastcall stale-mask gate, the forced
  recompute (a pure arm computes; there is nothing to force).
- Sleep as pause: `Held` residents at the scrutinee and guard ride
  sites, `CachedVals` staging across sleep, the resident refresh at
  wake.
- The restart-on-sleep contract of `once`/`take`/`skip`/`uniq`/
  `hold`/`count` (they are not in arms; at block level they never
  restarted).
- "Constants fire at init only, and again at wake"; the select
  chapter's "Writing From an Arm" as a list of cases (it becomes one
  sentence: an arm's writes and effects are gated by the arm, and a
  gated level is born at entry).
- The select wake re-match (`select-wake-rematch-sep2026`), the
  sleep-preserves-caches, arm-local-bind and sleep-restart-gate
  finding classes — as CLASSES, not just pins.
- Fusion residue: `Sync` builtins in arms can no longer block a select
  from fusing, because they are not in arms.

Stays: organic firing; bottom scrutinee ⇒ bottom select; the
consulted-guard rule; `~` waiting on every absence; variables as
queues; catch as a handler; activation multiplicity — with one
implementation change: the interp reclaims unreached recursion
activations by reach stamp (the kernel's mechanism) instead of through
`CallSite::sleep` under `shrink_unwind`, since there is no sleep to
route it through. Semantically nothing moves there: an unreached
activation of a pure arm was never observable.

## 6. Worked examples

**The TUI handler** (`input_handler`'s `handle`):

```graphix
let handle_event = |e: Event| -> [`Stop, `Continue] select e {
    `Key(k) => select k.kind {
        `Press => select k.code {
            k@`Up if selected > 0 => { selected <- (k ~ selected) - 1; `Stop },
            _ => `Continue
        },
        _ => `Continue
    },
    _ => `Continue
};
```

Expands to a pure three-level select returning `` `Stop ``/`` `Continue ``
and, in the lambda's body block, `selected <- gate(k) ~ gate(selected -
1)` with the composed gate (Key ∧ Press ∧ Up ∧ guard). `e` is a
callable parameter, an occurrence; so is its facet `k`. The select is
a kernel; the connect fires per Up.

**A screen effect**: `select screen { \`Confirm => spawn(cmd), _ =>
never() }` — `let r = spawn(gate(cmd))`; spawns at entry to Confirm and
whenever `cmd` changes while there; a second `screen <- \`Confirm`
delivery with the arm already taken does not re-spawn (no entry, `cmd`
unchanged). Today's constant rule gave the same for the constant case
only.

**The modal**: `select screen { \`Modal => submitted <- t ~ (submitted
+ 1) }` with `t` the Enter key. `gate(t)` is an occurrence: no birth at
entry, no phantom. The 9b2e7231 class is unrepresentable.

**Eric's table** `select cond { true => in0 + 1, false => in0 + 42 }`,
`in0` 1 then 20, `cond` false → true → false: 43, 2, 21, 62 — the false
arm is recomputed from `in0 = 20` when re-taken, because it is pure and
that is all "recomputed" can mean. No bits.

**Recursion**: `let rec f = |n| select n { 0 => 1, n => n * f(n - 1) }`
— pure arms; only the taken arm is demanded; unreached depths are
reclaimed by reach stamp. Fuses as today.

## 7. Deltas from today, stated

The sugar reproduces today's meaning in the common cases (§3). Where
it does not:

1. **A `Sync` builtin in an arm sees the entry birth.** `count(x)` in
   an arm increments at entry (the gated `x` is born) and on `x`'s
   changes while taken. Today it increments at wake only if a fire was
   missed and unconsumed. Memoryless (P2) and simpler; the old
   behavior was the tracker.
2. **`once`/`take`/`skip` in an arm do not restart.** `once(x)` is
   once ever. The per-selection idiom is not needed for entry — any
   level read in an arm is born at entry, so `y <- x` in the arm
   already writes at entry — and "first change while taken" is rare.
3. **A level sampled in an arm fires at every entry.** `x <- r ~ v`
   with `r` a level (an rpc reply): at each entry where `r` is
   present, the gated `r` is born, the sample fires, `x` is written.
   Today: once if `r`'s arrival was missed and unconsumed, never
   otherwise. This is the one behavior the pure model cannot reproduce
   without memory of what a reader saw, and P2 says not to. A reply
   that should act once acts where it arrives — at block level — not
   inside an arm that may be away.
4. **An effect inside an impure callee that does not depend on the
   gated args stays live while the arm is not taken.** The callee is
   hoisted to block level with gated args; its body's effects that
   derive from those args go absent with them; an effect on constant
   args inside the body (a timer in a handler) does not. Today the
   whole instance slept. The fix, where it matters, is to gate the
   effect's input — which is also the sentence a reader would write.
5. **Publications and subscriptions of an untaken arm are torn
   down**, not paused. Today's sleep tore subscriptions down too; a
   publication's standing value was the open question.

None of these is a program in the port as far as I can tell; the
measurement is the plan's step 1.

## 8. Where it is limiting

- **Without the sugar**, every write in an arm is a select per
  written variable at block level: the actions-as-data discipline
  (Elm/Redux), workable at scale but a tax on exactly the handler code
  the port is made of. The sugar removes the syntax tax; the
  DISCIPLINE is still what the semantics is, and `--expand` shows it.
- **Per-selection state** (a counter that restarts each time an arm
  is entered) has no arm-local spelling; it is block-level state reset
  by the entry event: `n <- gate(0)` written in the arm resets it at
  entry, which reads fine.
- **The kind is still required** for gated occurrences (§3); without
  it, `gate(k)` would birth the last key at entry and the phantom
  returns. The residual kind proposal of `levels_and_events.md` §0
  (callback parameters, event-scrutinee pattern binds, ticks, stream
  items, `queue` output have no standing value) is exactly the part
  this design needs, and no more.
- **`~` inside arms is gone** as a semantic position (it hoists). A
  `~` deep inside a pure expression was never meaningful; the hoist
  gives it the one meaning it could have had.
- **The bottom-out cliff**: an arm computing from an absent input
  yields absent, and an effect fed by it turns OFF. Today an effect in
  an arm whose arg bottomed kept its standing state (sleep is pause;
  a bottomed arg does not call `eval`). Under presence a transient
  absence of a subscription path is a resubscribe. `hold` on the
  input is the tool, as it is for the scrutinee today.

## 9. Relation to `seq` and to the kind model

`seq` (`seq_blocks.md`) lowers to a select with connects and effects in
its arms; under this design the sugar hoists them and the lowering is
correct as written. The better target is the hoisted form directly: a
pure TRANSITION select (`pc <- select (pc, inputs) { … }`, one kernel)
plus presence-gated issuers (`spawn(select pc { \`A3 => cmd, _ =>
never() })`) — a Mealy machine the compiler writes. §4.3's atoms:
the presence select is gone (`~` waits), the `entered` variable is the
gate's birth, the busy gate is unchanged, the constant-RHS rule is the
gate rule, the abort ordering is P5.

Against `levels_and_events.md`: that model kept sleep and made the
wake rules structural for occurrences; this one removes sleep and
needs the occurrence kind only at the gate. It deletes more (the
tracker, `Held`, the restart contracts, 115 sleep implementations) and
adds less (a hoist pass, presence hooks, one kind fact). It also draws
the fusion line and the language line in the same place: a select is
a kernel, and everything that is not pure is wiring.

## 10. Plan

1. **Measure**: the hoist as a printer (`--expand` over selects) run
   on the admin TUI, the examples and the findings corpus — count the
   hoists by kind (connect / effect / `~` / `Sync` builtin / catch /
   arm-local state), and flag §7's deltas where they occur (a `Sync`
   builtin in an arm; a level-sampled `~` in an arm; an ungated effect
   in a hoisted callee). No runtime change. Go/no-go.
2. **Hand-hoist one screen** of the port (the remote tab's connect
   form) and run it — the ergonomic check Eric's worry is about.
3. **Presence rulings** per level effect (sys::net's six, sys::time's
   three, `tui::suspend`, the gui/tui runners' subscriptions), each a
   pin.
4. **Build**: the hoist as an AST→AST pass at compile (like `seq`'s
   desugar), the gate as a compiler node (a select over `taken_i` with
   the birth rule and the occurrence exemption), delete sleep; the
   interp's reach-stamp reclaim for recursion; re-adjudicate every
   sleep/wake pin against §6–§7 (expected: identical traces except the
   §7 deltas, each of which becomes a new pin stating the new rule).
5. **Soak**, a quiet round-day before "landed".
