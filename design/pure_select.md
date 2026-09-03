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

## 11. The always-update variant (Eric, same day)

> Always update every arm of the select, but only update the variables
> in the taken arm. That implementation might be easier than hoisting.
> `select x { null => net::subscribe("/foo"), s => net::subscribe(s) }`:
> today "/foo" isn't subscribed when `x` is null; in this world it
> would still be subscribed, but its output ignored unless `x` is null.
> Change stateful builtins like publish and subscribe to take an
> Option for the path; unpublish/unsubscribe on null.

Nearly equivalent to §3, and simpler to build. The differences are
real and I think each one is a point in its favor:

- **Effect lifetime is explicit, not implied by selection.** Under
  hoisting a subscription in an arm exists while the arm is taken
  (§4's presence rule, an implicit contract per builtin). Here a
  select never turns anything on or off — it picks a value — and the
  program turns an effect off by giving it `null`, which is visible in
  the signature (`subscribe(path: [string, null])`). The presence
  ruling of §4 is unnecessary; a bottomed argument means "no change",
  as it does for every builtin today.
- **A stateful builtin in an arm sees everything.** `count(x)` in an
  untaken arm keeps counting. No entry birth, no §7 delta 1. This is
  the "count outside, read inside" semantics with the count written
  inside.
- **No entry birth for connects.** A connect in an arm writes when
  its RHS fires and the arm is taken. `screen <- \`Pick` writes only
  if the constant fires while taken — at init, if the arm is taken
  then — so the on-entry tool is the sampled form the select chapter
  already documents, `screen <- x ~ \`Pick`, which fires per delivery
  of the scrutinee while taken. The constant-fires-per-selection
  behavior goes, replaced by one explicit spelling. §7 delta 3 (a
  level sampled in an arm fires at every entry) does not arise either:
  `x <- r ~ v` fires when `r` fires, taken or not, and writes only if
  taken. That is the memoryless rule with no births at all.
- **The phantom classes still vanish.** `submitted <- t ~ (submitted +
  1)` in the modal arm: the `~` fires when Enter fires; if the arm is
  not taken the write is dropped; when the arm becomes taken nothing
  fires. No standing value is re-raised because nothing is ever
  re-surfaced — every node saw every fire live. The occurrence kind
  is not needed for correctness here; it remains a diagnostic (an
  event read as a level) if wanted.

**The one thing "always update" cannot mean literally: recursion.**
`select n { 0 => 1, n => n * f(n - 1) }` with every arm updated
dispatches `f(-1)` when `n` is 0, and never stops. An untaken arm
holding a recursive call must not be evaluated, and the clean rule is
§1's: **a pure arm is evaluated only when taken** (unobservable, so
free), **an impure arm is always updated** (observable, so it must
be). Per-arm purity comes from the existing analysis
(`lambda_is_stateless` over the arm's subtree). A recursive call
through an IMPURE callee under its own select is then the one shape
with no meaning — it would be evaluated untaken and diverge — and it
is decidable at compile time (`mark_recursion` knows the cycle, the
effect analysis knows the state), so it is refused with a diagnostic.
The kernel side is unchanged: kernels are pure, their selects are
lazy chains already.

**Implementation.** No AST transform. `Select::update` updates its
impure arms every cycle and its pure arms on demand; the taken arm's
index rides down as a per-cycle context bit (like the frame flags) so
that `Connect` writes only under a taken arm, nested selects
conjoining; `sleep()` and everything in §5 goes; `subscribe`/`publish`/
`timer`/`suspend` take `[T, null]` keys and tear down on `null`. The
interp's per-cycle cost grows by the walk of impure untaken arms
(organic ride-skips make it a tree walk, not a recompute) — measurable
on the admin TUI with `milestone_latency`, and bounded by fusion: a
pure screen is a kernel whose walk is one fire-bit check.

**Verdict.** Prefer this over §3. It has one fewer implicit contract
(presence), one fewer special case (entry birth), one fewer dependency
(the kind), and a smaller implementation. What it asks of the
programmer is exactly one thing: an effect whose lifetime should
follow a selection must be told so, with `null`.

## 12. Open: is dense bottom still necessary?

Eric, same day: "Before JIT we were getting along just fine with no
Bottom representation besides update returning None. I think we may
have added Bottom when we were trying to fuse stateful kernels and it
may not even be necessary anymore."

The record (`dense_delivery.md`, built 2026-08-13) gives three defects
of the sparse currency and one structural reason the tag could not be
removed. Read against what has been deleted since, each stands on a
leg that is gone or going:

| reason for dense delivery | what it stood on | status |
|---|---|---|
| tag-blindness: 81 raw `Apply` builtins read `Some(_)` as fired — `once` burned on a stale RE-SURFACING, `count` over-counted, `print` duplicated | standing values re-surfaced at arm wake and frame re-derivation — i.e. SLEEP | gone under §1/§11: every awake node sees every fire live; nothing is re-surfaced |
| "the tail spine's becoming-selected path needs 'emitted, but not an event' — a second bit by definition" | stateful interiors inside tail-loop frames and re-selected arms | gone: arms are pure (a tail loop's body is a select), and a stateful body was never collapsed to one activation anyway |
| two bottoms (`None` vs tainted production), taint propagation R3, "bottom never reaches authors" | poisoning consumers so that stateful kernels and their residents would not RIDE a pre-bottom value | strict fusion deleted the residents, the replay and DynCall; a pure kernel has nothing to ride |
| the empirical capstone, `fire_gate_missing_fire_aug08d` | a DynCall fire gate (`dyncall-fire-gate-aug2026/00`) | DynCall is deleted; the pin must still AGREE, but it no longer argues for the currency |

The hypothesis, then: with pure select and strict fusion, the interp
can return to `Option<Value>` — `None` is "nothing this cycle",
consumers keep the one cache they always had — and the two bits
survive only where they are load-bearing, at the KERNEL BOUNDARY: the
wrapper's per-input cache is empty (never produced) or full, and an
input fired this cycle or did not. Same bits, one place.

What it would delete: the four-state algebra and `TagView`, R3 bottom
propagation, `FreshBottom`/`StaleBottom` (consumed in 12 compiler
files today, densest in `collection.rs`, `tval.rs`, `op.rs`), the
bottom-scrutinee and consulted-guard rulings (a quiet scrutinee is
quiet; a guard that never produced leaves the select waiting, which is
what it does now), the strict-`&&` ruling, "bottom never reaches
authors", the FreshBottom-only logging rule, `Bind`'s quiet
re-publish, the phantom resident. What changes: "became absent" is
inexpressible — `1/0` logs and does not update, and consumers keep
their last value, which is the pre-dense semantics and is consistent
with the language's actual answer to failure (errors are VALUES;
`?`/`$`/checked arithmetic). A collection whose callback produced
nothing for a slot waits for the slot, as it did before.

What must be checked before believing it: the `builtin-taint-gate-
jul2026`, `taint-cache-callee-jul2026` and `dyncall-fire-gate-aug2026`
pins under the new currency (AGREE is required; the mechanism they
pinned is gone); the fuzz oracle's trace, which records tags; whether
any bench program's fusion depends on taint bits reaching the kernel
mid-region (a `?` inside a kernel raises onto the delivery queue and
needs no taint — `QOP_RAISES`); and the interp's per-cycle cost, which
sparse delivery should reduce (no ride-downgrade walk on quiet
cycles). Sequence: pure select first (it removes two of the four
legs), then a sparse-currency prototype behind the same corpus gate,
as its own design doc.

## 13. Interaction with `seq`, and what it relieves in the port

### 13.1 `seq` under always-update

The `seq_blocks.md` §7 lowering — a select over a step variable `pc`
with effectful arms — leaned on sleep to gate each step's effects.
Under §11 an arm gates nothing but connects, so the lowering must key
every effect explicitly, and the explicit form is smaller than the
`pc` machine:

- A straight-line `seq` is a `~` CHAIN: each step's leaves are
  sampled on the previous step's completion event, which is an event
  by construction (an async step's first production, a same-cycle
  effect's issue, a pure step's derivation). No `pc`.
- R1 (drop retriggers while busy) is one variable: `busy <- start ~
  true`, cleared by the last step's completion and by the handler, and
  `filter(trigger, |_| !busy)` at the head.
- Abort is the catch: a failed step produces nothing, so no later step
  is ever triggered; the handler runs the user's cleanup and clears
  `busy`. Nothing to unwind.
- A branch is a pure select choosing which chain's head trigger fires;
  a loop is `any(entry, back_edge)` as a step's trigger; `until c` is
  `select c { true => c, false => never() }` — the port's `go`.
- `~` waits on every absence (K5), so no presence select anywhere.

The port's privileged handoff is already this form, by hand
(`local.gx` ~690–720): `suspended <- cmd ~ true`, `let released =
tui::suspend(suspended)?`, `let go = select released { true =>
released, false => never() }`, `println(go ~ …)`, `spawn(options(#args:
cmd.args, go ~ cmd.program))?`, `wait(child.proc)?`, `suspended <-
status ~ false`. So under §11, `seq` is exactly "the compiler writes
the `~`s, the busy flag and the catch scope, and names the steps" —
the same construct as proposed, with a lowering a reader can check by
eye. Of §4.3's atoms: the presence select is gone (K5); the `entered`
variable is gone (a step's trigger IS the previous completion); the
busy gate stays as one variable; the constant-RHS rule has nothing to
apply to (every leaf in the chain is sampled); the abort order is P5.

They are a package. §11 alone makes the ceremony class HARDER to
hand-write — an effect not keyed on an event issues whenever its
arguments fire, and the arm no longer saves you — and `seq` is what
writes the keys. `seq` alone, on today's semantics, needs §7's `pc`
machine and its atoms. Together, the port's chains become blocks, and
the blocks lower to what the port already writes.

### 13.2 What it relieves in the port

Against the campaign's findings, honestly:

1. **The sleep/wake phantom class** — the phantom submit, the
   sibling-bind phantom, the select wake re-match, the default-arg
   birth, a month of the fired/stale seam flip-flopping. The largest
   engine-side sink of the campaign. §11 deletes the class. On the
   port's side these cost test-driven diagnosis, not code: every fix
   was an engine fix, and the port changes nothing here.
2. **The sampling discipline** — 126 `<- x ~` sites; "handlers sample
   the event". NOT relieved: `x <- f(x)` loops are inherent, and an
   effect keyed on an outside level must sample. But it becomes THE
   rule instead of one of two (constants fire per selection vs
   sampled), which is what makes it teachable. Three genuine constant
   writes in the port (`toast <- null` landing.gx:173, `policy_open <-
   true` panels.gx:927, `gate_sel <- 0` panels.gx:967) go silent under
   §11 and need `x ~`. A constant RHS of a connect inside an arm is
   DEAD CODE under §11 — it never fires after init — so a diagnostic
   for it is a dead-write warning, not the lint on a working tool that
   ledger 3+13 declined.
3. **The `run` ladders** (the ceremony dispatch, local.gx 455–500,
   755–790): `x@ \`Renew => renew(x)` keys the effect on the arm's own
   bind, which is absent unless the arm matches. Already
   always-update-safe. Unchanged.
4. **The outer-bind dispatches** (app.gx 24–25, remote.gx 117–119,
   local.gx 1259–1262: `\`Landing => land.handle(ev)` with `ev` bound
   by an ENCLOSING select): under §11 every screen's handler runs on
   every key — the arm is impure, so it is always updated — with its
   connects suppressed by the taken bit, which therefore must
   propagate into callees dispatched from an arm (a per-cycle context
   bit, like the frame flags). Correct, but the per-key cost becomes
   the sum over screens' handlers (the role-menu key is 3.2ms today).
   The shape §11 wants, and the one "select decides what data flows"
   names, is to route the KEY: `let ev_for = select screen {
   \`Landing => \`Landing(ev), \`Connect => \`Connect(ev), … }` and key
   each handler on its own tag. Three sites; measure with
   `milestone_latency` either way.
5. **Panel loads**: keyed on open/refresh already and carried by the
   question bus (panels.gx issues no effect directly). Unchanged; no
   "every panel loads at connect".
6. **The chain class** (the handoff; install/finish/uninstall): relieved
   by `seq`, not by §11; §11 makes `seq` smaller (13.1).
7. Everything else on the ledger — `never<T>`, the ⊥-seed rule,
   coverage distribution, the union rectangle, `could_match`'s Fn arm,
   place references, `tui::form`, the parser's furthest point, compile
   time — is orthogonal.

**The one rule §11 must add.** Which effects the taken bit gates.
Connects, certainly (Eric's words). Level effects never (his example:
the untaken `subscribe` stays live). Event effects in an untaken arm —
a `println`, a `spawn` keyed on an OUTSIDE event? Either (a) gate them
too — "an untaken arm's outputs are ignored: its writes and its
issues", the natural reading of "select decides what flows", and it
makes `\`A => spawn(k ~ cmd)` safe — or (b) gate connects only and
rely on the sampling discipline. The port is safe under either (its
event effects are keyed on binds). (a) is the safer default and needs
the level/event effect distinction declared per builtin, since
`Effect::Async` covers both `subscribe` and `spawn` today.

**Verdict.** §11 relieves the engine's pain nearly entirely and the
port's pain barely at all — because the port was already written in
§11's discipline, taught by the bugs §11 removes. The port's remaining
pain is the chain class, which is `seq`'s, and `seq` is smaller under
§11. The port's bill: three constant writes become sampled, three
dispatches route the key, and a latency measurement.

### 13.3 The ledger, classified (both `seq` and §11 adopted)

The admin campaign's findings file has 50 dated entries plus the
open-items ledger. Sorted by what solves them:

| class | entries | solved by |
|---|---|---|
| sleep/wake semantics (phantom replay, sibling-bind phantom, constant-RHS-per-selection; ledger 4, 13) | 5 | §11 — the class is gone |
| sequencing (the privileged handoff; the ceremony `Trigger` stall; the accidental counter inside a chain) | 3 | `seq` — the handoff is a block; a step samples every leaf at entry, so a self-read inside a step cannot loop |
| types, coverage, diagnostics, the compiler (reserved words ×4, slice coverage, def/use resolution, abstract predicates, or-captures, the rectangle, the caller's env, `never()` typing, `Error as _`, `[fn, null]` arms, payload narrowing, bool pooling, set-contains residue, datetime arithmetic, the fastcall sweep, place references) | ~22 | neither — all fixed as they came, except ledger 5 (arithmetic traits) and "no union subtraction in select" |
| package and tui capabilities (overlay, suspend, the question pump, forms, services, `tui::exit`, the harness, deps) | ~12 | neither — port work, done |
| perf and scale milestones | 4 | neither — measured; the fusion pre-gate is open |
| test-side | ~4 | neither |

By count, the two together close about 8 of 50. By cost they close
the deepest class (the wake seam, a month of engine churn whose last
two rounds were the port's) and the one that recurs by construction
(every ceremony is a chain).

What I listed as remaining, and Eric's rulings on each (same day):

- The option-narrowing ladder `select x ~ y { null as _ => never(),
  v => … }` — 54 sites — is `opt::or_never(x ~ y)`, which exists in
  core, is `Stateless` with a fast fn, and the port never used
  (`opt::` appears zero times). Not a language gap: the quick
  reference had no `opt` entry. Fixed there; the port's 54 sites
  convert mechanically.
- Sampling is the nature of the beast. Making the async part
  effortless in exchange for some sample points is the trade, and
  `seq` keeps it from getting out of hand. Not pain; not to be
  designed away.
- The self-loop is the same: the nature of the beast, and useful once
  understood.
- Fusion: this application will be started by netidx-tools, which sets
  up a shell like the browser's. If fusion is not worth it for a UI,
  that shell turns it off (`CFlag::FusionDisabled` through
  `ShellBuilder::enable_flags`, the switch `--no-fusion` already uses),
  and probably the browser should too. Fusion is a deployment decision
  of the embedder: on for stream processors, off for UIs. The
  pre-gate stops being a UI item.
