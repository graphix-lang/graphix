# Levels and events — the kind model

Status: proposed, 2026-09-03; REVISED the same day (§0); see `pure_select.md` for the alternative that removes sleep instead, which needs only §0's residual kind. The concrete form of P1–P4 in
`seq_blocks.md` §4.2, written for Eric's question: *how can levels and
events be different while still keeping the good features of the
language?*

The one-line answer: keep the runtime's (value, fired) pair exactly as
it is and stop pretending every node has both halves. A **level** is a
node whose value channel is retained; an **event** is a node whose
value channel is not. The kind is inferred, converts in one direction
implicitly and the other only by name, and every rule that today asks
"was this fire a change or an occurrence?" is answered by the kind
instead of by a table. Organic firing is untouched: it is a rule about
the fire bit, and both kinds keep it.

## 0. Revision (2026-09-03, same day — Eric's objection)

> "The very property that made the async part of the netidx-admin
> TUI port effortless is the property you're proposing we eliminate."

Correct. §2–§5 below classify async results (`wait`, an rpc reply, an
http response, `lines`) as events, so `let status = wait(child.proc)?`
would need `hold` before a widget could show it — the plumbing tax
(reflex's `holdDyn` on every event) that the port never paid: a reply,
a probe result, a process exit all become values the graph follows,
from anywhere, whenever they land. Retaining every value IS the
feature. The revision, which supersedes the bullets it names:

- **An async result is a LEVEL with a birth** (K3, §5 class 5):
  absent until it arrives, then a standing value — like a
  subscription, a variable, a file read. Retention stays for all of
  them; nothing in the port's style changes.
- **Only OCCURRENCES lack a standing value**: callback parameters,
  key/mouse events, timer ticks, stream items, rpc-server call
  arrivals, `queue` output. "The last key press" is not state, and
  treating it as state is every phantom the campaign found.
- **The tracker stays** (§4, retracting "the tracker deletes"). For a
  level, conflation is the RIGHT semantics: a change missed during
  sleep is one fact about the present, delivered once — it is what
  lets `toast <- r ~ "done"` in an arm work when the reply landed
  while the arm slept. Under the revision the tracker serves levels
  only; events have nothing standing to deliver, so the pattern-bind
  exclusion (`Bind::pattern`) becomes structural instead of a special
  case. `Bind::facet` (let siblings of a level) stays as is.
- **What the kind still buys**: a compile error where an occurrence is
  read as state (`&e`, `text(&"[e]")`); event selects that are sync
  and pure (K7 — a key handler fuses end to end, no `k ~`); the
  exclusion rules falling out of the type. **What it no longer buys**:
  deleting the tracker; fusing `~` (already withdrawn at K5).
- **Most of the `seq` simplification needs no kind.** Three rulings
  on existing semantics do it: `~` waits on every absence (K5,
  settled); `once(x)` inside an arm IS the entry event (the restart
  contract, today); and, optionally under P4, constants do not
  re-fire at wake with `once(c)` as the spelled tool. None touches the
  type language. The residual kind proposal earns a type only if the
  occurrence-as-state class shows up in the checker's counts (§6 step
  1) as a real bug source — the phantom submit, the landing sibling
  phantom and the `k ~` rule are all of that class — and it is a much
  smaller change than §2–§5 describe. Measure before arguing.

The sections below are kept as written for the record; read them
through this revision.

## 1. What the runtime already has

Under dense delivery every node produces a `TagValue` each cycle:
`Fired(v)`, `Stale(v)`, `FreshBottom`, `StaleBottom`. The fire bit is
the event channel; the value is the level channel. What makes the two
one thing today is that EVERY node retains its last value: `Stale(v)`
is the retained level, and a key press retains the last key press as a
standing value, so a re-woken arm reads it as if it were true now. The
phantom submit (9b2e7231), the sibling-bind phantom (`ev@ \`Key(k)`)
and the whole stale-at-wake ruling are consequences of retaining a
value that had no business being retained. The fix is not a fifth tag.
It is a kind: some nodes retain, some do not, and the type says which.

## 2. The model

**Level** `T`: has a present value or is absent; readers derive from
it; its *change* is an event, `changes(l)`, and its birth (the first
value) is a change.

**Event** `Event<T>`: occurrences, each carrying a payload `T`; absent
between occurrences; no standing value, ever.

**Bottom** is absent. For a level: no present value. For an event: no
occurrence this cycle. One bottom (P2), with no memory of having been
present.

The rules:

- **K1 Derivation.** An expression whose inputs are all levels is a
  level; it recomputes when a consumed input changes. This is organic
  firing, word for word.
- **K2 Occurrence.** An expression with an event input is an event; it
  fires at the occurrence, with its level inputs read as they stand at
  that occurrence. Two event inputs fire together only when both occur
  (today's strict `&&`/`+` over bottoms); merging is explicit (`any`).
- **K3 No implicit latch.** An event never becomes a level on its own.
  `hold(e)` (§7 on the name) latches the last occurrence into a level.
  Reading an event outside its occurrence is absent, by construction
  rather than by rule.
- **K4 Change coercion.** Where an event is required and a level is
  given, the level's change event is taken. `count(l)`, `x <- l`,
  `l ~ v` all mean today what they mean today. This is the rule that
  keeps existing programs compiling.
- **K5 Sample.** `e ~ l` is the value of `l` at each occurrence of `e`,
  and if `l` is absent at the occurrence, its next value: the trigger
  is banked and paid when `l` arrives. Arrival order is not under the
  programmer's control (Eric): a trigger at init or a timer tick
  routinely precedes the subscription or async result it samples, and
  a sample that dropped those would make `f(trigger ~ x)` silently do
  nothing. Today's `Sample` banks exactly this way until the right
  side's FIRST value and drops a trigger during any later absence
  (`held()`'s tainted arm) — the memory of having been present that
  P2 forbids. Under K5 the bank applies to every absence, which is the
  contract `hold(#clock, v)` already documents ("if clock updates when
  no v is held, record the number of times and pass that many through
  when they happen"), so `~` and `hold(#clock: e, l)` become one thing.
  The counter stays, so `~` stays outside fusion.
- **K6 Connect.** `x <- e` writes the level `x` at each occurrence of
  `e`; variables are queues (P5). `x <- l` is `x <- changes(l)` by K4,
  so the loop idiom `x <- x + 1` is unchanged and now reads as what it
  is: on each change of `x + 1`, write it back.
- **K7 Select.** Over a LEVEL the selection is itself a level: arms
  sleep and wake (pause), and their bodies are derivations and effects
  over levels and events — today's select. Over an EVENT the match is
  per occurrence: the result is an event, no selection persists, an
  arm body is evaluated in the occurrence's cycle, and a connect in an
  arm writes at the occurrence with its level RHS read present. The
  `k ~` in `selected <- (k ~ selected) - 1` goes; `selected <-
  selected - 1` inside an event arm is an effect issued by the
  occurrence. An event arm is sync by rule (an async producer inside
  one is a type error: that is a level select or a `seq`), which makes
  the whole TUI key handler a pure function of (occurrence, present
  levels) — fusable end to end.
- **K8 Effects, two classes.** A LEVEL effect follows a level: a
  publication, a widget argument, `tui::suspend(bool)` — its present
  value is what the world sees, reconciled at wake. An EVENT effect
  issues at an occurrence: `println`, `spawn`, `write`, an rpc call —
  a level argument coerces to its changes (K4), and nothing issues at
  wake. The builtin's signature says which class it is.
- **K9 Init and wake.** Birth is a change; wake is not. At wake a
  level arm recomputes from present values (forced recompute, as
  built), event sources resume, nothing replays, and a constant does
  not re-fire because it did not change. Occurrences while an arm slept
  are not delivered to it; `queue(#clock, e)` OUTSIDE the arm is the
  lossless tool, and it needs no tracker because its clock is the
  reader's own. The RESTART builtins (`once`/`take`/`skip`/`uniq`/
  `hold`/`count`) keep their contract — state cleared at sleep — so
  they are REBORN at wake and their birth fires. That is the
  per-selection tool, spelled: `cursor <- once(0)` writes on every
  entry to the arm; `cursor <- 0` writes once, ever. Today's "a constant
  RHS fires once per selection" is this event leaking through
  constants; under K9 it has a name and the leak closes.
- **K10 Errors.** A raise is an occurrence: `catch(e)` binds an event
  and the handler runs at it; `?` on a level raises at each change into
  an error. This is how `catch` already behaves; the kind just says so.

## 3. The cases that drove the rulings

Each of the wake-catch-up witnesses (`wake_catchup.md`), replayed under
the kinds, with no fire bits anywhere:

| witness | today's mechanism | under the kinds |
|---|---|---|
| Eric's table `select cond { true => in0 + 1, false => in0 + 42 }` → 43/2/21/62 | bit consumed at init, stale delivery, forced recompute | `cond`, `in0` levels; the woken arm recomputes from present: 62 (K1, K9) |
| the modal phantom submit | wake reads standing `e` STALE so `t ~ (submitted + 1)` does not tick | `e` is a callable argument, an event: absent at wake, nothing to read (K3) |
| the fork witness (`dyncall-arm-init-stale-aug2026/00`, 1 0 1 1) | `in1`'s bit survives the other arm and delivers a genuine catch-up fire | `in1` is a level; the map arm recomputes to 1 and the select fires on its consulted scrutinee: 1 0 1 1 (K1) |
| the shared-input effect `publish(p2, v)` at wake | forced recompute republishes | `publish` is a level effect: it follows `v` and reconciles at wake (K8); the sleeping arm's standing `p1` stays the open publish/sleep question |
| the landing sibling phantom (`ev@ \`Key(k)`, `k` fired at a nested flip) | pattern binds of every enclosing select excluded from the tracker | `k` is a payload of an event and has no standing value; the inner select sees it only at an occurrence (K3) |
| the accidental counter `count <- count + 1` | no lint (Eric, ledger 3+13) | unchanged, legible by K4/K6 |
| a `let` sibling (`let (a, b) = pair`) | one tracked input, catch-up to every read sibling | `a`, `b` are facets of a level: present at wake (K1) |

And the `seq` atoms (`seq_blocks.md` §4.3), which were the measurement:
the presence select is unnecessary — `f(entry ~ x)` waits on every
run, not only the first (K5); the sibling `entered` variable is
`once(pc)` (K9); the busy gate is a choice between `queue` and drop
(K4/K9); the "never write a constant RHS" rule has nothing left to
avoid (K9); the abort ordering is P5, stated. A `seq` step IS an event
arm (K7) whose completion is the step's own event.

## 4. What the kinds delete

- `Sample`'s asymmetry: `held()`'s tainted-after-first bottom. The
  bank itself stays and applies uniformly (K5).
- The tracker: `TrackedFires`, `Bind::pattern`, `Bind::facet`, the
  catch-up injection, the conflation rule, the at-most-once-per-select
  consumption, the wake bit's fastcall stale-mask gate. Forced
  recompute at wake stays (it is K1 for a node that missed changes).
- The two-era question "fired or stale at wake?" — a level is read, an
  event is absent; there is no tag to choose.
- "Constants fire at init only" as a rule with a wake exception: a
  constant is a level born once.
- The `~` in callbacks and event arms (`click ~ counter + 1`,
  `k ~ selected`) — still legal, no longer required (K7).

What stays exactly: organic firing; sleep as pause (residents survive,
refreshed at wake); activation multiplicity; catch as a handler;
bottom scrutinee ⇒ bottom select (an absent level decides nothing; an
event select between occurrences is absent — one rule, no exception);
the consulted-guard rule; the init-phantom guard (a guard over an
absent level is undecidable, P2); the restart contracts; variables as
queues; strict fusion, with event selects joining it and `~` still
outside it.

## 5. What it costs

The migration is the price, and it is countable before any semantics
changes (§6). The classes:

1. **An event used as a level** — the ONE new error class. `&e`
   (a reference is a place; an event is not one), `text(&"[e]")`,
   `let r = f(trig ~ x)` followed by a level use of `r`. Today's
   meaning of every such program is "the last occurrence", which is
   `hold(e)` — one word, and it says what it does. Results routed
   through a variable (`let res = never<T>(); res <- …`, the port's
   dominant idiom) are already levels and need nothing.
2. **`~` sites that relied on the drop.** A trigger during a
   transient absence of the right side (a derivation that errored, a
   subscription that went away) now defers to the next value instead
   of vanishing. The checker can flag `e ~ l` where `l` is a fallible
   derivation; each is either the intended wait or wants an explicit
   `filter`. Expected rare: the drop was the accident, the wait was
   the contract.
3. **Constant writes in arms** (`screen <- \`Pick`). Write once ever
   under K9; the per-entry meaning is `once(\`Pick)`. Grep-able: a
   literal RHS of `<-` inside a select arm. The port has a handful.
4. **An event source outside an arm read inside it** (`let t =
   timer(..)` outside, `count(t)` in an arm). Ticks during sleep
   conflate to one catch-up today; under K9 they are not delivered
   (a paused counter counts what it saw). All ticks: count outside,
   read the level inside — which is the sentence a reader would
   write anyway.
5. **Every builtin signature says its kinds.** Sources: `timer`, a
   callback parameter, `lines`, an rpc reply, `wait`, an http response
   are events; `subscribe`, `now`-style reads, constants are levels.
   Cadence: `filter`/`uniq`/`once`/`take`/`skip`/`throttle` are
   kind-preserving; `count`/`sum`/`mean`/`hold` produce levels;
   `queue`/`~` produce events. Effects declare their class (K8). This
   is the bulk of the work and also the documentation the stdlib has
   never had.

The typing is per-instance, which the compiler already is: a body
typechecks over its formals as levels, and a CALL whose argument is an
event lifts the instance — the formal's store entry is not retained
and the result is an event. `f(trigger ~ x)` instantiates once for the
event and once for a level caller. Kinds ride the existing
monomorphic instantiation; no new pass ("passes earn their place").

## 6. Plan

1. **A kind checker as a throwaway analysis** — infers kinds over the
   admin TUI, the examples, the stdlib and the findings corpus and
   reports the five classes of §5 with counts and locations. No
   runtime change. This is the go/no-go: if class 1 and 2 are a
   handful, the model is cheap; if they are everywhere, the
   coercion rules are wrong and the numbers say where.
2. **Non-retention for event-kinded nodes**: a store entry that clears
   at cycle end (Bind), `CachedVals` slots that do not stand, `Held`
   residents that do not ride. Delete `Sample`'s debt and the tracker
   in the same change; re-adjudicate every pin in
   `findings/wake-catchup-sep2026/`, `dyncall-arm-init-stale-aug2026`,
   `select-wake-rematch-sep2026` and `default-arg-birth-sep2026`
   against §3 — the expected outcome is identical traces with less
   machinery, and any pin that moves is a case §3 missed.
3. **Event selects** (K7): a select mode, sync arms, per-occurrence
   match, no sleep; the kernel emits it as a pure select.
4. **`seq`** on top (`seq_blocks.md` §10), with the lowering from §4.3's
   right-hand column.
5. Soak each of 2–4 (a semantics change is not landed until it has a
   quiet round-day).

## 7. Open

- **The name of the latch.** `hold(e)` is FRP's word (`stepper`/`hold`
  in Elliott, `hold` in reflex). Under K5 today's `hold(#clock, v)` is
  `clock ~ v`, so the name is free for the latch once the clocked form
  is retired; the alternative is `latch(e)` and no retirement.
- **Spelling `Event<T>`.** A type constructor is the honest form and
  what makes the class-1 error a compile error rather than a runtime
  absence. An alternative is kinds as an analysis fact with no type
  syntax (the runtime behavior changes, `text(&"[e]")` silently shows
  nothing). The typed form is the one that makes invalid states
  unrepresentable; it costs a constructor in the type language and a
  kind on tvars.
- **Two events in one expression.** K2 says simultaneous-only; a
  reader may expect merge. `any` is the merge and the checker can warn
  on `e1 + e2` with two independent sources.
- **The standing publication of a sleeping arm** (`wake_catchup.md`'s
  noted question) is untouched by the kinds and still needs a ruling.
