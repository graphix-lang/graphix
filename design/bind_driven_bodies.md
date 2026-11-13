# Bind-driven bodies

How a select arm, a guard, a collection slot and a function activation are driven. Sleep itself (state
survives, async calls are aborted, a wake restarts nothing) is `sleep_and_wake.md`; the seq machine is
`seq_blocks.md`.

## 1. The rule

**Entering a body is not an event, and neither is waking one.**

A body is a select arm, a guard, a collection slot or a function activation. A read inside a body has the
tag its producer gives it in this cycle. Entry and resume add nothing to it.

| Read | Fires in the body when |
|---|---|
| A pattern bind | the scrutinee fires (a tuple scrutinee: any member) |
| A slot's element | the collection's source fires, the element changed or not |
| A fold's accumulator | what feeds it fires: the init for slot 0, the slot before it |
| A formal | its argument fires |
| A capture | it fires in a cycle the body runs |
| A constant | in the cycle it leaves bottom: a birth |

An omitted default is its expression, evaluated in the callee: a constant default is a constant, and
`#d = c` is a read of the capture `c`.

What follows:

1. **No memory.** Nothing is recorded for a body while it does not run. No fire is kept for an arm, a
   guard, a paused slot or an absent call. A shrink deletes the callee.
2. **Births.** Every value is bottom before its program's first cycle, and a value that leaves bottom
   fires (`organic_firing.md`). A birth is a cycle in which that happens: a root's first cycle (a script,
   each REPL input, a dynamic module's load) and a seq step's run. In a birth everything reached fires, in
   every body entered in that same update, since its constants are leaving bottom then. A body entered at
   any other time is entered into a world that stands: its constants left bottom when its program started,
   so they never fire, and what it reads stands stale. A new activation, a regrown recursion depth
   included, has fresh state and is entered, not born.
3. **A resume recomputes.** A body that ran before and runs again recomputes from what stands. Its state
   survived the sleep.
4. **A wake fires nothing for anyone.** What a resume or an entry recomputes is stale to every reader:
   inside the body or outside it, awake or waking, in the same cycle or the next. A reader outside takes
   the new value and no event.
5. **Values and events.** A node that computes from values has a value in a body entered later, stale: an
   operator, a structure, a pure select, a `let`, a pure call, a `Stateless` builtin, a builtin that takes
   no trigger, an Io call's result (the call does not perform). A node that consumes events gives nothing
   until an input fires: `~`, `any`, `<-`, a raise to a handler, the act of an Io call, every `Sync` or
   `Async` builtin. The split for a builtin is its `BuiltIn::EFFECT`.
6. **The site is outside the body.** A select's result fires when its scrutinee or a consulted guard
   fires; a call's when its function or its body's result fires; a collection's when a slot fires, or its
   source fires and the collection resized, is empty, or gives back the source's elements (a filter, a
   find). A resize, or a source's return from bottom, is no fire of its own. A site entered or
   re-routed while nothing fires gives a quiet result.
7. **Guards.** A guard runs on every run of its select where the scrutinee is present and its arm's
   pattern admits the value, whichever arm takes the value. Where the pattern does not admit the value the
   guard sleeps as an arm does, and its next run is an entry. A guard keeps no memory and consumes nothing.
   A consulted guard whose value is bottom makes the selection undecidable.
8. **A written binding is state.** A binding something writes (a `<-` target, a `&mut`'s referent) is
   driven by events: its initializer's fires and the writes. A write that landed while its body did not
   run stands, and is not raised again at the wake. A fire of its initializer while the body slept is a
   missed event: at the wake the binding keeps the value it holds, whatever its initializer would now
   compute, and the initializer writes it again when it next fires. A `let` nothing writes is a value and
   recomputes.

## 2. The tag of every read

B = the binds, C = a capture that stands, K = a constant. A capture delivered this cycle fires in every
row. "Inside" is what fires in the body, "site" is the site's own result.

| Site, event | Inside | Site |
|---|---|---|
| Arm, entry or re-entry, scrutinee fires | B | fires |
| Arm, entry by a guard's flip | nothing; B stale | fires, with what the stale binds compute |
| Arm, entry by an outer arm's wake, or by a scrutinee that changed quietly | nothing; B stale | quiet |
| Arm, selected in a root's first cycle | B, C, K | fires |
| Slot, born in a birth | B, C, K | fires |
| Slot, born later; source fires | B | fires |
| Slot, source returns from bottom | B | fires |
| Slot, born at an outer wake (the source grew while the arm slept) | nothing; B stale | quiet |
| Callee, activated in a birth, its argument present or not | B, C, K | fires |
| Callee, an argument fires | that formal | fires if the body's result does |
| Callee, new activation, its argument stands | nothing | quiet |
| Callee, the function changes or returns from bottom | nothing | fires, with what the stale formals compute |
| A depth regrown at an outer wake, its argument stands | nothing | quiet |

## 3. What the author writes

| To get | Write |
|---|---|
| A constant written when an arm is taken | connect outside: ``screen <- select key { `Esc => `Menu, _ => never() }`` |
| A write or an effect at every entry | sample the bind: ``k@ `Esc => screen <- k ~ `Menu`` |
| Once per arm | `once(k) ~ v` |
| A capture's value at entry | `k ~ target` |
| At entry and at every live change | `any(target, k ~ target)` |
| An event with the state it should see | `select (ev, ev ~ state) { (e, s) => .. }` |
| A stateful builtin over a value that stands | feed it the bind: `count(k ~ 7)`, `uniq(k ~ arg)` |
| A stateful builtin in a guard | hoist it: `let en = uniq(enabled); select key { k if en => .. }` |
| A callee activated later that must act | the caller fires the argument: `comp(k ~ "open")` |
| A function with no argument that must act in a body entered later | give it the event as a parameter |
| An app that waits for an async value | call it at the top level: `app(cfg$)`, not `select cfg { c => app(c) }` |
| State a body computes from captures and shares outside | hoist it out of the body, or sample the capture on the bind inside it: `let y = any(c, t ~ c) + 1`. A sample at the reader alone reads the value the body last stood |
| A handler's reply that does not read its request | `|req| req ~ reply` |
| A raise of a constant error | raise outside the select, or ``error(k ~ `Quit)?`` inside the arm |
| An event source over constants in a body entered later | start it on the bind: `timer(once(k) ~ duration:100.ms, true)` |
| An arm inside another arm, entered by the outer arm's wake | the inner scrutinee samples the outer event: `select any(inner, outer ~ inner) { .. }` |

The check warns where an arm acts on constants alone: a `<-`, a raise to a handler or an effectful call
that reads no variable (`select.rs::warn_constant_action`), and a `seq` with no trigger or one whose
trigger names no variable (`warn_triggerless_seq`). Such an arm acts only where it is taken in its
root's first cycle. A select of the seq machine's world is not warned (§4).

## 4. Seq

Seq is where the rule stops: the machine makes births of its own. The boundary is lexical, as a step's
snapshot is.

| Thing | World |
|---|---|
| A step's own text (not inside a lambda) | seq: each run's entry is a birth into a standing world; the fires it missed are owed once |
| A select written in a step's text, and the lowering's own selects (`CFlag::SeqStep`, `CFlag::RunEvent`) | seq: an arm is born at its first selection in each run, and keeps the fire tracker |
| A call a step issues | born at its issue: it reads its arguments and captures as delivered |
| A body entered in the update of a step's birth | part of that birth |
| A body inside a step's callee entered afterwards (an arm taken when a reply arrives) | a body: bind-driven |
| A seq written in a body | the machine is as built; its trigger is an expression of the body. A missing or constant trigger fires only in a birth, so such a seq never runs in a body entered later |
| The machine's own state (`pc`, a `seqq`'s first credit) | made at the machine's first update, wherever that is |

A select's image carries `CFlag::SeqStep` and `CFlag::RunEvent`, so a cached start runs a step's select as
a cold start does.

## 5. How both engines compute it

**The node-walk.** A body's entry and its resume run under `View::Wake` (`ExecCtx::enter_body`, joined
with an enclosing birth): standing reads are stale and constants stand. Sleep state is the `slept` bit of
each node that owns one, a select's selected arm, and one bit a guard for whether it ran at the select's
last run. A body's select holds no fire bits; `node/wake.rs`'s `TrackedFires` serves the seq machine, a
step's select and a step's call alone (`node/select.rs::seq_world`, `wake::FreshFrame`).

**The moved mark.** The node-walk recomputes a node only when an input fires or the node wakes; a kernel
computes every run. A value a resume recomputed can differ from the one a reader outside last computed
from, so it must reach that reader as a value to recompute from. Every quiet read under `View::Wake` is
tagged `Tag::MOVED` (`VarRead::production`, a select's stale binds, a resize under a quiet source), and so
is the error of a raise its catch gave up, which lands in the binder with no event for the handler
(`node/error.rs::Receipt::Passed`, `design/catch.md`); the
bit joins over quiet computations, and a consumer that sees it recomputes and stands its own result moved
(`dense_gate!`, `Bind::update`, `ExecCtx::stand`). The bit is an event to no one: `Tag::triggers` ignores
it, a kernel's fire test ignores it, and the seq tracker sets no fire bit for it
(`TrackedFires::observe`). A moved stand is stored as a fire is (`ExecCtx::stand_moved`), stamped this
cycle, so the boundaries of its readers are marked and a reader the walk already passed takes it, quiet,
the next cycle. By that stamp it is a write to a seq step's catch-up scope (`wake::owed`): the stand
replaced the entry of whatever fire the step missed.

Whether a value moved is never decided by comparing values: `x * 0` moved when `x` did. Everything that
computes from values passes the bit on: `Tag::join` ORs it over a quiet join and drops it where the join
fires, so an operator, a data constructor, a field read, a select's result (from its arm, its scrutinee
and its consulted guards), a call, a collection and a `Stateless` builtin carry it. A `Stateless` builtin's
call site marks its quiet result moved when an argument moved, whatever its `Apply` made of the tag
(`CallSite::update_call`): `CachedArgs` computes the value again from the slots and tags it with its
arguments' join, for an `Io` builtin too (`EvalCached::quiet`), a builtin that is its own `Apply` computes
from its arguments at every update, and a kernel's fast call ORs the bit as an operator does. A bottom made
of a moved value carries it too: the bottom `$` or `?` makes of a null or an error that stands
(`node/error.rs::strip_production`; nothing is raised or reported, since nothing
fired), and a loop's bottom over a bottom source, which ran no slot to carry it
(`fusion/emit/scaffold.rs::SlotFlags::apply`). Everything that consumes events drops it: a stateful
builtin keeps its state, a sample whose trigger did not fire rides, an `Io` call does not act and its
result moved when its trigger argument did. An `any` keeps its value too, except at a wake that finds the
child the value came from bottom: it stands bottom then, moved (`node::Any::update`). A resident never
keeps the bit (`Tag::quiet`). A kernel ORs the bit into a disc wherever it ORs `TAINT`
(`emit::abi::propagate_taint`, the slot accumulators, `emit::body::fold_consumed`) and tests it nowhere;
it runs, and never rides, when a feeder moved. A guard's tentative binds are retracted and stand nowhere:
the store holds them quiet, so no reader outside the guard takes one as a stand that moved, and the guard
reads them from the event, moved where its scrutinee moved, so it recomputes
(`PatternNode::bind_tentative`; pin: `lang::wake_outside::a_moved_scrutinee_moves_its_guards_bind`).

**Kernels.** A kernel's one source of birth is the context word its wrapper passes; a call site's first
use, a loop slot's first iteration and a self-call's fresh block are not births, so a tail recursion that
performs or raises to a handler is one native loop whose passes act as the activations do. A guard's binds
are masked by its arm's shape, and under an unguarded arm by the type that arm narrows it to
(`fusion/emit/select.rs::emit_narrowed_test`). A loop fires when a slot fires or its source fires (resized,
empty, or passed through); a resize under a quiet source is a quiet result that moved, and so is a source
that moved to bottom. A core trait hook's first call is an entry too (`node/coretraits.rs`).

**An effect in an arm or a guard fuses.** An Io call or a raise to a handler in an arm's body performs
or raises on a live fire of its trigger or operand in both engines, so it fuses. A kernel computes a guard
ahead of the arm chain, and only where the scrutinee is present and the guard's arm admits the value: it
branches around the guard otherwise, so a guard that does not run performs, raises and loops in neither
engine (`fusion/emit/select.rs::emit_select_arms`; pin `findings/guard-sleeps-unadmitted-nov2026/`).

## 6. Pins

`lang::select` (arms, guards, constants in an arm), `lang::wake_outside` (a wake fires nothing; the value
reaches every reader), `lang::collection` and `lang::functions` (slots, activations, defaults),
`lang::delivery`, `lang::prune`, `lang::image` (a step's select through an image),
`graphix-shell/tests/shell/wake_outside_roots.rs` (readers in other roots),
`stdlib/graphix-tests/tests/fusion_diagnostics.rs` (a raise from a constant in an arm reports in neither
engine).
