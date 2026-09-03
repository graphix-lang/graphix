# `seq` blocks: sequencing across cycles

Status: **proposed, not built** (2026-09-03). Origin: the post-port
assessment of the netidx-admin rewrite (5,434 lines of Graphix). Eric:
"we actually wrote a sync language subset at one point and concluded it
was a total disaster and the sync language subset was Rust" — the
design below is deliberately not that (§3), and it grew in the same
conversation from a plain step sequence to branches and loops (Eric:
"you can already loop with select, and if should be possible to add as
well") and to progress reporting through captured variables (Eric:
"why couldn't it `progress <- 0, ..., progress <- 1` where progress is
a capture?").

The one-line version: **a `seq` block is a `select` the compiler
writes.** Every ceremony in the port is a hand-written state machine
over a step variable — `select` over it, `<-` advancing it, `~` gating
each step, `never<T>()` seeds carrying values between steps, a catch
per block routing failures. The construct lowers a statement list to
exactly that machine, with the sampling discipline applied
mechanically, so the source reads in execution order. It adds no
evaluation model: every step is an ordinary reactive expression, and
the cycle boundary is the yield point.

## 1. The problem

Real programs have sequential parts. In the port they are the
lifecycle ceremonies (install, join, add-parent, restore in two
stages, uninstall, the privileged handoff), the change-password route,
and the landing's connect flow. Each is "issue an async operation,
wait for its result, branch on it, write some UI state, issue the
next", and each came out as a chain of gated connects that the reader
has to re-sequence in their head. The privileged handoff as it stands
(`local.gx`, abridged):

```graphix
let priv_req = never<{ what: string, argv: Array<string>, then: Then }>();
let priv_cmd = escalate_command(priv_req.argv, priv_req);
let suspended = false;
let priv_exit = never<i64>();
{
  catch(e) {
    select (e.0).error { ... => toast <- ..., a => fail("...", a) };
    suspended <- e ~ false
  };
  let cmd = priv_cmd?;
  suspended <- cmd ~ true;
  let released = tui::suspend(suspended)?;
  let go = select released { true => released, false => never() };
  println(go ~ "\nAdministrator privileges are needed to [priv_req.what].");
  let child = sys::process::spawn(sys::process::options(#args: cmd.args, go ~ cmd.program))?;
  let status = sys::process::wait(child.proc)?;
  suspended <- status ~ false;
  priv_exit <- select status.code { null as _ => -1, c => c }
};
select priv_exit ~ priv_req.then { ... }
```

Four things the reader reconstructs, and the author gets wrong:

1. **The order.** It is the order of the block only by convention;
   nothing enforces that `spawn` waits for the release except the
   `go ~` on its program argument, and a missing `~` is a program that
   runs the child before the terminal is released.
2. **The later-bound locals.** `priv_req`, `priv_exit`, `svc_ok`,
   `verify_req` are `never<T>()` seeds: variables whose only purpose is
   to carry a value from one step to a step that a different block
   reads. Thirteen of them in `local.gx` alone.
3. **The sampling.** Every step's arguments are gated on the previous
   step's value (`cmd ~ true`, `status ~ false`, `go ~ cmd.program`),
   by hand, per argument. The port has 262 `~` across 4,162 lines and
   the ceremonies are the densest users.
4. **The error routing.** A catch per block, each resetting whatever
   state the block's steps had set (`suspended <- e ~ false`), with
   the reset duplicated per block.

The pattern is a state machine written longhand. The line counts by
file: `local.gx` 1,300 lines, 13 seeds, 21 catch blocks, 74 samples.

## 2. What it looks like

```graphix
let privileged = |req: PrivReq| seq req {
  catch(e) {
    select (e.0).error {
      `TerminalError(m) | `ProcessError(m) =>
        toast <- { title: "The privileged step could not run", lines: [m], error: true },
      a => fail("The privileged step could not start", a)
    };
    suspended <- false
  };
  let cmd = escalate_command(req.argv, req)?;
  suspended <- true;
  until released;
  println("\nAdministrator privileges are needed to [req.what].");
  let child = sys::process::spawn(sys::process::options(#args: cmd.args, cmd.program))?;
  let status = sys::process::wait(child.proc)?;
  suspended <- false;
  let code = select status.code { null as _ => -1, c => c };
  select req.then {
    `VerifyService({ name, for_user }) => verify_req <- { name, for_user, code },
    `UninstallAgain(remove_ca) => run <- `UninstallWith(remove_ca),
    `Nothing => select code {
      0 => refresh <- code,
      c => toast <- { title: "The privileged step failed", lines: ["It exited with status [c]."], error: true }
    }
  };
  code
};
let priv_exit = privileged(priv_req);
```

`suspended` and `released` stay outside: `tui::suspend(suspended)` is
a LEVEL and a level lives at module scope, driven by a variable the
steps write (§8). Progress, toasts, focus and the exit level are all
written the same way — a seq block is an ordered issuer of effects that
waits between them, not primarily a value producer.

A poll loop:

```graphix
seq go {
  let st = loop {
    select status(target) {
      `Ready(r) => break r,
      `Pending => sys::time::timer(duration:1.s, false)
    }
  };
  ...
}
```

## 3. Not the sync subset

`design/sync_subset.md` (2026-07-09, removed 07-13) proposed
`sync { let mut ..; for v in a { .. } }`: sequential semantics WITHIN a
cycle — mutation, loops that run to completion inside one evaluation,
`for` desugared to a fold, an elaboration ladder deciding per call site
whether the block became one kernel or per-element slots. It converged
on being a second language, and a second language inside Graphix
converges on Rust, which the project already has as its computation
leaf. The collection intrinsics replaced it.

`seq` is the other axis. Nothing inside a seq block runs sequentially
within a cycle: each step is a live reactive expression, and "next"
means "the cycle after this one produced". There is no mutation
(bindings are ordinary lets; cross-step values ride variables), no
within-cycle loop (an iteration is a re-selection, one per cycle at
least), and nothing a kernel needs to know (a seq block is `Async` by
construction; its steps' interiors fuse or not exactly as today). It
is the Rust `async fn` move, not the Rust `fn` move: Rust did not add a
second evaluation model for async either, it lowers the function to
the state machine you would write by hand, and the win is that the
source reads in execution order while the semantics stay the
machine's. The compute loops stay where they are: `let rec`, the HOFs,
`#[native]`.

## 4. The semantics underneath

This section is here because the construct and the language's firing
semantics are intertwined both ways (Eric, 2026-09-03): the lowering
depends on the semantics as they stand, and the construct may absorb
some of the complexity a program otherwise meets bare. So: what
bothers me about the semantics, root causes before symptoms, then the
semantics I would want, then what each has to do with `seq`.

### 4.1 What is hard, and why

It is not `select`. `select` is where the difficulties meet, because
it is the one place arms sleep and wake, but its rules are symptoms.
The roots, in the order I would rank them:

1. **The event channel is implicit.** Every expression carries two
   things, a value (present, absent, standing) and an event (fired
   this cycle), and the surface shows only the value. The event
   channel is what decides when a connect writes, when an effect
   issues and when a callback runs, and programs steer it with `~`,
   `uniq`, `filter`, `once`, `hold`, `queue`, the structure of a
   select, and the constant-versus-sampled distinction in a connect's
   right-hand side. `~` alone does three jobs: sequence a call
   (`f(t ~ x)`), sample state at an event (`c ~ counter`), and gate a
   constant (`t ~ \`Pick`). The deeper form of the same thing: a
   LEVEL (a value with a present; a reader wants the current one) and
   an EVENT (a fire that matters once; a reader must not miss it) are
   the same kind of thing in the language, and the wake catch-up
   design (`design/wake_catchup.md`) exists to reconcile them after
   the fact: events that fired while an arm slept are re-raised, once,
   conflated, while levels are read as they stand. Eric's 43/2/21/62
   table is the cost of deciding case by case what a mixed thing is.

2. **Bottom is several things.** Never produced; dropped this cycle
   (`filter`, `never()`, a `$`); an async result not yet arrived; a
   standing bottom after a value. The engine's fired-by-bottom algebra
   is principled, but the distinctions leak: `~` holds a trigger's
   debt if its right-hand side has NEVER materialized and pays it as a
   fresh bottom if the side has materialized before (§7.3 is a
   workaround for exactly this — a step that waited on its first run
   stalls on its second); a bottom read stalls silently, with no way
   to tell "not yet" from "never"; a bottom scrutinee bottoms a
   select whose taken arm was an active producer (ruled; `hold` is the
   tool; still a trap the reader has to know).

3. **Init and wake are special cycles.** Constants fire at init and
   never again, so a constant write in an arm fires once per selection
   (a tool, per the ruling, and also the reason a `println` in an arm
   runs on the first selection only); a guard that has never produced
   makes the select undecidable at init (the init-phantom); a woken
   arm forces a recompute, reads standing values stale, re-matches its
   scrutinee, and receives conflated catch-up fires; the kernel wire
   distinguishes genuine init from wake by a bit. Each rule is right in
   isolation. Together they mean a program's first cycle and an arm's
   re-selection follow rules its tenth cycle does not.

4. **`select`'s own rules** are where the three above surface: the
   consulted-guard rule, bottom-out, own-firing through a retained
   selection, pattern binds as facets of the scrutinee delivery and
   therefore excluded from catch-up (§7.2 leans on this), the once-
   per-selection constant write. A user who knows 1–3 predicts them;
   one who does not meets them one at a time.

5. **Three failure channels.** A bottom (silent, the unchecked
   operators, `$`), an in-band `Error` value (typed, matched), and a
   thrown error (`?` to the nearest installed handler along the call
   chain). Which channel a builtin uses is convention ("hot operators
   log and bottom; rare stdlib functions return a catchable Error").
   A reader has to know all three and the convention.

6. **Variables are queues of writes**, one delivery per cycle, in
   write order; a second write to a variable already delivered this
   cycle is re-queued for the next (`push_var_event!`). This is a
   clean rule and nobody states it. It matters here: a step variable
   written by a transition and by the abort in the same cycle is two
   deliveries, and the lowering has to make the abort's win.

7. **State multiplicity.** A stateful builtin (`count`, `once`) inside
   a lambda, a callback, an arm or a recursion is per instance, per
   slot, per activation — principled (`design/activation_state.md`)
   and hard to explain; smaller than the rest.

The typing subtleties of the last month (coverage distribution, the
union rectangle, never-typing) are second order next to these: they
produce compile errors, not surprises at runtime.

### 4.2 The semantics I would want

Stated as principles, each with the rule it would replace.

- **P1 — Levels and events are different kinds, and the reader can
  tell which is which.** A binding is a level: reading it yields the
  present value and never history. An event is a fire consumed
  exactly once; `queue` makes a stream of them lossless; `hold` turns
  an event into a level; `~` samples a level on an event, and that is
  its one job. Conversion is always explicit. With this, wake catch-up
  is not a table: levels need none (read the present) and events need
  a queue, and the conflation rule (deliver an unconsumed fire once at
  the current value) disappears, because a conflated event is a level
  read. The language already gropes toward it: `Any` is the de facto
  event type in every trigger parameter (`|t: Any|`, `#trig: Any`), and
  the sibling-bind ruling (a pattern bind is a facet of a delivery,
  not an event) is P1 applied to one case. The full kind model — the
  rules, the witnesses replayed, what it deletes and what it costs —
  is `levels_and_events.md`.
- **P2 — One bottom.** Bottom means absent: no event, and no memory of
  ever having been present. No program-visible construct behaves
  differently because a bottom was once a value. `~`'s debt is the
  violation: either drop when the level is absent, or wait for it
  consistently.
- **P3 — Effects issue on events; derivations follow levels.** A pure
  expression is a live derivation of its inputs. An effect (an async
  call, a connect, a print) is issued by an event and not re-issued
  by a level changing. Organic firing already says this; what it lacks
  is a way to say which inputs are events.
- **P4 — No special cycles.** Init is one event, program start;
  constants are levels (present from birth, never firing); an effect
  at top level issues on the start event; a wake is not an event.
  Under P4 the constant write in an arm does nothing, and "on entering
  this state" is written `cursor <- s ~ 0` with `s` the entry event,
  which is what the lowering writes anyway (§7.6). The tool survives,
  spelled.
- **P5 — Variables are queues.** Already true (4.1 item 6); write it
  down, and say what two writes in one cycle mean.

Not on the list, because I would keep them: organic firing as the
core rule; sleep as pause; activation multiplicity; catch as a handler
rather than control flow; bottom scrutinee ⇒ bottom select (under P2
it is "an absent level decides nothing", which is right); the
consulted-guard rule (a guard is a level; absent is undecidable).

### 4.3 What this has to do with `seq`

A `seq` block is P1–P4 applied to one program class by construction.
Its trigger is an event, consumed once per run (R1). Its inputs are
levels, read as they stand at a step's entry (R2). Its effects issue
on the entry event and never on a level moving (R2, R3). It has no
special cycles: entry IS an event, whether first, re-entry, or a
loop's back-edge, so a step behaves the same on every run (R2, §7.2).
Where the language's rules leak through the lowering, an atom absorbs
the leak, and each atom is priced by the principle it stands in for:

| leak | today's rule | the atom | under the principle |
|---|---|---|---|
| a step's input absent at entry | `~` debt asymmetry (P2) | the presence select, §7.3 | `f(pc ~ x)` waits or drops, consistently |
| the entry must reach a nested watch | pattern binds excluded from catch-up; a scrutinee's own variable maybe (P1) | a sibling `entered` variable, §7.2 | the entry is an event; the watch queues it |
| a retrigger while running | `~` holds one pending trigger (P1) | `filter` as the busy gate, §7.1 | the policy is a choice between `queue` and drop |
| a transition on a same-arm re-match | constants fire per selection (P4) | never write a constant RHS, §7.6 | nothing to avoid: constants never fire |
| two writes to the step variable | variables are queues (P5) | the abort must deliver last | the same, stated |

Two consequences. First, the construct can be built on today's
semantics: every atom is expressible, and step 0 of the plan is the
proof. Second, the atoms are the measurement of what the language's
rules cost a program that wants sequential meaning: if P1–P4 were
adopted language-wide, the lowering would shrink to `f(pc ~ x)` and
`pc <- pc ~ \`Next`, and the same simplification would reach every
hand-written machine that does not use `seq`. That is the sense in
which `seq` mitigates the complexity: it is a pilot of the semantics
under P1–P4 in the class of programs that suffers most, and if it
reads right there, the principles have earned a hearing for the
language as a whole. The book test from the assessment stands: if the
rules of §4.2 cannot be written in five pages a newcomer can hold, the
model is still too subtle.

## 5. Syntax

```
seq [trigger] { stmt* [expr] }

stmt   := let pat = expr ;
        | expr ;                         // an effect, a watch, a derivation
        | until expr ;                   // sugar: select expr { true => null, false => never() }
        | select expr { pat [if g] => body, ... } ;   // body = { stmt* [expr] } or expr
        | if expr { stmt* } [else { stmt* }] ;        // sugar over select on bool
        | loop { stmt* } | while expr { stmt* } | for pat in expr { stmt* }
        | break [expr] ; | continue ;
        | catch(e) expr ;                // at the top of the block only (§7.8)
```

`seq { .. }` without a trigger runs once at init. `if` is already a
reserved word. The keyword reclaims the integer-sequence builtin (§11).

## 6. Semantics

Numbered so fixtures can cite them.

**R1 — Run.** A run starts when the trigger fires while no run is in
progress; a trigger during a run is dropped (the busy policy; §11 for
restart/queue). If the trigger is a bare variable, the body's reads of
that name see the value the run started with.

**R2 — Steps evaluate in order, once per entry.** A step's leaves —
constants and reads of variables outside the step — are taken as they
stand when the step is reached, and the step's effects are issued
exactly once per reaching. Nothing in a passed step re-fires: its arm
is asleep. This is `f(trigger ~ x)` applied mechanically, and it is the
rule the hand-written ceremonies get wrong.

**R3 — Completion is the step's effect class.** A step the analysis
classifies `Async` completes on its first non-bottom fired production.
A step with only same-cycle effects (a connect, `println`, a stateful
`Sync` builtin) completes at issue. A pure step is a derivation: it
completes when its value is present, and it stays live while its arm
is active (that is what lets `until released` wait for a level to
flip). The classification is `analysis::infer_effects`' per-node
fact, so the lowering pays nothing new. Consecutive same-cycle steps
coalesce into one arm; an async completion and every connect end an
arm (§7.4).

**R4 — A `let` binds the step's first production for the rest of the
run.** Later steps read it; a later ITERATION overwrites it before its
readers run. Shadowing is sequential as in a block.

**R5 — A `select` step branches once.** Its scrutinee is a step; the
arm is chosen when it is present and does not switch mid-run; the
arm's statements are steps; every arm's last step transitions to the
statement after the select. In value position each arm's value is the
select's. `if` is the bool special case.

**R6 — A loop is a label and a back-edge.** The body's last step
transitions to the label; `continue` does the same from anywhere in
the body; `break v` writes the loop's value and transitions past it.
`while c` is `loop { select c { false => break, true => null }; .. }`
with the exit at the top;
`for x in xs` is an index loop over `xs` taken at the loop's entry.
An iteration re-enters arms; it never re-instantiates anything (§7.6).
Accumulators are captured variables written as steps (`total <- total
+ x`), read by the next iteration.

**R7 — A `?` aborts the run.** The lowered block installs ONE handler
outermost: it runs the user's cleanup (the block's own `catch`, if
any), resets the step variable to idle, and rethrows to the enclosing
handler. A step cannot swallow an error and continue; to handle a
failure inside the ceremony, match the Result in a select step instead
of writing `?`.

**R8 — The value.** The block's value is its last expression's, fired
once per completed run, stale between runs, bottom before the first
completion. A `seq` inside a lambda is a callable ceremony; a call to
one from a step is itself an async step and composes by R3.

**R9 — Levels live outside.** A level effect (`tui::suspend`,
`sys::net::publish`, a subscription the ceremony watches) must not be a
step: a passed step sleeps, and a slept level is torn down. Steps write
the variable that drives the level and `until` waits for its response.

**R10 — Do blocks are `Async`.** The machine node-walks; each step's
sync interior fuses as it would anywhere. `#[sync]` on a seq block is a
compile error, `#[native]` inside a step means what it means today.

## 7. The lowering

An AST-to-AST desugar (`expr/seq_desugar.rs`, the precedent is the sync
subset's P1 desugar, which validated "no new node types; both
evaluators inherit the semantics from one spec"). Positions carry from
each statement to the nodes it lowers to, so a type error names the
step. `graphix --expand` prints the lowered program: the machine is
inspectable, which is the debugging story.

### 7.1 The skeleton

```graphix
{
  let pc = `Idle;                       // [`Idle, `A1, `A2, ..]
  let x_c = never();                    // one cell per let read across arms
  let idle = pc == `Idle;
  catch(e) { <user cleanup>; pc <- e ~ `Idle; e? };
  let t = filter(<trigger>, |_| idle);  // R1: dropped while running
  pc <- t ~ `A1;
  select pc {
    `Idle => never(),
    `A1 => <arm 1>,
    ...
  }
}
```

The cells need no annotations: the Bind ⊥-seed rule (2026-09-03)
types an unannotated `let x = never()` from its writers. The busy gate
is `filter`, not `t ~ select pc { .. }`: `~` holds a trigger's debt
until its RHS first materializes and then pays it, which is a queue of
one, not a drop (§11 lists it as the `queue` policy).

### 7.2 The entry event

Inside arm `Ak`, the step variable read as a free variable — `pc`
itself — fires on every delivery into the arm: first entry, re-entry
after another arm, and a same-arm re-delivery on a loop's back-edge.
It is the one event every atom below samples on. It must be a free
variable and not the arm's pattern bind, because a nested watch (§7.3)
relies on the wake catch-up tracker re-raising it, and pattern binds
are excluded from that tracker by design (`Bind::pattern`, 2026-09-02:
a pattern bind is a facet of its arm's scrutinee delivery). Whether a
free read of the scrutinee's own variable is tracked is the first
thing the prototype confirms; if not, the machine writes a sibling
`entered` variable beside every `pc` write and samples on that.

### 7.3 The issue atom

A step with an effect and variable inputs `x1..xn`:

```graphix
select (x1, .., xn) {
  v => f(pc ~ v.0, .., pc ~ v.(n-1), pc ~ "constant")
}
```

Constants sample on the entry event directly; variables go through a
presence select. Why not `f(pc ~ x1, ..)`: if `x1` is bottom when the
entry fires, `~` produces a fresh bottom and consumes the debt (a
bottoming RHS that has materialized before), so the call bottoms and
the step stalls forever on every run after the first. With the
presence select, a bottom input leaves no arm selected, the entry's
fire bit stays unconsumed in the select's tracker, and when the inputs
materialize the arm wakes and the catch-up delivers the entry once,
FIRED, at the current value — the wake catch-up ruling (2026-09-01)
doing exactly "wait for presence, then issue once". On a re-entry with
present inputs the select re-matches at wake and the entry's delivery
that cycle is consumed by the arm: one issue.

### 7.4 Arms and chains

Steps chain inside an arm as an ordinary block: each step's inputs
include the previous step's completion (its binding, or a hidden
`let _k = e` for a bare expression step), so a watch sequences the
effect after it and two pure lets in one arm are same-cycle dataflow
with no cell in between. An arm ENDS after an async step (at its
completion) and after every connect (its write lands next cycle, and a
later step may read the written variable, directly or through a
derivation the compiler cannot see, so the next step must start a
cycle later). A same-cycle effect (`println`) does not end the arm.

The transition is `pc <- done ~ `A(k+1)`, `done` being the arm's last
completion; the cells of every let a later arm reads are written in
the same cycle (`x_c <- x`), so both land in one batch and the next
arm's entry samples the new values (`Sample::update` updates its
argument before it samples).

### 7.5 Branches

A select step is an arm boundary: the scrutinee is the arm's last
step; its transition writes the FIRST arm-label of the chosen
alternative (`pc <- v ~ select v { pat1 => `B1, pat2 => `C1 }`, with
the pattern binds re-established in the alternative's first arm from a
cell). Each alternative's last arm transitions to the join label.

### 7.6 Loops

A label is an arm; the back-edge is a `pc` write of the label; a
same-arm back-edge (a one-arm body) is a same-value write, which
delivers like any write (`set_var` pushes unconditionally), so the
entry event fires and the body issues again. `break v` writes the
loop's cell and the after-label; `continue` writes the label. The
lowering never writes a constant RHS: a constant connect fires once
per SELECTION and not on a same-arm re-match (the select chapter's
"Writing From an Arm"), so every transition is sampled on the entry
event or on the step's completion.

### 7.7 What sleep does for free

A passed arm sleeps, and sleep is pause: a timer step's pending timer
is cancelled (`Timer::sleep` unrefs it), a `sys::net` level effect
tears down, a process spawn is not cancelled (`kill_on_drop` is on
drop). §8 states what this does and does not give.

### 7.8 Errors

The user's `catch` is allowed only at the top of the block and it is
cleanup, not a handler: the lowered machine has one handler, outermost,
whose body is the cleanup, the reset, and the rethrow. Handler-side
`?` resolves to the predecessor, so the enclosing block's catch sees
the error as it would from a plain block. The step variable is a queue
(§4.1, item 6): a transition and the abort's reset written in one cycle
deliver over two, in write order, so a transition must not be queued
behind a reset — the transition writes are gated on the handler not
having fired this cycle, or the reset carries a run generation the
stale transition fails to match. A mid-block catch (covering
only later steps) would have to be duplicated into every later arm;
not in v1.

## 8. Costs and limits

- **Cycles.** One per async completion, one per connect, one per
  back-edge. A cycle is well under a millisecond in release (a text
  key is 0.17ms end to end at 5.4k lines), so a six-step ceremony adds
  about a millisecond to work that takes seconds.
- **No cancellation.** A retrigger cannot cancel an in-flight step
  beyond what sleep does (§7.7); the busy policy is the default because
  restart would deliver a stale production into a fresh run. A step
  that never completes (a watch on a level that never flips, an
  operation that never answers) stalls the run, exactly as a
  hand-written machine stalls today. A `timeout` policy is the honest
  fix and is open (§11).
- **No within-cycle iteration.** A loop whose body is all same-cycle
  steps runs one iteration per cycle: the counter idiom, observable and
  interruptible. Collapsing it into a within-cycle loop is possible
  and is the sync subset arriving through the back door; not proposed.
- **Levels outside** (R9). The compiler cannot tell a level effect from
  a one-shot; the book has to say it, and `tui::suspend` is the worked
  example.
- **Fusion** is untouched (R10).

## 9. Typing and diagnostics

The step variable's type is a generated variant; cells are seeded
cells (Bind ⊥-seed); `break` values unify with the loop's binding; the
block's type is its last expression's. Every lowered node carries its
source statement's position, so a type error inside a step reads like
the same error in a plain block. A statement that is neither an
effect nor a derivation of anything (a bare constant) is a warning.
The desugared program must print (`--expand`) and re-parse to the same
machine — the round-trip test covers the lowering's output.

## 10. Plan

0. **The go/no-go, before any parser work.** Lower the privileged
   handoff (§2) BY HAND in the port, atoms and all, and diff it against
   `local.gx` as it stands. Two questions: does the machine written
   with the atoms behave (the §7.2 tracker question; the §7.3 presence
   gate on a second run; a timer loop's re-arm), and is the surface
   form the first version of that ceremony a reader follows top to
   bottom. If the hand-lowered machine is only shorter, the construct
   saves typing; if the surface form is the readable one, it earns the
   keyword.
1. Parser + AST: `ExprKind::Seq { trigger, body }` with a `Stmt` enum;
   the printer; the proptest generator; tree-sitter.
2. The desugar (`expr/seq_desugar.rs`) and `--expand`.
3. Pins, one fixture per rule (R1..R10), each run on both engines:
   drop-while-busy; once-per-entry issue; presence-wait on the second
   run; sync coalescing; the let cell crossing an arm; branch with a
   payload bind; a two-arm loop and a one-arm loop; `break v`; `for`
   over an array taken at entry; abort with cleanup and the enclosing
   catch seeing the error; the value fired once per run; a seq-lambda
   called from a step.
4. Port: every ceremony in `local.gx`, the change-password route, the
   landing's connect. Measure lines, `--check` time, and read it.
5. Book: a chapter beside `select`, with R9 and the counter-idiom
   warning.

## 11. Open questions

- **The keyword** is `seq` (Eric, 2026-09-03: "that's what it actually
  is"; `do` was the draft's name and collides with the block AST's
  `ExprKind::Do`). The integer-sequence builtin `seq(i, j)` becomes
  `range(i, j)` — about ten sites: the fuzz generator, three findings
  fixtures, one example, one test, the embedding chapter — and `seq`
  joins the reserved words. Nothing else in the tree uses the name.
- **`if` generally.** If `if` becomes bool sugar over select inside seq
  blocks it is hard to justify refusing it outside them. Decide once.
- **Retrigger policies.** `busy` (drop) is the default. `queue` is
  what `~` gives for free (one pending run). `restart` needs the
  stale-production guard (a run generation in the step variable) and a
  cancellation story. `timeout(d)` aborts a stalled run through the
  error path. Spelling: `seq(#on_retrigger: `Queue) go { .. }` or
  nothing in v1.
- **Nested `seq`** as a statement (a sub-machine triggered by the
  entry) — falls out of R3 and R8 if the inner block's trigger is the
  outer entry, but a lambda call is the same thing; v2.
- **Mid-block `catch`** (§7.8).
- **`until`** as sugar or as a documented select spelling.
- **The trigger snapshot** (R1's last sentence): needed, or is the
  one-cycle race between the trigger's delivery and the first arm's
  entry acceptable?
