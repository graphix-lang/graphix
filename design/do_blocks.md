# `do` blocks: sequencing across cycles

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

The one-line version: **a `do` block is a `select` the compiler
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
let privileged = |req: PrivReq| do req {
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
steps write (§7). Progress, toasts, focus and the exit level are all
written the same way — a do block is an ordered issuer of effects that
waits between them, not primarily a value producer.

A poll loop:

```graphix
do go {
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

`do` is the other axis. Nothing inside a do block runs sequentially
within a cycle: each step is a live reactive expression, and "next"
means "the cycle after this one produced". There is no mutation
(bindings are ordinary lets; cross-step values ride variables), no
within-cycle loop (an iteration is a re-selection, one per cycle at
least), and nothing a kernel needs to know (a do block is `Async` by
construction; its steps' interiors fuse or not exactly as today). It
is the Rust `async fn` move, not the Rust `fn` move: Rust did not add a
second evaluation model for async either, it lowers the function to
the state machine you would write by hand, and the win is that the
source reads in execution order while the semantics stay the
machine's. The compute loops stay where they are: `let rec`, the HOFs,
`#[native]`.

## 4. Syntax

```
do [trigger] { stmt* [expr] }

stmt   := let pat = expr ;
        | expr ;                         // an effect, a watch, a derivation
        | until expr ;                   // sugar: select expr { true => null, false => never() }
        | select expr { pat [if g] => body, ... } ;   // body = { stmt* [expr] } or expr
        | if expr { stmt* } [else { stmt* }] ;        // sugar over select on bool
        | loop { stmt* } | while expr { stmt* } | for pat in expr { stmt* }
        | break [expr] ; | continue ;
        | catch(e) expr ;                // at the top of the block only (§5.8)
```

`do { .. }` without a trigger runs once at init. `if` is already a
reserved word. The keyword is provisional (§10).

## 5. Semantics

Numbered so fixtures can cite them.

**R1 — Run.** A run starts when the trigger fires while no run is in
progress; a trigger during a run is dropped (the busy policy; §10 for
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
arm (§6.4).

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
An iteration re-enters arms; it never re-instantiates anything (§6.6).
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
completion. A `do` inside a lambda is a callable ceremony; a call to
one from a step is itself an async step and composes by R3.

**R9 — Levels live outside.** A level effect (`tui::suspend`,
`sys::net::publish`, a subscription the ceremony watches) must not be a
step: a passed step sleeps, and a slept level is torn down. Steps write
the variable that drives the level and `until` waits for its response.

**R10 — Do blocks are `Async`.** The machine node-walks; each step's
sync interior fuses as it would anywhere. `#[sync]` on a do block is a
compile error, `#[native]` inside a step means what it means today.

## 6. The lowering

An AST-to-AST desugar (`expr/do_desugar.rs`, the precedent is the sync
subset's P1 desugar, which validated "no new node types; both
evaluators inherit the semantics from one spec"). Positions carry from
each statement to the nodes it lowers to, so a type error names the
step. `graphix --expand` prints the lowered program: the machine is
inspectable, which is the debugging story.

### 6.1 The skeleton

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
one, not a drop (§10 lists it as the `queue` policy).

### 6.2 The entry event

Inside arm `Ak`, the step variable read as a free variable — `pc`
itself — fires on every delivery into the arm: first entry, re-entry
after another arm, and a same-arm re-delivery on a loop's back-edge.
It is the one event every atom below samples on. It must be a free
variable and not the arm's pattern bind, because a nested watch (§6.3)
relies on the wake catch-up tracker re-raising it, and pattern binds
are excluded from that tracker by design (`Bind::pattern`, 2026-09-02:
a pattern bind is a facet of its arm's scrutinee delivery). Whether a
free read of the scrutinee's own variable is tracked is the first
thing the prototype confirms; if not, the machine writes a sibling
`entered` variable beside every `pc` write and samples on that.

### 6.3 The issue atom

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

### 6.4 Arms and chains

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

### 6.5 Branches

A select step is an arm boundary: the scrutinee is the arm's last
step; its transition writes the FIRST arm-label of the chosen
alternative (`pc <- v ~ select v { pat1 => `B1, pat2 => `C1 }`, with
the pattern binds re-established in the alternative's first arm from a
cell). Each alternative's last arm transitions to the join label.

### 6.6 Loops

A label is an arm; the back-edge is a `pc` write of the label; a
same-arm back-edge (a one-arm body) is a same-value write, which
delivers like any write (`set_var` pushes unconditionally), so the
entry event fires and the body issues again. `break v` writes the
loop's cell and the after-label; `continue` writes the label. The
lowering never writes a constant RHS: a constant connect fires once
per SELECTION and not on a same-arm re-match (the select chapter's
"Writing From an Arm"), so every transition is sampled on the entry
event or on the step's completion.

### 6.7 What sleep does for free

A passed arm sleeps, and sleep is pause: a timer step's pending timer
is cancelled (`Timer::sleep` unrefs it), a `sys::net` level effect
tears down, a process spawn is not cancelled (`kill_on_drop` is on
drop). §7 states what this does and does not give.

### 6.8 Errors

The user's `catch` is allowed only at the top of the block and it is
cleanup, not a handler: the lowered machine has one handler, outermost,
whose body is the cleanup, the reset, and the rethrow. Handler-side
`?` resolves to the predecessor, so the enclosing block's catch sees
the error as it would from a plain block. A mid-block catch (covering
only later steps) would have to be duplicated into every later arm;
not in v1.

## 7. Costs and limits

- **Cycles.** One per async completion, one per connect, one per
  back-edge. A cycle is well under a millisecond in release (a text
  key is 0.17ms end to end at 5.4k lines), so a six-step ceremony adds
  about a millisecond to work that takes seconds.
- **No cancellation.** A retrigger cannot cancel an in-flight step
  beyond what sleep does (§6.7); the busy policy is the default because
  restart would deliver a stale production into a fresh run. A step
  that never completes (a watch on a level that never flips, an
  operation that never answers) stalls the run, exactly as a
  hand-written machine stalls today. A `timeout` policy is the honest
  fix and is open (§10).
- **No within-cycle iteration.** A loop whose body is all same-cycle
  steps runs one iteration per cycle: the counter idiom, observable and
  interruptible. Collapsing it into a within-cycle loop is possible
  and is the sync subset arriving through the back door; not proposed.
- **Levels outside** (R9). The compiler cannot tell a level effect from
  a one-shot; the book has to say it, and `tui::suspend` is the worked
  example.
- **Fusion** is untouched (R10).

## 8. Typing and diagnostics

The step variable's type is a generated variant; cells are seeded
cells (Bind ⊥-seed); `break` values unify with the loop's binding; the
block's type is its last expression's. Every lowered node carries its
source statement's position, so a type error inside a step reads like
the same error in a plain block. A statement that is neither an
effect nor a derivation of anything (a bare constant) is a warning.
The desugared program must print (`--expand`) and re-parse to the same
machine — the round-trip test covers the lowering's output.

## 9. Plan

0. **The go/no-go, before any parser work.** Lower the privileged
   handoff (§2) BY HAND in the port, atoms and all, and diff it against
   `local.gx` as it stands. Two questions: does the machine written
   with the atoms behave (the §6.2 tracker question; the §6.3 presence
   gate on a second run; a timer loop's re-arm), and is the surface
   form the first version of that ceremony a reader follows top to
   bottom. If the hand-lowered machine is only shorter, the construct
   saves typing; if the surface form is the readable one, it earns the
   keyword.
1. Parser + AST: `ExprKind::Do { trigger, body }` with a `Stmt` enum;
   the printer; the proptest generator; tree-sitter.
2. The desugar (`expr/do_desugar.rs`) and `--expand`.
3. Pins, one fixture per rule (R1..R10), each run on both engines:
   drop-while-busy; once-per-entry issue; presence-wait on the second
   run; sync coalescing; the let cell crossing an arm; branch with a
   payload bind; a two-arm loop and a one-arm loop; `break v`; `for`
   over an array taken at entry; abort with cleanup and the enclosing
   catch seeing the error; the value fired once per run; a do-lambda
   called from a step.
4. Port: every ceremony in `local.gx`, the change-password route, the
   landing's connect. Measure lines, `--check` time, and read it.
5. Book: a chapter beside `select`, with R9 and the counter-idiom
   warning.

## 10. Open questions

- **The keyword.** `do` reads well and has the Haskell precedent, but
  `ExprKind::Do` is the block AST today (rename it `Block`). `seq` is
  a builtin. Alternatives: `run`, `flow`, `steps`.
- **`if` generally.** If `if` becomes bool sugar over select inside do
  blocks it is hard to justify refusing it outside them. Decide once.
- **Retrigger policies.** `busy` (drop) is the default. `queue` is
  what `~` gives for free (one pending run). `restart` needs the
  stale-production guard (a run generation in the step variable) and a
  cancellation story. `timeout(d)` aborts a stalled run through the
  error path. Spelling: `do(#on_retrigger: `Queue) go { .. }` or
  nothing in v1.
- **Nested `do`** as a statement (a sub-machine triggered by the
  entry) — falls out of R3 and R8 if the inner block's trigger is the
  outer entry, but a lambda call is the same thing; v2.
- **Mid-block `catch`** (§6.8).
- **`until`** as sugar or as a documented select spelling.
- **The trigger snapshot** (R1's last sentence): needed, or is the
  one-cycle race between the trigger's delivery and the first arm's
  entry acceptable?
