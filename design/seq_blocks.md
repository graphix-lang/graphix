# `seq` blocks: sequencing across cycles

Status: built 2026-09-07 (straight-line, `until`, `try … with`, `seqq`;
`if`/loops inside a seq are not built); arms by read-after-write and the
`{ … }` block built 2026-09-11 (the `do` keyword is gone).
Pins: `stdlib/graphix-tests/src/lang/{seq,seq_calls,seq_try,seq_errors,seqq,seq_shadow}.rs`,
`graphix-fuzz/src/generate/reactive.rs` (`ceremony`, the differential lane's seq/seqq programs),
`lib_tests/bottom.rs` (`strict_sample`, `strict_bottom`),
`graphix-compiler/src/expr/parser/test.rs` (`seq_parses`, `try_with_parses`,
`seq_do_statement_list_is_capped`), `expr/seq.rs` unit tests
(`do_body_over_limit_is_a_compile_error`).
Supersedes: pure_select, pure_dataflow_plan, levels_and_events,
seq_review_2026-09-04, seq_review_2026-09-06.

**A `seq` block is a `select` the compiler writes.** Every multi-step
ceremony in a reactive program is a hand-written state machine over a
step variable: `select` over it, `<-` advancing it, `~` gating each
step, `never<T>()` seeds carrying values between steps, a catch per
block routing failures. `seq` lowers a statement list to exactly that
machine with the sampling discipline applied mechanically, so the
source reads in execution order. It adds no evaluation model: every
step is an ordinary reactive expression and the cycle boundary is the
yield point.

## 1. The problem

The port of the netidx-admin TUI (5,434 lines) had a dozen ceremonies
of the form "issue an async operation, wait for its result, branch,
write some UI state, issue the next". Each came out as a chain of
gated connects the reader has to re-sequence in their head, wrong in
the same four ways: the ORDER is enforced only by a per-argument `~`
(a missing one runs the child before the terminal is released);
later-bound locals are `never<T>()` seeds carrying a value to a step
in another block (thirteen in `local.gx` alone); the SAMPLING is by
hand, per argument (262 `~` across 4,162 lines, densest in the
ceremonies); and the error routing is a catch per block, each
resetting the block's state, duplicated per block.

## 2. What it looks like

```graphix
let privileged = |req: PrivReq| {
  catch(e) {
    select (e.0).error {
      `TerminalError(m) | `ProcessError(m) =>
        toast <- { title: "The privileged step could not run", lines: [m], error: true },
      a => fail("The privileged step could not start", a)
    };
    suspended <- false
  };
  seq req {
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
  }
};
let priv_exit = privileged(priv_req);
```

`suspended` and `released` stay outside: `tui::suspend(suspended)` is
a LEVEL, and a level lives at module scope driven by a variable the
steps write (§6). A seq block is an ordered issuer of effects that
waits between them, not primarily a value producer.

## 3. Not the sync subset

An earlier design (`sync { let mut ..; for v in a { .. } }`, removed
2026-07) proposed sequential semantics WITHIN a cycle: mutation, loops
that run to completion inside one evaluation, an elaboration ladder
deciding per call site whether the block became one kernel or
per-element slots. It converged on being a second language, and a
second language inside Graphix converges on Rust, which the project
already has as its computation leaf. Eric: "we actually wrote a sync
language subset at one point and concluded it was a total disaster and
the sync language subset was Rust." The collection intrinsics replaced
it.

`seq` is the other axis. Nothing inside a seq block runs sequentially
within a cycle: each step is a live reactive expression and "next"
means "the cycle after this one produced". There is no mutation
(cross-step values ride variables), no within-cycle loop, and nothing
a kernel needs to know (a seq block is `Async` by construction; a
step's sync interior fuses exactly as it would anywhere). It is the
Rust `async fn` move: no second evaluation model, the function lowers
to the state machine you would write by hand, and the win is that the
source reads in execution order while the semantics stay the machine's.

## 4. Syntax

```
seq  [trigger] { stmt* [expr] }
seqq [trigger] { stmt* [expr] }          // queued form, §8

stmt := let pat = expr ;
      | { stmt* [expr] } ;               // a block: statements issued together
      | expr ;                           // an effect, a watch, a derivation
      | until expr ;                     // wait for a bool level to be true
      | try { stmt* [expr] } with(e[: T]) { stmt* [expr] } ;   // §7
```

`seq { .. }` without a trigger runs once at init. `let rec` is not a
step. `catch` is refused anywhere in a seq body — as a statement,
inside a block, or nested in a step's expression; a lambda literal's
body and its defaults are exempt, because a function is its own dynamic
scope and its own firing world ("as soon as you introduce a lambda
you're basically back in graphix"). `seq`, `seqq`, `until`, `try` and
`with` are reserved words; the old integer-sequence builtin `seq(i, j)`
is `range(i, j)`.

`if`, loops, `break`/`continue` and a `select` whose arms are step
lists are not built; a `select` inside a seq is an ordinary expression
step.

## 5. Semantics

**Run.** A run starts when the trigger fires while no run is in
progress; a trigger during a run is dropped (`seqq` queues instead,
§8). A bare-variable trigger is snapshotted: the body's reads of that
name see the value the run started with.

**Steps evaluate in order, once per entry.** A step's leaves —
constants and reads of variables outside the step — are taken as they
stand when the step is reached, and its effects are issued exactly
once per reaching. A passed step's arm is asleep and nothing in it
re-fires. This is `f(trigger ~ x)` applied mechanically, the rule the
hand-written ceremonies get wrong.

**A statement starts in the first cycle its predecessor's effect can
be seen.** A let's effect is its binding and a call's is its value;
dataflow carries both, so the next statement is issued in the cycle
the one before it produced in. A connect's effect is its write, which
lands the next cycle, so a statement that reads a variable an earlier
statement wrote, or writes it again, starts the next cycle and sees the
write. The statements between two such points share one arm of the
machine (§6.4): `a <- x; b <- y; let s = a + b` writes `a` and `b` in
one cycle and binds `s` the next; `n <- n + 1; n <- n + 1` is two
cycles and `n + 2`. The analysis is by name and cannot see into a call
(a closure may read anything), a read through a reference, or a nested
seq; such a statement is taken to read every pending write, so
`a <- f(x); b <- g(y)` issues `g` after `f` has produced. A block is
the override.

**A block `{ … }` issues its statements together.** Every statement
of a block is issued at the block's entry: `{ a <- f(x); b <- g(y) }`
has both calls in flight at once. A let is local to the block and a
later statement reads it by dataflow, so a call over a block let waits
for it. Each connect and call is clocked to the entry. The block
completes when every statement has produced, with the value of the
last. Inside, it is ordinary Graphix: a statement that reads a variable
a sibling wrote reads the old value, and an error in one statement does
not retract what its siblings issued that cycle. `until` and `try` are
refused inside a block.

**Completion is a FIRED production after the step's entry.** A value
standing at entry is the previous run's answer — a `~`'s held
resident, a lambda instance's own cell, both re-presented at wake
because sleep is pause — and the completion guard holds bottom until
the step fires. A call is re-issued at entry and its own fire is the
answer. A level is fired at entry as it stands (`any(pc ~! e, e)`) and
waited for if absent; `until` is the same shape over a bool. A
connect completes at issue; its write lands next cycle. An async call
completes when the operation completes, not when its arguments arrive;
`print`/`println`/`log` emit `null` after each message, repeats
included, which is what makes them waitable. Bottom means no
completion, so `never()` in a step stalls the run and needs no special
treatment; a third-party effect with no completion event cannot be
waited on without adapting its API.

The casualty of the rule: a lambda that returns a standing level it
does not derive from its argument (`|v| k`) produces no fire when
re-called, in a seq as anywhere else, so a step calling it never
completes on the second run. `|v| v ~ k` is the spelling.

**A `let` binds the step's production for the rest of the run.**
Later steps read it through a carried cell; shadowing is sequential as
in a block.

**A `?` aborts the run, unless a `try` takes it.** The machine
installs ONE handler outermost: it resets the step variable to idle
and rethrows to the enclosing handler. An error raised inside a `try`
body takes the with branch instead (§7). A wrapper `catch` around the
seq is ordinary Graphix and sees an aborted run's error once.

**The value** is the last expression's, fired once per completed run,
stale between runs, bottom before the first completion. A `seq` inside
a lambda is a callable ceremony; a call to one from a step is itself an
async step.

**A nested seq is a step that produces later.** A seq written inside a
step runs as its own machine. Without a trigger it starts every time
its statement is entered (its constant trigger fires at the arm's
wake); with one it waits for that trigger. The enclosing step counts
it as a call, not a level: its result cell stands from the previous
run, and the step completes only on the production of this run. It
costs two cycles over the same statements written inline, one for the
inner start and one for its result. An error in the inner seq resets
the inner machine and rethrows: outside a `try` it aborts the outer
run too; inside one it takes the with branch, and the next run's inner
seq starts afresh.

**Levels live outside.** A level effect (`tui::suspend`,
`sys::net::publish`, a subscription the ceremony watches) must not be a
step: a passed step sleeps and a slept level is torn down. Steps write
the variable that drives the level and `until` waits for its response.
The compiler cannot tell a level effect from a one-shot; the book has
to say it.

**Seq blocks are `Async`.** The machine node-walks; each step's sync
interior fuses as it would anywhere. `#[sync]` on a seq is a compile
error.

## 6. The lowering

An AST-to-AST desugar (`expr/seq.rs`), so both engines inherit the
semantics from one spec. Positions carry from each statement to the
nodes it lowers to, so a type error names the step. `graphix --expand
file.gx` checks the file and prints each seq's lowered machine, source
position first: the machine is inspectable, which is the debugging
story. The completion guards are compiler-only nodes and print as
their operand, so re-parsing the expansion gives the machine without
its error boundaries.

### 6.1 The skeleton

```graphix
{
  let pc: [`Idle, `S0, `S1, ..] = `Idle;
  let idle = select pc { `Idle => true, _ => false };
  let r = never();                          // the block's value
  let x_c = never();                        // one cell per let read across arms
  catch(e) { pc <- `Idle; e? };             // the machine's handler + abort action
  let go = filter(<trigger>, |x| x ~ idle); // busy-drop
  pc <- go ~ `S0;
  select pc {
    `Idle => never(),
    `S0 => <arm 0>,
    `S1 => <arm 1>,
    ..
  };
  r
}
```

One arm per statement; labels are allocated as statements are lowered
and the pc type is the set of every label. The cells need no
annotations: an unannotated `let x = never()` takes its type from its
writers, and a `let` annotation in the source passes to its cell (that
is how a union-typed `try` value is spelled). The busy gate is
`core::filter`, not `t ~ select pc { .. }`: `~` holds a trigger's debt
until its RHS first materializes and then pays it, which is a queue of
one, not a drop. The predicate must consume the trigger
(`|x| x ~ idle`, not `|_| idle` — an unused parameter does not fire
the lambda).

### 6.2 The entry event

Inside an arm, the step variable read as a FREE variable — `pc`
itself — fires on every delivery into the arm: first entry, re-entry,
a same-arm re-delivery. It is the one event every atom below samples
on. It must be a free read and not the arm's pattern bind, because a
nested watch relies on wake catch-up re-raising it, and pattern binds
are excluded from that tracker by design (a pattern bind is a facet of
its arm's scrutinee delivery). The lowering never writes a constant
RHS: a constant connect fires once per SELECTION and not on a same-arm
re-match, so every transition (`pc <- pc ~ \`Sk`) and every carried
write (`x_c <- pc ~ x`) is sampled on the entry event or on the step's
completion, and both land in one batch so the next arm's entry samples
the new values.

### 6.3 The issue atom

A call with explicit arguments `x1..xn` lowers to one snapshot per
entry:

```graphix
{
  let args = (pc, x1, .., xn);
  select (any(pc, core::once(args)) ~! args) {
    issued => f(issued.1, .., issued.n)
  }
}
```

The strict tuple includes the entry clock, so present standing inputs
produce a fresh tuple on entry; `~!` is the strict sample so an entry
that finds an input bottom clears the previous snapshot without
banking a trigger, and `once` supplies the readiness event when the
missing inputs arrive (it resets its admission on sleep). All
arguments, constants and labeled arguments included, come from one
snapshot. Inline lambda arguments stay at the call site, sampled with
`pc ~!` on the same cycle: moving them into the tuple would deprive
their bodies of the call's contextual parameter types. A nullary call
has nothing to re-issue and is a level read at entry.

The completion guard sits on the CALL inside the snapshot select, not
outside it: the select fires at entry carrying the call's resident,
and only the call's own fired production is this invocation's answer.
The select watches the snapshot, not live inputs: once issued, a
pending callee keeps running if an input later bottoms, and later
input events cannot reissue it. Calls nested in expressions and in
blocks lower the same way; lambda bodies and defaults, `until`
conditions and reference contents keep their own reactive clocks.

A call-free step and an `until` condition are `any(pc ~! e, e)`: fired
at entry with the standing value, then tracked, so a level costs the
completion rule nothing and a level that flips after entry is seen.
A compound `e` is bound once so its nodes are not duplicated (a `?`
inside it must raise once). A call-free `?` is therefore sampled on
the entry event too, so a carried error raises at every entry rather
than only when a catch-up fire happens to deliver it.

### 6.4 Arms, blocks, `until`

`split_arms` cuts a statement list into arms: `until` and `try` stand
alone; other statements share an arm until one reads or rewrites a
variable an earlier statement of the arm wrote, or is opaque (a call, a
deref, a nested seq) while such a write is pending; a write through a
reference ends its arm, its target being unknown. The last statement
of an arm writes the carried cells and the transition, `pc`-sampled so
they land with the arm's writes. Inside an arm each statement's
completion arm holds the statements after it (`lower_group`), so a
statement is issued in the cycle its predecessor produced in; the arm
lowers to nested selects and a run is cut at the parser's nesting
limit. A block (`lower_block`) hoists each statement into a `let` under
the arm, connects `pc ~ value`, and joins the statements with a select
over the tuple of their values, present once every statement has
produced. `until` is refused where its value would be used: the last
statement of a seq, or of a try or with body whose value is used, must
be an expression.

### 6.5 What sleep does for free

A passed arm sleeps, and sleep is pause: a timer step's pending timer
is cancelled (`Timer::sleep` unrefs it), a `sys::net` level effect
inside it tears down, a process spawn is not cancelled (`kill_on_drop`
is on drop). There is no cancellation beyond that: a retrigger cannot
stop an in-flight step, and a step that never completes stalls the run
exactly as a hand-written machine does. Busy-drop is the default
because a restart would deliver a stale production into a fresh run.

## 7. Errors

### 7.1 Why a branch, not a `catch`

A `catch` is an install: it can observe an error, but it cannot
produce the value the next step waits for, and it cannot say "the
rest of the block does not run". Inside a sequence it can therefore
only rethrow or stall. Both spellings were built and withdrawn: a
seq-toplevel `catch` as cleanup delivered its rethrown error twice,
and an ordinary `catch` inside a grouped step wedged the machine on a swallow
(the failed statement never produces, so the completion guard keyed
on the user's handler never released). Eric: "we're trying to adapt
an event monitor (`catch`) to something that should be control flow."
A sequence has a program counter, so its error handling is a branch.

### 7.2 `try … with`

```graphix
seq req {
  let cmd = escalate_command(req.argv, req)?;
  let code = try {
    let child = sys::process::spawn(options(cmd))?;
    let status = sys::process::wait(child.proc)?;
    status.code
  } with(e) {
    toast <- failed(e);
    -1
  };
  report(code)
}
```

An error raised anywhere in the try body — a `?` in a step, a callee's
throw, a fused `?` — transfers control to the with body's first step
with `e` bound to the FIRST error of the failure; the with body's last
step continues to the statement after the `try`. An error in the with
body goes to the enclosing `try`, else to the machine (abort, reset,
rethrow). Cleanup-then-abort is `with(e) { cleanup; e? }`; there is no
double delivery because the try consumed the original. No `finally`:
success cleanup is the next statement, failure cleanup is the with
body. `with(_)` is accepted.

The statement's value is the union of the two bodies' last values; a
with body ending in `e?` has an uninhabited residual and types Bottom,
so the union is the try body's type. `e` is typed as a `catch` bind is
— the union of the try body's throws, as `Error<ErrChain<..>>` — and
`with(e: T)` follows `catch(e: T)`'s rule: `T` must cover that union.
A `let` inside either body is scoped to that body; `e` to the with
body. Seq level only: `try` inside a block is refused, like `until`.

**Why a handler underneath, not a syntactic match on `?`.** Errors
reach a region through the DYNAMIC scope, not the text. A callee's `?`
resolves at its call site's handler, so `try { f(x) } with(e) { .. }`
where `f` throws internally has no `?` in the try body at all, and a
fused `?` raises through the same `ErrorHandler` as the interpreter's.
The try body must install a real `Catch` node whose dynamic scope
covers exactly its arms. The same fact is why the machine installs its
own handler unconditionally: syntactic inspection cannot establish
whether a call throws.

### 7.3 The lowering of `try`

A try is a branch whose edge is an error instead of a value. Try-body
arms and with-body arms are ordinary arms of the one select; both
tails write the statement's cell and transition to the join label.
Each try-body arm carries a generated `Catch` whose handler body is
`never()` and whose `seq_abort` action is a JUMP (`pc <- \`W0` instead
of the machine's `pc <- \`Idle`); its `seq_capture` names the `e`
cell. At runtime the capture writes the first delivery of each failure
to the cell; at typecheck it unions its bind's inferred throws into the
cell's type after its siblings, so the union is exact and an arm that
cannot throw contributes ⊥. (A handler-side write `e_c <- once(e)` was
tried first and failed: the connect aliased the cell to the first
arm's frozen bind cell, so a second arm's different error type was
refused, and `once`'s return cell is unresolved when the with body
typechecks.)

Nesting composes by arm ownership: an arm carries the jump handler of
the innermost `try` whose BODY contains its statement; a with-body arm
carries the enclosing try's handler if any and none otherwise. The
handler is per arm but it is only a capture and a jump — one phantom
`Catch` node per try-body arm is the cost of a try body that never
fails. After the jump the failed arm sleeps and gets sleep's cleanup.

### 7.4 Completion guards and the handler ledger

`seq` requires two facts before accepting a step: its value fired after
entry, and no error has escaped to the step's nearest handler. An
ordinary reactive expression can raise an error and still produce a
value; that value must not advance a failed sequence.

Each installed handler owns an error-generation counter
(`ErrorHandler::generation`) and a count of errors pending in
descendant handlers (`has_nested_errors`). A raise advances the
generation and increments the pending count along the dynamic parent
chain BEFORE delivery — the interpreter marks it before
`deliver_error`, a compiled `?` before appending to the kernel's
delivery queue — so frame outboxes and cross-top scheduling can delay
the error value reaching the catch but never the guard observing the
raise. Processing a catch delivery decrements the pending count; a
rethrow raises its new delivery before acknowledging the original, so
the sequence never sees a false gap while errors move outward. A
nested ordinary catch inside a step can consume its own error without
aborting the sequence.

The lowering wraps each step expression, let initializer, connect RHS
and `until` condition in a compiler-only `SeqGuard` (`node/error.rs`),
and each issued call inside its snapshot select in another. The guard
records its handler's generation on activation, holds bottom until the
first FIRED production after activation, then passes every production
(an arm's nested continuation must keep routing on stale cycles). A
generation change produces bottom and latches failure until sleep; the
failed child keeps updating only while nested catches still have
pending errors to drain, its output suppressed. So a failed step cannot
schedule the next pc, write its carried cells, issue a generated
connect or publish the block result, and the handler's reset never
competes with a queued advance. In a try-body arm the nearest handler
is the jump handler, whose generation advances at the raise, and
unlike a user `catch` it always LEAVES the region, so the latch is
never a wedge. The guard is a fusion boundary; its child fuses
normally, and there is no rollback of effects the child already
performed.

The machine's handler body is a compiler-only `Rethrow` over the
existing `?` node: it forwards the catch bind's inferred error type
without adding another `ErrChain`, and a sequence that throws nothing
leaves that input bottom — it neither acquires a throws type nor
raises an unhandled-error warning.

**Multiple errors from one step** (two `?` in one block both raise in
the same cycle): the rethrow runs for every error that reaches the
machine's handler, and the abort action runs ONCE, after the failed
step's errors have all been delivered — the catch counts fresh
deliveries (standing values and activation replay are not deliveries),
marks an abort pending on the first, and fires `seq_abort` under a
forced init view when the received count matches the handler's
generation and no nested errors are outstanding. The counters span the
handler's lifetime, so sleeping does not turn old deliveries into a
fresh abort. For `try`, `e` is the first error and the rest are
consumed. This is the one place the construct differs from an
exception system, and it is the reactive fact underneath: both `?`
evaluated.

## 8. `seqq`: the queued form

`seqq [trigger] { steps }` is the ordinary machine behind one FIFO
queue; there is no separate evaluator. Requests are dropped by `seq`
while a run is busy; `seqq` queues them, each with a snapshot of the
body's captured inputs taken at enqueue time rather than at step entry.

The preamble (`desugar_queued`) identifies the body's free reads by a
scoped rewrite over the current environment — nested binds, patterns
and lambda parameters respected, qualified names included, trait
method dispatchers and function-typed bindings left as static call
targets, since a call through a projection of the request tuple would
resolve dynamically and never fuse — and projects them
out: every trigger samples ONE tuple (the request plus its captures),
never separate queues of independently updating captures. Each capture
is `hold`-latched on activation so a standing input is present for the
first request; before all captures have produced, the tuple is absent
and the sample accumulates request debt, so the first complete tuple
can satisfy several pending requests with the same snapshot — ordinary
sample behaviour, deliberately inherited; exact arrival-time snapshots
require initialized inputs. The body reads its captures as tuple
projections of the dequeued request. `until` conditions, address-taking
and variables the body itself writes stay LIVE (read-modify-write
state must see its own writes across requests); direct write targets
are never redirected into a snapshot, and a dereferenced write uses
the queued reference handle.

The queue is `core::queue` clocked by a credit variable: one initial
credit starts the first request; each block output returns one; an
abort returns one after all the run's errors have reached the handler
and been rethrown (multiple errors cannot release extra requests, and
the captures stay pinned throughout delivery). Neither a standing
output nor an intermediate step releases a request. The credit
variable is separate from the activation latch, so sleep and wake
cannot suppress the initial credit by preserving a connect target's
old value. A taken `with` branch is not an abort: the run continues
and returns its credit at completion; an abort from the with body is
the machine's abort.

## 9. Costs

One cycle per async completion and per read of an earlier statement's
write; a cycle is well under
a millisecond in release (a text key is 0.17ms end to end at 5.4k
lines), so a six-step ceremony adds about a millisecond to work that
takes seconds. A taken with branch costs the failed step's drain (one
cycle per extra error that step raised) plus the jump. The machine is
longer than the `~` chain it replaces — it does not save typing; the
keyword earns its keep as the surface in §2.
