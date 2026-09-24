# Dependency summaries, and a seq machine that uses them

Status: built 2026-09-24 (summaries, the machine node, `seqq`
captures from summaries).
Pins: `stdlib/graphix-tests/src/lang/seq_steps.rs` (each failure in §1,
the wake re-raise, a same-cycle `until`, a `let`'s fire in a later
cycle), the netidx-admin tests (`roster_adds_and_edits_an_admin` found
the carried `let`), the seq suites listed in
`seq_blocks.md`, `graphix-compiler/src/expr/seq.rs`
(`a_long_seq_lowers_flat`).

**What each piece of code reads and writes, known after resolution.**
A summary says which variables a node reads and which it writes,
following statically resolved calls into their bodies. The analysis
pass (`graphix-compiler/src/analysis.rs`) computes it, and its first
consumer is `seq`: the machine is a compiler node that decides its step
boundaries per instance from the summaries. Automatic parallel
evaluation is the second consumer (§6).

## 1. The problem

A seq starts each statement in the first cycle the effect of the
statement before it can be seen (`seq_blocks.md` §5). The lowering used
to decide this from the seq's own text, before compilation: it saw
`<-` targets and `&` operands and treated every call as a reader of all
pending writes. Four programs showed where that fell short:

```graphix
let b = 0; let put = |v| { b <- v; v };
seq go { put(5); let s = b; s }                      // was 0, is 5

let c = 0; let r = &c; let set = |p: &i64, v| { *p <- v; v };
seq go { set(r, 5); let s = c; s }                   // was 0, is 5

seqq go { put2(5); until true; let s = b2; s }       // was 0 for good, is 5

let v = 0; let w = never();                          // v <- 7 at 50ms, w <- 1 at 100ms
seq go { let a = v; let b = w; a + b }               // was 8, is 1
```

1. A closure's write to a variable it captured was invisible.
2. So was a write through a reference that reached the call through a
   variable; only a `&` written in the seq counted.
3. `seqq` captured a variable only a callee wrote, so no step ever saw
   the write.
4. Statements that shared an arm lowered to nested selects, so a passed
   statement stayed awake while the next waited, and `a` followed `v`.

Failure 4 governs the fix: an arm boundary was not just a cycle of
delay, it decided which steps were awake. The two facts the boundary
needs are known only after typecheck: which body a call reaches
(`CallSite::typecheck1` pre-binds, a callback resolves per instance),
and which instance is which (`|f| seq go { f(1); let s = b; s }` needs a
cut where `f` writes `b` and none elsewhere).

## 2. The summary

```rust
struct Vars { all: bool, refs: bool, ids: IntSet<BindId> }
struct Summary { reads: Vars, writes: Vars }
```

For a node (a seq step, an instance body), `analysis::local_summary`
walks it (`fusion::for_each_node`):

- a `Ref` reads its id, a `Connect` writes its target;
- a `Deref` reads `refs` and a `ConnectDeref` writes `refs`: whatever
  some reference points to, which may be any variable;
- a call reaches its instance (a static target, a resolved lambda, a
  self-bind of the call graph); a builtin handed a reference, a
  function or an `Any` reads and writes `all`, and one handed neither
  touches nothing; any other call, and a dynamic module, read and
  write `all`.

An instance's summary is its body's joined with its callees', to a
fixpoint over the instances the machines' calls reach
(`instance_summaries`, a union worklist over the `StaticCallGraph`
the effect inference builds). Nothing is stored: each analysis pass
recomputes what its machines need, and a pass with no machine computes
nothing. An imaged callee not yet decoded has no body to walk and is
opaque. There is no alias analysis.

A callback written inline in a seq step is issued as `pc ~! |x| ..`
(`seq_blocks.md` §6.3); `CallSite` resolves it as the literal it
samples (`callsite.rs::lambda_literal`), so a step calling
`array::map(xs, |x| ..)` reaches the callback's instance, and the
loop fuses as it does outside a seq.

## 3. The seq machine

**Every statement is a step, and a passed step sleeps.** Which steps
are awake no longer depends on grouping; the one decision per boundary
is whether step `j+1` enters in the cycle `j` completes or the next.

The desugar (`expr/seq.rs`) keeps the preamble (the `pc` variable, the
busy gate, the machine's handler, the `abort` event, the `seqq` queue)
and the per-statement atoms (issue snapshots, `SeqGuard`, `try`'s jump
handlers), and emits the steps as `ExprKind::SeqMachine`: per step a
label, its items (handlers, `let <value> = ..`, completion writes), the
index of the step after it and a lexical scope (a try or with body's
own). `node/seq_machine.rs` runs it:

- **Entry.** A step enters when `pc` fires its label. At a same-cycle
  boundary the machine publishes the new label within the cycle
  (overlay, store, `notify_set`, as a `Bind` publishes), so the atoms'
  entry event is `pc` in both cases. Entering wakes the step under the
  wake view with `Select`'s catch-up (`node/wake.rs::TrackedFires`,
  moved out of `select.rs` and shared): one fire bit per step-body
  input per machine, consumed by whichever step reads it. After the
  steps it ran, the machine records the fires they made for the steps
  that did not run (`observe_except`); a step's own `let` is recorded
  even where a step of its cycle read it, so its fire also reaches the
  first step that reads it in a later cycle, as a carried cell's fire
  reached the next arm (`seq_steps::let_fire_reaches_a_later_cycle`:
  `let v = f(go); *r <- v; v ~ 5` would otherwise never complete).
- **Completion.** The value's `let` publishing a FIRED production (a
  fired `true` for an `until`); only then do the completion writes run.
  The completed step sleeps (`deselect`, as a select's arm), then the
  next enters in this cycle or `pc` is written for the next.
- **Lets** are ordinary binds in the machine's scope, or a try or with
  body's scope; the step that bound one sleeps once passed, so it keeps
  its step's value. There are no carried cells. A `try`'s value, when
  used, is a join cell both bodies' last steps write and a join step
  reads.
- **Sleep.** A sleeping machine sleeps its steps and is idle; its
  handler also writes `pc` idle (`seq_blocks.md` §6.5).

**The boundary rule** (`analysis::plan_machines`, per instance, before
the first cycle; `analyze_bound_callee` for a runtime-bound body).
Step `j+1` enters in the next cycle when its summary's reads or writes
meet the writes pending since the last next-cycle boundary (`all` and
`refs` meet any non-empty set; `pc` is left out). Otherwise it enters
in the same cycle. Every statement follows it, `until` and `try`
included; the jump into a `with` body writes `pc`, so it is the next
cycle. A call with no static target costs a cycle before a later read
of an outer variable; that price is accepted. A machine not planned
enters every step the cycle after (`Step::same_cycle` starts false).
There is no nesting, so no nesting limit.

`--expand` prints the machine at compile and each instance's
boundaries after the analysis; `GXDBG_SEQPLAN=1` prints every step's
summary, each capture's choice and every opaque call.

**The plan is imaged** with the machine (`same_cycle` per step), so a
warm start runs the plan the cold compile decided.

## 4. `seqq` captures

`desugar_queued` captures every outer variable the body only reads by
name. A captured variable some step may write through a callee is a
`SeqCapture` (`ExprKind::SeqCapture`, `node/seq_machine.rs`): the
queued snapshot, or the variable itself when the analysis finds that a
step of its machine names it among its writes (`Vars::names`: an `all`
counts, `refs` does not). A write through a reference keeps the queued
handle and makes no capture live, as before; `&x` in the body already
keeps `x` live. A live capture's reads count as reads of its variable
for the boundary rule. The trigger's own capture is always the queued
request.

## 5. What changed for existing programs

- A `let` no longer follows its source once its step has passed.
- A step after an `until` enters in the cycle the `until` completes and
  reads what stands then (`seq_calls::until_stays_live`: 10, was 11);
  a run of several same-cycle steps ends sooner, so an abort must come
  earlier to win (`seq_abort::abort_beats_completion`).
- A run whose last write targets its own trigger ends before the write
  lands, and the write starts the next run, as a one-statement seq over
  its own trigger always did; the old lowering's name match cut such a
  run a cycle longer whenever a later statement named the trigger
  (`seq_let::trigger_writes_reach_the_variable` now holds each run past
  its write).
- A callback written inline in a seq step resolves statically, so its
  collection loop fuses.

## 6. Parallel evaluation

What the summary provides: within a cycle, reads never conflict. A
connect's write lands next cycle, and a `let`'s value delivered within
the cycle is an edge of the graph, not shared state. Two subgraphs
conflict over variables only when both write the same variable: the
next cycle's queue for that variable is in evaluation order
(CLAUDE.md, Runtime). Write sets decide that.

What it doesn't provide:
- the order of external effects (`println`, network writes);
- node state reached through `&mut ExecCtx`.

Both belong to the evaluator's own design. The summary is its variable
half.
