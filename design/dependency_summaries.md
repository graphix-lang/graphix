# Dependency summaries, and a seq machine that uses them

Status: proposal 2026-09-24; nothing here is built.
Pins: none yet. §7 lists the pins to write first; each fails on the
tree today.

**What each piece of code reads and writes, known after
resolution.** A summary says which variables a node reads and which it
writes, following statically resolved calls into their bodies. The
analysis pass computes it next to the effect facts it already infers
(`graphix-compiler/src/analysis.rs`). Its first consumer is `seq`,
whose machine then becomes a compiler node that decides its step
boundaries per instance. Automatic parallel evaluation is the second
consumer (§6).

## 1. The problem

A seq starts each statement in the first cycle the effect of the
statement before it can be seen (`seq_blocks.md` §5). `split_arms`
(`expr/seq.rs`) decides this from the seq's own text, before
compilation. It sees `<-` targets and `&` operands in that text, and
treats every call as a reader of all pending writes. Four programs show
where that falls short, all run on the current build:

```graphix
let b = 0; let put = |v| { b <- v; v };
seq go { put(5); let s = b; s }                      // s = 0; §5 says 5

let c = 0; let r = &c; let set = |p: &i64, v| { *p <- v; v };
seq go { set(r, 5); let s = c; s }                   // s = 0; §5 says 5

seqq go { put2(5); until true; let s = b2; s }       // s = 0, for good

let v = 0; let w = never();                          // v <- 7 at 50ms, w <- 1 at 100ms
seq go { let a = v; let b = w; a + b }               // 8; §5 says 1
```

1. **A closure's write to a variable it captured is invisible.** The
   read of `b` shares the arm with the call, so it reads the old value.
2. **So is a write through a reference that reached the call through a
   variable.** Only a `&` written in the seq itself counts.
3. **`seqq` captures a variable that only a callee writes.** Its body
   only reads `b2`, so the capture snapshots `b2`, and no step ever
   sees the write, even across a cut.
4. **A step that has passed stays live when it shares an arm with the
   step after it.** An arm lowers to nested selects, so the select for
   `let a = v` is still awake while `let b = w` waits, and `a` follows
   `v` to 7. §5 says a `let` binds its step's production for the rest
   of the run, and that a passed step is asleep. Across a cut the
   earlier arm sleeps and `a` stays 0.

Failure 4 is independent of summaries, but it governs the fix: an arm
boundary is not just a cycle of delay. It also decides which steps are
still awake. So a boundary cannot become "same cycle or next, decided
later" while the arms are nested selects.

Two facts are known only after typecheck:
- **Which body a call reaches.** `CallSite::typecheck1` pre-binds
  statically resolvable calls, and a higher-order function's callback
  resolves per instance.
- **Which instance is which.** `|f| seq go { f(1); let s = b; s }`
  needs a cut in an instance whose `f` writes `b`, and none in another.

A rewrite that runs before compilation has neither.

The rule also costs cycles in the other direction: `a <- f(x); b <-
g(y)` waits a cycle for `a` even when `g` never reads it.

## 2. The summary

```rust
enum Vars { Set(SmallVec<[BindId; 8]>), All }
struct Summary { reads: Vars, writes: Vars }
```

For a node N (a seq statement, or an instance body):

- **`reads`**: the variables N reads, excluding those N binds. `Refs`
  already collects exactly this (`refed` minus `bound`), and
  `CallSite::refs` already folds in a statically resolved callee's
  body. An imaged callee that hasn't been decoded yet answers with its
  `RefsSummary`.
- **`writes`**: the target of every `Connect` in N, excluding N's own
  binds, through statically resolved callees, transitively. Nothing
  collects this today: `Connect::refs` reports only its value's reads.
- **`All`** is the answer where the target can't be named:
  - `ConnectDeref` writes `All`, since a reference can come from
    anywhere (failure 2);
  - `Deref` reads `All`;
  - a call with no static target, and a dynamic module, read and write
    `All`.

  This is today's treatment of an opaque statement, applied only
  where the code really is opaque. There is no alias analysis.

**Computation.** Summaries are computed in `analysis.rs`, over the
`StaticCallGraph` the effect inference already builds, with the same
worklist:
- an instance's summary is its body's own contribution joined with its
  callees' summaries;
- the order is least-first: start empty, join is union, `All` absorbs
  everything;
- recursion converges because the set of variables is finite.

A callee outside the current analysis (a runtime-bound body analyzed by
`analyze_bound_callee`) contributes its stored summary; an unknown one
contributes `All`. A statement's summary is its node's contribution
joined with the summaries of the call sites in it.

**Storage.** The summary is stored per instance, on `GXLambda`, and set
before the instance first runs. `RefsSummary` gains the `writes` half,
so an imaged callee answers without being decoded.

**Cost.** Summaries are computed on demand: only for instances
reachable from a consumer, memoized per instance. The pass runs inside
`--check`, and the typechecker must stay instant, so it is measured on
the GUI suite and on the admin package before it lands.

## 3. The seq machine

**Every statement is a step, and a passed step sleeps.** Which steps
are awake no longer depends on how the statements are grouped. The one
decision left per boundary is whether step j+1 enters in the cycle
where step j completes, or in the next one. The analysis makes that
decision, per instance, from summaries.

**What stays a desugar:**
- the preamble: the `pc` variable, the busy gate, the machine's
  handler, the `abort` event and the `seqq` queue with its credit;
- the per-statement atoms: issue snapshots, `SeqGuard`, and `try`'s
  jump handlers.

These already have pins, and none of them depends on where the arms
split.

**What becomes a node:** the `select pc { .. }`, the nesting within an
arm, the carried cells and `split_arms`. A compiler-only `SeqMachine`
node holds the steps, each a label, a compiled statement and the
boundary that follows it.

**How the machine runs:**
- **Entry.** Today an atom samples on `pc`, read as a free variable. A
  same-cycle entry does not write `pc`, so each step reads a hidden
  entry variable instead. The machine delivers it within the cycle,
  through the overlay path a `let` uses, before it updates the step.
- **Lets.** The statements compile in one block scope, so a `let` is an
  ordinary bind over the later steps. The step that bound it sleeps
  once passed, so the value stands for the rest of the run. The carried
  cells go away, and so does the rebind rule (a write or a `&` of an
  arm-local name forcing a cut). A write to a seq `let` writes its
  bind.
- **Same-cycle boundary.** When step j completes (its update returns a
  fired production), the machine sleeps step j, delivers the entry of
  step j+1 and updates it, all in the same cycle.
- **Next-cycle boundary.** The machine writes `pc`, which lands next
  cycle, as today. A `try` jump and the machine's resets (handler and
  abort) keep writing `pc`.
- **Sleep.** When its own arm sleeps, the machine sleeps its current
  step and resets to idle, as today.
- **Wake.** Entering a step wakes it, and the wake follows the
  language's rule (`wake_catchup.md`): the step recomputes from the
  present, and each input fire no awake reader saw is re-raised once.
  For wake purposes the machine is a `Select` whose arms are its
  steps: one fire bit per step-body input per machine, consumed by
  whichever step reads the input. Today every step is entered through
  a `Select` wake, whether in its own arm or nested in a shared arm,
  and programs observe it:

  ```graphix
  let click = never(); click <- sys::time::after_idle(duration:20.ms, 1);
  seq go { sys::time::after_idle(duration:60.ms, 0); let r = click ~ 7; r }
  ```

  gives 7 in both engines. The step is entered at 60 ms, and the fire
  at 20 ms is re-raised at its entry. Without catch-up, `click ~ 7`
  has nothing at entry and the run stalls until the next click. So
  the machine uses `Select`'s tracker (`TrackedFires`, `deselect` in
  `node/select.rs`), moved out where both nodes share it, not a second
  implementation. The one difference is that the machine can change
  steps within a cycle. A fire in the cycle of a same-cycle entry is
  delivered live to the entered step, as it is today to a nested arm
  selected in that cycle.

**The boundary rule.** Step j+1 enters in the next cycle when its
summary's reads or writes meet the writes pending since the last
next-cycle boundary (`All` meets any non-empty set). Otherwise it
enters in the same cycle.

Every statement follows this rule, `until` and `try` included; neither
is cut on both sides any more. A block `{ .. }` is one step. Entering a
`try` body is an ordinary boundary. The jump into the `with` body is a
`pc` write, so the `with` body enters in the next cycle.

A call with no static target reads and writes `All`, so a read of an
outer variable after it costs a cycle; that price is accepted. The
nesting limit on arms goes away, because there is no nesting.

**Per instance.** A seq in a lambda body has its own node in each
instance, and each instance gets its own plan. `analyze` runs before
the program's first cycle, and `analyze_bound_callee` before a
runtime-bound body's first dispatch. A machine whose plan isn't set
yet uses next-cycle everywhere, which is always sound.

**What else has to follow:**
- **Fusion** must descend into the machine's steps, as it descends into
  `Select` arms today, or step interiors lose fusion. The fusecheck
  manifest catches a loss.
- **The image codec** writes the steps and each instance's plan. Images
  are written before the first cycle, so there is no run state to
  encode.
- **`--expand`** prints after analysis: the desugared statements, then
  each instance's boundaries.

## 4. `seqq` captures

Failure 3 has the same cause. `desugar_queued` captures every outer
variable the body only reads, and "only reads" is decided from the
text. With summaries, a variable any step writes, including through a
callee, stays live.

Captures are a tuple projection chosen before compilation, so moving
the choice later means the machine owns the snapshot: during a run it
delivers the captured values as overlays for the captured variables. Its
own step, after the machine lands (§7). Until then the book says a
capture of a variable only a callee writes never sees the write.

## 5. What changes for existing programs

- **Failure 4 is fixed.** A `let` no longer follows its source once its
  step has passed, so the program in §1 gives 1. Any program that
  relied on the old behaviour changes; none is known. The pins come
  first, then the soak.
- **Some seqs lose cycles:** where a call is shown not to read a
  pending write.
- **Some gain cycles:**
  - where a callee writes something the next step reads (failures 1
    and 2, the point of the change);
  - where a call with no static target comes before a read of an outer
    variable.
- **`until` and `try` can start in the cycle their predecessor
  completes**, where today each starts an arm of its own.
- **Timing-sensitive tests will move.** The netidx-admin tests watch
  cycle timing, so they run after each step (CLAUDE.md).

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

## 7. Order of work

1. **Pins.** One for each failure in §1, asserting what §5 of
   `seq_blocks.md` says, in both engines. All fail today; the `seqq`
   pin stays failing until step 4. Two more that pass today and must
   keep passing:
   - the wake re-raise in §3;
   - a same-cycle `until` whose condition is already true at entry.
2. **The summary.** Build it in `analysis.rs`, add `writes` to
   `RefsSummary`, and add a debug flag that prints each instance's
   summary. Measure `--check` on the GUI suite and the admin package.
3. **`SeqMachine`.** Delete `split_arms`, the carried cells and the
   nesting cut. Then run: the gate, fuzz regress with fusecheck, the
   admin tests, and a soak (this is a semantics change).
4. **`seqq` captures from summaries.**
5. **Docs.** Rewrite `seq_blocks.md` §5 and §6 as built, then update
   `book/src/core/seq.md` and the graphix-lang skill.
