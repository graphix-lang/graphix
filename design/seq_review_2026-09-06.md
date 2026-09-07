# Seq review follow-up — what remains (2026-09-06)

Re-check of the 2026-09-05 `seq` review (the informal one, saved outside
the tree) against `c46fd6c1`. Distinct from
[seq_review_2026-09-04.md](seq_review_2026-09-04.md) and its F-series;
items here are numbered R1–R7.

The lowering changed substantially in `2fa4bd64`, `ee591d28` and
`13323013`, and the design grew §7.3's issue atom, §7.4's arm-chaining
rule and §7.8's completion guards. Most of the review is closed. Each
verdict below was reproduced with a witness on the current debug shell,
in both engines; the witnesses are inline so this file stands on its own.

Run one with:

```sh
timeout -k 2 40 ~/tmp/target/debug/graphix repro.gx
timeout -k 2 40 ~/tmp/target/debug/graphix --no-fusion repro.gx
```

## Status of the original items

| item | verdict |
|---|---|
| §7.3 issue atom never built | **fixed** — `issue_call` ([seq.rs:691](../graphix-compiler/src/expr/seq.rs#L691)), `Rewrite::Issue` |
| (a) a step that doesn't fire at entry stalls the run | **fixed** |
| (b) ledger 16 — a step reading a let from two steps back | **fixed** |
| (c) the port's privileged handoff (println, then a call with standing args) | **fixed** |
| (d) a step re-issues its effect | **fixed** (one issue per run) |
| (d) …and reads late values | **withdrawn** — this is the design; see below |
| `lower_do_stmts` unguarded recursion | **open (R1)** |
| trigger-name shadowing kills the machine | **fixed** — pins in `lang/seq_shadow.rs` |
| `expr_may_throw` is syntactic: no handler for a callee-thrown error | **fixed** |
| …a user catch + no syntactic `?` swallows | **fixed** |
| …a user handler that rethrows delivers twice | **closed (R2)** — seq-toplevel `catch` is refused; `catch` inside `do` is ordinary |
| the machine calls bare `filter` | **open (R3)** |
| `until` as the last step is a silent bottom | **open (R4)** |
| `until` inside `do` / nested `do` dead code | **partly open (R5)** |
| `rewrite` re-spells the whole `ExprKind` enum | **open (R6)** |
| `seq.rs`'s dead `Until(_) \| Bind(_) \| _` patterns | **open (R5)** |
| docs and coverage drift | **open (R7)** |
| the proptest round-trip doesn't generate `seq` | **partly fixed (R7)** |
| a `do`-level `catch` that swallows wedges the machine | **closed (R8)** — `catch` refused anywhere in a seq; `try … with` built (seq_blocks.md §7.9) |
| a handler's `e?` double-wraps the error TYPE of a call-originated error | **fixed (R9)** — `fix_echain_typ` recognizes the expanded chain; `error_payloads` destructures again |
| a `~` step re-enters on its previous run's resident | **open (R10)** — found by the try pins; pre-existing |

### The withdrawn item

The review claimed that during an async wait an unrelated input firing
lets the step read a late value: `seq go { let v = slow + x; v }` with
`x` moving before `slow` arrives produced 102 where the review wanted
101. The re-issue half is real and fixed — the effect now runs once.
The *value* is the design: §7.3's `once` admits the first **complete**
tuple, so an input that is bottom at entry issues at readiness with
every argument sampled at that moment. `inputs_ready_together`
(`lang/seq_calls.rs`) pins exactly this (`a` moves 10 → 20 before `b`
arrives; the expected result is 50). The review assumed entry-sampling;
the tree is self-consistent and pinned, and the claim is withdrawn.

## R1 — P1: `lower_do_stmts` is still unguarded

Location: [seq.rs:500](../graphix-compiler/src/expr/seq.rs#L500)
(`lower_do_stmts`, which recurses once per statement through its `tail`
closure) and [seq.rs:756](../graphix-compiler/src/expr/seq.rs#L756)
(`rewrite_with`). `crate::stack::ensure_sufficient` was added to
`collect_step_binds` ([seq.rs:454](../graphix-compiler/src/expr/seq.rs#L454))
and `expose_step_binds` ([seq.rs:479](../graphix-compiler/src/expr/seq.rs#L479)),
but not to these two.

```graphix
{
  let go = 1;
  seq go { do {
    println("s0");
    // ... 2000 statements
    println("s1999")
  } }
}
```

Expected: a `--check` error or a clean compile. Observed:

```
thread 'tokio-rt-worker' has overflowed its stack
fatal runtime error: stack overflow, aborting
```

`graphix --check` exits 134. 1000 statements compile; 2000 abort. A
plain 2000-statement block (no `seq`) is fine, so the parser's
`DEFAULT_MAX_NESTING` does not bound this — statement lists are parsed
iteratively and uncounted, and the desugar builds a nested-select AST
from them. `gdb -batch -ex run -ex bt` names the frames:

```
#9  graphix_compiler::expr::seq::simple_name
#10 graphix_compiler::expr::seq::rewrite_path
#11 graphix_compiler::expr::seq::rewrite_with
#12 graphix_compiler::expr::seq::rewrite_with
#13 graphix_compiler::expr::seq::lower_do_stmts
#14 graphix_compiler::expr::seq::lower_do_stmts     (repeating)
```

An abort cannot be caught, and CLAUDE.md's stack discipline says the
nesting limit is what makes the drop cycles unreachable — a desugar that
builds AST deeper than the limit is outside that guarantee. Two
`ensure_sufficient` wrappers close it.

Aside, not the same bug: a 2000-**step** `seq` body (no `do`) does not
abort, but `--check` does not finish in 180s. That is a scaling
question about the arm count and the `pc` type, not a stack question.

## R2 — closed: seq-toplevel `catch` is refused

The double delivery was a seq-toplevel `catch` inlined into the same
node as the machine's `Rethrow`. A seq-statement `catch` would abort
the run; the same `catch` inside `{ ... }` would not. That split is
not worth the surface. Seq-toplevel `catch` and `{ ... }` are compile
errors. Cleanup wraps the seq. Grouped ordinary Graphix, including
`catch`, lives in `do { ... }`: the catch is an install covering later
do-statements, not a value-gated step, and a rethrow reaches the
generated handler once.

Original write-up follows.

## R2 (original) — P1: a cleanup handler that rethrows delivers twice

Location: [seq.rs:147](../graphix-compiler/src/expr/seq.rs#L147). The
generated handler body is `[user handler (if any), Rethrow(e)]`, and the
`Rethrow` is pushed unconditionally. A user cleanup handler that
rethrows therefore rethrows twice.

```graphix
{
  let step = 0;
  step <- select step { n if n < 30 => n + 1, _ => never() };
  let go = select step { 1 => 1, _ => never() };
  let outer = 0;
  let bad = |v| { error(`Oops)?; v };
  catch(e) outer <- (e ~ outer) + 1;
  seq go { catch(e) e?; bad(go) };
  select step { 29 => { println("outer=[outer]"); sys::exit(0) }, _ => never() }
}
```

Expected: `outer=1` — one error, one delivery. Observed: `outer=2`.

It reproduces in every spelling: `seq` and `seqq`, the error raised in a
callee or inline in the step, and with a selective rethrow
(`catch(e) select (e.0).error { `Oops => e? }`) as well as a bare `e?`.
The inner handler itself runs once, so it is the appended `Rethrow`
that doubles. Controls that give the correct `outer=1`: no user catch;
a user catch that swallows (`catch(e) null`); and — the important one —
the same handler written one block deeper, `seq go { { catch(e) e?; bad(go) } }`,
which is an ordinary catch rather than the §7.8 cleanup catch.

That last control is why the suite misses this. `nested_handler_choices`
(`lang/seq_errors.rs`) is written in exactly that inner-block form, so it
exercises ordinary catch semantics, not the machine's handler. No test
covers a cleanup handler that rethrows.

§7.8's own sentence settles what it should be: "Handler-side `?` resolves
to the predecessor, so the enclosing block's catch sees the error as it
would from a plain block" — and from a plain block, `catch(e) e?` delivers
once. Three plausible fixes, Eric's call: skip the appended `Rethrow`
when the user handler rethrows on every path; make `?` in a cleanup
handler a compile error (the machine always rethrows, so it is never
needed); or make `Rethrow` idempotent within one error generation.

## R3 — P2: the machine calls bare `filter`

Location: [seq.rs:645](../graphix-compiler/src/expr/seq.rs#L645),
`apply_filter`, whose function is `r#ref(pos, "filter")` — an
unqualified name resolved in the user's scope.

```graphix
{
  let step = 0;
  step <- select step { n if n < 20 => n + 1, _ => never() };
  let go = select step { 1 => 1, _ => never() };
  let filter = 42;
  let out = 0;
  seq go { out <- go + filter };
  select step { 19 => { println("out=[out]"); sys::exit(0) }, _ => never() }
}
```

Expected: `out=43`. Observed, a hard compile error inside generated code
the user never wrote:

```
in: let seqgo6184 = filter(go, |x| x ~ seqidle6184)
in: filter(go, |x| x ~ seqidle6184)
expected fn not i64
```

`filter` is an ordinary name in a TUI. `core::filter` resolves correctly
even under the shadow, so this is a one-word fix. The same audit applies
to every other bare name the desugar emits.

## R4 — P2: `until` as the last step is a silent bottom

Location: [seq.rs:383](../graphix-compiler/src/expr/seq.rs#L383),
`step_arm`'s `Until` arm. It ignores its `last` parameter, so it never
writes the result cell; when `until` is the final step the seq's value
is never produced.

```graphix
{
  let step = 0;
  step <- select step { n if n < 20 => n + 1, _ => never() };
  let go = select step { 1 => 1, _ => never() };
  let r = seq go { let x = 1; until (x > 0) };
  println(r ~ "seq produced a value");
  select step { 19 => { println("done"); sys::exit(0) }, _ => never() }
}
```

Expected: a compile error — the last step must be an expression.
Observed: `--check` exits 0, and only `done` prints; `r` is permanently
bottom. Compare `until` inside `do`, which now gives a clean "until is
not a do statement" ([seq.rs:530](../graphix-compiler/src/expr/seq.rs#L530)).
The same treatment here would be consistent.

## R5 — P3: two dead sites

Both are the review's original claims; the third part of that item —
`collect_do_bind` and its `j * 32 + k + 1` index scheme — is gone.

**Dead patterns.** [seq.rs:79](../graphix-compiler/src/expr/seq.rs#L79):

```rust
ExprKind::Until(_) | ExprKind::Bind(_) | _ => steps.push(e),
```

The `_` subsumes both named patterns. (This moved from line 50 to 79
during the rework, which is how it survived a first pass of this
re-check.)

**Dead arm.** [seq.rs:538](../graphix-compiler/src/expr/seq.rs#L538),
`lower_do_stmts`'s `SeqDo` flatten arm, which handles a `do` nested
inside a `do`. `seq_do`'s body parser is `choice((until_expr(), expr()))`
and `expr()` has no `do`, so the grammar cannot produce that shape:
`seq go { do { do { let x = 1; x } } }` is a parse error. Either delete
the arm or make nested `do` legal.

## R6 — P3: `rewrite_with` re-spells the enum

[seq.rs:756](../graphix-compiler/src/expr/seq.rs#L756) is 310 lines of
the file's 1161, almost all of it rebuilding `ExprKind` variants
unchanged. A scope-aware `Expr::map_children` would reduce it to the
`Ref`/`Connect`/binder arms. There is no such helper today, and
`graphix-fuzz/src/mutate.rs` (955 lines) has the same rebuild half — a
real second consumer, which is what clears the abstraction bar.

## R7 — P3: docs and coverage drift

- [README.md](README.md) lists `seq_blocks.md` twice (line 39 as built,
  line 46 as proposed-not-built). The index has also drifted: this file
  and `seq_review_2026-09-04.md` are indexed under "Review records"
  below, but `async_sleep_outputs.md`, `place_references.md`,
  `seq_error_guards.md` and `seqq.md` are still absent.
- `range` still carries `` `SeqError ``, a public error tag named for a
  builtin that no longer exists —
  `stdlib/graphix-package-core/src/graphix/mod.gxi:157`, `mod.gx:15`,
  and two `literal!("SeqError")` in that package's `lib.rs`.
- `--expand` is still not built (no "expand" anywhere in
  `graphix-shell/src`). §7 calls the machine's inspectability "the
  debugging story"; it would have made the four §7.3 symptoms obvious
  on sight.
- CLAUDE.md:352 still says dev builds are `opt-level=0`; `Cargo.toml`'s
  `[profile.dev]` is `opt-level = "s"`.
- The proptest round-trip now generates `ExprKind::Seq`
  (`expr/test.rs:1443`), which closes the original item, but `SeqDo` and
  `Until` appear only in the comparison helper (`:2121`, `:2122`) and are
  never generated. Printer/parser drift for `do` and `until` is still
  uncovered. (`do_block!` in the generator builds `ExprKind::Do`, an
  ordinary block — not `SeqDo`.)

## R8 — P1: a `do`-level `catch` that swallows wedges the machine

New with the R2 change. Location:
[error.rs:580](../graphix-compiler/src/node/error.rs#L580) —
`SeqGuard::compile` takes `scope.dynamic.handler()`, the NEAREST
handler — together with [seq.rs:547](../graphix-compiler/src/expr/seq.rs#L547),
which installs a do-level `catch` as a block sibling ahead of the guarded
statements, so the user's catch IS that nearest handler.

```graphix
{
  let step = 0;
  step <- select step { n if n < 40 => n + 1, _ => never() };
  let go = select step { 1 | 10 => step, _ => never() };
  let runs = 0;
  catch(e) println(e ~ "outer");
  let r = seq go {
    do {
      catch(e) println(e ~ "swallowed [go]");
      select go { 1 => { error(`Oops)?; go }, _ => go }
    }
  };
  runs <- r ~ runs + 1;
  select step { 39 => { println("runs=[runs]"); sys::exit(0) }, _ => never() }
}
```

Expected, per §7.8 and the book ("an error handled by a `catch` inside
`do { ... }`, without rethrowing to the sequence, does not abort the
run") and because the statement still produced a value: `swallowed 1`,
the first run completes with `r = 1`, request 10 runs, `runs=2`.
Observed in both engines: `swallowed 1` then `runs=0`. The first run
never completes, request 10 is busy-dropped, and nothing ever resets
the PC — the sequence is wedged for good. Control: the same catch one
brace deeper, `do { { catch(e) ...; select go { ... } } }`, completes
with `r = 1` and takes request 10. A different result for a pair of
braces is the split R2 set out to remove.

Why: a guard records ITS handler's error generation, and a raise
advances the handler's generation immediately, whether or not the
handler will rethrow. For a do-level catch the guard's handler is the
user's catch, so the guard latches `Failed` on the raise, never passes
the value, and the machine's handler (which is what resets the PC)
never hears of it. In the nested-block spelling the user's catch sits
INSIDE the guarded subtree: the guard's handler is the machine's, and
the nested count holds the guard until the user's handler has run —
swallow passes the value, rethrow advances the machine's generation and
latches.

Fix: key every guard on the SEQUENCE's handler, not the nearest one.
`Catch::compile` knows which catch is the machine's
(`c.seq_abort.is_some()`); flag that node in the `DynScope` chain and
have `SeqGuard::compile` walk to the nearest flagged node. The nested
count already propagates through intermediate handlers, which is what
makes the nested-block spelling right today, so the do-level spelling
inherits the same behavior: a swallowed error leaves the statement's
own value in charge — a value continues, a bottom stalls like
`never()`, exactly §7.8's sentence.

Pin: `lang/seq_errors.rs` beside `nested_handler_choices`, the same
swallow/rethrow handler pair with the catch as a do-statement; expected
`[1, 2, 3]` / `[2, 3]` like the nested one.

**Disposition (2026-09-07, Eric):** superseded rather than fixed, and
BUILT the same day. The nearest-vs-machine keying is a symptom: a
`catch` is an event monitor, and a sequence needs control flow.
`catch` is refused anywhere in a seq body and the construct is
`try … with` ([seq_blocks.md §7.9](seq_blocks.md#79-try--with), R11).
Under it the nearest-handler keying is correct by construction,
because the nearest handler in a try-body arm always leaves the
region. The witness above is a compile error naming `try`
(`lang/seq_try.rs` `refusals`); its `try` spelling is
`recovers_value`.

## R9 — fixed: a handler's `e?` double-wraps the TYPE of a call-originated error

**Fixed 2026-09-07** with the `try … with` build: `fix_echain_typ`
(`node/error.rs`) now recognizes the chain in its expanded struct form
and as a `Ref` resolved in another scope (the callee's throws carry
both spellings), so a handler-side `e?` forwards the type without
nesting it. The witness below prints `outer 1`; `error_payloads`
destructures the payload at both handlers again. Original write-up
follows.

Pre-existing in plain Graphix, but R2 made it the only spelling of a
seq cleanup that rethrows, and `error_payloads` pinned the symptom.
Location: [error.rs:474](../graphix-compiler/src/node/error.rs#L474) —
`fix_echain_typ` recognizes an `ErrChain` only as the root `Ref`, not in
its expanded struct form.

```graphix
{
  let step = 0;
  step <- select step { n if n < 20 => n + 1, _ => never() };
  let go = select step { 1 => 1, _ => never() };
  let bad = |v| { error(`Oops(v))?; v };
  catch(e) select (e.0).error { `Oops(n) => println("outer [n]") };
  { catch(e) e?; bad(go) };
  select step { 19 => sys::exit(0), _ => never() }
}
```

Expected: compiles and prints `outer 1` — which it does when the inner
error is raised inline (`error(`Oops(go))?` in place of `bad(go)`).
Observed: refused, "missing match cases … `` `Oops('_) `` does not
contain `{cause: [null, ErrChain<`Oops(i64)>], error: `Oops(i64), ori:
Ori, pos: Pos}`" — the outer `e` is typed
`Error<ErrChain<ErrChain<`Oops(i64)>>>`. The VALUE is right
(`wrap_error` chains: `cause` is the previous chain, `error` stays the
original payload); only the type is wrapped twice. With
`println("[(e.0).error]")` in place of the select the typed printer logs
"type … does not match value" and falls back to the naked form,
`["Oops", 1]`.

Why: a call-originated error reaches the inner catch through the call
site's `throws`, which carries the alias in EXPANDED form (CLAUDE.md,
"Type Alias Expansion in Contains"), and `fix_echain_typ` matches only
`Ref(root::ErrChain)`, so it wraps the struct again. The machine's own
`Rethrow` had the expanded case handled (the sentence the R2 commit
removed from `seq_error_guards.md`); a user's `e?` does not.

Consequence: `error_payloads` (`seq_errors.rs:375`) now asserts
`caught ["Oops", 1]` as the expected output — the naked fallback. Its
previous `caught 1` (the outer select destructuring `` `Oops(n) ``) was
the correct behavior and stopped compiling for exactly this reason. Fix
`fix_echain_typ` (recognize the expanded struct, or compare through the
env the way `ERRCHAIN.is_a` does for values), then restore the select
spelling in the test; the JIT shares the typing so both engines move
together.

Aside, R4-adjacent: a step that can only throw, `seq go {
error(`Oops(1))? }`, is refused with the generated-code message
"pattern `'_: []` will never match `'_: []`, unused match cases" — the
continuation select over an empty-typed step. Same family as R4's
silent bottom; the refusal is right, the message is not.

## R10 — P2: a `~` step re-enters on its previous run's resident

Found 2026-09-07 by the first draft of `lang/seq_try.rs`
(`seqq_credit`), not caused by `try … with`. A step spelled as a
sample over an issued call:

```graphix
seqq request {
  let x = try { bad(request)? } with(e) { println("toast"); 0 };
  sys::time::timer(duration:2.ms, false) ~ x
}
```

with requests 1, 2, 3 (1 fails and recovers to 0). Expected `[0, 2, 3]`.
Observed in both engines: `[0, 0, 0]`, `toast` once. Run 1 is right.
Runs 2 and 3 complete IMMEDIATELY at the tail step's entry with run 1's
value: the `Sample` keeps its held resident across the arm's sleep
(sleep is pause; `~`'s arg is one of the three `Held` ride sites), the
wake presents it stale-present, the completion guard passes it, and the
continuation select routes on a present scrutinee with no retained
selection — exactly the path a pure derivation of carried cells
legitimately completes through. The timer is re-issued (the presence
select's `once` cleared on sleep) but its fire is never waited for.

A second instance, found the same day pinning the lambda exemption:
`let f = |v| { let r = never(); catch(e) r <- e ~ -2; r <- bad(v)?; r };
f(request)` as a step gives `[-2, -2, 2]` for requests 1, 2, 3 — each
run completes on the PREVIOUS run's `r`. The lambda instance is
retained across runs, `r` is per-instance state, and at re-issue the
body's value is the standing `r` a cycle before the new write lands.
Same mechanism: a present-but-stale production at re-entry is taken as
the step's completion.

The same tail written as a call, `sys::time::after_idle(duration:2.ms, x)`,
is correct, because the issue atom snapshots `x` at entry and an
async call presents bottom until its new result. So the hazard is
specific to a `~` whose RHS is a level and whose LHS is the step's
wait: the step's value is present before the wait completes. The
guard cannot tell the two stale-present cases apart by tag alone.
The tag cannot decide it: a level read as it stands at entry (R2, to
be accepted) and a previous run's answer (to be rejected) are both
present-stale. Requiring a FIRED production for call-containing steps
would fix both witnesses but stall a step like `f(x)` whose body
returns a level the call does not fire. Candidates that stay inside
the lowering: rewrite a top-level `a ~ b` step to `a ~! b` (covers the
first witness only), or give the presence select's `once` a per-entry
generation so a production older than the entry is not admitted
(covers both, if the runtime can stamp productions). Eric's call; the
pins are the two programs above.


## Suggested order

1. **R1.** An abort is the worst outcome available and the fix is two
   `ensure_sufficient` wrappers.
2. **R8.** Closed by `try … with` (seq_blocks.md §7.9).
3. **R2.** Closed: seq-toplevel `catch` / `{ ... }` refused.
4. **R3**, **R4** and **R10** — small, user-visible as confusing
   failures (R10 is a wrong value, which is worse than a stall).
   R9 is fixed.
5. **R5**, then **R6**/**R7** as cleanup.

Each of R1–R4, R8 and R9 is a short `run!` fixture. R2 is pinned by
`seq_statement_refusals` and by `nested_handler_choices` using `do`.
