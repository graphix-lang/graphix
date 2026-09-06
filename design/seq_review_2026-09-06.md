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
| …a user handler that rethrows delivers twice | **open (R2)** |
| the machine calls bare `filter` | **open (R3)** |
| `until` as the last step is a silent bottom | **open (R4)** |
| `until` inside `do` / nested `do` dead code | **partly open (R5)** |
| `rewrite` re-spells the whole `ExprKind` enum | **open (R6)** |
| `seq.rs`'s dead `Until(_) \| Bind(_) \| _` patterns | **open (R5)** |
| docs and coverage drift | **open (R7)** |
| the proptest round-trip doesn't generate `seq` | **partly fixed (R7)** |

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

## R2 — P1: a cleanup handler that rethrows delivers twice

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

## Suggested order

1. **R1.** An abort is the worst outcome available and the fix is two
   `ensure_sufficient` wrappers.
2. **R2.** A wrong error count is a correctness bug in the construct's
   own error discipline, and it is invisible to the suite.
3. **R3** and **R4** — both small, both user-visible as confusing
   failures in generated code.
4. **R5**, then **R6**/**R7** as cleanup.

Each of R1–R4 is a short `run!` fixture; R2 in particular should be
pinned in `lang/seq_errors.rs` next to `nested_handler_choices`, since
the two differ only by a pair of braces.
