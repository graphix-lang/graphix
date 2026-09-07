# Seq review — open items (2026-09-06, pruned 2026-09-07)

What remains of the `seq` reviews of 2026-09-05/06. Closed items are
gone; their record is `git log` and `seq_blocks.md` §7.9. Run a witness
with:

```sh
timeout -k 2 40 ~/tmp/target/debug/graphix repro.gx
timeout -k 2 40 ~/tmp/target/debug/graphix --no-fusion repro.gx
```

| item | status |
|---|---|
| the machine calls bare `filter` | open (R3) |
| `until` as the last step is a silent bottom | open (R4) |
| dead `SeqDo` flatten arm in `lower_do_stmts` | open (R5) |
| `rewrite_with` re-spells the enum | open (R6) |
| docs and coverage drift | open (R7) |
| a step re-enters on its previous run's value | open (R10) |

## R3 — P2: the machine calls bare `filter`

`apply_filter` (`seq.rs`) emits `r#ref(pos, "filter")`, resolved in
the user's scope. A program with `let filter = 42` in scope fails with
`expected fn not i64` inside generated code. Use `core::filter` (as
`apply_core` already does for `once`/`hold`/`queue`) and audit every
other bare name the desugar emits.

```graphix
{ let filter = 42; let out = 0; seq { out <- 1 + filter }; out }
```

## R4 — P2: `until` as the last step is a silent bottom

`step_arm`'s `Until` arm never performs the sink's writes, so
`seq go { let x = 1; until (x > 0) }` compiles and its value is
permanently bottom. Refuse it: the last step must be an expression,
as `until` inside `do` is already refused.

Related: a step that can only throw (`seq go { error(`Oops)? }`) is
refused with the generated-code message "pattern `'_: []` will never
match". Right refusal, wrong message.

## R5 — P3: dead `SeqDo` flatten arm

`lower_do_stmts_inner` flattens a `do` nested in a `do`, but the
parser cannot produce one (`seq_do`'s items are `until` or `expr()`,
and `expr()` has no `do`). Delete the arm or make nested `do` legal.

## R6 — P3: `rewrite_with` re-spells the enum

`rewrite_with_inner` is ~300 lines rebuilding `ExprKind` unchanged
except for the `Ref`/`Connect`/binder arms. A scope-aware
`Expr::map_children` would shrink it to those arms;
`graphix-fuzz/src/mutate.rs` rebuilds the same way and is the second
consumer that justifies the helper.

## R7 — P3: docs and coverage drift

- `design/README.md` lists `seq_blocks.md` twice (lines 39 and 46);
  `async_sleep_outputs.md`, `place_references.md`, `seq_error_guards.md`
  and `seqq.md` are not indexed.
- `range` still raises `` `SeqError `` (core `mod.gxi`, `mod.gx`,
  `lib.rs`), named for a builtin that no longer exists.
- `--expand` (the machine printer §7 calls the debugging story) is not
  built.
- CLAUDE.md:352 says dev builds are `opt-level=0`; `Cargo.toml` says
  `"s"`.
- The proptest generator produces `Seq` but never `SeqDo`, `Until` or
  `TryWith`; printer/parser drift for those is uncovered.

## R10 — P2: a step re-enters on its previous run's value

Two witnesses, both engines, both pre-existing:

```graphix
seqq request {
  let x = try { bad(request)? } with(e) { 0 };
  sys::time::timer(duration:2.ms, false) ~ x     // [0, 0, 0]; expected [0, 2, 3]
}
```

```graphix
seq request {
  let f = |v| { let r = never(); catch(e) r <- e ~ -2; r <- bad(v)?; r };
  f(request)                                     // [-2, -2, 2]; expected [-2, 2, 3]
}
```

Run 1 is right. Later runs complete at the step's entry with the
previous run's value: the `~`'s held resident (sleep is pause) and the
lambda instance's `r` are present-but-stale at wake, the completion
guard passes them, and the continuation select routes on a present
scrutinee. The same tail as a call (`after_idle(duration:2.ms, x)`) is
right, because the issue atom snapshots `x` and an async call is
bottom until its new result.

The tag cannot decide it: a level read as it stands at entry (R2,
accept) and a previous run's answer (reject) are both present-stale.
Requiring a FIRED production for call-containing steps fixes both
witnesses but stalls `f(x)` when the body returns a level the call
does not fire. Candidates: rewrite a top-level `a ~ b` step to
`a ~! b` (first witness only), or give the presence select's `once` a
per-entry generation so a production older than the entry is not
admitted (both, if productions can be stamped). Eric's call.

## Suggested order

R10 (a wrong value), then R3 and R4 (confusing failures in generated
code), then R5, R6, R7.
