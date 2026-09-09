# Seq review — open items (2026-09-06, pruned 2026-09-07)

What remains of the `seq` reviews of 2026-09-05/06. Closed items are
gone; their record is `git log`, `seq_blocks.md` §7.9 and R3's re-entry
rule (R10). Run a witness
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

## Suggested order

R3 and R4 (confusing failures in generated code), then R5, R6, R7.
