# Seq review — open items (2026-09-06, pruned 2026-09-07)

What remains of the `seq` reviews of 2026-09-05/06. Closed items are
gone; their record is `git log` and `seq_blocks.md` (§7.9; the
completion rule under R3, review R10). Run a witness
with:

```sh
timeout -k 2 40 ~/tmp/target/debug/graphix repro.gx
timeout -k 2 40 ~/tmp/target/debug/graphix --no-fusion repro.gx
```

| item | status |
|---|---|
| `rewrite_with` re-spells the enum | open (R6) |
| docs and coverage drift | open (R7) |

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

R6, then R7.
