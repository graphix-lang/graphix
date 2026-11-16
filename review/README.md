# The review kit

A full-repository review: one reviewer agent per file unit and per
cross-cutting dimension, each returning findings with the place its CR goes.
There is no verify phase; reviewers confirm by running. The findings become
CRs in the code and a summary in `design/REVIEW-<date>.md`.

State lives outside the tree: `~/ptmp/reviews/review-<date>` (prompts,
`args.json`, results; small, persistent) and `~/tmp/review-<date>` (probe
binaries and the reviewers' scratch; bulk, tmpfs).

## A round

```sh
export REVIEW_DATE=2026-11-16
review/setup.sh        # debug binaries at HEAD behind memory-capped wrappers
review/units.py        # file units -> units.json, prompts; prints files no unit covers
$EDITOR ~/ptmp/reviews/review-$REVIEW_DATE/context.md   # what changed since the last review
review/dims.py         # dimensions, re-run groups, open_crs.txt, args.json
```

Then run `review/workflow.js` with the Workflow tool, `args` = the contents
of `args.json`. The tool takes a script only from the session's own script
directory, so copy it there or pass it inline. A run that dies part way is
resumed with the same script and args: only the missing reviewers run again.

Before a round, edit:

- `units.py`: every tracked source file belongs to one unit. Split a unit
  over about 6,000 lines (`ranged` for one large file, `parts` for a list).
  The run prints files no unit covers and globs that match nothing.
- `dims.py`: the missions that name what is new, `PREV` (the last review:
  its repros are re-run in `GROUPS` groups) and `FABLE` (the reviewers that
  run on Fable; the rest run on Opus).

When the run ends:

```sh
review/tools/collect.py <transcript dir>/journal.jsonl    # -> results.json
review/tools/build_crs.py                                 # -> crs.json, crs_excluded.json
review/tools/insert_crs.py $S/crs.json --root . --dry-run # then without --dry-run
git diff --name-only | grep -E '\.gxi?$' | xargs review/tools/fix_gx_crs.py ~/tmp/review-$REVIEW_DATE/bin/real/graphix
cargo xtest                                               # the CRs must not break the gate
review/tools/gen_tables.py                                # -> tables.md
```

`design/REVIEW-<date>.md` is a narrative written by hand followed by
`tables.md`. Commit the CRs, the document and `design/review-<date>/repro`
together.

## What bites

- `probe-run` starts the real binary under `setpriv --pdeathsig KILL`:
  without it a timeout kills only the wrapper and the orphans fill the
  slice's task limit.
- A program given to `graphix-fuzz check` must not call `sys::exit`: the
  oracle excludes it and compares nothing.
- A comment in `ide/tree-sitter-graphix/src/` fails the grammar revision
  test; `build_crs.py` keeps CRs out of it.
- A CR in a `.gx`/`.gxi` file is legal only above an expression, an arm or an
  item: `fix_gx_crs.py` moves a block up until the file parses.
- A literal `{{#include` in a markdown CR breaks mdbook; `insert_crs.py`
  escapes it.
- Messaging a finished reviewer resumes it, and its new report lands outside
  the journal.
- A reviewer's temporary test left under a crate's `tests/` fails the gate:
  look for `review_*.rs` before committing.
