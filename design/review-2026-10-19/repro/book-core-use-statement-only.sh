#!/bin/bash
# Title: use.md says `use` is valid anywhere an expression is; the compiler takes it as a statement only
# Command: bash design/review-2026-10-19/repro/book-core-use-statement-only.sh
# Expected (book/src/core/use.md, Scoping): both programs compile.
# Observed: "a use declaration is not an expression — it may only appear as a statement in a
#   block or module body, not where a value is expected" for each.
D=$(mktemp -d); G=/home/eric/tmp/review-2026-10-19/bin/graphix
echo 'let f = |x| use array::len; 1' > $D/a.gx
echo 'array::len(use array::len)' > $D/b.gx
for f in a b; do timeout -s KILL 10 $G --check $D/$f.gx 2>&1 | tail -1; done
rm -rf $D
