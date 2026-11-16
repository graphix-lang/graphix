#!/bin/sh
# t-parser-a: whether a list's separator is named as expected depends on the
# whitespace before the failure, and on whether the list has read a second
# item. `[1` says "Expected `,` or `]`", `[1 ` says "Expected `]`", and so
# does `[1, 2`; `let x = 1)` says "Expected `;` or end of input", `let x =
# 1 )` says "Expected end of input". Input::drop_since keeps a failed
# alternative's labels only when it STARTED at the reported offset, but the
# separator's `sptoken` (and a binop's `attempt(spaces; ..)`) skips the
# whitespace inside its attempt, so its failure stands at the reported
# offset while its start does not; Input::many also drops the failing
# iteration once the run has read anything.
#
# Run: sh design/review-2026-11-16/repro/t-parser-a-expected-depends-on-space.sh
# Expected: the two lines of each pair say the same thing.
# Observed (HEAD e1be2fbf2): the second of each pair drops the separator.
G=${G:-/home/eric/tmp/review-2026-11-16/bin/graphix}
d=$(mktemp -d)
probe() {
  printf '%s' "$1" > "$d/p.gx"
  printf '== [%s]: ' "$1"
  timeout -s KILL 10 "$G" --check --no-cache "$d/p.gx" 2>&1 | grep -m1 Expected
}
probe 'let x = [1'
probe 'let x = [1 '
probe 'let x = [1'
probe 'let x = [1, 2'
probe 'let x = 1)'
probe 'let x = 1 )'
rm -rf "$d"
