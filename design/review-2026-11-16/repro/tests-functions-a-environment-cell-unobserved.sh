#!/bin/sh
# Title: environment_cell_is_not_generalized cannot fail for the bug it names.
# Command: sh design/review-2026-11-16/repro/tests-functions-a-environment-cell-unobserved.sh
# Expected: the fixture observes the type the call gives `y`.
# Observed (HEAD e1be2fbf2): the fixture's value is the constant 42 and an
#   undecided `y` is accepted (first two programs), so a definition that
#   generalized `y`'s cell (leaving `y` undecided) would still yield 42.
#   A read of `y` at another type is what tells the two apart (third program:
#   refused only because the call made `y` an i64).
G=${G:-/home/eric/tmp/review-2026-11-16/bin/graphix}
d=$(mktemp -d)
run() {
  printf '%s\n' "$1" > "$d/p.gx"
  echo "== $1"
  out=$(timeout -s KILL 60 "$G" --check --no-cache "$d/p.gx" 2>&1 | grep "type mismatch" | sed 's/ (line.*//')
  echo "${out:-   accepted}"
}
run '{ let y = array::iter(never()); let t = |x| x + y; let m = array::map([1], t); 42 }'
run '{ let y = array::iter(never()); let t = |x| x + y; 42 }'
run '{ let y = array::iter(never()); let t = |x| x + y; let m = array::map([1], t); let z: string = y; 42 }'
rm -rf "$d"
