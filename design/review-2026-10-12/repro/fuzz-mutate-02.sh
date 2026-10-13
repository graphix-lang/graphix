#!/bin/sh
# fuzz-mutate-02: same-form mutates selects over Any, where no arm is
# checked for one runtime form (mustreject::same_form never reads the
# type map).
# Run: sh fuzz-mutate-02.sh [path/to/graphix-fuzz]
# Expected: no same-form probe (the scrutinee's type holds Any; the design
# skips such a site).
# Observed:
#   FLIP same-form#3 LEAK: accepted at select
#   MUTANT { let s: Any = "a"; select select 1 == 1 { true => s, false => `TmSame } { string as x => 1, _ => 2 } }
# The mutant is rightly accepted: rep_collision finds no head for Any,
# while the same mutant over `let s = "a"` is refused ("can't tell string
# from `TmSame").
FUZZ=${1:-graphix-fuzz}
OUT=$(mktemp)
printf '%s\n' '{ let s: Any = "a"; select s { string as x => 1, _ => 2 } }' \
  | timeout -s KILL 180 "$FUZZ" typemorph-one "$OUT" 5000
grep -A1 same-form "$OUT"
rm -f "$OUT"
