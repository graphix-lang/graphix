#!/bin/sh
# Title: Type::implies probes `c ⊇ self` with the signature's own variables
#        open, so a declared bound `'a: Array<'b>` "implies" the
#        implementation's `'a: Array<i64>`; the check passes and the build
#        refuses the instance.
# Command: sh design/review-2026-10-26/repro/t-contains-implies-open-cell.sh
# Expected: the check refuses the interface (the implementation requires
#   'a: Array<i64>, which the signature does not declare).
# Observed: `--check` exits 0; the run fails with "an instance at fn(x:
#   Array<string>) -> i64 of a definition typed fn<'a: Array<i64>>(x: 'a:
#   unbound within Array<i64>) -> i64" (an elaboration refusal of what the
#   check accepted).
G=${G:-/home/eric/tmp/review-2026-10-26/bin/graphix}
d=$(mktemp -d)
printf 'mod inner;\ninner::f(["s"])\n' > "$d/main.gx"
printf "val f: fn<'a: Array<'b>>(x: 'a) -> i64\n" > "$d/inner.gxi"
printf "let f = 'a: Array<i64> |x: 'a| -> i64 x[0]\$ + 1\n" > "$d/inner.gx"
timeout -s KILL 30 "$G" --check "$d/main.gx"; echo "check exit $?"
timeout -s KILL 30 "$G" --no-cache "$d/main.gx"; echo "run exit $?"
rm -r "$d"
