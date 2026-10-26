#!/bin/sh
# Title: sig_matches refuses a concrete signature type against an
#        implementation union of open cells (`|b, x, y| select b { true => x,
#        false => y }` returns ['x, 'y]).
# Command: sh design/review-2026-10-26/repro/t-contains-sig-concrete-vs-open-union.sh
# Expected: exit 0 (the signature is the instance 'x := i64, 'y := i64).
# Observed: "val f is declared fn(b: bool, x: i64, y: i64) -> i64 but implemented
#   fn(b: bool, x: '_N: unbound, y: '_M: unbound) -> ['_N: unbound, '_M: unbound]:
#   in return type: type mismatch: signature has i64, implementation has ['_N, '_M]"
#   The signatures fn(.., x: 'a, y: 'a) -> 'a and fn(.., x: 'a, y: 'b) -> ['a, 'b] pass.
G=${G:-/home/eric/tmp/review-2026-10-26/bin/graphix}
d=$(mktemp -d)
printf 'mod inner;\ninner::f(true, 1, 2)\n' > "$d/main.gx"
printf "val f: fn(b: bool, x: i64, y: i64) -> i64\n" > "$d/inner.gxi"
printf "let f = |b: bool, x, y| select b { true => x, false => y }\n" > "$d/inner.gx"
timeout -s KILL 30 "$G" --check "$d/main.gx"; echo "exit $?"
rm -r "$d"
