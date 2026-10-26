#!/bin/sh
# Title: sig_matches pairs a union's members by position when only a nested
#        member holds an open cell, so an interface that is exactly the
#        implementation's type is refused.
# Command: sh design/review-2026-10-26/repro/t-contains-sig-union-order.sh
# Expected: exit 0 (the inferred type is fn(b: bool, x: 'a) -> [Array<i64>, Array<'a>]).
# Observed: "val f is declared fn(b: bool, x: 'a) -> [Array<i64>, Array<'a>] but
#   implemented fn(b: bool, x: '_N: unbound) -> [Array<'_M: unbound>, Array<i64>]:
#   in return type: type variable usage mismatch: expected 'a: unbound, got i64"
# Annotating the implementation's return with the same union passes.
G=${G:-/home/eric/tmp/review-2026-10-26/bin/graphix}
d=$(mktemp -d)
printf 'mod inner;\ninner::f(true, 1)\n' > "$d/main.gx"
printf "val f: fn(b: bool, x: 'a) -> [Array<i64>, Array<'a>]\n" > "$d/inner.gxi"
printf "let f = |b: bool, x| select b { true => [x], false => [3] }\n" > "$d/inner.gx"
timeout -s KILL 30 "$G" --check "$d/main.gx"; echo "exit $?"
rm -r "$d"
