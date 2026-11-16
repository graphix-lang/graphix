#!/bin/sh
# Title: no_widest_arg_refused pins "refused, in either order" with one
#        program, and the check reports only the first call's refusal.
# Command: sh design/review-2026-11-16/repro/tests-functions-a-either-order-half-pinned.sh
# Expected: the fixture's message depends on both calls.
# Observed (HEAD e1be2fbf2): the fixture program and the same program with
#   its second call replaced by an accepted one give the same refusal,
#   "'a: `A does not contain `B"; the second order alone gives
#   "'a: `B does not contain `A", which the fixture never sees.
G=${G:-/home/eric/tmp/review-2026-11-16/bin/graphix}
d=$(mktemp -d)
run() {
  printf '%s\n' "$1" > "$d/p.gx"
  echo "== $1"
  timeout -s KILL 60 "$G" --check --no-cache "$d/p.gx" 2>&1 | grep "type mismatch\|^[^ ]" | grep -v "^Error\|^Caused" || echo "accepted"
}
run '{ let f = |x: '"'"'a, y: '"'"'a| -> '"'"'a y; (f(`A, `B), f(`B, `A)) }'
run '{ let f = |x: '"'"'a, y: '"'"'a| -> '"'"'a y; (f(`A, `B), f(`B, `B)) }'
run '{ let f = |x: '"'"'a, y: '"'"'a| -> '"'"'a y; f(`B, `A) }'
rm -rf "$d"
