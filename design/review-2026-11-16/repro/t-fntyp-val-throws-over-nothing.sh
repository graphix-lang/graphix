#!/bin/bash
# Title: a val that declares `throws` over an implementation that throws
#        nothing is accepted when the implementation is a bare lambda and
#        refused when its let is annotated
# Command: bash design/review-2026-11-16/repro/t-fntyp-val-throws-over-nothing.sh
# Expected: one verdict for both (a val declares exactly its
#           implementation's type: refused, "the signature throws `E, the
#           implementation throws nothing").
# Observed: `let f = |x: i64| x` passes, and the consumer is warned of an
#           error nothing raises ("error `E raised from function call foo::f
#           will not be caught"); `let f: fn(x: i64) -> i64 = |x| x` is
#           refused with the message above.
G=${GRAPHIX:-/home/eric/tmp/review-2026-11-16/bin/graphix}
D=$(mktemp -d)
trap 'rm -rf "$D"' EXIT
run() {
  mkdir -p "$D/$1"
  printf 'val f: fn(x: i64) -> i64 throws `E;\n' > "$D/$1/foo.gxi"
  printf '%s\n' "$2" > "$D/$1/foo.gx"
  printf 'mod foo;\nfoo::f(1)\n' > "$D/$1/main.gx"
  echo "--- $2"
  ( cd "$D/$1" && timeout -s KILL 60 "$G" --no-cache --check main.gx 2>&1 | tail -1 | cut -c1-300; echo "exit ${PIPESTATUS[0]}" )
}
run lambda 'let f = |x: i64| x'
run annotated 'let f: fn(x: i64) -> i64 = |x| x'
