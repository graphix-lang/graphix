#!/bin/sh
# A guard's tentative bind fires a reader in an EARLIER REPL root, at the
# retracted (old) value, though the arm is not taken.
#
# Each REPL input is a root. `r = f()` (root 2) reads, through the closure
# `|| n`, the bind of the guarded arm of the select in a later root. At
# v = 5 and v = 6 the guard `n > 10` is false and `m => m` takes the value.
#
# Command: sh design/review-2026-11-16/repro/c-prune-wake-tentative-bind-late-roots.sh
# Expected: r and c do not move for v = 5 and 6:
#   r=20 c=1 / s=20 / s=5 / s=6 / s=30 / r=30 c=2 ...
# Observed: `r` fires again at the old value each time the guard is consulted:
#   r=20 c=1 / s=20 / s=5 / r=20 c=2 / s=6 / r=20 c=3 / s=30 / r=30 c=4
BIN=${GRAPHIX:-/home/eric/tmp/review-2026-11-16/bin/graphix}
(printf '%s\n' \
  'let f: fn() -> i64 = never()' \
  'let r = f()' \
  'let c = count(r)' \
  'let p1 = println("r=[r] c=[c]")' \
  'let vs = [20, 5, 6, 30]' \
  'let x = 0' \
  'let v = vs[x]$' \
  'let s = select v { n if n > 10 => { f <- n ~ (|| n); n }, m => m }' \
  'let p2 = println("s=[s]")' \
  'let t = sys::time::timer(duration:300.ms, true)' \
  'x <- t ~ x + 1'; sleep 3) \
  | timeout -s KILL 20 "$BIN" --no-cache 2>&1 | grep -vE '^-:|Welcome|Press'
