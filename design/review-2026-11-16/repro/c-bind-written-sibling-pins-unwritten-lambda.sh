#!/bin/sh
# Title: a write to one name of a destructuring `let` over a literal makes the
# lambda its unwritten sibling names one instance
#
# Command: sh design/review-2026-11-16/repro/c-bind-written-sibling-pins-unwritten-lambda.sh
#
# `n` is written, `f` is not. generics.md: a function stays generic where a
# binding names it, "a `let` of a lambda or of a data literal holding lambdas";
# one instance is "a binding something writes". Nothing writes `f`.
#
# Expected: all three check.
# Observed: 1 and 2 check; 3 is refused at `f("s")`, "type mismatch i64 does
# not contain string", which names neither the write nor a fix.
BIN=${BIN:-/home/eric/tmp/review-2026-11-16/bin/graphix}
D=$(mktemp -d)
n=0
while IFS= read -r line; do
  n=$((n+1))
  printf '%s\n' "$line" > "$D/p.gx"
  if out=$(timeout -s KILL 30 "$BIN" --check "$D/p.gx" 2>&1); then
    echo "$n ok: $line"
  else
    echo "$n refused: $line"
    echo "$out" | tail -1
  fi
done <<'GX'
let (f, n) = (|x| x, 1); (f(1), f("s"))
let s = (|x| x, 1); let (f, n) = s; n <- 2; (f(1), f("s"))
let (f, n) = (|x| x, 1); n <- 2; (f(1), f("s"))
GX
rm -rf "$D"
