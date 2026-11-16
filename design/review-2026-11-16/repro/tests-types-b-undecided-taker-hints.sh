#!/bin/sh
# Title: the three lang::types fixtures that pin only TWO_DECIDED_TAKERS
#        ("and nothing has decided which") leave their hints unpinned.
# Command: sh design/review-2026-11-16/repro/tests-types-b-undecided-taker-hints.sh
# Prints the last line of each refusal. Observed hints:
#   typedef union after its deciders: "`v` is undecided at this call; annotate `v` (..)"
#   write `x <- [v]`:                 "annotate the value (bind it to a `let` with its type written)"
#                                     (names no binding, where the call site names `v`)
#   untyped lambda after deciders:    "this argument is undecided at this call; annotate it (..)"
G=/home/eric/tmp/review-2026-11-16/bin/graphix
D=$(mktemp -d)
cat > "$D/u1.gx" <<'GX'
let x: [Array<i64>, Array<string>] = [1];
let v = never();
x <- [v];
v <- "s";
x
GX
cat > "$D/u2.gx" <<'GX'
type P<'p, 'q> = [Array<'p>, Array<'q>];
let f = |y: 'a, z: 'b, x: P<'a, 'b>| x;
let v = never();
let r = f(1, "s", [v]);
v <- "t";
r
GX
cat > "$D/u3.gx" <<'GX'
type T<'c, 'd> = [fn(x: 'c) -> i64, fn(x: 'd) -> i64];
let l = |y: 'a, z: 'b, k: T<'a, 'b>| -> i64 1;
l(1, "s", |x| 0)
GX
for f in u1 u2 u3; do
  echo "== $f"
  timeout -s KILL 60 "$G" --no-cache --check "$D/$f.gx" 2>&1 | tail -1
done
rm -rf "$D"
