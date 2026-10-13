#!/bin/sh
# x-expr-walks-02: fn_names (graphix-types/src/expr/seq.rs) counts only
# `let` patterns as rebinding a name, so a select-arm bind `g` that
# shadows an outer function `g` is taken for the function: issue_call
# leaves it live at the call site instead of snapshotting it at entry,
# and after_idle is re-issued each time `v` moves.
# Command: sh design/review-2026-10-12/repro/x-expr-walks-02.sh [path/to/graphix]
# Expected: both programs print 0 (the arm's value at the step's entry).
# Observed: the `g` program prints 5, the `h` program prints 0
# (`--expand` shows `g` passed bare, `h` read from the seqissued tuple).
B=${1:-graphix}
D=$(mktemp -d)
for n in g h; do
cat > $D/$n.gx <<GX
let g = |x| x;
let v = 0;
v <- sys::time::timer(duration:50.ms, 5)\$ ~ v + 1;
let t = 1;
let r = seq t {
  select v { $n => sys::time::after_idle(duration:200.ms, $n) }
};
sys::exit(sys::time::after_idle(duration:600.ms, 0));
r
GX
printf '%s: ' $n; timeout -s KILL 30 $B --no-cache $D/$n.gx
done
rm -rf $D
