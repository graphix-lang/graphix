#!/bin/sh
# core-aux-05: opt::contains lacks 'a: Discernible and compares references
# by cell, disagreeing with `==`.
# (core-aux-05.gx in this directory is an unrelated queuefn probe.)
# command: sh core-aux-05.sh /path/to/graphix
# Expected: line 1 "true false true false" (contains agrees with r1$ == r);
#   the program's mixed-type contains refused by --check, as `x == y` is
#   ("Array<i64> and (i64, i64) have the same runtime form").
# Observed: "true false false false" (contains(r1, &a) is false although
#   r1 is &a), then "mixed=true" (a tuple equals an array), check passes.
G=${1:-graphix}
D=$(mktemp -d)
cat > $D/p.gx <<'GX'
let a = 1;
let b = 1;
let r1: [&i64, null] = &a;
let r2 = &a;
let r3 = &b;
println("[r1$ == r2] [r1$ == r3] [opt::contains(r1, r2)] [opt::contains(r1, r3)]");
let x: [(i64, i64), Array<i64>, null] = (1, 2);
let y: [(i64, i64), Array<i64>] = [1, 2];
println("mixed=[opt::contains(x, y)]");
sys::exit(sys::time::after_idle(duration:100.ms, 0));
null
GX
cat > $D/eq.gx <<'GX'
let x: [(i64, i64), Array<i64>] = (1, 2);
let y: [(i64, i64), Array<i64>] = [1, 2];
x == y
GX
timeout -s KILL 30 $G --no-cache $D/p.gx
echo "--- x == y over the same types:"
timeout -s KILL 30 $G --check $D/eq.gx
rm -rf $D
