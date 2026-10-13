#!/bin/sh
# fuzz-lib-a-07: the SLOW rule drops a one-sided node-walk timeout beside a
# JIT trace on the reference's CPU burn alone, so it cannot tell a node-walk
# spinning forever inside a cycle from an honestly slow one.
#
# Command: sh fuzz-lib-a-07.sh <path to graphix-fuzz>   (about 90 s)
# The program terminates in both engines (4000 cycles, a 20000-deep tail
# recursion each); the JIT runs it as a native loop in well under the 10 s
# budget, the node-walk needs ~20 ms a cycle. The check reruns the node-walk at
# the 80 s slow budget, sees it burn >= 5 s CPU, and drops the comparison.
# Observed:
#   SLOW — the reference burned 71.6s CPU over a 80s budget without finishing; not recorded
#   UNSURE — no divergence, ...        (exit 0)
# The decision reads only the reference's burn: a node-walk that loops forever
# inside one cycle beside the same JIT trace takes the same branch and is never
# recorded, though design/graphix_fuzz.md calls fusion removing
# nontermination a top-tier finding.
FUZZ=${1:-graphix-fuzz}
D=$(mktemp -d)
cat > "$D/p.gx" <<'GX'
// schedule-v1: cap=4000 events=8000
let rec go = |i: i64, acc: i64| -> i64 select i { 0 => acc, i => go(i - 1, acc + i) };
let x = 0;
x <- x + 1;
go(20000, x)
GX
timeout -s KILL 180 "$FUZZ" check "$D/p.gx"
echo "exit $?"
rm -rf "$D"
