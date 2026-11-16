#!/bin/bash
# Title: #[native] refusals list fusion-internal reasons ("emit_clif: ...", "not discovered", "doesn't freeze concrete")
# Command: bash design/review-2026-11-16/repro/x-diagnostics-native-internal-reasons.sh
# Expected: each bullet says in the program's terms what keeps the expression in the node-walk,
#   once: "`g` writes a binding (`y <- x`), which keeps its value at a wake"; "a select over Any".
# Observed:
#   - lambda `g` has no kernel: its body: a binding something writes keeps its value at a wake
#   - emit_clif: lambda call site `g(x)` not discovered — subtree node-walks; lambda `g` has no
#     kernel: its body: a binding something writes keeps its value at a wake      (restates the first)
#   - emit_clif: select scrutinee type Any doesn't freeze concrete
G=${GRAPHIX:-/home/eric/tmp/review-2026-11-16/bin/graphix}
d=$(mktemp -d)
printf 'let g = |x: i64| { let y = 0; y <- x; y };\nlet f = |x: i64| #[native] (g(x) + 1);\nf(1)\n' > $d/a.gx
printf 'let f = |x: Any| #[native] select x { i64 as n => n, _ => 0 };\nf(1)\n' > $d/b.gx
for f in a b; do timeout -s KILL 60 $G --no-cache --expand $d/$f.gx 2>&1 | grep '^ *- '; done
rm -rf $d
