#!/bin/bash
# Title: lang::types::union_keeps_distinct_cells passes whether the select keeps
# [a] and [b] apart or makes them one array: its predicate is refused("does not contain").
# Command: bash design/review-2026-11-16/repro/tests-types-c-union-keeps-distinct-cells-vacuous.sh
# Expected: a pin that fails if a and b become one cell.
# Observed (HEAD e1be2fbf2):
#   fixture (cells apart)      -> "type mismatch Array<string> does not contain [Array<string>, Array<i64>]"
#                                 (the refusal is of indexing a union of arrays, not of the claim)
#   same call, a and b one cell -> "type mismatch string does not contain i64"  (also matches)
#   the claim pinned directly  -> (pick(true, "x", 41), pick(false, "x", 41)) runs: (["x"], [41])
G=${GRAPHIX:-/home/eric/tmp/review-2026-11-16/bin/graphix}
d=$(mktemp -d)
run() { printf '%s\n' "$2" > $d/p.gx; printf '%-28s -> ' "$1";
  if timeout -s KILL 60 $G --check --no-cache $d/p.gx > $d/out 2>&1; then echo accepted; else tail -1 $d/out | sed 's/^ *[0-9]*: //'; fi; }
run "fixture" 'let pick = |c: bool, a, b| select c { true => [a], false => [b] }; pick(true, "x", 41)[0]$ + 1'
run "a and b one cell" 'let pick = |c: bool, a, b| { let u = [a]; u <- [b]; u }; pick(true, "x", 41)[0]$ + 1'
run "claim pinned directly" 'let pick = |c: bool, a, b| select c { true => [a], false => [b] }; (pick(true, "x", 41), pick(false, "x", 41))'
rm -rf $d
