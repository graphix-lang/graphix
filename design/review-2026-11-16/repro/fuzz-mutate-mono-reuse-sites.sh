#!/bin/bash
# Title: mono-reuse reports MISPLACED for refusals its own mutation causes away from a use of `f`
# Command: bash design/review-2026-11-16/repro/fuzz-mutate-mono-reuse-sites.sh
# Expected: "typemorph: no flips" for each base (the refusal is the family's own, at a
#   statement its argument reaches), or the site skipped.
# Observed (graphix-fuzz at e1be2fbf2):
#   1) TYPEFLIP mono-reuse#1: MISPLACED: N: cannot compare iN with string ...
#      `n` (a let over never()) takes i64 from the one instance at `f(n)`; the refusal is at `b == "x"`.
#   2) TYPEFLIP mono-reuse#1: MISPLACED: N: xs is Array<iN> and cannot hold Array<string> ...
#      the open `xs` takes i64 at `array::map(xs, f)`; the refusal is at the writer `xs <- ["s"]`.
#   3) TYPEFLIP mono-reuse#1: MISPLACED: N: type mismatch iN does not contain string
#      `s = {g: f}` holds the one instance; `s.g("str")` refuses before the second use of `f`.
#   4) the same through `let s = [f]` and `s[0]$("str")`.
F=/home/eric/tmp/review-2026-11-16/bin/graphix-fuzz
d=$(mktemp -d)
i=0
for p in \
  '{ let f = |x| x; let n = never(); let a = f(1); let b = f(n); (a, b == "x") }' \
  '{ let f = |x| x; let xs = []; let a = array::map([1], f); let b = array::map(xs, f); xs <- ["s"]; (a, b) }' \
  '{ let f = |x| x; let s = {g: f}; let a = f(1); let c = s.g("str"); let b = f(2.5); (a, b, c) }' \
  '{ let f = |x| x; let s = [f]; let a = f(1); let c = s[0]$("str"); let b = f(2.5); (a, b, c) }'
do
  i=$((i+1)); printf '%s\n' "$p" > "$d/b$i.gx"
  echo "== $p"
  timeout -s KILL 170 "$F" typemorph "$d/b$i.gx" 2>&1 | grep -E "TYPEFLIP|mutant|no flips"
done
rm -rf "$d"
