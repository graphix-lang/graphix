#!/bin/sh
# A holder with a call part is generic outside a seq and one instance in a
# seq step (pinned as intended by
# lang::quantified::a_holder_with_a_call_part_in_a_seq_step_is_one_instance).
#
# Command: sh design/review-2026-11-16/repro/tests-lang-rest-c-seq-holder-call-part.sh
# Expected: all five programs check (the let names a data literal holding a
#   lambda, which is generic wherever a binding names it).
# Observed: 1, 2 and 5 check; 3 and 4 (the same lets inside a seq step,
#   their call part not hoisted) are refused "type mismatch i64 does not
#   contain string".
G=${G:-/home/eric/tmp/review-2026-11-16/bin/graphix}
d=$(mktemp -d)
n=0
for p in \
  '{ let s = {g: |x| x, n: str::len("ab")}; (s.g(1), s.g("s"), s.n) }' \
  'seq { let s = {g: |x| x, n: 2}; (s.g(1), s.g("s"), s.n) }' \
  'seq { let s = {g: |x| x, n: str::len("ab")}; (s.g(1), s.g("s"), s.n) }' \
  '{ let f = |q| q; seq { let s = {g: |x| x, n: f(2)}; (s.g(1), s.g("s"), s.n) } }' \
  'seq { let n = str::len("ab"); let s = {g: |x| x, n}; (s.g(1), s.g("s"), s.n) }'
do
  n=$((n + 1))
  printf 'let result = %s\n' "$p" > "$d/p$n.gx"
  if timeout -s KILL 60 "$G" --no-cache --check "$d/p$n.gx" > "$d/out" 2>&1; then
    echo "$n: checks   $p"
  else
    echo "$n: REFUSED  $p"; tail -1 "$d/out"
  fi
done
rm -rf "$d"
