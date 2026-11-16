#!/bin/sh
# Title: a generic formal of one constructor (Array<'a>, ('a, 'b), List<'a>,
#   Map<'k, 'v>, a two-field struct) refuses a decided argument that is a
#   union of two members of that constructor: the open variable is bound by
#   the first member and the second no longer fits. A one-field struct and a
#   variant pass (their unions normalize to one member).
# Command: sh design/review-2026-11-16/repro/tests-types-a-union-argument-binds-first-member.sh
# Expected: every line is accepted ('a := [i64, string] holds both members,
#   as it does for a one-field struct), or refused naming the annotation.
# Observed (HEAD e1be2fbf2): lines 1-5 refused, e.g.
#   type mismatch Array<'a: i64> does not contain [Array<i64>, Array<string>]
#   (no annotation named); lines 6-7 accepted; line 8 (the annotation) accepted.
G=${G:-/home/eric/tmp/review-2026-11-16/bin/graphix}
d=$(mktemp -d)
i=0
while IFS= read -r p; do
  i=$((i+1)); printf '%s\n' "$p" > $d/p.gx
  echo "== $i: $p"
  timeout -s KILL 30 $G --no-cache --check $d/p.gx 2>&1 | grep -v ' at: ' | tail -1
done <<'PROGS'
{ let b = true; let w = select b { true => [1], false => ["a"] }; array::len(w) }
{ let w: [Array<i64>, Array<string>] = [1]; let w2: Array<'q> = w; w2 }
{ let w: [(i64, string), (f64, i64)] = (1, "z"); let k = |u: ('a, 'b)| 1; k(w) }
{ let w: [List<i64>, List<string>] = [<1>]; list::len(w) }
{ let w: [{a: i64, b: f64}, {a: string, b: i64}] = {a: 1, b: 1.5}; let k = |u: {a: 'a, b: 'b}| 1; k(w) }
{ let w: [{a: i64}, {a: string}] = {a: 1}; let k = |u: {a: 'a}| 1; k(w) }
{ let w: [`A(i64), `A(string)] = `A(1); let k = |u: `A('a)| 1; k(w) }
{ let b = true; let w: Array<[i64, string]> = select b { true => [1], false => ["a"] }; array::len(w) }
PROGS
rm -rf $d
