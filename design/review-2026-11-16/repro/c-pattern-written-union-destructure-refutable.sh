#!/bin/sh
# Title: a constructor pattern that matches every member of the union it
#        stands over is refused as refutable: in a parameter or an annotated
#        let at any position, and in an unannotated let at a nested position.
#        The unannotated let at the top checks, a select arm with the same
#        pattern is exhaustive, a nested or-pattern covering the same union
#        checks, and the many-members refusal names the refused form as its
#        fix.
#
# Command: sh design/review-2026-11-16/repro/c-pattern-written-union-destructure-refutable.sh
#
# Expected: every program below checks (each pattern matches every value of
#   its type; 1, 7, 9 and 11 are the same destructures in forms that check,
#   and refusal 5 tells the author to write program 4).
# Observed:
#   1 ok
#   2 refutable patterns are not allowed in let
#   3 refutable patterns are not allowed in lambda arguments (a, b)
#   4 refutable patterns are not allowed in lambda arguments { a, b: _ }
#   5 the pattern { a, .. } matches more than one member, { a: i64, b: i64 }
#     and { a: string, b: string }; name every field of { a, .. }, which the
#     members share, or take the value apart in a select, `T as { a, .. }`
#   6 refutable patterns are not allowed in lambda arguments (a, b)
#   7 ok
#   8 refutable patterns are not allowed in let: ((i64, i64), '_NNNN: unbound)
#     matches only part of ([(i64, i64), (string, string)], bool)
#   9 ok
#   10 refutable patterns are not allowed in lambda arguments ((a, b), c)
#   11 ok
G=/home/eric/tmp/review-2026-11-16/bin/graphix
d=$(mktemp -d)
n=0
while IFS= read -r p; do
  n=$((n+1))
  printf '%s\n' "$p" > "$d/p.gx"
  if out=$(timeout -s KILL 30 "$G" --no-cache --check "$d/p.gx" 2>&1); then
    echo "$n ok"
  else
    echo "$n $(printf '%s\n' "$out" | grep -E '^    [0-9]+: ' | grep -v ': at: ' | tail -1 | sed -E 's/^ +[0-9]+: //')"
  fi
done <<'PROGS'
let f = |v: [(i64, i64), (string, string)]| { let (a, b) = v; "[a][b]" }; f((1, 2))
let f = |v: [(i64, i64), (string, string)]| { let (a, b): [(i64, i64), (string, string)] = v; "[a][b]" }; f((1, 2))
let f = |(a, b): [(i64, i64), (string, string)]| "[a][b]"; f((1, 2))
let f = |{a, b: _}: [{a: i64, b: i64}, {a: string, b: string}]| "[a]"; f({a: 1, b: 2})
let f = |{a, ..}: [{a: i64, b: i64}, {a: string, b: string}]| "[a]"; f({a: 1, b: 2})
let xs: Array<[(i64, i64), (string, string)]> = [(1, 2)]; array::map(xs, |(a, b)| "[a][b]")
let xs: Array<[(i64, i64), (string, string)]> = [(1, 2)]; array::map(xs, |x| { let (a, b) = x; "[a][b]" })
let v: ([(i64, i64), (string, string)], bool) = ((1, 2), true); let ((a, b), c) = v; "[a][b][c]"
let v: ([(i64, i64), (string, string)], bool) = ((1, 2), true); let (p, c) = v; let (a, b) = p; "[a][b][c]"
let xs: Array<([(i64, i64), (string, string)], bool)> = [((1, 2), true)]; array::map(xs, |((a, b), c)| "[a][b][c]")
let v: ([(i64, i64), (string, string)], bool) = ((1, 2), true); select v { ((a, b), c) => "[a][b][c]" }
PROGS
rm -rf "$d"
