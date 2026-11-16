#!/bin/sh
# Title: a `let` of an array literal holding one generic function is generalized,
#   one holding two (`[g, g]`, `[|x| x, |y| y]`) is not
# Command: sh tests-functions-b-array-of-two-fns-not-generic.sh [path/to/graphix]
# Expected (design/tvar_constraints.md: "A `let` nothing writes whose value holds
#   a lambda literal or a generalized binding's value in a data literal (... an
#   array ...) is generalized as a let of a lambda is ... an annotation of the
#   holder ... decides only that read's copy"; functions.rs above
#   generic_function_copies_in_an_array: "each call through either picks the
#   quantifier anew"): all four programs check.
# Observed: the one-element arrays check; the two-element arrays are refused:
#   two_lambdas: "Array<fn(x: string) -> string> does not contain Array<fn(y: i64) -> i64>"
#   two_copies:  "type mismatch u8 does not contain i64"
G=${1:-/home/eric/tmp/review-2026-11-16/bin/graphix}
D=$(mktemp -d)
cat > $D/one_lambda.gx <<'GX'
let fs = [|x| x];
let n: Array<fn(x: i64) -> i64> = fs;
let s: Array<fn(x: string) -> string> = fs;
(n[0]$(1), s[0]$("s"))
GX
cat > $D/two_lambdas.gx <<'GX'
let fs = [|x| x, |y| y];
let n: Array<fn(x: i64) -> i64> = fs;
let s: Array<fn(x: string) -> string> = fs;
(n[0]$(1), s[0]$("s"))
GX
cat > $D/one_copy.gx <<'GX'
let g = 'q: Number |x, y: 'q| -> 'q x + y;
let fs = [g];
(fs[0]$(u8:1, u8:2), fs[0]$(i64:1, i64:2))
GX
cat > $D/two_copies.gx <<'GX'
let g = 'q: Number |x, y: 'q| -> 'q x + y;
let fs = [g, g];
(fs[0]$(u8:1, u8:2), fs[0]$(i64:1, i64:2))
GX
for f in one_lambda two_lambdas one_copy two_copies; do
  printf '%s: ' $f
  if timeout -s KILL 30 "$G" --no-cache --check $D/$f.gx > $D/$f.out 2>&1; then echo ok
  else echo "refused: $(tail -1 $D/$f.out | sed 's/^ *[0-9]*: //')"; fi
done
rm -rf $D
