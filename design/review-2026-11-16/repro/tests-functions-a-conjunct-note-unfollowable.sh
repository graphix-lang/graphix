#!/bin/sh
# Title: an argument refused for its operator conjunct is told "annotate `p`",
#        and no annotation of `p` makes the program pass.
# Command: sh design/review-2026-11-16/repro/tests-functions-a-conjunct-note-unfollowable.sh
# Expected: the unannotated refusal names no annotation (as
#   lang::functions::undecided_mismatch_names_no_annotation says: "A refusal
#   no annotation of the undecided argument passes names none"), since every
#   annotation of `p` is refused too.
# Observed (HEAD e1be2fbf2):
#   p               -> 'a: string does not contain '_N: unbound within Number & Singleton:
#                      `p` is undecided at this call; annotate `p`
#   p: string       -> Number does not contain string
#   p: i64          -> 'a: string does not contain i64
#   p: [i64, string]-> Number does not contain [i64, string]
#   p: Any          -> Number does not contain Any
# The same note is pinned verbatim by lang::functions::
#   undecided_parameter_within_a_conjunct_refused,
#   undecided_parameter_within_a_conjunct_builtin_refused,
#   undecided_binding_within_a_conjunct_refused.
G=${G:-/home/eric/tmp/review-2026-11-16/bin/graphix}
d=$(mktemp -d)
for T in "" ": string" ": i64" ": [i64, string]" ": Any"; do
  cat > "$d/cj.gx" <<GX
let g = |p$T, q| {
  let s = p + q;
  array::push(["s"], p)
};
0
GX
  echo "== p$T"
  timeout -s KILL 60 "$G" --check --no-cache "$d/cj.gx" 2>&1 | grep "type mismatch" | sed 's/ (line.*//'
done
rm -rf "$d"
# The binding form (lang::functions::undecided_binding_within_a_conjunct_refused):
# unannotated it gets the same note; v: string and v: i64 are both refused.
d=$(mktemp -d)
for T in "" ": string" ": i64"; do
  cat > "$d/vb.gx" <<GX
let v$T = never();
let s = v + v;
let r = array::push(["s"], v);
v <- "x";
(s, r)
GX
  echo "== v$T"
  timeout -s KILL 60 "$G" --check --no-cache "$d/vb.gx" 2>&1 | grep "type mismatch" | sed 's/ (line.*//'
done
rm -rf "$d"
