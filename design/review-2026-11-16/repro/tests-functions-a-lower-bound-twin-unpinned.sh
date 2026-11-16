#!/bin/sh
# Title: undecided_binding_above_a_lower_bound_refused pins "annotate `v`" on a
#        program that annotation cannot make pass, and the twin it names is unpinned.
# Command: sh design/review-2026-11-16/repro/tests-functions-a-lower-bound-twin-unpinned.sh
# Expected: a refused fixture whose named annotation makes it pass, and that
#   annotated program pinned as accepted.
# Observed (HEAD e1be2fbf2):
#   fixture as written                  -> refused "... `v` is undecided at this call; annotate `v`"
#   fixture with `v: [i64, string]`     -> still refused, now at `let r: i64`
#   without `let r: i64`, unannotated   -> refused, same note (the fixture's point)
#   without `let r: i64`, annotated     -> accepted; no fixture or skill case pins it
G=${G:-/home/eric/tmp/review-2026-11-16/bin/graphix}
d=$(mktemp -d)
run() {
  printf '%s\n' "$2" > "$d/p.gx"
  echo "== $1"
  out=$(timeout -s KILL 60 "$G" --check --no-cache "$d/p.gx" 2>&1 | grep "type mismatch" | sed 's/ (line.*//')
  echo "${out:-   accepted}"
}
B='type B<'"'"'p> = Abstract<'"'"'p>; let x = B(1); x <- B("a");'
F='let f = |a: Array<'"'"'a>, b: '"'"'a| b;'
run "fixture"                     "{ $B let v = x.0; $F let r: i64 = f([10], v); r }"
run "fixture, v annotated"        "{ $B let v: [i64, string] = x.0; $F let r: i64 = f([10], v); r }"
run "no r, v unannotated"         "{ $B let v = x.0; $F f([10], v) }"
run "no r, v annotated"           "{ $B let v: [i64, string] = x.0; $F f([10], v) }"
rm -rf "$d"
