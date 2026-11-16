#!/bin/sh
# Title: a decided argument wider than a callback-held variable is refused
#        naming no annotation, though annotating the earlier argument passes.
# Command: sh design/review-2026-11-16/repro/tests-functions-a-callback-held-refusal-names-nothing.sh
# Expected: the refusal names the fix, as the undecided twin does ("annotate
#   that argument as wide as this one", callsite.rs::decided_elsewhere) and as
#   a bound-held variable does (callsite.rs::bound_held_note).
# Observed (HEAD e1be2fbf2):
#   fixture (lang::functions::callback_variable_does_not_widen):
#     type mismatch 'a: u64 does not contain [u64, null]      <- no note
#   first argument annotated [u64, null]: accepted (no fixture pins it)
#   undecided twin: "...; annotate that argument as wide as this one ..."
G=${G:-/home/eric/tmp/review-2026-11-16/bin/graphix}
d=$(mktemp -d)
run() {
  printf '%s\n' "$1" > "$d/p.gx"
  echo "== $1"
  out=$(timeout -s KILL 60 "$G" --check --no-cache "$d/p.gx" 2>&1 | grep "type mismatch\|annotate" | sed 's/ (line.*//')
  echo "${out:-   accepted}"
}
run '{ let n: [u64, null] = null; let g = |x: '"'"'a, f: fn(v: '"'"'a) -> '"'"'a, y: '"'"'a| -> '"'"'a f(y); g(u64:0, |v| v, n) }'
run '{ let n: [u64, null] = null; let z: [u64, null] = u64:0; let g = |x: '"'"'a, f: fn(v: '"'"'a) -> '"'"'a, y: '"'"'a| -> '"'"'a f(y); g(z, |v| v, n) }'
run '{ let f = |x: '"'"'a, y: '"'"'a, k: fn(v: '"'"'a) -> i64| -> '"'"'a y; f(1, str::parse("5"), |v| 0) }'
rm -rf "$d"
