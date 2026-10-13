#!/bin/sh
# fuzz-mutate-01: widen-consumer reports false LEAKs: the unannotated-callee
# skip (mustreject.rs widen_consumer `inferred`) only knows let-bound lambda
# names.
# Run: sh fuzz-mutate-01.sh /path/to/graphix-fuzz
# Expected: no FLIP line for any body (as for the control `let f = |y| z <- y;
#   f(1)`, which the skip covers): each mutant widens z's shared cell to
#   [i64, string], which the checker rightly accepts (it runs and prints 1).
# Observed (HEAD 7324207f): for each of the four bodies
#   FLIP widen-consumer#N LEAK: accepted at call
#   MUTANT ... s.f(select 1 == 1 { true => 1, false => "tm" }) ...
FUZZ=${1:-graphix-fuzz}
D=$(mktemp -d)
i=0
for body in \
  '{ let z = never(); let f = |y| z <- y; let r = f(1); (r, z) }' \
  '{ let z = never(); let s = {f: |y| z <- y}; let r = s.f(1); (r, z) }' \
  '{ let z = never(); let r = (|y| z <- y)(1); (r, z) }' \
  '{ let z = never(); let f = |y| z <- y; let g = f; let r = g(1); (r, z) }' \
  '{ let z = never(); let f = { let k = 0; |y| z <- y }; let r = f(1); (r, z) }'
do
  echo "== $body"
  printf '%s\n' "$body" | timeout -s KILL 170 "$FUZZ" typemorph-one "$D/v$i" 5000
  grep -A1 'widen-consumer' "$D/v$i" || echo "(no widen-consumer flip)"
  i=$((i+1))
done
rm -rf "$D"
