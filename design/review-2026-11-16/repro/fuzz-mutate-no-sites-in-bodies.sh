#!/bin/bash
# Title: the type-guided must-reject families never mutate inside a function body
# Command: bash design/review-2026-11-16/repro/fuzz-mutate-no-sites-in-bodies.sh
# Expected (design/must_reject.md, "The type map": "every expression's type"): the same
#   statements give the same families whether they stand at the top level or in a body.
# Observed (graphix-fuzz at e1be2fbf2), the stderr census of each run:
#   top level: ... retype 3/0 ... undecided-part 2/0 variant-widen 1/0 widen-consumer 3/0 widen-hop 3/0
#   in a body: block-wrap 3/0 parens-wrap 3/0 widen-consumer 1/0   (the one site is the call f(1))
# graphix_compiler::record_expr_types visits no lambda body, so TypeMap::of is empty
# there and every family that picks its site from the map skips it.
F=/home/eric/tmp/review-2026-11-16/bin/graphix-fuzz
d=$(mktemp -d)
printf '%s\n' '{ let k = 1; let a = 1; let s = {x: a}; let v: [`A, `B] = `A; let t = select v { `A => 1, `B => 2 }; s.x + a + t + k }' > "$d/top.gx"
printf '%s\n' '{ let f = |k: i64| -> i64 { let a = 1; let s = {x: a}; let v: [`A, `B] = `A; let t = select v { `A => 1, `B => 2 }; s.x + a + t + k }; f(1) }' > "$d/body.gx"
for f in top body; do
  echo "== $f: $(cat "$d/$f.gx")"
  timeout -s KILL 170 "$F" typemorph "$d/$f.gx" 2>&1 | grep "probes/flips"
done
rm -rf "$d"
