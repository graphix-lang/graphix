#!/bin/bash
# Title: a val's bound is refused once its walk crosses more than eight
#        typedef names (Meetings::expansions), with a message naming one
#        place twice
# Command: bash design/review-2026-11-16/repro/t-fntyp-bound-expansion-budget.sh
# Expected: depth 7 and depth 8 both pass: the implementation's bound
#           { v: .. [ 'p, null ] } is the signature's T<n><'a> with 'p for 'a.
# Observed: depth 7 passes; depth 8 is refused:
#   the implementation's 'p is one type wherever a bound names it: the
#   signature holds it at more than one place (T0<'a>, T0<'a>), and no bound
#   of its own says which type it is; ...
G=${GRAPHIX:-/home/eric/tmp/review-2026-11-16/bin/graphix}
D=$(mktemp -d)
trap 'rm -rf "$D"' EXIT
gen() {
  n=$1; d="$D/exp$n"; mkdir -p "$d"
  { echo "type T0<'a> = {v: 'a};"
    for i in $(seq 1 "$n"); do echo "type T$i<'a> = {v: T$((i-1))<'a>};"; done
    echo "val f: fn<'b: T$n<'a>>(x: 'b) -> 'b;"; } > "$d/foo.gxi"
  t="{v: ['p, null]}"; for i in $(seq 1 "$n"); do t="{v: $t}"; done
  echo "let f = 'q: $t |x: 'q| -> 'q x" > "$d/foo.gx"
  printf 'mod foo;\n1\n' > "$d/main.gx"
  echo "--- depth $n"
  ( cd "$d" && timeout -s KILL 60 "$G" --no-cache --check main.gx 2>&1 | tail -1 | cut -c1-600; echo "exit ${PIPESTATUS[0]}" )
}
gen 7
gen 8
