#!/bin/sh
# Title: a module refusal's diagnostic walks every binding's type with
#        ForeignWrite::held_by -> Type::holds_cell, which has no seen set: time
#        doubles per level of a pair DAG anywhere in scope.
# Command: sh t-tvar-held-by-dag-exponential.sh
# Expected: the refusal "the check decides a type the code around the module
#   left open: annotate `a`" in about the time the annotated program checks
#   (~40 ms at any depth).
# Observed (debug binary): annotated D=26 41 ms; refused D=24 329 ms,
#   D=26 1246 ms, D=28 4736 ms (x4 per two levels).
G=${G:-/home/eric/tmp/review-2026-10-26/bin/graphix}
D=${1:-28}
dir=$(mktemp -d)
cd "$dir" || exit 1
printf 'val y: i64;\n' > m.gxi
printf 'let y = { super::a <- 1; 1 };\n' > m.gx
{
  echo "let a = never();"
  echo "let x0 = 1;"
  i=1
  while [ $i -le $D ]; do echo "let x$i = (x$((i-1)), x$((i-1)));"; i=$((i+1)); done
  echo "mod m;"
  echo "(m::y, x$D)"
} > main.gx
time timeout -s KILL 120 "$G" --check main.gx 2>&1 | tail -1
rm -rf "$dir"
