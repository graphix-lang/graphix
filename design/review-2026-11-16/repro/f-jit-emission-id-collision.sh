#!/bin/sh
# f-jit: a run-time bind in a forked branch emits into a fresh Emission whose FuncIds
# restart at the helper table's end, and its link overwrites Jit::records entries the
# root emission's cached lambda bodies still name: a later root compile calls the wrong
# kernel.
# Command: sh f-jit-emission-id-collision.sh [path-to-graphix]
# Expected (both runs): the last value printed is 18 (sq(4) + 1)
# Observed: GRAPHIX_PAR=off prints 18; GRAPHIX_PAR=force prints 65 (cube(4) + 1): the
#   root's cached `sq` body (u0:178) was replaced in Jit::records by the forked bind's
#   `cube` body, also numbered u0:178 (GRAPHIX_DUMP_CLIF=1 shows both).
G=${1:-/home/eric/tmp/review-2026-11-16/bin/graphix}
input='let sq = |x: i64| x * x + 1
let cube = |x: i64| x * x * x
let a = once(sq(5) + 1)
a
let g = |p: i64| { let u = once(cube(p) + 2); let v = once(cube(p) + 3); u + v }
let h = |p: i64| p
let x = 1
let f = select x { 1 => g, _ => h }
let t = (f(2), 0)
t
let b = once(sq(4) + 1)
b'
for par in off force; do
  echo "== GRAPHIX_PAR=$par"
  printf '%s\n' "$input" | GRAPHIX_PAR=$par timeout -s KILL 20 "$G" --no-cache 2>&1 | grep -v '^-:'
done
# Variant: the forked bind's kernel at the colliding id takes an Array, so the root's
# region calls it with an i64 payload.
# Expected: 18. Observed under GRAPHIX_PAR=force: Segmentation fault (exit 139).
input2='let sq = |x: i64| x * x + 1
let alen = |a: Array<i64>| array::len(a) + 1
let a = once(sq(5) + 1)
a
let g = |p: i64| { let u = once(alen([p, p]) + 2); let v = once(alen([p]) + 3); u + v }
let h = |p: i64| p
let x = 1
let f = select x { 1 => g, _ => h }
let t = (f(2), 0)
t
let b = once(sq(4) + 1)
b'
echo "== GRAPHIX_PAR=force, mismatched signature"
printf '%s\n' "$input2" | GRAPHIX_PAR=force timeout -s KILL 20 "$G" --no-cache 2>&1 | grep -v '^-:'
echo "exit $?"
