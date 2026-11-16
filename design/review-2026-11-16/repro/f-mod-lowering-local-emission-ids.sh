#!/bin/sh
# Title: a run-time bind in a forked branch numbers its functions from the
#   JIT's first id, over the root emission's cached kernel bodies: a later
#   region calls another function's code.
#
# Command: sh design/review-2026-11-16/repro/f-mod-lowering-local-emission-ids.sh
#   (uses the review's debug binary; GRAPHIX_PAR=force puts the bind of
#   `k(a)` / `k(ys)` in a forked branch, which under the default Auto any
#   fork point that pays does.)
#
# Expected: both sessions end with 1007 (`g(a) + 2`, g = |x| x + 1000),
#   as they do under GRAPHIX_PAR=off.
# Observed: session 1 prints 18: the last region's call of the cached
#   kernel `g` runs the kernel of `f` the forked bind built (f(5) + 2).
#   Session 2, where that kernel takes an array, dies of SIGSEGV (exit 139).
#
# Cause: fusion::fuse_instance gives a non-emitting compile task a fresh
#   root emission (`local`), whose Names start at the helper table's next
#   id, the same id the context's own emission started at; Jit::records is
#   keyed by that id alone, so the link of the task's functions replaces
#   (or, for a region's parent, removes) the records of the bodies the
#   context's emission still hands out from its cache.
G=${GRAPHIX:-/home/eric/tmp/review-2026-11-16/bin/graphix}
feed() { while IFS= read -r l; do echo "$l"; sleep 0.5; done; }
echo "== session 1: same ABI, wrong answer"
feed <<'GX' | GRAPHIX_PAR=force timeout -s KILL 30 "$G" --no-cache 2>&1 | tail -4
let g = |x: i64| x + 1000;
let f = |x: i64| x * 3 + 1;
let a = 5;
g(a) + 0
let k = |x: i64| x + 1;
k <- |x: i64| f(x) - 7;
{ let u = k(a); let v = a * 2; u + v }
g(a) + 2
GX
echo "== session 2: another ABI, SIGSEGV"
feed <<'GX' | GRAPHIX_PAR=force timeout -s KILL 30 "$G" --no-cache 2>&1 | tail -4
let g = |x: i64| x + 1000;
let f = |xs: Array<i64>| array::len(xs) + 1;
let a = 5;
g(a) + 0
let ys = [1, 2, 3];
let k = |xs: Array<i64>| 0;
k <- |xs: Array<i64>| f(xs) - 7;
{ let u = k(ys); let v = a * 2; u + v }
g(a) + 2
GX
