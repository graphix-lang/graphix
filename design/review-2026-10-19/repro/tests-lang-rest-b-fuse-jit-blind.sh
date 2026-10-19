#!/bin/sh
# Title: FuseExpect::Jit on nested_same_intrinsic (and its siblings) passes
#        when the nested fold it is about node-walks.
# Command: sh design/review-2026-10-19/repro/tests-lang-rest-b-fuse-jit-blind.sh
# Expected: a fixture whose point is that the nested fold fuses fails when the
#           fold de-fuses.
# Observed: with the inner callback made unfusable (`y ~ y`), the program
#           still reports fused=1 (the separate `array::init` region), so
#           `FuseExpect::Jit` (FUSION_INVOCATIONS > 0) still holds; only the
#           `#[native]` form refuses it.
#   ns2:   fusion: attempted=10 fused=1 ... failed ... `y ~ y` — subtree node-walks
#   guard: fusion: attempted=9 fused=2 ... `n ~ n` — subtree node-walks
#   native control: --expand refuses `#[native] array::fold(..y ~ y..)`
BIN=/home/eric/tmp/review-2026-10-19/bin
D=$(mktemp -d)
cat > $D/ns2.gx <<'GX'
let result = {
  let src = array::init(i64:200, |i| i);
  array::fold(src, i64:0, |acc, x| array::fold([x + i64:1, x * i64:2], acc, |a, y| a + (y ~ y)))
};
result
GX
cat > $D/native.gx <<'GX'
let src = array::init(i64:200, |i| i);
#[native] array::fold(src, i64:0, |acc, x| array::fold([x + i64:1, x * i64:2], acc, |a, y| a + (y ~ y)))
GX
timeout -s KILL 180 $BIN/graphix-fuzz run $D/ns2.gx 2>&1 | sed -n '/^Jit\/InLanguage:/,/^Interp/p'
timeout -s KILL 60 $BIN/graphix --no-cache --expand $D/native.gx 2>&1 | head -3
# organic_deltas guard_fire: its select made unfusable, still fused=2
cat > $D/guard.gx <<'GX'
let result = {
  let x = array::iter([i64:1, i64:2, i64:3]);
  let g = x % i64:2;
  count(select i64:5 {n if g >= i64:0 => (n ~ n), _ => i64:0})
};
result
GX
timeout -s KILL 180 $BIN/graphix-fuzz run $D/guard.gx 2>&1 | sed -n '/^Jit\/InLanguage:/,/^Interp/p'
rm -rf $D
