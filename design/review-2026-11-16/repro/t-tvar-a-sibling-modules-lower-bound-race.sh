#!/bin/sh
# Title: a module's check moves a lower bound into its parent's open cell
#        unrecorded (TVar::merge_one), so the check of two sibling modules
#        is accepted or refused by the schedule.
#
# Command: sh design/review-2026-11-16/repro/t-tvar-a-sibling-modules-lower-bound-race.sh
#          (GRAPHIX=/path/to/graphix to pick the binary)
#
# Expected: every run refuses, "the check decides a type the code around the
#           module left open: annotate `x`" (design/parallel_compile.md, Module
#           checks: a module's check writes no cell created outside it), and
#           `only-ma` is refused the same way.
# Observed: `only-ma` is accepted and decides the parent's x as i64
#           (`x <- "s"` after the mod: "x is i64 and cannot hold string");
#           `only-mb` is refused; `ma+mb` is accepted in about half the runs
#           and refused in the rest (16/14 and 13/17 of 30 in the review).
G=${GRAPHIX:-/home/eric/tmp/review-2026-11-16/bin/graphix}
D=$(mktemp -d)
trap 'rm -rf "$D"' EXIT
printf 'val g: fn(a: i64) -> i64;\n' > "$D/ma.gxi"
cp "$D/ma.gxi" "$D/mb.gxi"
cat > "$D/ma.gx" <<'GX'
let g2 = 'x: Array<['a, null]> |y: 'x, d: 'a| -> 'a d;
let r = g2([1], super::x);
let g = |a: i64| a
GX
cat > "$D/mb.gx" <<'GX'
let g3 = 'x: Array<['a, null]> |d: 'a, y: 'x| -> 'a d;
let r = g3(super::x, [1]);
let g = |a: i64| a
GX
printf 'let x = never();\nmod ma;\nx\n' > "$D/only_ma.gx"
printf 'let x = never();\nmod ma;\nx <- "s";\nx\n' > "$D/ma_then_write.gx"
printf 'let x = never();\nmod mb;\nx\n' > "$D/only_mb.gx"
printf 'let x = never();\nmod ma;\nmod mb;\nx\n' > "$D/both.gx"
run() { timeout -s KILL 60 "$G" --no-cache -n -i --check "$1" 2>&1; }
echo "== only-ma"; run "$D/only_ma.gx" > "$D/out"; echo "exit $?"; tail -1 "$D/out"
echo "== only-ma, then x <- \"s\" in the parent"; run "$D/ma_then_write.gx" | tail -1
echo "== only-mb"; run "$D/only_mb.gx" | tail -1
ok=0; bad=0
for i in 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20; do
    if run "$D/both.gx" >/dev/null; then ok=$((ok + 1)); else bad=$((bad + 1)); fi
done
echo "== ma+mb, 20 runs: accepted $ok, refused $bad"
