#!/bin/sh
# Title: the node-walk runs every guarded arm's full type-and-structure test on
#   every update of its select, though the scrutinee stands: Select::update
#   calls PatternNode::shape_matches for each guarded arm before it looks at
#   the scrutinee's tag, and a test that needs the deep `is_a` walk costs the
#   size of the value each time another input of the select fires.
# Command: sh design/review-2026-11-16/repro/c-select-seq-guard-shape-test-every-update.sh
# The scrutinee is a 300000-element array that fires once; `n` fires 3000 times
#   and only re-evaluates the taken arm. The twin differs only in its guard.
# Expected: the guarded select costs what its twin costs (the shape verdict of a
#   standing scrutinee is the `guard_ran` bit the select already keeps).
# Observed (debug build, --no-fusion, a loaded machine): guarded 7.0 s,
#   unguarded 1.7 s: about 1.8 ms a cycle for the walk of the standing array.
G=${G:-/home/eric/tmp/review-2026-11-16/bin/graphix}
D=$(mktemp -d)
cat > "$D/guarded.gx" <<'GX'
let big: Array<[i64, string]> = array::init(300000, |i| i);
let n = 0;
n <- filter(n + 1, |v| v < 3000);
let ok = true;
let s = select big { Array<i64> as [h, ..] if ok => n, _ => 0 };
sys::exit(select s { 2999 => 0, _ => never() });
s
GX
sed 's/ if ok => n/ => n/' "$D/guarded.gx" > "$D/unguarded.gx"
for p in guarded unguarded; do
    s=$(date +%s%N)
    timeout -s KILL 170 "$G" --no-cache --no-fusion "$D/$p.gx" > /dev/null 2>&1
    e=$(date +%s%N)
    echo "$p: $(( (e - s) / 1000000 )) ms"
done
rm -rf "$D"
