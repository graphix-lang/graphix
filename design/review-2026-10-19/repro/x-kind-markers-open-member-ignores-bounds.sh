#!/bin/sh
# Title: the open-member rule reads a bounded cell as any type at all: an
# 'a: Int or 'a: Number member, which can only bind a number, is refused beside
# an array or a tuple test as one that "may share" its runtime form.
# Command: sh design/review-2026-10-19/repro/x-kind-markers-open-member-ignores-bounds.sh
# Expected: (-1, 2) and (0, 3)
# Observed: both refused: "this pattern tests for Array<i64> among 'a: unbound, but a
#           type variable there isn't known here and may share Array<i64>'s runtime
#           form: annotate it" (and the same for (i64, i64))
d=$(mktemp -d)
G=/home/eric/tmp/review-2026-10-19/bin/graphix
cat > "$d/a.gx" <<'GX'
let f = 'a: Int |x: ['a, Array<i64>]| -> i64 select x { Array<i64> as a => array::len(a), _ => -1 };
sys::exit(sys::time::after_idle(duration:100.ms, 0));
(f(5), f([1, 2]))
GX
cat > "$d/b.gx" <<'GX'
let f = 'a: Number |x: ['a, (i64, i64)]| -> i64 select x { (a, b) => a + b, _ => 0 };
sys::exit(sys::time::after_idle(duration:100.ms, 0));
(f(5), f((1, 2)))
GX
for f in a b; do echo "== $f.gx"; timeout -s KILL 30 $G --no-cache "$d/$f.gx" 2>&1 | tail -1; done
rm -rf "$d"
