#!/bin/sh
# Title: cast_from threads the static source type through every element (src_head per node), ~1.6x the cost of the same cast from Any
# Command: sh design/review-2026-10-26/repro/t-cast-setops-src-head-cost.sh
# Expected: the two casts cost about the same (the source type decides nothing a list's marker does not)
# Observed (debug build, 200k-element Array<P>): typed src: ~56 ms; Any src: ~34 ms
G=/home/eric/tmp/review-2026-10-26/bin/graphix
D=$(mktemp -d)
cat > $D/typed.gx <<'GX'
type P = {x: i64, y: [i64, null]};
let xs: Array<P> = array::init(200000, |i| {x: i, y: null});
let t0 = sys::time::now(xs);
let ys = cast<Array<P>>(xs)$;
let t1 = sys::time::now(ys);
sys::exit(sys::time::after_idle(duration:100.ms, 0));
println("typed src: [sys::time::diff(t1, t0)]")
GX
cat > $D/any.gx <<'GX'
type P = {x: i64, y: [i64, null]};
let xs: Array<P> = array::init(200000, |i| {x: i, y: null});
let a: Any = xs;
let t0 = sys::time::now(a);
let ys = cast<Array<P>>(a)$;
let t1 = sys::time::now(ys);
sys::exit(sys::time::after_idle(duration:100.ms, 0));
println("Any src: [sys::time::diff(t1, t0)]")
GX
timeout -s KILL 120 $G --no-cache $D/typed.gx | head -1
timeout -s KILL 120 $G --no-cache $D/any.gx | head -1
rm -rf $D
