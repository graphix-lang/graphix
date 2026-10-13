#!/bin/sh
# c-pattern-01: or-alternative struct test ignores field names: binds the wrong
# field, an i64 holds a string, and a fused kernel panics on it.
#
# StructPatternNode's Struct arm (is_match_inner / bind_inner) reads each bound
# field by its index in the alternative's struct type and never compares the
# field name; an or-pattern takes the first alternative whose structure passes,
# while the check (`footprint` + `rep_collision`) compares field names and so
# accepts the alternatives as distinguishable.
#
# command: sh design/review-2026-10-12/repro/c-pattern-01.sh <path-to-graphix>
# expected: first program prints [0, "s"] (both engines);
#           second program prints 4 (fused and --no-fusion).
# observed: first program prints ["q", 1] with and without --no-fusion
#           (r: i64 holds "q"; h's y binds field x);
#           second program, fused: the runtime panics
#             "kernel param `x`: runtime String("q") does not match the compiled
#              Scalar(I64) slot" and the shell exits "runtime did not respond";
#           --no-fusion: logs an arith error and the result is bottom.
G=${1:-graphix}
D=$(mktemp -d)
cat > "$D/a.gx" <<'GX'
let g = |p: [{a: i64, x: i64}, {x: i64, z: string}]| select p {
  {a: 0, x} | {x, z: "q"} => x,
  _ => -1
};
let h = |p: [{y: i64}, {x: i64, y: string}]| select p {
  {y} | {x: 1, y} => y,
  _ => "none"
};
let r: i64 = g({x: 0, z: "q"});
sys::exit(sys::time::after_idle(duration:100.ms, 0));
(r, h({x: 1, y: "s"}))
GX
cat > "$D/b.gx" <<'GX'
let g = |p: [{a: i64, x: i64}, {x: i64, z: string}]| select p {
  {a: 0, x} | {x, z: "q"} => x * 2 + 1,
  _ => -1
};
let k = |p: [{a: i64, x: i64}, {x: i64, z: string}]| g(p) + 3;
let v: [{a: i64, x: i64}, {x: i64, z: string}] = {x: 0, z: "q"};
sys::exit(sys::time::after_idle(duration:100.ms, 0));
k(v)
GX
echo "== a.gx fused";      timeout -s KILL 30 "$G" --no-cache "$D/a.gx"
echo "== a.gx --no-fusion"; timeout -s KILL 30 "$G" --no-cache --no-fusion "$D/a.gx"
echo "== b.gx fused";      timeout -s KILL 30 "$G" --no-cache "$D/b.gx"
echo "== b.gx --no-fusion"; timeout -s KILL 30 "$G" --no-cache --no-fusion "$D/b.gx"
rm -rf "$D"
