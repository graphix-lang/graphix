#!/bin/sh
# Title: a write through a field of a union of structs/tuples is checked against the
#   union of the members' field types, so it builds a value of no member
# Command: sh c-data-map-union-field-write.sh   (GRAPHIX=path to the graphix binary)
# Expected: each program refused by the check (or typed so its select covers the value)
# Observed: all three pass --check; at run time the node-walk reports "no arm matches ...
#   a coverage hole in the check" on {a: 5, b: 9} / (5, 9), and the JIT (program 1, `pick`
#   fused) takes the wildcard arm and reads the i64 field as a string: ({a: 123456, b: 9}, "!false")
G=${GRAPHIX:-/home/eric/tmp/review-2026-10-19/bin/graphix}
D=$(mktemp -d)
cat > "$D/with.gx" <<'GX'
type S = [{a: i64, b: string}, {a: string, b: i64}];
let h = |s: S, k: i64| -> S {s with a: k};
let pick = |t: S| -> string select t {
  {a: i64, b: string} as x => x.b,
  v => "[v.a]![v.a == "q"]"
};
let t = h({a: "y", b: 9}, 123456);
let r = pick(t);
sys::exit(sys::time::after_idle(duration:100.ms, 0));
(t, r)
GX
cat > "$D/place_struct.gx" <<'GX'
type S = [{a: i64, b: string}, {a: string, b: i64}];
let s: S = {a: "y", b: 9};
let r = &mut s.a;
*r <- 5;
let out = select s {
  {a: i64, b: string} as x => "first [x.b]",
  {a: string, b: i64} as y => "second [y.a]"
};
sys::exit(sys::time::after_idle(duration:100.ms, 0));
(s, out)
GX
cat > "$D/place_tuple.gx" <<'GX'
type T = [(i64, string), (string, i64)];
let t: T = ("y", 9);
let r = &mut t.0;
*r <- 5;
let out = select t {
  (i64, string) as x => "first [x.1]",
  (string, i64) as y => "second [y.0]"
};
sys::exit(sys::time::after_idle(duration:100.ms, 0));
(t, out)
GX
for p in with place_struct place_tuple; do
  echo "=== $p: --check"; timeout -s KILL 30 "$G" --check "$D/$p.gx"; echo "rc=$?"
  echo "=== $p: jit"; timeout -s KILL 30 "$G" --no-cache "$D/$p.gx" 2>&1 | tail -3
  echo "=== $p: node-walk"; timeout -s KILL 30 "$G" --no-cache --no-fusion "$D/$p.gx" 2>&1 | tail -3
done
rm -rf "$D"
