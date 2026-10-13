#!/bin/sh
# c-data-map-02: eta-expansion of a trait method makes its optional labeled arguments required.
# Run: sh design/review-2026-10-12/repro/c-data-map-02.sh [path/to/graphix]
# Expected: case 1 prints ("i4", "i4"); case 2 prints ["i1", "i2"], as case 3
#   (an ordinary function with #n = 1 in the same position) does.
# Observed: case 1 refused "missing required argument n"; case 2 refused
#   "type mismatch fn(x: 'a: i64) -> 'b ... does not contain
#   fn<'eta_self: Show>(#n: i64, eta1: 'eta_self: i64) -> string"; case 3 prints ["i1", "i2"].
G=${1:-graphix}
D=$(mktemp -d)
cat > "$D/1.gx" <<'GX'
trait Show { val show: fn(?#n: i64, self) -> string };
impl Show for i64 { let show = |#n = 1, i| "i[i * n]" };
let g = Show::show;
sys::exit(sys::time::after_idle(duration:100.ms, 0));
(Show::show(4), g(4))
GX
cat > "$D/2.gx" <<'GX'
trait Show { val show: fn(?#n: i64, self) -> string };
impl Show for i64 { let show = |#n = 1, i| "i[i * n]" };
sys::exit(sys::time::after_idle(duration:100.ms, 0));
array::map([1, 2], Show::show)
GX
cat > "$D/3.gx" <<'GX'
let show = |#n = 1, i: i64| "i[i * n]";
sys::exit(sys::time::after_idle(duration:100.ms, 0));
array::map([1, 2], show)
GX
for c in 1 2 3; do echo "== case $c"; timeout -s KILL 30 "$G" --no-cache "$D/$c.gx" 2>&1 | tail -1; done
rm -rf "$D"
