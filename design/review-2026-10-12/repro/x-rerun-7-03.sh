#!/bin/sh
# x-rerun-7-03: an interface val is refused against a ground union binding
# sig_matches did not normalize ([i64, i64] vs i64).
# command: sh design/review-2026-10-12/repro/x-rerun-7-03.sh [path-to-graphix]
# expected: exit 0 (x is i64; the same program checks without m.gxi, and with
#   `let y: i64 = never()`)
# observed: exit 1, "val z is declared i64 but implemented '_N: type mismatch:
#   signature has i64, implementation has [i64, i64]" (--check and a run alike)
G=${1:-graphix}
D=$(mktemp -d)
cat > "$D/main.gx" <<'GX'
let b = true;
let y = never();
let x = select b { true => y, false => 1 };
y <- 2;
mod m;
m::z
GX
echo 'let z = super::x' > "$D/m.gx"
echo 'val z: i64;' > "$D/m.gxi"
timeout -s KILL 60 "$G" --check "$D/main.gx"
r=$?
rm -rf "$D"
echo "exit $r"
exit $r
