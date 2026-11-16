#!/bin/sh
# Title: Graphix programs in untagged ``` fences escape examples_compile's gate
# Command: sh design/review-2026-11-16/repro/tests-shell-compiler-b-untagged-fences.sh
# book/src/udt/polymorphic.md holds three programs in bare ``` fences (lines 8,
# 29 and 44), so page_fences files them as Open::Other and nothing checks them.
# Expected (what the page shows):
#   {x: 1, y: 1, z: 3.14}
#   1: type mismatch Point3<'a: [Int, Real]> does not contain {x: string, y: string, z: string}
# Observed:
#   {x: 1.0, y: 1.0, z: 3.14}
#   2: type mismatch Point3<'a: unbound within Number> does not contain { x: string, y: string, z: string }
GX=${GX:-/home/eric/tmp/review-2026-11-16/bin/graphix}
D=$(mktemp -d)
cat > "$D/a.gx" <<'EOF'
type Point3<'a: Number> = {x: 'a, y: 'a, z: 'a};
let f = |p: Point3<'a>, x: 'a| {p with x: p.x + x};
sys::exit(sys::time::after_idle(duration:100.ms, 0));
f({x: 0., y: 1., z: 3.14}, 1.)
EOF
cat > "$D/b.gx" <<'EOF'
type Point3<'a: Number> = {x: 'a, y: 'a, z: 'a};
let v: Point3<'a> = {x: "foo", y: "bar", z: "baz"};
v
EOF
timeout -s KILL 30 "$GX" --no-cache --no-netidx "$D/a.gx"
timeout -s KILL 30 "$GX" --no-cache --no-netidx --check "$D/b.gx" 2>&1 | tail -1
rm -rf "$D"
