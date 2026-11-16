#!/bin/sh
# Title: two references to one variable compare unequal when one names an
# interface val and the other the binding that implements it
#
# Command: sh design/review-2026-11-16/repro/c-bind-ref-equality-across-val.sh
#
# m.gx binds `x` and `r = &x`; m.gxi exports both as vals. The val `m::x` and
# the body's `x` are one variable (CompileCtx::join_interface_pair): the write
# `m::x <- 9` is read through `m::r` and through `&m::x` alike.
# design/place_references.md: "`==` and `!=` compare what they name".
#
# Expected: (0, true, true, 5, 5) (1, true, true, 9, 9) (2, true, true, 9, 9)
# Observed: (0, false, false, 5, 5) (1, false, false, 9, 9) (2, false, false, 9, 9)
BIN=${BIN:-/home/eric/tmp/review-2026-11-16/bin/graphix}
D=$(mktemp -d)
printf 'val x: i64;\nval r: &i64;\nval same: fn(a: &i64) -> bool;\n' > "$D/m.gxi"
printf 'let x = 5;\nlet r = &x;\nlet same = |a: &i64| a == &x;\n' > "$D/m.gx"
cat > "$D/main.gx" <<'GX'
mod m;
let c = array::iter([0, 1, 2]);
m::x <- select c { 0 => 9, _ => never() };
let o = &m::x;
(c, m::r == o, m::same(o), *m::r, *o)
GX
timeout -s KILL 10 "$BIN" --no-cache "$D/main.gx"
rm -rf "$D"
