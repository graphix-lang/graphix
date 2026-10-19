#!/bin/sh
# Title: an interface typedef whose fn type has a trait-typed parameter is
#        refused against its identical implementation.
# Command: sh design/review-2026-10-19/repro/t-typ-mod-sig-trait-param-typedef.sh
# Expected: the check passes (the same program without the module runs and prints "3").
# Observed: signature mismatch in F, expected type F = fn(x: Display) -> string,
#           found type F = fn(x: Display) -> string
GX=${GX:-/home/eric/tmp/review-2026-10-19/bin/graphix}
d=$(mktemp -d)
cat > "$d/m.gxi" <<'X'
type F = fn(x: Display) -> string;
val apply: fn(f: F, x: i64) -> string
X
cat > "$d/m.gx" <<'X'
type F = fn(x: Display) -> string;
let apply = |f: F, x: i64| -> string f(x)
X
cat > "$d/main.gx" <<'X'
mod m;
m::apply(|x| "[x]", 3)
X
timeout -s KILL 30 "$GX" --no-cache --check "$d/main.gx"
echo "exit $?"
rm -rf "$d"
