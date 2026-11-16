#!/bin/sh
# Title: sibling_written_from_outside_its_module_fires_alone never checks `m::a`
# The predicate is `ends_with(", i64:1]")`: it pins count(m::b) and accepts
# any m::a. The program's first value is (61, 1); a build that dropped every
# outside write to `m::a` would give (1, 1) and still pass.
# Command: sh tests-modules-sibling-outside-loose.sh  (G= the debug graphix)
# Observed: (61, 1) first, then (71, 1)
G=${G:-/home/eric/tmp/review-2026-11-16/bin/graphix}
D=$(mktemp -d)
cat > "$D/main.gx" <<'GX'
mod m;
let n = 0;
n <- select n { k if k < 6 => (k ~ n) + 1, _ => never() };
m::a <- n ~ m::a + 10;
let sel = select n { 2 => false, 3 => false, _ => true };
let out = select sel { true => (m::a, count(m::b)), false => (-1, -1) };
sys::exit(sys::time::after_idle(duration:100.ms, 0));
select n { 6 => out, _ => never() }
GX
echo 'let (a, b) = (1, 2)' > "$D/m.gx"
timeout -s KILL 30 "$G" --no-cache "$D/main.gx"
rm -rf "$D"
