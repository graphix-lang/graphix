#!/bin/bash
# Title: a declared bound inside a bound "holds at the call" -- only the
#   accepting call is pinned
# Command: bash design/review-2026-10-19/repro/tests-modules-image-bound-inside-bound-call.sh
# Expected: m::f([1.5]) refused, since the interface declares 'b: Int.
# Observed (HEAD 9445a1d21): refused ("Array<'_N: unbound within Int> does not
#   contain Array<f64>"); interfaces.rs a_declared_bound_inside_a_bound_holds
#   calls only m::f([1]), which passes whether or not the bound is enforced.
G=${G:-/home/eric/tmp/review-2026-10-19/bin/graphix}
d=$(mktemp -d); cd "$d"
printf "val f: fn<'a: Array<'b>, 'b: Int>(x: 'a) -> 'a;\n" > m.gxi
printf "let f = 'a: Array<'b>, 'b: Int |x: 'a| -> 'a x\n" > m.gx
printf 'mod m;\nm::f([1.5])\n' > main.gx
timeout -s KILL 30 "$G" --check main.gx 2>&1 | tail -1
rm -rf "$d"
