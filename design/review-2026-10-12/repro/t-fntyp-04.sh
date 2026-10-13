#!/bin/sh
# t-fntyp-04: sig_matches misses implementation bounds on variables reachable only through a bound
# usage: sh t-fntyp-04.sh /path/to/graphix
# The .gxi declares fn<'a: Array<'b>>, the implementation 'a: Array<'b>, 'b: Int.
# Expected: the interface is refused (the implementation requires 'b: Int, which it does not declare),
#   or at least --check refuses m::f([1.5]).
# Observed: --check exits 0; the run refuses
#   "an instance at fn(x: Array<f64>) -> Array<f64> of a definition typed fn<'a: Array<'b>>(x: 'a) -> 'a".
# Control: with 'b: Int declared in the .gxi, --check refuses the call.
B=${1:-graphix}
D=$(mktemp -d)
printf "val f: fn<'a: Array<'b>>(x: 'a) -> 'a;\n" > $D/m.gxi
printf "let f = 'a: Array<'b>, 'b: Int |x: 'a| -> 'a x;\n" > $D/m.gx
printf "mod m;\nm::f([1.5])\n" > $D/main.gx
echo "== --check (bound omitted in .gxi)"; timeout -s KILL 60 $B --check $D/main.gx; echo "exit=$?"
echo "== run"; timeout -s KILL 30 $B --no-cache $D/main.gx; echo "exit=$?"
printf "val f: fn<'a: Array<'b>, 'b: Int>(x: 'a) -> 'a;\n" > $D/m.gxi
echo "== control: --check (bound declared in .gxi)"; timeout -s KILL 60 $B --check $D/main.gx; echo "exit=$?"
rm -rf $D
