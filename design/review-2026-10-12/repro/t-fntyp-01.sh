#!/bin/sh
# t-fntyp-01: graphix fmt drops a quantifier's bound when the variable occurs only inside another bound
# usage: sh t-fntyp-01.sh /path/to/graphix
# expected: fmt keeps `'b: Int` (or refuses), and the formatted file is still refused by --check
# observed: fmt rc=0 rewrites the type to `fn<'a: Array<'b>>(x: 'a) -> 'a`; original --check fails
#   ("Array<'_N: unbound within Int> does not contain Array<f64>"), formatted --check passes, run prints [1.5]
G=${1:-graphix}
D=$(mktemp -d)
printf "type F = fn<'a: Array<'b>, 'b: Int>(x: 'a) -> 'a;\nlet f: F = |x| x;\nf([1.5])\n" > $D/a.gx
echo "--- original --check"; timeout -s KILL 60 $G --check $D/a.gx; echo rc=$?
timeout -s KILL 60 $G fmt $D/a.gx; echo "--- fmt rc=$?"; cat $D/a.gx
echo "--- formatted --check"; timeout -s KILL 60 $G --check $D/a.gx; echo rc=$?
rm -rf $D
