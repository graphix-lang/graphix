#!/bin/sh
# Title: interface matching lets a signature's free variable satisfy an implementation bound by probing it
# Command: TMPDIR=<scratch> sh design/review-2026-10-26/repro/t-fntyp-sig-open-var-probe.sh
# The interface declares `fn(x: ['b, null]) -> string` ('b unbounded: any caller type);
# the implementation requires `'a: [Number, null]`.
# Expected: the module check refuses the implementation ('b is not declared a Number).
# Observed: --check exits 0 (FnType::sig_bounds probes `[Number, null] ⊇ ['b, null]`
#   with the open 'b free to bind, which answers yes); the run then refuses the instance:
#   "an instance at fn(x: [null, string]) -> string of a definition typed
#   fn<'a: [null, Number]>(x: 'a ..) -> string". Check and build disagree.
G=${G:-/home/eric/tmp/review-2026-10-26/bin/graphix}
T=$(mktemp -d)
cat > $T/m.gxi <<'X'
val f: fn(x: ['b, null]) -> string;
X
cat > $T/m.gx <<'X'
let f = 'a: [Number, null] |x: 'a| -> string "[x]";
X
printf 'mod m;\nm::f("hello")\n' > $T/main.gx
echo "== --check"; timeout -s KILL 30 $G --check $T/main.gx; echo "exit $?"
echo "== run"; timeout -s KILL 30 $G --no-cache $T/main.gx 2>&1 | tail -2
rm -rf $T
