#!/bin/sh
# Title: the "write 'a: .." fix of a body that needs an undeclared bound is
#        printed as the cells stand, and may name the variable itself: text
#        no program can write, or a bound the check refuses.
#
# Command: sh design/review-2026-11-16/repro/t-tvar-a-undeclared-bound-unwritable.sh
#          (GRAPHIX=/path/to/graphix to pick the binary)
#
# Expected: 1: "... write 'a: Array<'d>"
#           2: no "write 'a: Array<'a>" (the check refuses that bound: "'a's
#              bound names 'a"); the mismatch alone, or the infinite type.
# Observed: 1: "the body needs 'a within Array<'d: unbound within Any>, which
#              the signature does not declare: write 'a: Array<'d: unbound
#              within Any>"
#           2: "the body needs 'a within Array<'a: unbound>, which the
#              signature does not declare: write 'a: Array<'a: unbound>"
G=${GRAPHIX:-/home/eric/tmp/review-2026-11-16/bin/graphix}
D=$(mktemp -d)
trap 'rm -rf "$D"' EXIT
cat > "$D/1.gx" <<'GX'
let f = 'a: Any, 'd: Any |x: 'a, y: 'd| { let h = 'b: Array<'d> |v: 'b| -> 'b v; h(x) };
1
GX
cat > "$D/2.gx" <<'GX'
let f = |x: 'a, u| -> 'a { let h = 'b: Array<'a> |v: 'b| -> 'b v; h(u) };
1
GX
for p in 1 2; do
    echo "== $p"
    timeout -s KILL 60 "$G" --no-cache -n -i --check "$D/$p.gx" 2>&1 | tail -1
done
