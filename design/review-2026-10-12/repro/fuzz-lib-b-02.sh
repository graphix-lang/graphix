#!/bin/sh
# fuzz-lib-b-02: An Excluded tier masks a pin's compile verdict, so
# `// expect: reject` is never enforced on an Excluded pin.
#
# Run: sh design/review-2026-10-12/repro/fuzz-lib-b-02.sh [graphix-fuzz binary]
#
# Expected: an Excluded program both engines reject gets verdict `reject`
# (as the same refusal without sys::time does), so run_regression's
# undeclared_rejection and the outcome manifest can tell it from one that
# compiles.
# Observed: both the refused program (a) and the compiling one (c) print
# EXCLUDED; only the Exact-tier refusal (b) prints AGREE (reject). In
# run_regression a Verdict::Excluded is trusted and never passed to
# undeclared_rejection, and its manifest row reads `excluded` either way,
# so neither direction of the marker is held for an Excluded pin
# (findings/select-guard-prenarrow-bind-aug2026/02_guard_leaks_fn_value.gx
# is the one marked pin recorded `excluded`).
F=${1:-graphix-fuzz}
D=$(mktemp -d)
echo '{ let t = sys::time::now; i64:1 + "a" }' > $D/a.gx
echo '{ i64:1 + "a" }' > $D/b.gx
echo '{ let t = sys::time::now; i64:1 + i64:2 }' > $D/c.gx
for f in a b c; do
    echo "== $f: $(cat $D/$f.gx)"
    timeout -s KILL 180 $F check $D/$f.gx 2>&1 | tail -1
done
rm -rf $D
# observed at 7324207f:
# == a: { let t = sys::time::now; i64:1 + "a" }
# EXCLUDED — the oracle compares no values of this program; its sessions agree in kind
# == b: { i64:1 + "a" }
# AGREE (reject) — every comparison the program owes holds
# == c: { let t = sys::time::now; i64:1 + i64:2 }
# EXCLUDED — the oracle compares no values of this program; its sessions agree in kind
