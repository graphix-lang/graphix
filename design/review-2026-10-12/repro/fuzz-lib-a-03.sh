#!/bin/sh
# fuzz-lib-a-03: Excluded programs owe no engine comparison at all; markers
# also match string literals and identifiers.
# Run: sh design/review-2026-10-12/repro/fuzz-lib-a-03.sh <path-to-graphix-fuzz>
# Expected: all three programs print DIVERGENCE (interp Trace vs jit CompileErr:
#   an Excluded program should still owe Pair::Engine at Strength::Kind, and a
#   string literal or a binding name is not a nondeterministic source).
# Observed: plain.gx -> DIVERGENCE (exit 1); string.gx and ident.gx ->
#   "EXCLUDED — the oracle compares no values of this program; its sessions
#   agree in kind" (exit 0).
F=${1:-graphix-fuzz}
d=$(mktemp -d)
printf 'let x = 1;\nlet y = #[native] count(x);\ny\n' > "$d/plain.gx"
printf 'let s = "throttle";\nlet x = 1;\nlet y = #[native] count(x);\ny\n' > "$d/string.gx"
printf 'let local_addr = 1;\nlet y = #[native] count(local_addr);\ny\n' > "$d/ident.gx"
for p in plain string ident; do
    echo "== $p.gx"
    timeout -s KILL 180 "$F" check "$d/$p.gx"
    echo "exit $?"
done
rm -rf "$d"
