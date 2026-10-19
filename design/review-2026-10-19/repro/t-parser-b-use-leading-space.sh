#!/bin/sh
# Title: a `use` tree (or a dynamic module's `sandbox` kind) preceded by more
# than one whitespace character fails to parse.
# Command: sh design/review-2026-10-19/repro/t-parser-b-use-leading-space.sh
# Expected: all three programs check (exit 0, no output).
# Observed: 1 and 2 fail "Unexpected `a` / Expected whitespace or `{`" at the
# first segment; 3 fails "Unexpected `w` / Expected whitespace or unrestricted".
# Cause: modexp.rs use_tree's and sandbox's `choice` arms each start with
# spaces(); the first arm consumes the whitespace, fails, and commits.
G=${G:-/home/eric/tmp/review-2026-10-19/bin/graphix}
D=$(mktemp -d)
printf 'use  array::map;\nmap([1], |x| x + 1)\n' > $D/1.gx
printf 'use\n    array::map;\nmap([1], |x| x + 1)\n' > $D/2.gx
cat > $D/3.gx <<'GX'
let status = mod foo dynamic {
    sandbox  whitelist [core];
    sig { val y: i64 };
    source r"let y = 1"
};
status
GX
for f in 1 2 3; do echo "== $f"; timeout -s KILL 30 $G --check $D/$f.gx 2>&1 | grep -E 'Unexpected|Expected'; done
rm -rf $D
