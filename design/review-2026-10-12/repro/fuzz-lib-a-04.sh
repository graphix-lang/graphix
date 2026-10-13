#!/bin/sh
# fuzz-lib-a-04: an Excluded pin that stopped compiling passes regress
# (Oracle::verdict returns Excluded before it looks at the engine runs, and
# run_regression runs undeclared_rejection only for Verdict::Rejected).
#
# Command: sh design/review-2026-10-12/repro/fuzz-lib-a-04.sh <bin dir>
#
# Two programs with the same compile error; only the first carries an
# oracle_tier exclusion marker (sys::time).
#   expected: both report a rejection verdict (a compile verdict owes nothing
#             to values, design/graphix_fuzz.md), so regress would run
#             undeclared_rejection on both
#   observed: excluded.gx -> "EXCLUDED — the oracle compares no values of this
#             program; its sessions agree in kind" (exit 0)
#             plain.gx    -> "AGREE (reject) — every comparison the program owes holds"
#   graphix --check excluded.gx -> "missing required argument: expected 1
#             positional, received 0" (exit 1)
B=${1:-/tmp/claude-1000/-home-eric-proj-graphix/4ed578a5-7145-438d-b21b-e50e92617f4d/scratchpad/bin}
D=$(mktemp -d)
printf 'let t = sys::time::now();\nt + 1\n' > "$D/excluded.gx"
printf 'let t = str::len();\nt + 1\n' > "$D/plain.gx"
timeout -s KILL 30 "$B/graphix" --no-cache --check "$D/excluded.gx"
timeout -s KILL 180 "$B/graphix-fuzz" check "$D/excluded.gx"
timeout -s KILL 180 "$B/graphix-fuzz" check "$D/plain.gx"
rm -rf "$D"
