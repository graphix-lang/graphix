#!/bin/bash
# Title: typemorph swallows a harness failure as "no flips"
# Command: bash design/review-2026-10-26/repro/fuzz-lib-b-typemorph-harness-error-passes.sh
# Expected: a subject typemorph_subject cannot run (here a malformed schedule
#           header; in a soak, a runtime that failed to start) is reported as a
#           harness failure and the command fails.
# Observed: "typemorph: no flips", exit 0. The typemorph-one child writes
#           "HARNESS <error>", which tm_flips ignores; typemorph_work_order maps
#           the same Err to an agreeing "V i A" line.
set -u
W=$(mktemp -d)
printf '// schedule-v1: cap=x events=64; in0=i64:1\n{ let a = in0; a + 1 }\n' > "$W/badsched.gx"
F=${GRAPHIX_FUZZ:-/home/eric/tmp/review-2026-10-26/bin/graphix-fuzz}
timeout -s KILL 170 "$F" typemorph "$W/badsched.gx"
echo "exit=$?"
rm -rf "$W"
