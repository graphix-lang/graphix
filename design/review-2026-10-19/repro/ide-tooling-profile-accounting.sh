#!/bin/bash
# ide-tooling: bench/profile.py (and bench/instances.py, which calls its read_profile)
# raise "phase accounting mismatch" on the GRAPHIX_PROFILE log of any program.
#
# command: bash design/review-2026-10-19/repro/ide-tooling-profile-accounting.sh [graphix-binary]
# expected: profile.py prints each root's phase table
# observed (HEAD 9445a1d21): ValueError: phase accounting mismatch ... {'root': 'Compile',
#   'duration_ns': 15831799, ...}; the root's phases' self_ns sum to 20612472 (> its
#   wall duration), and Typecheck1 reports self_ns 3954279 > total_ns 1188915. Same
#   under GRAPHIX_PAR=off GRAPHIX_FUSE_SERIAL=1.
set -uo pipefail
REPO="$(cd "$(dirname "$0")/../../.." && pwd)"
G="${1:-graphix}"
W="${TMPDIR:-/tmp}/ide-tooling-profile.$$"
mkdir -p "$W"
trap 'rm -rf "$W"' EXIT
printf 'let f = |x: i64| x + 1;\nlet b = array::map([1, 2], |x| f(x));\nsys::exit(sys::time::after_idle(duration:100.ms, 0));\nb\n' > "$W/p.gx"
GRAPHIX_PROFILE=1 GRAPHIX_PROFILE_INSTANCES=1 timeout -s KILL 60 "$G" --no-cache --no-netidx "$W/p.gx" > /dev/null 2> "$W/log"
PYTHONDONTWRITEBYTECODE=1 timeout -s KILL 60 python3 "$REPO/bench/profile.py" "$W/log" 2>&1 | tail -1 | cut -c1-200
(cd "$REPO/bench" && PYTHONDONTWRITEBYTECODE=1 timeout -s KILL 60 python3 instances.py "$W/log" 2>&1 | tail -1 | cut -c1-200)
