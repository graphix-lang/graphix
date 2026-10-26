#!/bin/bash
# Title: bench/instances.py's elaboration tree is always flat: no INSTANCE row
#        ever carries a parent, so "critical path" and its "Nx" are bogus
# Command: GRAPHIX=<graphix binary> bash design/review-2026-10-26/repro/ide-tooling-instances-flat-tree.sh
# Expected: nested instances (a generic call inside another instance's body)
#   carry parent=<id>; the tree lines show chain depth > 1
# Observed: "267 instances, 0 with a parent"; every tree line reads
#   "chain depth 1, widest 0 children" (e.g. "work 11.108 ms, critical path
#   2.655 ms (4.2x), 71 roots, chain depth 1, widest 0 children"), same with
#   GRAPHIX_PAR=off GRAPHIX_FUSE_SERIAL=1.
set -euo pipefail
REPO="$(cd "$(dirname "$0")/../../.." && pwd)"
GX="${GRAPHIX:-graphix}"
d=$(mktemp -d)
trap 'rm -rf "$d"' EXIT
GRAPHIX_PAR=off GRAPHIX_FUSE_SERIAL=1 GRAPHIX_PROFILE=1 GRAPHIX_PROFILE_INSTANCES=1 \
    timeout -s KILL 90 "$GX" --no-cache --expand "$REPO/book/src/examples/tui/table_basic.gx" \
    >/dev/null 2>"$d/log"
echo "$(grep -c '^INSTANCE' "$d/log") instances, $(grep '^INSTANCE' "$d/log" | grep -vc ' parent=0 ' || true) with a parent"
timeout -s KILL 60 python3 "$REPO/bench/instances.py" "$d/log" | grep 'tree:'
