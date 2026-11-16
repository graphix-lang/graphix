#!/bin/bash
# Title: `graphix package list` (and `search`) fail without cargo on PATH,
#        though neither runs cargo: GraphixPM::new looks cargo up eagerly.
# Command: bash design/review-2026-11-16/repro/package-list-needs-cargo.sh [GRAPHIX]
# Expected: the default stdlib package list (no packages.toml yet).
# Observed: "Error: can't find the cargo command / Caused by: cannot find binary path", rc=1.
set -u
REAL=${1:-/home/eric/tmp/review-2026-11-16/bin/real/graphix}
W=$(mktemp -d)
mkdir -p $W/home $W/data $W/emptybin
timeout -s KILL 30 env HOME=$W/home XDG_DATA_HOME=$W/data PATH=$W/emptybin "$REAL" package list </dev/null
echo "rc=$?"
rm -rf $W
