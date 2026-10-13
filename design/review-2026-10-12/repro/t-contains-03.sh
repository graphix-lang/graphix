#!/bin/sh
# t-contains-03: contains_tvars skips an open cell's constraints in a probe
# against a bound cell; set_commit admits the member, its commit binds 'q and
# then fails on 'a: Number, leaving 'q := string behind.
# (t-contains-03.gx in this directory belongs to a different finding.)
# Run: sh design/review-2026-10-12/repro/t-contains-03.sh [path/to/graphix]
# Expected: both checks pass (member 2 admits the tuple; 'q stays free).
# Observed: t-contains-04.gx is refused with
#   type mismatch 'q: string does not contain i64
# while t-contains-04-control.gx (v annotated Array<string>) is accepted.
G=${1:-graphix}
D=$(dirname "$0")
echo "== bound-cell element (expect accepted)"; $G --no-cache --check "$D/t-contains-04.gx"; echo "exit $?"
echo "== annotated element (control)"; $G --no-cache --check "$D/t-contains-04-control.gx"; echo "exit $?"
