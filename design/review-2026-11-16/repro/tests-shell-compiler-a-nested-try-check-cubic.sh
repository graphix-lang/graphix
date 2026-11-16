#!/bin/bash
# The check of nested `try` blocks grows near-cubically in their depth;
# deep_nesting.rs::nested_trys_check_in_time says quadratic.
#
# The program is deep_nesting.rs's "trywith" shape (`seq { try { 1; ... 1; 1
# } with(e) { 0 } ... }`). `graphix fmt` of the same text stays cheap, so the
# cost is the check's, and --expand shows the lowered machine growing
# linearly (22 lines a level).
#
# Run:  bash design/review-2026-11-16/repro/tests-shell-compiler-a-nested-try-check-cubic.sh
# Expected (quadratic): user time x4 per doubling of the depth.
# Observed (debug binary, an empty program costs 0.12 s):
#   d=248 0.28 s   d=496 0.86 s   d=700 2.03 s   d=993 5.44 s
#   extra over empty: 0.16 / 0.74 / 1.91 / 5.32 s: x4.6 then x7.2 per
#   doubling, an exponent of about 2.8.
GX=${GX:-/home/eric/tmp/review-2026-11-16/bin/graphix}
T=$(mktemp -d)
export XDG_CACHE_HOME=$T/cache
TIMEFORMAT=%U
for d in 248 496 700 993; do
  python3 -c "d=$d; print('seq { ' + 'try { 1; '*d + '1; 1' + ' } with(e) { 0 }'*d + ' }')" > $T/t.gx
  s=$( { time timeout -s KILL 170 "$GX" --check --no-cache $T/t.gx >/dev/null 2>&1; } 2>&1 )
  echo "d=$d check user ${s}s"
done
rm -rf "$T"
