#!/bin/bash
# Title: shell.md: --check also skips #[parallel] (not listed); --fusion-stats
#   needs no --no-cache (a fusion profile never starts from the program entry)
# Command: bash design/review-2026-10-26/repro/book-core-shell-check-cache-notes.sh
# Expected per book/src/shell.md: (1) --check verifies everything but the
#   listed attributes; (2) a warm start prints no fusion profile
# Observed: (1) `--check` exits 0 on `#[parallel] (1 + 2)`, `--expand` refuses
#   it ("#[parallel] has nothing to run in parallel"); (2) a second, warm
#   `--fusion-stats` run prints the profile again.
G=${G:-/home/eric/tmp/review-2026-10-26/bin/graphix}
D=$(mktemp -d); export XDG_CACHE_HOME=$D/cache
printf 'let x = #[parallel] (1 + 2);\nx\n' > $D/p.gx
timeout -s KILL 20 "$G" --no-cache --check $D/p.gx; echo "check exit $?"
timeout -s KILL 20 "$G" --no-cache --expand $D/p.gx 2>&1 | tail -1
printf 'let f = |x: i64| x * 2 + 1;\nsys::exit(sys::time::after_idle(duration:50.ms, 0));\nf(3)\n' > $D/f.gx
timeout -s KILL 20 "$G" $D/f.gx > /dev/null 2>&1
timeout -s KILL 20 "$G" --fusion-stats $D/f.gx 2>&1 | head -2
timeout -s KILL 20 "$G" --fusion-stats $D/f.gx 2>&1 | head -2
rm -rf $D
