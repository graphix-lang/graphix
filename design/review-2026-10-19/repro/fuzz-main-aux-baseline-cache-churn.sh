#!/usr/bin/env bash
# Title: graphix without --no-cache writes a ~1.6 MB program image per script,
# so stdout-baseline.sh (~600 programs, no --no-cache) writes ~1 GB of entries
# into the user's real cache, past the 512 MB KEEP_BYTES the shell's cache
# collector keeps (graphix-shell/src/cache.rs), evicting the user's own entries.
# Command: bash design/review-2026-10-19/repro/fuzz-main-aux-baseline-cache-churn.sh [graphix-bin]
# Expected (for a capture tool): no cache writes.
# Observed: one registration entry plus one ~1.6 MB program entry per script run.
set -u
bin=${1:-$HOME/tmp/review-2026-10-19/bin/graphix}
d=$(mktemp -d); trap 'rm -rf "$d"' EXIT
for i in 1 2 3; do
  printf 'sys::exit(sys::time::after_idle(duration:100.ms, 0));\ni64:%s\n' $i > "$d/p$i.gx"
  XDG_CACHE_HOME=$d/xdg timeout -s KILL 30 "$bin" "$d/p$i.gx" >/dev/null
done
find "$d/xdg" -name '*.img' -printf '%s %f\n'
