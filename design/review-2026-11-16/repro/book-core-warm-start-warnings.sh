#!/bin/sh
# Title: shell.md "Startup Cache" says a restored program prints no compile warnings; a warm start replays them
# Command: sh design/review-2026-11-16/repro/book-core-warm-start-warnings.sh
# Expected (the book, shell.md: "warnings printed while compiling appear only on a cold start:
#   pass --no-cache to see them"): the second run, restored from the cache, prints no WARNING line.
# Observed: both runs print "WARNING ... error raised by ? will not be caught"; the second run's
#   log has no "program init time" line (the program was restored, not compiled).
#   graphix-rt/src/gx.rs restore_registration calls env.replay_warnings (commit fb65c9ea7).
G=/home/eric/tmp/review-2026-11-16/bin/graphix
D=$(mktemp -d /home/eric/tmp/review-2026-11-16/work/book-core/warmwarn.XXXX)
chmod 700 "$D"; mkdir -p "$D/cache" "$D/logs"; chmod 700 "$D/cache"
printf 'let a = [1, 2];\nlet i = 0;\na[i]?\n' > "$D/p.gx"
export XDG_CACHE_HOME="$D/cache"
echo "== --warm (compiles)"; timeout -s KILL 30 $G --warm "$D/p.gx"
echo "== warm start"; RUST_LOG=info timeout -s KILL 3 $G --log-dir "$D/logs" "$D/p.gx"
echo "== compiled on the warm start? (no line = restored)"; grep -h "program init time" "$D"/logs/* || true
