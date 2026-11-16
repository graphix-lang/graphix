#!/bin/bash
# Title: Ctrl-C (SIGINT) during a script's compile is held until the compile
#        ends; SIGTERM at the same point ends the shell at once
# Command: bash design/review-2026-11-16/repro/shell-sigint-waits-for-compile.sh
# Expected: both signals end the shell within a fraction of a second, as
#   Shell::run's select over `self.start` and `terminated.recv()` does for
#   SIGTERM ("heard from before the program compiles, which can take a while").
# Observed (debug build at e1be2fbf2, a ~3 s cold compile):
#   SIGINT: exit 130 after ~2.3 s (the rest of the compile)
#   SIGTERM: exit 143 after ~0.05 s
# Shell::start installs KeySignals before `init`, so SIGINT no longer kills
# the process, but nothing hears the key until the key task is spawned after
# the runtime has compiled the program.
G=${G:-/home/eric/tmp/review-2026-11-16/bin/graphix}
D=$(mktemp -d)
python3 - "$D/big.gx" <<'EOF'
import sys
lines = [f"let f{i} = |x: i64| -> i64 select x {{ 0 => {i}, n if n > 10 => n * {i} + 1, n => n - {i % 7} }};"
         for i in range(30000)]
lines.append("sys::exit(sys::time::after_idle(duration:100.ms, 0));")
lines.append("f1(3) + f29999(20)")
open(sys.argv[1], "w").write("\n".join(lines) + "\n")
EOF
for sig in INT TERM; do
  timeout -s KILL 120 "$G" --no-cache "$D/big.gx" >/dev/null 2>&1 &
  W=$!
  sleep 0.6
  P=$(pgrep -n -f "graphi[x] --no-cache $D/big.gx" | tail -1)
  # the wrapper's child is the real binary: signal the newest match
  t0=$(date +%s.%N)
  kill -$sig "$P"
  wait $W; rc=$?
  t1=$(date +%s.%N)
  echo "SIG$sig: exit $rc after $(python3 -c "print(round($t1-$t0,2))") s"
done
rm -rf "$D"
