#!/bin/sh
# Title: FinalValues settle rounds keep the first segment's end_cycle, so
#   prints made after the first idle are dropped (last epoch) or filed under
#   the next epoch (graphix-fuzz/src/lib.rs drive_inner, wait_settled!).
# Command: sh design/review-2026-10-26/repro/fuzz-lib-a-settle-prints-dropped.sh
# Expected: every run's trace carries stdout=[0+N:got 40000000\n] (the print
#   happens in epoch 0, beside the value event at offset 4), and in the
#   scheduled variant it stays in epoch 0.
# Observed (HEAD e6ffc6eed, debug binary):
#   Interp/InLanguage: Trace([4:40000000])                          <- no stdout at all
#   Interp/InLanguage: Trace([4:(40000000, 0)]; [0:(40000000, 1)]; stdout=[1+-7:got 40000000\n])
#                                                                   <- epoch 0's print filed as epoch 1, offset -7
set -e
BIN=${BIN:-/home/eric/tmp/review-2026-10-26/bin/graphix-fuzz}
D=$(mktemp -d)
python3 -c "
with open('$D/big.txt','w') as f:
    f.write(('x'*99+'\n')*400000)
"
printf 'let s = sys::fs::read_all("%s/big.txt")$;\nlet n = str::len(s);\nprintln("got [n]");\nn\n' "$D" > "$D/a.gx"
printf '// schedule-v1: cap=64 events=512; in0=i64:1\nlet s = sys::fs::read_all("%s/big.txt")$;\nlet n = str::len(s);\nprintln("got [n]");\n(n, in0)\n' "$D" > "$D/b.gx"
cd "$D"
timeout -s KILL 120 "$BIN" run a.gx 2>&1 | grep -v '^  '
timeout -s KILL 120 "$BIN" run b.gx 2>&1 | grep -v '^  '
rm -rf "$D"
