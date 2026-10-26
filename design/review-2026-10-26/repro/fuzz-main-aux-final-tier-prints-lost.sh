#!/usr/bin/env bash
# Title: FinalValues tier: a print made after the first idle wait is dropped (last epoch)
#        or filed under the next epoch at a negative offset.
# Command: bash design/review-2026-10-26/repro/fuzz-main-aux-final-tier-prints-lost.sh
# Expected: stdout=[0+N:len 40000000\n] in every mode, for both programs.
# Observed (HEAD e6ffc6eed):
#   one epoch:  Interp/InLanguage: Trace([3:40000000])                    -- no stdout at all
#   two epochs: Interp/InLanguage: Trace([4:40000000]; [0:40000001]; stdout=[1+-7:len 40000000\n])
#   Control (a 6-byte file, read before the first idle): stdout=[0+3:len 6\n].
# Cause: graphix-fuzz/src/lib.rs drive_inner's wait_settled! merges the
# FinalValues extension rounds' events into `seg` but keeps the first round's
# `seg.end_cycle`, which `sink.take_through` uses to cut each epoch's prints.
set -u
BIN=${GRAPHIX_FUZZ:-/home/eric/tmp/review-2026-10-26/bin/graphix-fuzz}
d=$(mktemp -d); trap 'rm -rf "$d"' EXIT
head -c 40000000 /dev/zero | tr '\0' 'a' > "$d/big.txt"
printf 'let s = sys::fs::read_all("%s")$;\nprintln("len [str::len(s)]");\nstr::len(s)\n' "$d/big.txt" > "$d/one.gx"
printf '// schedule-v1: cap=64 events=512; in0=i64:1\nlet s = sys::fs::read_all("%s")$;\nprintln("len [str::len(s)]");\nstr::len(s) + in0\n' "$d/big.txt" > "$d/two.gx"
for f in one two; do
    echo "== $f"
    timeout -s KILL 160 "$BIN" run "$d/$f.gx" 2>&1 | grep -E '^(Interp|Jit)/InLanguage:'
done
