#!/bin/bash
# sys-io: a write_all aborted by its arm's sleep lands after the write that follows it
# Command: bash sys-io-write-all-lands-after-successor.sh [path/to/graphix]
# The arm's write_all_bin of 30 MB starts, the arm sleeps the next cycle and
# wakes the cycle after, and the woken site writes "B" to the same path.
# Expected: the file holds "B" (len 1), as it does when the arm never sleeps
#   (drop the two `on <-` lines: the Fifo site writes in order, len 1).
# Observed: (null, u64:31457280) every run: the sleep "aborts" the first
#   write, but tokio::fs runs it on the blocking pool, which no abort stops,
#   so the stale 30 MB write finishes after the "B" write and wins.
GX=${1:-/home/eric/tmp/review-2026-11-16/bin/graphix}
DIR=$(mktemp -d)
trap 'rm -rf "$DIR"' EXIT
head -c 31457280 /dev/zero > "$DIR/big.bin"
cat > "$DIR/p.gx" <<GX
let src = "$DIR/big.bin";
let dst = "$DIR/dst.bin";
let big = sys::fs::read_all_bin(src)\$;
let data = big;
data <- big ~ buffer::from_string("B");
let on = true;
on <- big ~ false;
on <- on ~ select on { false => true, true => never() };
let w = select on { b if b => sys::fs::write_all_bin(#path: b ~ dst, data), _ => never() };
let md = sys::fs::metadata(sys::time::timer(duration:1500.ms, false) ~ dst)\$;
sys::exit(md ~ 0);
(w, md.len)
GX
timeout -s KILL 30 "$GX" --no-cache "$DIR/p.gx"
