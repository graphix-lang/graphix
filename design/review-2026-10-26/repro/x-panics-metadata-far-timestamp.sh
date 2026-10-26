#!/bin/bash
# x-panics: sys::fs::metadata panics in chrono on a file whose mtime is past chrono's range
# Command: bash design/review-2026-10-26/repro/x-panics-metadata-far-timestamp.sh
# Expected: the file's metadata, `modified` null or an error naming the timestamp; no panic.
# Observed (HEAD e6ffc6eed debug):
#   thread 'tokio-rt-worker' panicked at chrono-0.4.44/src/datetime/mod.rs:1931:38:
#   No such local time
#   sys_fs_metadata panicked
#   error:`IOError("sys_fs_metadata panicked")
# btrfs and tmpfs store 64-bit seconds, so any user can set such a time (os.utime).
set -e
G=${G:-/home/eric/tmp/review-2026-10-26/bin/graphix}
d=$(mktemp -d)
touch "$d/far.txt"
python3 -I -c "import os,sys; os.utime(sys.argv[1], (99999999999999, 99999999999999))" "$d/far.txt"
cat > "$d/prog.gx" <<GX
let m = sys::fs::metadata("$d/far.txt");
sys::exit(sys::time::after_idle(duration:200.ms, m ~ 0));
m
GX
timeout -s KILL 30 "$G" --no-cache "$d/prog.gx" 2>&1 | tail -5
rm -rf "$d"
