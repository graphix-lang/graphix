#!/bin/sh
# x-image-01: the shell's exit request overtakes queued output.
# Shell::run's select! loop ends on exit_rx; value batches still queued in
# from_gx (or sent after the cycle that called sys::exit) are never printed.
#
# Run: sh design/review-2026-10-12/repro/x-image-01.sh [path/to/graphix] [dir]
# The scratch dir (default: the current directory) must be on a disk
# filesystem, as ~/.cache is: the image write fsyncs, and on tmpfs it is fast
# enough that part 1 rarely loses the race (0 of 10 cold runs there).
#
# Part 1 (cold image, earlier cycles lost): with an empty XDG_CACHE_HOME the
# shell writes the images in Shell::init while the runtime already cycles, so
# the exit request and the earlier batches are pending together as the loop
# starts, and the exit branch wins.
#   expected: every run prints 42 / 0 1 2 3 4 5
#   observed (debug build, HEAD 7324207f, cache on btrfs): 42 printed in 3 of
#   18 cold runs; the counter printed nothing or only "0" in 8 of 8 cold runs;
#   warm and --no-cache runs printed 42 (10/10) and 0..5 (3/3) every time.
# Part 2 (no cache, same cycle): sys::exit requests the exit mid-cycle; the
# runtime sends the cycle's batch only after the cycle (GXRt do_cycle), so
# the value produced in the exit's own cycle is usually lost.
#   expected: 42 every run (sys::exit's doc: "flush stdout/stderr and terminate")
#   observed: 42 in 2 of 8 runs; the counter's 6 is never printed.
G=${1:-graphix}
D=$(mktemp -d -p "${2:-$PWD}")
cat > "$D/a.gx" <<'GX'
let x = 41 + 1;
sys::exit(sys::time::after_idle(duration:30.ms, 0));
x
GX
cat > "$D/s5.gx" <<'GX'
let clock = sys::time::timer(duration:5.ms, true);
let n = 0;
n <- clock ~ n + 1;
sys::exit(select n { 6 => 0, _ => never() });
n
GX
cat > "$D/same.gx" <<'GX'
let x = sys::time::after_idle(duration:200.ms, 42);
sys::exit(x ~ 0);
x
GX
for p in a s5; do
    for i in 1 2 3 4 5; do
        rm -rf "$D/cache"; mkdir "$D/cache"
        XDG_CACHE_HOME=$D/cache timeout -s KILL 60 "$G" "$D/$p.gx" > "$D/out" 2>&1
        printf 'cold %s: [%s]\n' $p "$(tr '\n' ' ' < "$D/out")"
        XDG_CACHE_HOME=$D/cache timeout -s KILL 60 "$G" "$D/$p.gx" > "$D/out" 2>&1
        printf 'warm %s: [%s]\n' $p "$(tr '\n' ' ' < "$D/out")"
    done
done
for i in 1 2 3 4 5; do
    timeout -s KILL 60 "$G" --no-cache "$D/same.gx" > "$D/out" 2>&1
    printf 'same-cycle --no-cache: [%s]\n' "$(tr '\n' ' ' < "$D/out")"
done
rm -rf "$D"
