#!/usr/bin/env bash
# x-image-03: image entries carry no integrity check: a corrupt entry runs
# silently wrong, crashes in restored JIT code, or breaks every later run.
#
# decode_registration (graphix-compiler/src/image/registration.rs:313) checks
# the magic, the format byte, the ISA, the header offsets and the id counts;
# every other byte is trusted, kernel machine code included (installed as
# stored by define_function_bytes, graphix-compiler/src/fusion/emit/jit.rs:620).
# The shell maps the entry (graphix-shell/src/cache.rs:165), writes it with no
# fsync before the rename (cache.rs:198-199), and hands the runtime no Save
# channel for an entry that exists (graphix-shell/src/lib.rs:287-310), so an
# entry that fails the read is never rewritten.
#
# command: GRAPHIX=/path/to/graphix bash design/review-2026-10-05/repro/x-image-03.sh
#
# expected: a corrupted entry fails the read and the run is cold (the output
#   --no-cache gives), and an entry that failed the read is written again.
#
# observed (HEAD c722befe, debug build):
#   literal: cold "TOTAL-DUE 2469134"; bit 5 of the first byte of the 4th copy
#            of TOTAL-DUE in the program entry flipped: the warm run prints
#            "tOTAL-DUE 2469134", exit 0, nothing on stderr (and so does every
#            later run: the entry is kept)
#   kernel:  one bit of a fused kernel's machine code flipped (first prologue
#            + 1341, bit 4): fact(n) prints 1 for every n, exit 0
#   ud2:     the first kernel's first two bytes set to ud2: exit 132 (SIGILL)
#   builtin: bit 2 of the first copy of "sys_time_after_idle" in the
#            registration entry flipped: every program calling
#            sys::time::after_idle fails "unknown builtin function
#            wys_time_after_idle", exit 1, on every run; the entry stays as it
#            is; --no-cache runs fine
#   sticky:  program entry truncated to 10 bytes: every run warns "compiling
#            cold" and the entry is still 10 bytes after
set -u
GRAPHIX=${GRAPHIX:-graphix}
ulimit -c 0
dir=$(mktemp -d)
trap 'rm -rf "$dir"' EXIT
cd "$dir"

# entry MARKER / other MARKER: the .img under $XDG_CACHE_HOME holding MARKER / not
entry() { grep -l -a -- "$1" "$XDG_CACHE_HOME"/graphix/registration/*/*.img | head -1; }
other() { grep -L -a -- "$1" "$XDG_CACHE_HOME"/graphix/registration/*/*.img | head -1; }
run() {
    out=$(timeout -s KILL 60 "$GRAPHIX" "$@" 2> err.txt); rc=$?
    echo "exit $rc: $(echo "$out" | tr '\n' ' ') $(grep -o 'unknown builtin.*\|Illegal.*\|Segmentation.*\|panicked at.*' err.txt | head -1)"
}
# flip FILE OFFSET MASK
flip() { python3 -c "
import sys
p, off, m = sys.argv[1], int(sys.argv[2]), int(sys.argv[3])
d = bytearray(open(p, 'rb').read()); d[off] ^= m; open(p, 'wb').write(bytes(d))" "$@"; }
# offsets FILE PATTERN: every offset of PATTERN (a python bytes literal)
offsets() { python3 -c "
import sys
d = open(sys.argv[1], 'rb').read(); pat = eval(sys.argv[2]); j = 0
while (i := d.find(pat, j)) >= 0: print(i); j = i + 1" "$1" "$2"; }

echo "=== literal"
export XDG_CACHE_HOME="$dir/cache-literal"
cat > p.gx <<'GX'
let price = 1234567;
let label = "TOTAL-DUE";
sys::exit(sys::time::after_idle(duration:50.ms, price ~ 0));
"[label] [price * 2]"
GX
echo "cold:       $(run p.gx)"
img=$(entry TOTAL-DUE); cp "$img" good.img
for off in $(offsets good.img 'b"TOTAL-DUE"'); do
    cp good.img "$img"; flip "$img" "$off" 32
    echo "flip @$off: $(run p.gx)"
done
cp good.img "$img"

echo "=== kernel"
export XDG_CACHE_HOME="$dir/cache-kernel"
cat > f.gx <<'GX'
let clk = sys::time::timer(duration:10.ms, 6);
let n = 0;
n <- clk ~ n + 1;
let poly = |x: i64| -> i64 x * x * 3 + x * 2 - 7;
let rec fact = |k: i64| -> i64 select k { 0 => 1, k => k * fact(k - 1) };
let sq = |xs: Array<i64>| -> Array<i64> array::map(xs, |x| poly(x) % 1000);
let total = |xs: Array<i64>| -> i64 array::fold(xs, 0, |a, b| a + b);
sys::exit(sys::time::after_idle(duration:150.ms, n ~ 0));
"[poly(n)] [fact(n)] [total(sq(array::init(n + 3, |i| i)))]"
GX
echo "cold:       $(run f.gx)"
img=$(entry "$dir/f.gx"); cp "$img" good.img
reg=$(other "$dir/f.gx")
first=$(offsets good.img 'bytes([0x55, 0x48, 0x89, 0xe5])' | head -1)
flip "$img" $((first + 1341)) 16
echo "bit 4 @$((first + 1341)): $(run f.gx)"
cp good.img "$img"
python3 -c "
import sys
p, off = sys.argv[1], int(sys.argv[2])
d = bytearray(open(p, 'rb').read()); d[off:off + 2] = b'\x0f\x0b'; open(p, 'wb').write(bytes(d))" "$img" "$first"
echo "ud2 @$first: $(run f.gx)"
cp good.img "$img"

echo "=== builtin"
rm "$img"
cp "$reg" reg_good.img
for off in $(offsets reg_good.img 'b"sys_time_after_idle"'); do
    cp reg_good.img "$reg"; flip "$reg" "$off" 4
    r=$(run f.gx)
    case "$r" in *"unknown builtin"*) echo "flip @$off: $r"; break ;; esac
    rm -f $(ls "$XDG_CACHE_HOME"/graphix/registration/*/*.img | grep -v "$reg")
done
cp "$reg" reg_bad.img
echo "run 2 f.gx: $(run f.gx)"
echo "run 3 p.gx: $(run p.gx)"
cmp -s "$reg" reg_bad.img && echo "registration entry: unchanged, still corrupt"
echo "--no-cache p.gx: $(run --no-cache p.gx)"

echo "=== sticky"
export XDG_CACHE_HOME="$dir/cache-sticky"
echo "cold:       $(run p.gx)"
img=$(entry TOTAL-DUE)
truncate -s 10 "$img"
mkdir -p log
for i in 1 2; do
    out=$(RUST_LOG=warn timeout -s KILL 60 "$GRAPHIX" --log-dir "$dir/log" p.gx 2>&1)
    echo "run $i: $out; entry now $(stat -c %s "$img") bytes"
done
grep -h 'compiling cold' log/* | sort -u | cut -c1-200
