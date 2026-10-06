#!/usr/bin/env bash
# x-image-06: id counts are trusted: overflow panic, and a failed read
# poisons the id allocator.
#
# IdCounts::decode (graphix-types/src/image/mod.rs:95) takes any floor and
# extent from an image's id-count trailer. IdSpans::len (graphix-types/src/
# ids.rs:55) sums the two span lengths unchecked, and reserve_above (ids.rs:88)
# lifts the process-wide minted counter to the image's minted extent BEFORE
# the fit check (ids.rs:94-98) can refuse the block. CLAUDE.md: "an entry that
# fails to read leaves the session untouched and starts cold".
#
# Each case rewrites only the bind domain's spans in the trailer of a good
# entry (the trailer starts at counts_at, the header's third u64):
#   a: program entry, reserved [0, 2^62) + minted [0, 2^64-1): the sum
#      overflows.
#   b: registration entry, reserved [0, 2^62) (unreservable) + minted extent
#      2^64-5, program entry removed: the read fails, the cold fallback mints
#      BindIds up to u64::MAX and writing the program entry hits
#      IdSpan::count's `raw + 1` (ids.rs:41).
#   c: registration entry, minted extent 3*2^62, program entry removed: the
#      read fails, the fallback mints BindIds >= 3*2^62, to_wire (ids.rs:107)
#      shifts their top bit out, and the program entry it writes never reads.
#   control: registration entry with only the reserved span unreservable:
#      the fallback writes a good program entry and the next run is warm.
#
# command: GRAPHIX=/path/to/debug/graphix bash design/review-2026-10-05/repro/x-image-06.sh
#
# expected: every run prints 0 2 4 .. 20 and exits 0; a failed read costs one
#   cold start and the entry written by it reads warm next time.
#
# observed (HEAD c722befe, debug build):
#   a: run 1: exit 1: panicked at graphix-types/src/ids.rs:55:9: attempt to
#      add with overflow ... channel closed (no program output)
#   b: run 1: exit 1: panicked at graphix-types/src/ids.rs:41:39: attempt to
#      add with overflow ... channel closed (no program output); log:
#      Registration image entry read, compiling cold
#   c: run 1: exit 0, output ok; log: Registration image entry read,
#      compiling cold, image written. runs 2 and 3: exit 0, output ok; log:
#      Program image entry read, compiling cold (every later run is cold;
#      rewriting that entry's bind span to [2^62, 2^62+n) makes it read, so
#      its text holds the ids without their top bit)
#   control: run 1 as c's; run 2: log: Program image entry read (warm)
set -u
GRAPHIX=${GRAPHIX:-graphix}
dir=$(mktemp -d)
trap 'rm -rf "$dir"' EXIT
cd "$dir"
cat > prog.gx <<'GX'
let clk = sys::time::timer(duration:10.ms, 10);
let n = 0;
n <- clk ~ n + 1;
sys::exit(sys::time::after_idle(duration:150.ms, n ~ 0));
n * 2
GX
cat > counts.py <<'PY'
# counts.py IN OUT REGION FLOOR EXTENT [...]: set the bind domain's spans
import sys
def varint(b, i):
    r = s = 0
    while True:
        x = b[i]; i += 1; r |= (x & 0x7f) << s; s += 7
        if x < 0x80: return r, i
def enc(v):
    o = bytearray()
    while True:
        b = v & 0x7f; v >>= 7
        if v: o.append(b | 0x80)
        else: o.append(b); return bytes(o)
d = open(sys.argv[1], "rb").read()
isalen, i = varint(d, 5)
at = int.from_bytes(d[i + isalen + 16:i + isalen + 24], "big")
j, spans = at, []
for _ in range(10):
    f, j = varint(d, j); e, j = varint(d, j); spans.append([f, e])
assert j == len(d)
M, a = 1 << 64, sys.argv[3:]
while a:
    spans[["reserved", "minted"].index(a[0])] = [eval(a[1]), eval(a[2])]; a = a[3:]
open(sys.argv[2], "wb").write(d[:at] + b"".join(enc(f) + enc(e) for f, e in spans))
PY

# good entries: the registration entry alone (--check), then both
XDG_CACHE_HOME=$dir/regonly timeout -s KILL 60 "$GRAPHIX" --check prog.gx
XDG_CACHE_HOME=$dir/good timeout -s KILL 60 "$GRAPHIX" prog.gx > /dev/null
REG=$(cd regonly && ls graphix/registration/*/*.img)
PROG=$(cd good && ls graphix/registration/*/*.img | grep -v "$(basename "$REG")")

run() { # case label
    mkdir -p "$dir/log-$1"
    local out rc
    out=$( (ulimit -c 0; XDG_CACHE_HOME=$dir/$1 RUST_LOG=info timeout -s KILL 60 \
        "$GRAPHIX" --log-dir "$dir/log-$1" prog.gx 2>&1) ); rc=$?
    echo "  $2: exit $rc: $(echo "$out" | grep -v '^note:' | tr '\n' ' ' | cut -c1-200)"
    grep -ho '\(Program\|Registration\) image /\|compiling cold\|image written' \
        "$dir/log-$1"/* | sed 's| /| entry read|; s/^/      log: /'
    rm -rf "$dir/log-$1"
}
case_dir() { # case entry-to-corrupt spans...
    local c=$1 e=$2; shift 2
    mkdir -p "$c/$(dirname "$REG")"
    cp "good/$REG" "$c/$REG"
    [ "$e" = prog ] && cp "good/$PROG" "$c/$PROG"
    local f=$REG; [ "$e" = prog ] && f=$PROG
    python3 counts.py "good/$f" "$c/$f" "$@"
}

echo "=== a: program entry spans overflow on sum"
case_dir a prog reserved 0 '1<<62' minted 0 'M-1'
run a "run 1"
echo "=== b: registration entry minted extent 2^64-5, no program entry"
case_dir b reg reserved 0 '1<<62' minted '1<<62' 'M-5'
run b "run 1"
echo "=== c: registration entry minted extent 3*2^62, no program entry"
case_dir c reg minted '1<<62' '3<<62'
for n in 1 2 3; do run c "run $n"; done
echo "=== control: registration entry reserved span unreservable, no program entry"
case_dir ctl reg reserved 0 '1<<62'
for n in 1 2; do run ctl "run $n"; done
