#!/bin/sh
# Read::read_all's default (io.gxi) re-concatenates the whole accumulator for every
# 64 KiB chunk, so a Graphix-defined stream reads in time quadratic in its length.
# Run: sh design/review-2026-10-19/repro/gx-stdlib-read-all-quadratic.sh
# Expected: roughly linear (64 MB about twice 32 MB, both well under a second)
# Observed (debug build, --no-fusion): 8 MB 0.12 s, 16 MB 0.9 s, 32 MB 1.3-2.0 s, 64 MB 3.8 s
set -e
d=$(mktemp -d /home/eric/tmp/review-2026-10-19/review/work/gx-stdlib/readall.XXXX)
cd "$d"
for mb in 16 32 64; do
head -c ${mb}000000 /dev/zero > big.bin
cat > p.gx <<'GX'
use sys::io::Read;
type Mem = Abstract<&mut bytes>;
impl Read for Mem {
    let read = |s, n| {
        let cell = s.0;
        let b = *cell;
        let take = (s, n) ~ select buffer::len(b) < n { true => buffer::len(b), false => n };
        *cell <- take ~ b[take..]$;
        take ~ b[..take]$
    }
};
let src = sys::fs::read_all_bin("big.bin")$;
let m = Mem(&mut src);
let t0 = sys::time::now(once(src));
let r = Read::read_all(m)$;
let dt = sys::time::diff(sys::time::now(r), t0);
sys::exit(dt ~ 0);
(buffer::len(r), dt)
GX
timeout -s KILL 170 /home/eric/tmp/review-2026-10-19/bin/graphix --no-cache --no-fusion p.gx
done
cd / && rm -rf "$d"
