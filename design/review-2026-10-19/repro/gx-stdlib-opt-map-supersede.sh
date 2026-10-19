#!/bin/sh
# opt.gxi says opt::map is "fire-and-forget, no queueing. If v updates again before f
# produces a result, the new input supersedes the pending one" (flat_map, filter,
# is_some_and, is_none_or refer to it). opt::map is now the select spelling in opt.gx,
# so it has f's cadence: with a slow async f, both inputs are answered, in order.
# Run: sh design/review-2026-10-19/repro/gx-stdlib-opt-map-supersede.sh
# Expected (per opt.gxi): only (1, 6) -- the slow 30 MB read superseded by b.txt
# Observed: (1, 30000000) then (2, 6)
set -e
d=$(mktemp -d /home/eric/tmp/review-2026-10-19/review/work/gx-stdlib/optmap.XXXX)
cd "$d"
head -c 30000000 /dev/zero | tr '\0' 'a' > big.txt
echo world > b.txt
cat > p.gx <<'GX'
let v: [string, null] = "big.txt";
v <- once(v) ~ "b.txt";
let r = opt::map(v, |p| str::len(sys::fs::read_all(p)$));
let n = count(r);
sys::exit(sys::time::after_idle(duration:1.s, 0));
(n, r)
GX
timeout -s KILL 30 /home/eric/tmp/review-2026-10-19/bin/graphix --no-cache p.gx
cd / && rm -rf "$d"
