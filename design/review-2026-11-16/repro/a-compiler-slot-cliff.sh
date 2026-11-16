#!/bin/bash
# Title: a collection callback that does not fuse costs a node graph per element
#        (about 7.5 KB and 80 us of CPU each on the debug binary): 30x the memory
#        and 27x the CPU of the same loop with a fast-call builtin.
# Command: bash design/review-2026-11-16/repro/a-compiler-slot-cliff.sh
# Expected: the two programs differ only in the predicate (re::is_match vs
#           str::ends_with) and select the same 40000 lines; their costs should
#           be of one order.
# Observed (debug binary at e1be2fbf2, 2026-11-16):
#   re  n=400000  5.40s wall  user=33.13s  rss=3026MB  out=40000
#   str n=400000  0.13s wall  user= 1.22s  rss=  97MB  out=40000
#   (n=100000: 827MB vs 74MB; n=20000: 205MB vs 68MB; the plain `|x| x * 2 + 1`
#    map under --no-fusion: 850MB at n=100000 for two HOFs, ~4 KB a slot)
#   parse n=100000  1.89s wall  user=14.41s  rss=1133MB  out=100000
#   (str::parse of 100000 integer strings in a filter_map: ~11 KB an element)
B=/home/eric/tmp/review-2026-11-16/bin
D=$(mktemp -d)
cat > $D/rss.py <<'PY'
import sys,subprocess,resource,time
t=time.time()
p=subprocess.run(sys.argv[1:],capture_output=True,text=True)
dt=time.time()-t
r=resource.getrusage(resource.RUSAGE_CHILDREN)
out=(p.stdout.strip().split('\n') or [''])[-1][:80]
print(f"{dt:.2f}s user={r.ru_utime:.2f}s rss={r.ru_maxrss//1024}MB rc={p.returncode} out={out}")
PY
n=${1:-400000}
cat > $D/re.gx <<GX
let a = array::init($n, |i| "line [i]");
let b = array::filter(a, |l| re::is_match(#pat: r"7\$", l)\$);
let r = array::len(b);
sys::exit(r ~ 0);
r
GX
cat > $D/str.gx <<GX
let a = array::init($n, |i| "line [i]");
let b = array::filter(a, |l| str::ends_with(#sfx: "7", l));
let r = array::len(b);
sys::exit(r ~ 0);
r
GX
cat > $D/parse.gx <<GX
let a = array::init($n, |i| "[i]");
let b: Array<i64> = array::filter_map(a, |s| select str::parse(s) { error as _ => null, i64 as n => \`Keep(n) });
let r = array::len(b);
sys::exit(r ~ 0);
r
GX
for p in re str parse; do
  echo -n "$p n=$n "
  timeout -s KILL 170 $B/probe-run 6G /usr/bin/python3 $D/rss.py $B/real/graphix --no-cache $D/$p.gx | tail -1
done
rm -rf $D
