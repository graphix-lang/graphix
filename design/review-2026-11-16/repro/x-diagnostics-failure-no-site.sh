#!/bin/bash
# Title: three run-time failures are reported without the expression that failed
# Command: bash design/review-2026-11-16/repro/x-diagnostics-failure-no-site.sh
# Expected: each failure names its file, line and column, as `arith error in file F at line: L,
#   column: C ...` and `unhandled error in file F at ...` do (diagnostic_site), under
#   graphix::failure (FAILURE_TARGET).
# Observed:
#   references have no order, including under Any                     (no site)
#   collection init size 20000000 exceeds the 16777216 element limit  (no site)
#   WARN [graphix_compiler::node::bind] read through a reference: index 2 out of range for length 1
#       (no site, at warn, and NOT under the failure target: an embedder routing failures misses it)
G=${GRAPHIX:-/home/eric/tmp/review-2026-11-16/bin/graphix}
d=$(mktemp -d)
cat > $d/a.gx <<'G'
let x = 1;
let y = 2;
let a: Any = &x;
let b: Any = &y;
let s = array::sort([a, b]);
sys::exit(sys::time::after_idle(duration:100.ms, 0));
s
G
cat > $d/b.gx <<'G'
let n = 20000000;
let a = array::init(n, |i| i);
sys::exit(sys::time::after_idle(duration:100.ms, 0));
array::len(a)
G
cat > $d/c.gx <<'G'
let clk = sys::time::timer(duration:20.ms, true);
let n = 0;
n <- clk ~ n + 1;
let a = [1, 2, 3];
let r = &a[2];
a <- select n { 2 => [1], _ => never() };
let v = *r;
sys::exit(filter(n, |x| x > 4) ~ 0);
(n ~ v)
G
timeout -s KILL 30 $G --no-cache $d/a.gx 2>&1 | grep -v '^\[\|^$' | head -2
timeout -s KILL 30 $G --no-cache --no-fusion $d/b.gx 2>&1 | head -2
mkdir -p $d/log; RUST_LOG=warn timeout -s KILL 30 $G --no-cache --log-dir $d/log $d/c.gx >/dev/null 2>&1; grep "read through" $d/log/* | head -1
rm -rf $d
