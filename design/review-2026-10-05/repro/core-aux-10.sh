#!/usr/bin/env bash
# core-aux-10: queuefn #count does not track the queue depth. A burst
# lags one value per cycle, a sleep or a new target leaves it stale, and
# the default &null is written anyway.
# (stdlib/graphix-package-core/src/queuefn.rs: 108-121 one set_var per
# push, 353-364 #count handling, 439-444 sleep)
#
# command: GRAPHIX=/path/to/graphix bash design/review-2026-10-05/repro/core-aux-10.sh
#
# Program 1 prints one line per cycle c:
#   qb: 6 calls in cycle 0 (1 runs, 5 queue), one pop per cycle from c=1,
#       so the queue is empty from the end of c=5 (call 6 runs at c=6).
#   qc: 3 calls in cycle 0 (2 queue, never popped); its select arm sleeps
#       at c=3 and wakes at c=5; qc(6) is called at c=6.
#   qd: 3 calls in cycle 0 (2 queue, never popped); #count moves from &a
#       to &b at c=3.
# expected (the depth, landing one cycle after it changes, like a `<-`):
#   lag   5 4 3 2 1 0 at c=1..6, then 0
#   slept 2 up to c=3, 0 from c=4 (the sleep emptied the queue, which is
#         why "qc ran 6" runs at once at c=6)
#   b     2 from c=4
# observed (HEAD c722befe, debug build; the same with --no-fusion, and
# graphix-fuzz check says AGREE):
#   lag   1 2 3 4 5 4 3 2 1 0 at c=1..10: it peaks at c=5, when the
#         queue is empty, and reaches 0 four cycles late
#   slept 2 from c=2 to the end, though "qc ran 6" ran at once at c=6
#   b     0 to the end (a keeps the 2)
#
# Program 2 passes no #count. expected: no write ("If null (default),
# depth is not reported", core mod.gxi). observed: two writes into the
# cell of the default &null, which nothing reads:
#   SET_VAR BindId(..) = i64:1
#   SET_VAR BindId(..) = i64:2
set -u
GRAPHIX=${GRAPHIX:-graphix}
dir=$(mktemp -d)
trap 'rm -rf "$dir"' EXIT
export XDG_CACHE_HOME="$dir/cache"

cat > "$dir/count.gx" <<'EOF'
let c = 0;
c <- select c { n if n < 10 => n + 1, _ => never() };
let lag = 0;
let qb = queuefn(#count: &lag, #trigger: select c { n if n >= 1 => n, _ => never() }, |x: i64| -> i64 x);
let bs = array::map([1, 2, 3, 4, 5, 6], |x| qb(x));
let on = true;
on <- select c { 2 => false, 4 => true, _ => never() };
let slept = 0;
let qc = select on {
  true => queuefn(#count: &slept, #trigger: never(), |x: i64| -> i64 { println("qc ran [x]"); x }),
  false => never()
};
let c1 = qc(select c { 0 => 1, 6 => 6, _ => never() });
let c2 = qc(select c { 0 => 2, _ => never() });
let c3 = qc(select c { 0 => 3, _ => never() });
let a = 0;
let b = 0;
let useb = false;
useb <- select c { 2 => true, _ => never() };
let qd = queuefn(#count: select useb { true => &b, false => &a }, #trigger: never(), |x: i64| -> i64 x);
let d1 = qd(select c { 0 => 1, _ => never() });
let d2 = qd(select c { 0 => 2, _ => never() });
let d3 = qd(select c { 0 => 3, _ => never() });
println("c=[c] lag=[lag] slept=[slept] a=[a] b=[b]");
sys::exit(sys::time::after_idle(duration:200.ms, c ~ 0));
null
EOF
cat > "$dir/default.gx" <<'EOF'
let qf = queuefn(#trigger: never(), |x: i64| -> i64 x);
let out = array::map([1, 2, 3], |x| qf(x));
sys::exit(sys::time::after_idle(duration:100.ms, 0));
null
EOF

echo "== program 1"
timeout -s KILL 30 "$GRAPHIX" --no-cache "$dir/count.gx" | grep -v '^null$'
echo "== program 2 (no #count)"
GRAPHIX_DBG_VARS=1 timeout -s KILL 30 "$GRAPHIX" --no-cache "$dir/default.gx" 2>&1 \
    | grep '^SET_VAR'
