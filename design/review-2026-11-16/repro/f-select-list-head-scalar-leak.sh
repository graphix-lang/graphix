#!/bin/bash
# f-select: a fused guard prologue leaks the value under a scalar list-head bind.
#
# A guarded arm whose guard is not pure-of-binds installs its binds in the
# prologue, masked, over every value. A list head bound at a scalar kind is read
# with graphix_list_get_value (an owned clone of whatever the head is) and only
# its payload word kept: when the head is a string (here: a `B value's list
# reaching the `A arm's head bind) the clone is never dropped.
#
# Command: bash design/review-2026-11-16/repro/f-select-list-head-scalar-leak.sh
# Expected: peak RSS grows by about the same as the control between the two
#           sizes (control: ~70 MB -> ~82 MB, the source array).
# Observed: ~75 MB at 20000 -> ~163 MB at 400000 (one ~200-byte string leaked
#           per evaluation); the control with i64 heads stays at ~82 MB.
B=/home/eric/tmp/review-2026-11-16/bin
D=$(mktemp -d)
cat > $D/maxrss.py <<'EOF'
import resource, subprocess, sys
r = subprocess.run(sys.argv[1:], capture_output=True, text=True)
print("out", r.stdout.strip().splitlines()[-1:], "rc", r.returncode,
      "MAXRSS_KB", resource.getrusage(resource.RUSAGE_CHILDREN).ru_maxrss)
EOF
for payload in '`B([<"y[i] padding padding padding padding padding padding padding padding padding padding padding padding padding padding padding padding padding padding padding padding padding padding">])' '`A([<i, i>])'; do
  for n in 20000 400000; do
    cat > $D/p.gx <<EOF
let f = |x: [\`A(List<i64>), \`B(List<string>)]| #[native] select x {
  \`A([<h, ..>]) if h / 2 > 0 => h,
  \`A(_) => 1,
  \`B(_) => 2
};
let g = |i: i64| -> i64 {
  let a: [\`A(List<i64>), \`B(List<string>)] = $payload;
  f(a)
};
let r = array::fold(array::init($n, |i| i), 0, |acc, i| acc + g(i));
sys::exit(sys::time::after_idle(duration:100.ms, 0));
r
EOF
    echo -n "$(echo $payload | cut -c1-3) N=$n: "
    timeout -s KILL 170 $B/probe-run 6G /usr/bin/python3 -I $D/maxrss.py $B/real/graphix --no-cache $D/p.gx
  done
done
rm -rf $D
