#!/bin/bash
# A seq of n same-cycle steps takes memory and time quadratic in n to run once
# run: bash x-stack-seq-same-cycle-steps-quadratic.sh 4000
# expected: memory about what the build takes (the same seq that never runs,
#   `seq t { 1; ...; 1 }` with `t: i64 = never()`, peaks at 88 MB at n = 4000)
# observed (debug build, peak RSS): n = 1000: 101 MB, 2000: 187 MB, 4000: 513 MB,
#   20000: OOM-killed at the 6 GB cap after 8 s, while `--check` (0.3 s) and
#   `--expand` (0.5 s) of the 20000-step program are cheap. gdb samples sit in
#   SeqMachine::update_serial -> ExecCtx::deliver(pc) -> RtView::store_fire ->
#   each_reader / Reader::passed_by and in the TagValue clone for late_var.
n=${1:-4000}
f=$(mktemp --suffix=.gx)
python3 -c "
n=$n
open('$f','w').write('let x = seq { ' + '; '.join(['1']*n) + ' };\nsys::exit(sys::time::after_idle(duration:200.ms, x));\nx\n')"
timeout -s KILL 170 /home/eric/tmp/review-2026-11-16/bin/graphix --no-cache "$f"
echo "exit $?"
rm -f "$f"
