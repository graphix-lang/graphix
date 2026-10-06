#!/usr/bin/env bash
# fuzz-main-aux-05: Outcome::agrees_with (graphix-fuzz/src/lib.rs:194)
# counts a Timeout beside an eventless Trace as agreement, in both
# directions. A JIT run that never finishes beside a node-walk whose
# result is bottom, or that only prints, is AGREE, and check_verdict
# returns at lib.rs:1490 before the slow retry (lib.rs:1535) can run.
#
# command: design/review-2026-10-05/repro/fuzz-main-aux-05.sh [graphix-fuzz binary]
#
# The script makes only the JIT side hang, without touching the engines:
# GRAPHIX_DBG_INVOKE=1 makes every fused-kernel invocation print two lines
# to stderr, and stderr drains through a pipe at 3000 bytes/s. The JIT run
# (2002 kernel invocations, ~320KB) cannot finish its first cycle within
# check's 10s deadline, while the node-walk prints nothing and finishes at
# once. Sessions and the forked runs are off (GRAPHIX_FUZZ_SESSIONS=0,
# GRAPHIX_FUZZ_PAR=0), so only the engine pair runs. The programs differ
# only at the end:
#   bottom: never(ys)                        node-walk Trace([])
#   print:  println(..); never(ys)           node-walk Trace([]; stdout=[len 2000])
#   value:  array::len(ys)                   node-walk Trace([0:i64:2000])
#
# expected (design/graphix_fuzz.md section 3: "An asymmetric hang is a
# top-tier finding: fusion adding or removing nontermination", the one
# exception being an interp StackBudget beside a JIT value): all three are
# DIVERGENCE with jit: Timeout(Deadline) once the slow retry also times out.
#
# observed (debug build at c722befe):
#   detcheck-one (the JIT run alone) on bottom: exit 4 (Timeout); unstalled: exit 0
#   bottom: AGREE after 10.7s, no retry
#   print:  AGREE after 10.7s, no retry (has_events, lib.rs:204, ignores stdout)
#   value:  DIVERGENCE — fusion/JIT bug (interp != jit) after 91.5s
#             interp: Trace([0:i64:2000])
#             jit: Timeout(Deadline)
#           (the 80s slow retry ran and timed out too)
# The same predicate decides check_par (lib.rs:1578), session_divergence
# (lib.rs:1128) and the batch path's individual re-check, and in regress the
# agreement's verdict is `unsure`, which outcome_mismatches (lib.rs:3259)
# accepts against any recorded row.
set -u
fuzz=${1:-graphix-fuzz}
work=$(mktemp -d)
trap 'rm -rf "$work"' EXIT
body='  let g = |y: i64| -> i64 y * 3 + y / 7;
  let xs = array::init(2000, |i| i);
  let ys = array::map(xs, |x| once(x) + g(x));'
printf '{\n%s\n  never(ys)\n}\n' "$body" > "$work/bottom.gx"
printf '{\n%s\n  println("len [array::len(ys)]");\n  never(ys)\n}\n' "$body" > "$work/print.gx"
printf '{\n%s\n  array::len(ys)\n}\n' "$body" > "$work/value.gx"
cat > "$work/slow.py" <<'EOF'
import os, sys, time
while True:
    b = os.read(0, 1024)
    if not b:
        break
    time.sleep(len(b) / 3000.0)
EOF
export XDG_CACHE_HOME=$work GRAPHIX_DBG_INVOKE=1 GRAPHIX_FUZZ_SESSIONS=0 GRAPHIX_FUZZ_PAR=0
stalled() {
    mkfifo "$work/fifo"
    python3 "$work/slow.py" < "$work/fifo" &
    local reader=$! start=$SECONDS rc
    timeout -s KILL 170 "$@" 2> "$work/fifo"
    rc=$?
    kill $reader 2>/dev/null; wait $reader 2>/dev/null
    rm -f "$work/fifo"
    echo "  [exit $rc after $((SECONDS - start))s]"
}
echo "== detcheck-one (JIT only, exit 4 = Timeout) on bottom, stalled:"
stalled "$fuzz" detcheck-one < "$work/bottom.gx"
echo "== detcheck-one on bottom, not stalled:"
env -u GRAPHIX_DBG_INVOKE timeout -s KILL 60 "$fuzz" detcheck-one < "$work/bottom.gx" 2>/dev/null
echo "  [exit $?]"
for p in bottom print value; do
    echo "== check $p, JIT stalled:"
    stalled "$fuzz" check "$work/$p.gx"
done
