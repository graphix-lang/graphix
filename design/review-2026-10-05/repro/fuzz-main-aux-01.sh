#!/usr/bin/env bash
# fuzz-main-aux-01: the soak loses every crashing subject. When a gen-batch
# child dies, the program that killed it and the earlier suspects in its
# batch are never recorded.
#
# command: design/review-2026-10-05/repro/fuzz-main-aux-01.sh [graphix-fuzz binary]
#          RUN_OK=1 design/review-2026-10-05/repro/fuzz-main-aux-01.sh [binary]
#
# This script sends one work order to a `graphix-fuzz gen-batch` child the
# way run_order_child (graphix-fuzz/src/lib.rs:2249) does. The order goes on
# stdin, with GRAPHIX_FUZZ_SANDBOXED=1, the cwd set to a fresh sandbox and
# the stdout and stderr discarded. The child's order-out file is the only
# thing the parent ever reads. The order is `fuzz 9 24` with a two-program
# ring:
#   DIVERGER: a JIT divergence at HEAD (graphix-fuzz check: interp 42, jit 3)
#   CRASHER:  a JIT link panic at HEAD (fusion/emit/jit.rs:1187, unconditional)
# RUN_OK=1 runs the same order with CRASHER's f64 changed to i64. That
# version does not crash, and the subjects before it are generated the same.
#
# expected: the parent learns about every suspect in the batch and about
#   the subject that killed the child. batch_isolated does this for
#   run_pool_multi by re-running a dead batch's tail through check_isolated,
#   which records the crash.
#
# observed (debug build at c722befe):
#   child exit status: 134 (SIGABRT: subject 19 hit the JIT link panic)
#   order-out holds `V 0 A` .. `V 18 R`, with `V 6 O` and `V 14 O`, and
#   nothing else: no P line, no CPU line, and nothing about subject 19.
#   With RUN_OK=1: exit 0, the same V lines for subjects 0..18, then after
#   `V 23` come `P 6 181` and `P 14 182`. Both are DIVERGER with a mutated
#   schedule, and each is a DIVERGENCE under `graphix-fuzz check`
#   (interp 42, jit 3).
#   run_order_child reads the crashed file as ran=19, suspect=[],
#   clean=false and cpu=0. run_aggregator reads `clean` only at lib.rs:4803
#   (breakage.note). So the soak records nothing, and one crash and two
#   divergences are lost. Subjects 20..23 never run, and inflight keeps
#   24 - 19 = 5 units (lib.rs:4721).
set -euo pipefail
fuzz=${1:-graphix-fuzz}
work=$(mktemp -d)
trap 'rm -rf "$work"' EXIT
cat > "$work/diverger" <<'EOF'
// schedule-v1: cap=64 events=512; in0=i64:3; in0=i64:4
{ let slen = |s: string| -> i64 'str_len; let (slen, k) = (|s: string| -> i64 42, 0); let f = |s: string| slen(s); f("abc") }
EOF
if [[ ${RUN_OK:-0} == 1 ]]; then
    cat > "$work/crasher" <<'EOF'
// schedule-v1: cap=64 events=512; in0=i64:3; in0=i64:4
{ let rec last = |best: [i64, null], xs: Array<i64>| -> [i64, null] select xs { [] => best, [h, t..] => last(h, t) }; last(null, [1, 2, 3]) }
EOF
else
    cat > "$work/crasher" <<'EOF'
// schedule-v1: cap=64 events=512; in0=i64:3; in0=i64:4
{ let rec last = |best: [f64, null], xs: Array<f64>| -> [f64, null] select xs { [] => best, [h, t..] => last(h, t) }; last(null, [1.0, 2.0, 3.0]) }
EOF
fi
# WorkOrder::encode: "kind seed count nring pins.start pins.end", then
# each ring program as "<len>\n<bytes>"
{
    printf 'fuzz 9 24 2 0 0\n'
    for f in diverger crasher; do
        printf '%d\n' "$(wc -c < "$work/$f")"
        cat "$work/$f"
    done
} > "$work/order"
mkdir "$work/sandbox"
status=0
(
    cd "$work/sandbox"
    GRAPHIX_FUZZ_SANDBOXED=1 GRAPHIX_STACK_BUDGET=$((1 << 30)) \
        TOKIO_WORKER_THREADS=2 RAYON_NUM_THREADS=2 \
        "$fuzz" gen-batch "$work/sandbox/order-out" < "$work/order" > /dev/null 2>&1
) || status=$?
echo "child exit status: $status"
echo "--- order-out: everything the parent learns from this order ---"
cat "$work/sandbox/order-out" 2>/dev/null || true
echo "--- end ---"
