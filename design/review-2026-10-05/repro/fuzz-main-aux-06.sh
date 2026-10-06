#!/usr/bin/env bash
# fuzz-main-aux-06: a child killed by the 8GB address-space cap counts as
# agreement, whichever engine ran away.
#
# command: design/review-2026-10-05/repro/fuzz-main-aux-06.sh [graphix-fuzz binary]
#
# check_isolated_in (graphix-fuzz/src/lib.rs:4423) returns
# PoolResult::Agree { ran: false } for any child whose stderr holds
# "memory allocation of N bytes failed" or "mmap failed to allocate stack".
# The check-one child runs the node-walk and the JIT in one process
# (check_verdict's tokio::join!, lib.rs:1452), so that line names no engine.
#
# The subject uses a JIT bug present at HEAD: a destructuring `let` that
# shadows a builtin lambda. The node-walk calls the lambda `|n| [n]`. The
# kernel calls the shadowed builtin 'array_iota. This script spawns the
# check-one child the way check_isolated_in does: program on stdin, a fresh
# sandbox cwd, GRAPHIX_FUZZ_SANDBOXED=1 (the child sets RLIMIT_AS to 8GB),
# GRAPHIX_STACK_BUDGET=1GB and two tokio and rayon threads. It then applies
# the parent's exit-status rules to the result.
#   TINY: 4 calls; the JIT side allocates a few bytes
#   BIG:  40 calls of 16M elements; the JIT side asks for 40 x 256MB
# Running BIG takes the child to the 8GB cap (about 7GB resident).
#
# expected: BIG is a divergence like TINY (the node-walk's value is
#   [[16000000], [15999999], ..]; the JIT computes something else or dies).
#   At the least, it is not agreement: the design records an asymmetric
#   runaway (design/graphix_fuzz.md, Timeout), and the only exception is
#   on the node-walk side.
#
# observed (debug build at c722befe):
#   TINY: check-one exit 10 -> the parent records the divergence
#         (graphix-fuzz check: interp [[4], [3], [2], [1]],
#          jit [[0, 1, 2, 3], [0, 1, 2], [0, 1], [0]])
#   BIG:  check-one exit 134 (SIGABRT), stderr
#         "memory allocation of 255999592 bytes failed"
#         -> PoolResult::Agree { ran: false }; nothing is recorded or logged
#   BIG under `graphix-fuzz run` with the same cap, which runs the modes in
#   turn: Interp/InLanguage: Trace([0:[[16000000], [15999999], ..]]) is
#   printed, then the Jit run aborts with the same allocation failure.
set -u
fuzz=${1:-graphix-fuzz}
work=$(mktemp -d)
trap 'rm -rf "$work"' EXIT
ulimit -c 0
cat > "$work/tiny.gx" <<'EOF'
{ let h = |n: i64| -> Array<i64> 'array_iota; let (h, k) = (|n: i64| -> Array<i64> [n], 0); let f = |n: i64| h(n); array::map(array::init(4, |i| i), |i| f(4 - i)) }
EOF
cat > "$work/big.gx" <<'EOF'
{ let h = |n: i64| -> Array<i64> 'array_iota; let (h, k) = (|n: i64| -> Array<i64> [n], 0); let f = |n: i64| h(n); array::map(array::init(40, |i| i), |i| f(16000000 - i)) }
EOF
child_env() {
    env -u GRAPHIX_FUZZ_MEM_LIMIT GRAPHIX_FUZZ_SANDBOXED=1 \
        GRAPHIX_STACK_BUDGET=$((1 << 30)) TOKIO_WORKER_THREADS=2 \
        RAYON_NUM_THREADS=2 XDG_CACHE_HOME="$work/cache" "$@"
}
# lib.rs:4396-4433, the parent's reading of the child's exit
classify() {
    local status=$1 stderr=$2
    case $status in
        0) echo "PoolResult::Agree { ran: false }" ;;
        7) echo "PoolResult::Agree { ran: true }" ;;
        10) echo "PoolResult::Diverge (re-checked in process and recorded)" ;;
        143) echo "PoolResult::Agree { ran: false } (SIGTERM)" ;;
        *)
            if grep -Eq '^memory allocation of .*failed$|mmap failed to allocate stack' "$stderr"; then
                echo "PoolResult::Agree { ran: false } (address-space cap)"
            else
                echo "PoolResult::Crash"
            fi ;;
    esac
}
for subj in tiny big; do
    sb="$work/sandbox-$subj"
    mkdir -p "$sb"
    status=0
    (cd "$sb" && child_env timeout -s KILL 170 "$fuzz" check-one \
        < "$work/$subj.gx" > "$sb/stdout" 2> "$sb/stderr") || status=$?
    echo "== $subj: check-one exit $status"
    grep -E 'memory allocation of|mmap failed|panicked' "$sb/stderr" | head -2
    echo "   parent records: $(classify "$status" "$sb/stderr")"
done
echo "== big, one mode at a time under the same cap (graphix-fuzz run)"
mkdir -p "$work/sandbox-run"
(cd "$work/sandbox-run" && child_env timeout -s KILL 170 "$fuzz" run "$work/big.gx" 2>&1) \
    | grep -v 'could not send batch' | cut -c1-160
