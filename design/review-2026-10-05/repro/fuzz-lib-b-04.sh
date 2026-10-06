#!/usr/bin/env bash
# fuzz-lib-b-04: check-one's outer deadline is not derived from the check,
# so a slow subject that is still working is recorded as CRASH "HANG".
#
# check_isolated_in (graphix-fuzz/src/lib.rs:4388) kills a `check-one`
# child at timeout*4 + max(8*timeout, 60 s) + 30 s, 102 s at the campaign's
# 3 s budget, and returns PoolResult::Crash("HANG (outer deadline)"), which
# the campaign records as a crash finding. This script spawns the same
# child the same way (program on stdin, GRAPHIX_FUZZ_SANDBOXED=1, 1 GiB
# stack budget, 2 rayon threads, 2 tokio workers, a scratch cwd) on a
# callable-v1 subject whose node-walk is slow and whose JIT is fast, and
# times it. check_callable then reruns the node-walk on each route at the
# 60 s slow budget, one after the other (settle), before the sessions and
# the forked runs: legitimate work well past 102 s.
#
# command:
#   GRAPHIX_FUZZ=/path/to/graphix-fuzz bash design/review-2026-10-05/repro/fuzz-lib-b-04.sh
#   FIB and EPOCHS size the subject (fib(FIB) recomputed once per dispatch
#   epoch); the JIT must still finish EPOCHS dispatches inside 3 s, the
#   node-walk must not. The defaults suit a dev-profile build; a release
#   build needs FIB=23. MALLOC_ARENA_MAX defaults to 2: glibc's per-thread
#   arenas otherwise keep each finished run's tree resident and walk the
#   child into its own 8 GB RLIMIT_AS abort first (a separate limit; see
#   the last observation).
#
# expected: the deadline's comment says it "must cover the child's whole
#   legitimate worst case", so a child that ends with a verdict (exit 0,
#   7 or 10) ends before 102 s.
# observed (HEAD c722befe, 16 cores):
#   debug build (the defaults): exit 0 (agree) after 148 s; in a second run
#     with runtime-start stamps, after 151 s. Runtime starts
#     (RUST_LOG=graphix_package_core::testing=info): first four runs at
#     0 s; interp/in-language settle rerun 4-64 s, cut by its 60 s budget
#     ("timeout-involved disagreement at 4x — dropped"); jit 64-66 s;
#     interp/dispatch settle rerun 66-123 s; sessions and forked runs
#     123-151 s. At 102 s the child is inside the second rerun.
#   debug build, FIB=21, MALLOC_ARENA_MAX unset, under other probes' load:
#     exit 0 after 141 s; the same on a quieter box: 100 s (both reruns
#     end before their cap).
#   release build (~/tmp/target/release, Oct 4), FIB=23: exit 0 after
#     171 s, both reruns cut at 60 s.
#   release build, FIB=23, MALLOC_ARENA_MAX unset: still in the second
#     rerun at 102 s; its own end is the 8 GB RLIMIT_AS abort ("memory
#     allocation of 1376 bytes failed") at ~125 s, which the parent classes
#     as containment when it comes first.
# The parent kills each of these children at 102 s and the campaign
# prints "CRASH — child HANG (outer deadline)" and records the program.
# The oct03b ryouko crash findings (fuzz/pending-triage/oct03b/README.md)
# are two such records, both triaged "slow, not hung — containment".
set -u
BIN=${GRAPHIX_FUZZ:-$HOME/tmp/target/debug/graphix-fuzz}
FIB=${FIB:-22}
EPOCHS=${EPOCHS:-450}
KILL=${KILL:-600}
export MALLOC_ARENA_MAX=${MALLOC_ARENA_MAX:-2}
scale=${GRAPHIX_FUZZ_TIMEOUT_SCALE:-1}
budget=$((3 * scale))
slow=$((8 * budget > 60 ? 8 * budget : 60))
deadline=$((4 * budget + slow + 30))

dir=$(mktemp -d)
trap 'rm -rf "$dir"' EXIT
hdr="// callable-v1: handler=m0::handler"
for _ in $(seq 1 "$EPOCHS"); do hdr="$hdr; cx0=i64:$FIB"; done
cat > "$dir/subject.gx" <<EOF
$hdr
{ let o = m0::observe; o }
// file-v1: m0.gx
let rec fib = |n: i64| -> i64 select n { 0 => 0, 1 => 1, _ => fib(n - 1) + fib(n - 2) };
let state = 0;
let handler = |x: i64| -> null { state <- x; null };
let observe = fib(state)
EOF

mkdir "$dir/sandbox"
cd "$dir/sandbox" || exit 2
start=$(date +%s.%N)
GRAPHIX_FUZZ_SANDBOXED=1 GRAPHIX_STACK_BUDGET=1073741824 RAYON_NUM_THREADS=2 \
    TOKIO_WORKER_THREADS=2 timeout -s KILL "$KILL" "$BIN" check-one \
    < "$dir/subject.gx" > "$dir/stdout" 2> "$dir/stderr"
rc=$?
end=$(date +%s.%N)
wall=$(echo "$end - $start" | bc)
grep -v "could not send batch" "$dir/stderr" | tail -n 5
echo "check-one: exit=$rc (0/7 agree, 10 diverge, 134 abort) wall=${wall}s"
echo "check_isolated_in's outer deadline at budget ${budget}s: ${deadline}s"
if [ "$(echo "$wall > $deadline" | bc)" = 1 ]; then
    echo "REPRODUCED: the parent kills this child at ${deadline}s and records" \
        "CRASH \"HANG (outer deadline)\""
else
    echo "not reproduced here: raise FIB or EPOCHS (see the header)"
fi
