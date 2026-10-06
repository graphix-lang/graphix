#!/usr/bin/env bash
# fuzz-lib-a-06: the per-run deadline does not cover the program compile
# or restore at runtime construction. run_subject (graphix-fuzz/src/lib.rs
# 1009) and compile_with_stats (418) await init_session_with_setup, which
# compiles (or restores) the program inside GX::new, with no deadline;
# the `timeout` is armed only afterwards, in drive_inner (524). A compile
# that never ends never becomes Outcome::Timeout, and the in-process
# gates (check, run, regress, fusecheck, minimize, gen-check) hang.
#
# command: GRAPHIX_FUZZ_BIN=/path/to/debug/graphix-fuzz \
#          bash design/review-2026-10-05/repro/fuzz-lib-a-06.sh [N]
# (N lets in one block, default 8000, sized for a DEBUG build; an
# optimized build compiles faster: raise N until the JIT's program init
# time passes 10s)
#
# Every run below has a 10s budget (main.rs timeout()).
# expected: a run whose program init alone outlasts its budget is
#   Outcome::Timeout (or a compile containment of its own) at 10s; in
#   `check` that one-sided timeout is then retried at the slow budget
# observed (HEAD c722befe, debug build, N=8000):
#   run:   every line is Trace([0:i64:16002]), Jit/InLanguage and the
#          Jit sessions included, while every JIT `program init time`
#          is 13.6-20.9s across runs (Jit/InLanguage, nocache, cold;
#          warm restores) against the interp's 0.7-1.0s
#   check: AGREE after 17-26s wall (box load); exactly two program
#          inits, the interp's ~1s and the JIT's 15.6-20.2s: no deadline cut
#          the JIT's construction and no slow retry ran (a retry would
#          be a third init); a construction that never ends hangs
#          `check` the same way, with no output
set -u
FUZZ=${GRAPHIX_FUZZ_BIN:-$HOME/tmp/target/debug/graphix-fuzz}
N=${1:-8000}
dir=$(mktemp -d)
trap 'rm -rf "$dir"' EXIT
export XDG_CACHE_HOME="$dir/cache"
python3 - "$N" > "$dir/chain.gx" <<'EOF'
import sys
n = int(sys.argv[1])
print("{")
print("let x0 = 1;")
for i in range(1, n + 1):
    print(f"let x{i} = x{i-1} + {1 + i % 3};")
print(f"x{n}")
print("}")
EOF
export GRAPHIX_FUZZ_SESSIONS=0 GRAPHIX_FUZZ_PAR=0
start=$(date +%s.%N)
RUST_LOG=graphix_rt=info timeout -s KILL 170 "$FUZZ" run "$dir/chain.gx" \
    2> "$dir/err" | grep -E '^(Interp|Jit)/'
grep -o 'program init time: .*' "$dir/err"
echo "run: $(echo "$(date +%s.%N) - $start" | bc)s wall"
start=$(date +%s.%N)
RUST_LOG=graphix_rt=info timeout -s KILL 170 "$FUZZ" check "$dir/chain.gx" \
    2> "$dir/err"
grep -o 'program init time: .*' "$dir/err"
echo "check: $(echo "$(date +%s.%N) - $start" | bc)s wall (per-run budget 10s)"
