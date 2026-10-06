#!/usr/bin/env bash
# t-misc-01: the stack budget's charge depends on the thread and the
# fork schedule, so serial and parallel runs abort differently.
# graphix-types/src/stack.rs grow/grow_exceeds_budget charge a 32 MB
# segment once the CURRENT thread's stack is down to RED_ZONE. The
# serial cycle runs on a 2 MB tokio worker (graphix-rt gx.rs
# block_in_place), a forked part on a 16 MB eval-pool thread
# (branch.rs eval_pool), and concurrent parts sum on the Control.
#
# command: GRAPHIX=/path/to/graphix GRAPHIX_FUZZ=/path/to/graphix-fuzz \
#            bash design/review-2026-10-05/repro/t-misc-01.sh
#
# expected (the budget is containment, not a program outcome; a forked
# cycle computes what the serial node-walk does): a program the budget
# lets finish serially also finishes forked and the other way round,
# and graphix-fuzz does not report a budget-only difference as a
# parallel evaluation bug.
# observed (HEAD c722befe, debug build, 16 cores):
#   1. one recursion f(1000), budget 16M: off aborts ("runtime did not
#      respond"); force prints 1000 (it runs on the 16 MB pool stack and
#      grows nothing; under 16M the serial run first aborts between
#      depth 500 and 700, the forced one between 5000 and 10000)
#   2. map over 4 slots of f(15000 + i), budget 64M: off prints the
#      array; force aborts; force with GRAPHIX_EVAL_THREADS=1 prints it
#   3. the same map recomputed on a timer, default mode (auto), 64M:
#      dies at cycle 5-7 once the cost model forks the slots, the cycle
#      varying from run to run; off runs all 20 cycles
#   4. graphix-fuzz check of case 2's program under 64M:
#      "DIVERGENCE — parallel evaluation bug (forked node-walk != serial
#      node-walk)", interp: Trace(..), interp/par: Timeout(StackBudget)
#   5. graphix-fuzz check of f(1000) with `~` (keeps the JIT from fusing
#      it) under 16M: DIVERGENCE, interp: Timeout(StackBudget),
#      interp/par: Trace([0:i64:1000])
set -u
GRAPHIX=${GRAPHIX:-graphix}
GRAPHIX_FUZZ=${GRAPHIX_FUZZ:-graphix-fuzz}
dir=$(mktemp -d)
trap 'rm -rf "$dir"' EXIT
export XDG_CACHE_HOME="$dir/cache"
gx() { timeout -s KILL 90 "$GRAPHIX" --no-cache --no-fusion "$1" 2>&1 | tail -n "${2:-1}"; }

cat > "$dir/one.gx" <<'EOF'
let rec f = |n: i64| -> i64 select n { 0 => 0, n => f(n - 1) + 1 };
let r = f(1000);
println("[r]");
sys::exit(sys::time::after_idle(duration:100.ms, 0))
EOF
cat > "$dir/map4.gx" <<'EOF'
let rec f = |n: i64| -> i64 select n { 0 => 0, n => f(n - 1) + 1 };
let r = array::map([0, 1, 2, 3], |i: i64| f(15000 + i));
println("[r]");
sys::exit(sys::time::after_idle(duration:100.ms, 0))
EOF
cat > "$dir/auto.gx" <<'EOF'
let rec f = |n: i64| -> i64 select n { 0 => 0, n => f(n - 1) + 1 };
let t = sys::time::timer(duration:50.ms, true);
let c = 0;
c <- t ~ c + 1;
let r = array::map([0, 1, 2, 3], |i: i64| f(15000 + i + c));
println("[c] [r]");
sys::exit(select c { 20 => 0, _ => never() })
EOF
cat > "$dir/fuzz_map4.gx" <<'EOF'
let rec f = |n: i64| -> i64 select n { 0 => 0, n => f(n - 1) + 1 };
array::map([0, 1, 2, 3], |i: i64| f(15000 + i))
EOF
cat > "$dir/fuzz_one.gx" <<'EOF'
let rec f = |n: i64| -> i64 select n { 0 => 0, n => f(n - 1) + (n ~ 1) };
f(1000)
EOF

echo "1. f(1000), GRAPHIX_STACK_BUDGET=16M"
for p in off force; do
    printf '   %-6s ' "$p"; GRAPHIX_PAR=$p GRAPHIX_STACK_BUDGET=16M gx "$dir/one.gx"
done
echo "2. map over 4 slots of f(15000 + i), GRAPHIX_STACK_BUDGET=64M"
for p in off force; do
    printf '   %-6s ' "$p"; GRAPHIX_PAR=$p GRAPHIX_STACK_BUDGET=64M gx "$dir/map4.gx"
done
printf '   %-6s ' "force, GRAPHIX_EVAL_THREADS=1:"
GRAPHIX_EVAL_THREADS=1 GRAPHIX_PAR=force GRAPHIX_STACK_BUDGET=64M gx "$dir/map4.gx"
echo "3. the map on a timer, GRAPHIX_STACK_BUDGET=64M (last two lines)"
for p in off auto auto; do
    echo "   $p:"; GRAPHIX_PAR=$p GRAPHIX_STACK_BUDGET=64M gx "$dir/auto.gx" 2 | sed 's/^/     /'
done
echo "4. graphix-fuzz check, case 2's map, GRAPHIX_STACK_BUDGET=64M"
GRAPHIX_STACK_BUDGET=64M timeout -s KILL 180 "$GRAPHIX_FUZZ" check "$dir/fuzz_map4.gx" 2>&1 \
    | grep -v 'could not send batch\|stack budget (' | sed 's/^/   /'
echo "5. graphix-fuzz check, f(1000) with ~, GRAPHIX_STACK_BUDGET=16M"
GRAPHIX_STACK_BUDGET=16M timeout -s KILL 180 "$GRAPHIX_FUZZ" check "$dir/fuzz_one.gx" 2>&1 \
    | grep -v 'could not send batch\|stack budget (' | sed 's/^/   /'
