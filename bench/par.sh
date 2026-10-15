#!/usr/bin/env bash
# Run the parallel benches (bench/par_*.gx) serially and under
# GRAPHIX_PAR=auto, pinned: serial and 4 threads on the P-cores (0-3),
# 12 threads on the P- and E-cores (0-11), never the low-power cores.
# Best (min) elapsed_s of each over the iterations. No realtime
# priority: a SCHED_FIFO pool worker spinning beside the runtime's
# thread can starve it.
#
# Usage: bench/par.sh [iterations] [graphix-binary]

set -u
iters=${1:-3}
dir="$(cd "$(dirname "$0")" && pwd)"
source "$dir/lib.sh"
graphix=$(graphix_bin "${2:-}")
timeout_s=300

if [[ ! -x "$graphix" ]]; then
    echo "graphix binary not found/executable: $graphix" >&2
    exit 1
fi

# run <mode> <cpus> <threads> <program> [flags...]
run() {
    local mode="$1" cpus="$2" threads="$3"; shift 3
    prefix=(env GRAPHIX_PAR="$mode" GRAPHIX_EVAL_THREADS="$threads" taskset -c "$cpus")
    best "$@"
}

printf '%-14s %10s %12s %14s\n' "bench" "serial(s)" "auto P x4" "auto P+E x12"
for prog in "$dir"/par_*.gx; do
    name=$(basename "$prog" .gx)
    flags=()
    # node-walk benches: fused, each is a few milliseconds in all
    [[ "$name" == par_wide || "$name" == par_growth || "$name" == par_chain ]] &&
        flags=(--no-fusion)
    s=$(run off 0-3 4 "$prog" "${flags[@]}")
    a4=$(run auto 0-3 4 "$prog" "${flags[@]}")
    a12=$(run auto 0-11 12 "$prog" "${flags[@]}")
    printf '%-14s %10s %12s %14s\n' "$name" "$s" "$a4" "$a12"
done
