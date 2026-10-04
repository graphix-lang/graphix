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
graphix=${2:-${GRAPHIX:-target/release/graphix}}
timeout_s=300
dir="$(cd "$(dirname "$0")" && pwd)"

if [[ ! -x "$graphix" ]]; then
    echo "graphix binary not found/executable: $graphix" >&2
    exit 1
fi

# best <mode> <cpus> <threads> <program> [flags...]
best() {
    local mode="$1" cpus="$2" threads="$3" prog="$4"; shift 4
    local best="" t out
    for _ in $(seq 1 "$iters"); do
        out=$(GRAPHIX_PAR="$mode" GRAPHIX_EVAL_THREADS="$threads" \
            timeout "$timeout_s" taskset -c "$cpus" \
            "$graphix" --no-netidx --no-cache "$@" "$prog" 2>/dev/null)
        if [[ $? -eq 124 ]]; then echo "timeout"; return; fi
        t=$(sed -n 's/.*elapsed_s=\([0-9.eE+-]*\).*/\1/p' <<<"$out" | head -1)
        [[ -z "$t" ]] && { echo "fail"; return; }
        if [[ -z "$best" ]] || awk -v a="$t" -v b="$best" 'BEGIN{exit !(a<b)}'; then
            best="$t"
        fi
    done
    echo "$best"
}

printf '%-14s %10s %12s %14s\n' "bench" "serial(s)" "auto P x4" "auto P+E x12"
for prog in "$dir"/par_*.gx; do
    name=$(basename "$prog" .gx)
    flags=()
    # a fused map is one native loop, with nothing to fork
    [[ "$name" == par_wide ]] && flags=(--no-fusion)
    s=$(best off 0-3 4 "$prog" "${flags[@]}")
    a4=$(best auto 0-3 4 "$prog" "${flags[@]}")
    a12=$(best auto 0-11 12 "$prog" "${flags[@]}")
    printf '%-14s %10s %12s %14s\n' "$name" "$s" "$a4" "$a12"
done
