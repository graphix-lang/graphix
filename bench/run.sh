#!/usr/bin/env bash
# Run the self-timed benchmark corpus under both execution modes:
#   - JIT       (fusion on, the default)
#   - node-walk (--no-fusion)
#
# Each program self-times the computation with sys::time::now (excluding
# startup/compile) and prints "elapsed_s=<f>". We run each program a few
# times per mode and keep the best (min) time to cut scheduler noise,
# then report the node-walk / JIT ratio.
#
# Usage: bench/run.sh [iterations] [graphix-binary]
#   iterations  number of runs per mode (default 3)
#   graphix     path to the graphix binary (default: target/release/graphix
#               or $GRAPHIX if set)

set -u
iters=${1:-3}
graphix=${2:-${GRAPHIX:-target/release/graphix}}
timeout_s=120
dir="$(cd "$(dirname "$0")" && pwd)"

if [[ ! -x "$graphix" ]]; then
    echo "graphix binary not found/executable: $graphix" >&2
    echo "build it first: cargo build --release -p graphix-shell" >&2
    exit 1
fi

# Best (min) elapsed_s over $iters runs of one (mode, program). Echoes
# either a float, "timeout", or "fail" (no elapsed_s line — e.g. stack
# overflow).
best() {
    local prog="$1"; shift   # remaining args = mode flags
    local best="" t out
    for _ in $(seq 1 "$iters"); do
        out=$(timeout "$timeout_s" "$graphix" "$@" "$prog" 2>/dev/null)
        if [[ $? -eq 124 ]]; then echo "timeout"; return; fi
        t=$(sed -n 's/.*elapsed_s=\([0-9.eE+-]*\).*/\1/p' <<<"$out" | head -1)
        [[ -z "$t" ]] && { echo "fail"; return; }
        if [[ -z "$best" ]] || awk -v a="$t" -v b="$best" 'BEGIN{exit !(a<b)}'; then
            best="$t"
        fi
    done
    echo "$best"
}

printf '%-18s %14s %14s %12s\n' "bench" "jit(s)" "node-walk(s)" "speedup"
printf '%-18s %14s %14s %12s\n' "-----" "------" "------------" "-------"
for prog in "$dir"/*.gx; do
    name=$(basename "$prog" .gx)
    jit=$(best "$prog")
    nw=$(best "$prog" --no-fusion)
    if [[ "$jit" =~ ^[0-9.eE+-]+$ && "$nw" =~ ^[0-9.eE+-]+$ ]]; then
        speedup=$(awk -v n="$nw" -v j="$jit" 'BEGIN{ if (j>0) printf "%.0fx", n/j; else printf "n/a" }')
    else
        speedup="-"
    fi
    printf '%-18s %14s %14s %12s\n' "$name" "$jit" "$nw" "$speedup"
done
