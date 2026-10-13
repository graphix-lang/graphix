#!/usr/bin/env bash
# Run the self-timed benchmark corpus under both execution modes:
#   - JIT       (fusion on, the default)
#   - node-walk (--no-fusion)
#
# Each program self-times the computation with sys::time::now (excluding
# startup/compile) and prints "elapsed_s=<f>". We run each program a few
# times per mode and keep the best (min) time to cut scheduler noise,
# then report the node-walk / JIT ratio. Both run under the default fork
# mode, `auto`; `GRAPHIX_PAR=off` for one thread.
#
# Usage: bench/run.sh [iterations] [graphix-binary] [corpus-dir]
#   iterations  number of runs per mode (default 3)
#   graphix     path to the graphix binary (default: $GRAPHIX, else the
#               quick-profile build in cargo's target directory)
#   corpus-dir  the directory of programs (default: bench/); the par_*
#               programs are par.sh's and are skipped

set -u
here="$(cd "$(dirname "$0")" && pwd)"
iters=${1:-3}
source "$here/lib.sh"
graphix=$(graphix_bin "${2:-}")
dir="$(cd "${3:-$here}" && pwd)"
timeout_s=120

if [[ ! -x "$graphix" ]]; then
    echo "graphix binary not found/executable: $graphix" >&2
    echo "build it first: cargo build --profile quick -p graphix-shell" >&2
    exit 1
fi

printf '%-18s %14s %14s %12s\n' "bench" "jit(s)" "node-walk(s)" "speedup"
printf '%-18s %14s %14s %12s\n' "-----" "------" "------------" "-------"
for prog in "$dir"/*.gx; do
    name=$(basename "$prog" .gx)
    # par_* measure forking (par.sh); their node-walk pass cannot finish
    [[ "$name" == par_* ]] && continue
    jit=$(best "$prog")
    nw=$(best "$prog" --no-fusion)
    if [[ "$jit" =~ ^[0-9.eE+-]+$ && "$nw" =~ ^[0-9.eE+-]+$ ]]; then
        speedup=$(awk -v n="$nw" -v j="$jit" 'BEGIN{ if (j<=0) printf "n/a"; else if (n/j < 10) printf "%.2gx", n/j; else printf "%.0fx", n/j }')
    else
        speedup="-"
    fi
    printf '%-18s %14s %14s %12s\n' "$name" "$jit" "$nw" "$speedup"
done
