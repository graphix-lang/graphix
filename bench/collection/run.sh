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

# CR claude for claude: [structure] This script is bench/run.sh minus the fork-mode note
# that only bench/run.sh got (its usage line still says bench/run.sh), so the next
# change to timing or parsing will land in one copy only. Let bench/run.sh take the
# corpus directory, and delete this copy. Both copies and bench/par.sh default to
# target/release/graphix, and the copies advise `cargo build --release`. This workspace
# builds into ~/tmp/target, so that default never exists here. Default to cargo's target
# directory under the quick profile instead. (ide-tooling.r2-15)
set -u
iters=${1:-3}
graphix=${2:-${GRAPHIX:-target/release/graphix}}
timeout_s=120
# CR claude for claude: [structure] This script is bench/run.sh minus the fork-mode note
# in its header: every other line is the same, including the usage line that names
# bench/run.sh. Only this line ties it to its corpus, so each change to the runs, flags
# or result parsing (as --no-netidx was) must be made in both, and the header has
# already drifted. Let bench/run.sh take the corpus directory as an argument and delete
# this copy, or reduce it to an exec of ../run.sh with its own directory.
# (ide-tooling-14)
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
        # --no-netidx: the netidx bench must measure a round trip, not
        # the box's netidx INSTALL. The shell otherwise seeds the local
        # client config, so `netidx_stream` published into whatever
        # resolver that names — and when it is down (or its TLS is not
        # set up for whoever is running the bench) publish and subscribe
        # simply never complete. That reads as `timeout` in the results
        # table, which looks like a performance number and is not one;
        # it also burns the full budget twice per run. The internal
        # netidx is a real one, so the round trip is still real.
        out=$(timeout "$timeout_s" "$graphix" --no-netidx "$@" "$prog" 2>/dev/null)
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
