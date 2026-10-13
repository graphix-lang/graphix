# Shared by run.sh and par.sh: source it after setting `iters`, `timeout_s`
# and `graphix`; a caller that runs each program under a command (a fork
# mode, a CPU set) sets the `prefix` array.

# The graphix binary: $1, else $GRAPHIX, else the quick-profile build in
# cargo's target directory.
graphix_bin() {
    local here
    here="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
    echo "${1:-${GRAPHIX:-$(cd "$here/.." && cargo metadata --no-deps --format-version 1 2>/dev/null \
        | python3 -c 'import json, sys; print(json.load(sys.stdin)["target_directory"])')/quick/graphix}}"
}

prefix=()

# best <program> [flags...]: the best (min) elapsed_s over $iters runs, or
# "timeout", or "fail" when a run printed no elapsed_s (e.g. a stack
# overflow). --no-netidx: the netidx bench measures a round trip through
# the internal netidx, not the box's install, which may be down and would
# read as a timeout. --no-cache: every run compiles; none restores a
# session image from the user's cache.
best() {
    local prog="$1"; shift
    local best="" t out
    for _ in $(seq 1 "$iters"); do
        out=$(timeout -k 10 "$timeout_s" "${prefix[@]}" "$graphix" --no-netidx --no-cache "$@" "$prog" 2>/dev/null)
        if [[ $? -eq 124 ]]; then echo "timeout"; return; fi
        t=$(sed -n 's/.*elapsed_s=\([0-9.eE+-]*\).*/\1/p' <<<"$out" | head -1)
        [[ -z "$t" ]] && { echo "fail"; return; }
        if [[ -z "$best" ]] || awk -v a="$t" -v b="$best" 'BEGIN{exit !(a<b)}'; then
            best="$t"
        fi
    done
    echo "$best"
}
