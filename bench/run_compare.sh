#!/usr/bin/env bash
# Compare KIR-interpreter vs node-graph paths on the same binary.
# Uses /usr/bin/time for sub-second wall-clock resolution since the
# Graphix-side cast<f64>(now()) only gives us integer seconds.
#
# Usage: bench/run_compare.sh [iterations] [bench_file]

set -u
iters=${1:-5}
bench_file="${2:-bench/mandelbrot_bench.gx}"
graphix=target/release/graphix

if [[ ! -x "$graphix" ]]; then
    echo "Build the release binary first: cargo build --release -p graphix-shell" >&2
    exit 1
fi

# Verify both modes produce the same checksum (correctness check).
ck_kir=$(./target/release/graphix "$bench_file" 2>&1 | grep -oE 'checksum=[0-9]+' | head -1)
ck_no=$(GRAPHIX_NO_KIR=1 ./target/release/graphix "$bench_file" 2>&1 | grep -oE 'checksum=[0-9]+' | head -1)
echo "KIR-on  $ck_kir"
echo "KIR-off $ck_no"
if [[ "$ck_kir" != "$ck_no" ]]; then
    echo "ERROR: checksum mismatch — KIR path is wrong!" >&2
    exit 1
fi
echo "checksums match — running timings ($iters iterations each)..."
echo

run() {
    local label="$1"
    shift
    echo "=== $label ==="
    local total=0
    for i in $(seq 1 "$iters"); do
        local s e t
        s=$(date +%s.%N)
        "$@" "$graphix" "$bench_file" >/dev/null 2>&1
        e=$(date +%s.%N)
        t=$(awk -v a="$s" -v b="$e" 'BEGIN { printf "%.3f", b - a }')
        echo "  run $i: ${t}s"
        total=$(awk -v a="$total" -v b="$t" 'BEGIN { print a + b }')
    done
    avg=$(awk -v t="$total" -v n="$iters" 'BEGIN { printf "%.3f", t / n }')
    echo "  average: ${avg}s"
}

run "node graph (GRAPHIX_NO_KIR=1)" env GRAPHIX_NO_KIR=1
run "kir interpreter (default)" env
run "kir JIT (GRAPHIX_JIT=1)" env GRAPHIX_JIT=1
