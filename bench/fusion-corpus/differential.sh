#!/usr/bin/env bash
# Differential test runner. For each .gx in this directory:
#   1. Run the interpreter, capture stdout.
#   2. Build a standalone binary via `graphix compile --build --dev`.
#   3. Run that binary with stdin closed, capture stdout.
#   4. Diff (1) and (3). Same → PASS. Different → FAIL.
#
# Uses a shared CARGO_TARGET_DIR via `GRAPHIX_BUILD_TARGET` so only the
# first program pays full compile time (~4 min). Subsequent programs
# reuse compiled deps and build in ~10-30s each.
#
# Usage (from repo root):
#   bash bench/fusion-corpus/differential.sh            # all programs
#   bash bench/fusion-corpus/differential.sh mand tiny  # just these
set -u
CORPUS_DIR="$(cd "$(dirname "$0")" && pwd)"
WORKSPACE_ROOT="$(cd "$CORPUS_DIR/../.." && pwd)"
GRAPHIX="${GRAPHIX:-$WORKSPACE_ROOT/target/debug/graphix}"
# Shared build target. Persistent across runs of this script (and
# across individual --build invocations) so deps are cached.
export GRAPHIX_BUILD_TARGET="${GRAPHIX_BUILD_TARGET:-/tmp/corpus-build-target}"
mkdir -p "$GRAPHIX_BUILD_TARGET"

# Pick files to test. If args given, use them; else all *.gx.
if [[ $# -gt 0 ]]; then
    files=()
    for a in "$@"; do
        files+=("$CORPUS_DIR/$a.gx")
    done
else
    files=("$CORPUS_DIR"/*.gx)
fi

total=0
passed=0
failed=()
for f in "${files[@]}"; do
    [[ -f "$f" ]] || { echo "skip $f (not found)"; continue; }
    name=$(basename "$f" .gx)
    total=$((total + 1))
    echo "=== $name ==="
    # Variant programs that fuse 0 lambdas would still work but the
    # binary runs pure interpreter code — still a valid differential
    # test, the output must match.
    # 1. Interpreter
    interp_out=$("$GRAPHIX" "$f" 2>&1) || {
        echo "  FAIL: interpreter errored"
        failed+=("$name (interp)")
        continue
    }
    # 2. Fused binary via --dev --build
    out_dir=$(mktemp -d -t "diff-$name-XXXXX")
    rm -rf "$out_dir"
    build_out=$("$GRAPHIX" compile --out "$out_dir" "$f" --name "diff_$name" --build --dev 2>&1) || {
        echo "  FAIL: compile/build errored"
        echo "$build_out" | tail -5 | sed 's/^/    /'
        failed+=("$name (build)")
        continue
    }
    bin="$out_dir/diff_$name"
    [[ -x "$bin" ]] || {
        echo "  FAIL: no binary produced"
        failed+=("$name (nobin)")
        continue
    }
    # 3. Run the binary
    fused_out=$("$bin" < /dev/null 2>&1) || {
        echo "  FAIL: fused binary crashed"
        echo "$fused_out" | tail -3 | sed 's/^/    /'
        failed+=("$name (crash)")
        continue
    }
    # 4. Diff. Normalize common noise first:
    #    - `elapsed=Ns` from `sys::time::now` is expected to differ
    #      (that's the whole point — the fused binary is faster).
    #    - sub-second timing prints as "0s" because of the known
    #      netidx-value integer cast precision issue, but we still
    #      don't want timing to gate the diff.
    # If comparing programs that care about exact elapsed output,
    # strip this filter.
    norm_interp=$(echo "$interp_out" | sed -E 's/elapsed=[0-9]+s/elapsed=__/g')
    norm_fused=$(echo "$fused_out" | sed -E 's/elapsed=[0-9]+s/elapsed=__/g')
    if [[ "$norm_interp" == "$norm_fused" ]]; then
        passed=$((passed + 1))
        echo "  PASS"
    else
        echo "  FAIL: output differs"
        diff <(echo "$interp_out") <(echo "$fused_out") | head -20 | sed 's/^/    /'
        failed+=("$name (differ)")
    fi
done
echo ""
echo "$passed / $total differential tests passed"
if [[ ${#failed[@]} -gt 0 ]]; then
    echo ""
    echo "failures:"
    for f in "${failed[@]}"; do
        echo "  - $f"
    done
    exit 1
fi
