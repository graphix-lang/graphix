#!/usr/bin/env bash
# Corpus validator. For each .gx file in this directory, runs:
#   1. graphix <file>          — interpreter; captures stdout
#   2. graphix compile <file>  — fusion emitter; captures which
#                                 lambdas fused
#   3. cargo check <pkg>       — validates the emitted Rust compiles
# Uses a shared CARGO_TARGET_DIR so deps are recompiled once, not per
# corpus item.
#
# Usage: ./verify.sh [path/to/graphix]
#
# Run from the workspace root. Default graphix binary is
# ./target/debug/graphix.
set -eu
CORPUS_DIR="$(cd "$(dirname "$0")" && pwd)"
WORKSPACE_ROOT="$(cd "$CORPUS_DIR/../.." && pwd)"
GRAPHIX="${1:-$WORKSPACE_ROOT/target/debug/graphix}"
SHARED_TARGET="${CORPUS_SHARED_TARGET:-/tmp/corpus-shared-target}"
mkdir -p "$SHARED_TARGET"
export CARGO_TARGET_DIR="$SHARED_TARGET"

total=0
passed=0
for f in "$CORPUS_DIR"/*.gx; do
    name=$(basename "$f" .gx)
    total=$((total + 1))
    echo "=== $name ==="
    # 1. Interpret
    interp=$("$GRAPHIX" "$f" 2>&1 | head -20) || {
        echo "FAIL: interpreter errored"; continue
    }
    # 2. Fusion compile
    out_dir=$(mktemp -d -t "corpus-$name-XXXXX")
    rm -rf "$out_dir"
    summary=$("$GRAPHIX" compile --out "$out_dir" "$f" --name "corpus_$name" 2>&1 | grep -E "Fused|Error" | head -5) || {
        echo "FAIL: compile errored"; continue
    }
    echo "  $(echo "$summary" | head -1)"
    # 3. cargo check the emitted package
    if (cd "$out_dir" && cargo check 2>&1 | tail -1 | grep -q "Finished"); then
        passed=$((passed + 1))
        echo "  PASS"
    else
        echo "  FAIL: cargo check rejected the emitted package"
    fi
done
echo ""
echo "$passed / $total passed"
