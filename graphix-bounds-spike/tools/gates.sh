#!/bin/sh
# The two gates of the check by bounds: the whole suite with the switch off
# (cargo xtest), then graphix-tests module by module under `all`.
# usage: gates.sh <out dir> <file listing the modules>
OUT=$1; MODS=$2; TOOLS=$(dirname "$0")
cd ~/proj/graphix-lanes/bounds || exit 1
mkdir -p "$OUT"; find "$OUT" -type f -delete
cargo xtest > "$OUT/xtest-off.out" 2>&1; echo rc=$? >> "$OUT/xtest-off.out"
cargo test -p graphix-tests --lib --no-run > "$OUT/testbuild.out" 2>&1
T=$(grep -o '/[^ )]*graphix_tests-[0-9a-f]*' "$OUT/testbuild.out" | tail -1)
mkdir -p "$OUT/mods"
BOUNDS_MODE=all sh "$TOOLS/run_mods.sh" "$T" "$OUT/mods" $(cat "$MODS")
