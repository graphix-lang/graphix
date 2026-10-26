#!/bin/bash
# Title: a warm start of nested lambda literals overflows a 512K runtime stack
#   (image::defs::shared_decode recurses once per nested literal, unguarded)
# Command: bash tests-shell-compiler-warm-nested-lambda-overflow.sh [path/to/graphix]
# Expected: both runs exit 0 (the cold run writes the program image, the warm
#   run restores it), as every other deep_nesting shape does at its admitted depth.
# Observed: cold run exits 0; the warm run aborts (exit 134):
#   thread 'tokio-rt-worker' has overflowed its stack
#   gdb: ~600 frames of graphix_compiler::image::defs::shared_decode under def_decode
# RUST_MIN_STACK=524288 gives the tokio workers deep_nesting.rs's STACK (512K).
GX=${1:-graphix}
D=$(mktemp -d)
python3 - > "$D/lam.gx" <<'PY'
print("let f = " + "|x| " * 995 + "1;\nsys::exit(0)")
PY
for run in cold warm; do
  RUST_MIN_STACK=524288 XDG_CACHE_HOME="$D/cache" timeout -s KILL 60 "$GX" --no-netidx "$D/lam.gx" > "$D/$run.out" 2>&1
  echo "$run: exit $? $(grep -m1 overflowed "$D/$run.out")"
done
rm -rf "$D"
