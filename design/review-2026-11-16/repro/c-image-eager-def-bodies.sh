#!/bin/bash
# Title: a warm restore decodes every definition's body eagerly (ImageDefs),
# so restore time grows with the program's total function text even when
# no instance of those functions is reached in the first frame.
# Command: bash c-image-eager-def-bodies.sh
# Expected (design/program_image.md "Startup is proportional to what the
#   first frame reaches"): ImageDefs roughly flat as the bodies of uncalled
#   functions grow.
# Observed (debug build, 60 functions each in an untaken select arm, body of
#   S lets; ns): S=5 ImageDefs ~9.5e6, S=60 ~15e6, S=200 ~34e6, while
#   ImageNodes stays ~4-5e6 (the instance heap is lazy, the defs are not).
G=/home/eric/tmp/review-2026-11-16/bin/graphix
W=$(mktemp -d)
gen() { # n s
python3 - "$1" "$2" <<'PY'
import sys
n, s = int(sys.argv[1]), int(sys.argv[2])
print("let h = |a: i64, b: i64| a + b;")
for i in range(n):
    big = "{ " + "; ".join(f"let v{j} = x * {j} + {i}" for j in range(s)) + "; " + " + ".join(f"v{j}" for j in range(s)) + " }"
    print(f"let f{i} = |x: i64| {{ let y = {big}; h(x, y) }};")
print("let c = 0;")
print("sys::exit(sys::time::after_idle(duration:100.ms, 0));")
arms = ", ".join(f"{i + 1} => f{i}(c)" for i in range(n))
print(f"select c {{ 0 => 0, {arms}, _ => -1 }}")
PY
}
for s in 5 60 200; do
  gen 60 $s > $W/p$s.gx
  export XDG_CACHE_HOME=$W/cache$s
  timeout -s KILL 120 $G --no-fusion $W/p$s.gx > /dev/null 2>&1
  echo "S=$s: $(GRAPHIX_PROFILE=1 timeout -s KILL 60 $G --no-fusion $W/p$s.gx 2>&1 | grep 'phase=Image' | sed -E 's/.*phase=([A-Za-z]+).*self_ns=([0-9]+).*/\1 \2/' | tr '\n' ' ')"
done
rm -rf $W
