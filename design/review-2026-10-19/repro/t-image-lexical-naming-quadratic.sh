#!/bin/sh
# Title: a warm start's definition restore is quadratic in typedefs x lambdas
#   (image::lexical_decode names every abstract typedef of every snapshot).
# Command: sh t-image-lexical-naming-quadratic.sh /path/to/graphix
# Expected: ImageDefs time about the same whether the N typedefs come before
#   or after the N lambdas (each typedef map is a shared object decoded once).
# Observed (debug build, 2026-10-19): N=1500: 60 ms first vs 42 ms last;
#   N=3000: 170 ms first vs 68 ms last. The gap grows ~5.6x for 2x N.
G=${1:-graphix}
D=$(mktemp -d)
cd "$D" || exit 1
for N in 1500 3000; do
  python3 - "$N" <<'PY'
import sys
N = int(sys.argv[1])
tds = [f"type T{i} = Abstract<i64>;" if i % 2 else f"type T{i} = [`A{i}, i64];" for i in range(N)]
lams = [f"let f{i} = |x: i64| x + {i};" for i in range(N)]
tail = "sys::exit(sys::time::after_idle(duration:10.ms, 0));\nf7(1)\n"
open(f"first{N}.gx", "w").write("\n".join(tds + lams) + "\n" + tail)
open(f"last{N}.gx", "w").write("\n".join(lams + tds) + "\n" + tail)
PY
  for f in first$N last$N; do
    XDG_CACHE_HOME=$D/xdg timeout -s KILL 170 "$G" $f.gx >/dev/null 2>&1
    printf '%s ' $f
    XDG_CACHE_HOME=$D/xdg GRAPHIX_PROFILE=1 timeout -s KILL 170 "$G" $f.gx 2>&1 \
      | grep 'phase=ImageDefs' | grep -o 'self_ns=[0-9]*'
  done
done
rm -rf "$D"
