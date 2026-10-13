#!/bin/sh
# x-fix-fusion-04: writing the program image costs memory quadratic in a chain of
# nested array lets (`let x{i} = [x{i-1}]`), though design/program_image.md says the
# writer's walk and memo are linear (commit 04863920). Peak RSS, debug build:
#   N      --no-cache   with cache (program entry written)   extra
#   1000   162 MB       290 MB                                128 MB
#   2000   449 MB       950 MB                                501 MB
#   4000  1447 MB      3435 MB                               1988 MB
# GRAPHIX_PROFILE=1's ImageEncode phase: 0.18 s / 0.52 s / 2.26 s.
# usage: x-fix-fusion-04.sh <graphix binary> [N=2000]
G=${1:-graphix}; N=${2:-2000}; D=$(mktemp -d); F=$D/chain.gx
python3 - "$N" "$F" <<'PY'
import sys
n, out = int(sys.argv[1]), sys.argv[2]
lines = ["let x0 = 1;"] + [f"let x{i} = [x{i-1}];" for i in range(1, n + 1)]
lines += ["sys::exit(sys::time::after_idle(duration:100.ms, 0));", f"array::len(x{n})"]
open(out, "w").write("\n".join(lines) + "\n")
PY
for args in "--no-cache" ""; do
  XDG_CACHE_HOME=$D/cache python3 -c '
import resource, subprocess, sys
subprocess.run(sys.argv[1:], stdout=subprocess.DEVNULL)
print(sys.argv[2:-1], resource.getrusage(resource.RUSAGE_CHILDREN).ru_maxrss // 1024, "MB")
' "$G" $args "$F"
done
rm -rf "$D"
