#!/usr/bin/env bash
# f-jit-06: Emission::join interns a compile task's layout lists before it
# maps their kernel keys through `same` (graphix-compiler/src/fusion/emit/jit.rs,
# Emission::join), so a task body with an external call site (layout != 0)
# rekeys to a layout no parent region has: it is never `moved`, and every task
# that reached it compiles its own copy.
#
# The program: 20 array elements `g(x + i) + fact((x + i) % 12)`, where g calls
# h. Under the default walk each element fuses in its own compile task; under
# GRAPHIX_FUSE_SERIAL=1 they fuse in order on one context. Each run writes a
# program image; every compiled body is one record there, and a record carries
# its kernel's name twice (label and kernel signature).
#
# command: GRAPHIX=/path/to/graphix bash design/review-2026-10-05/repro/f-jit-06.sh
#
# expected: the same counts in both walks (CLAUDE.md: the join makes "the
#   decisions and the output ... the serial walk's").
# observed (HEAD c722befe, debug build):
#   default: gqgqgq=87 hqhqhq=14 fqfqfq=52
#   serial:  gqgqgq=49 hqhqhq=14 fqfqfq=52
#   i.e. 19 extra records of g (2 names each). h and fact, which call no
#   other kernel (layout 0), dedupe. GRAPHIX_DBG_KERNELS=1 prints 20
#   "KERNEL DEFINED gqgqgq" in the default walk and 1 in the serial one; both
#   print the same values.
set -u
GRAPHIX=${GRAPHIX:-graphix}
dir=$(mktemp -d)
trap 'rm -rf "$dir"' EXIT

python3 - "$dir/gh.gx" <<'EOF'
import sys
n = 20
s = "let x = sys::time::after_idle(duration:1.ms, 3);\n"
s += "let hqhqhq = |a: i64| a * 3 + 1;\n"
s += "let gqgqgq = |a: i64| hqhqhq(a) + hqhqhq(a + 1);\n"
s += "let rec fqfqfq = |n: i64| -> i64 select n { 0 => 1, n => n * fqfqfq(n - 1) };\n"
els = ["count(x)"] + [f"gqgqgq(x + {i}) + fqfqfq((x + {i}) % 12)" for i in range(n)]
s += "let r = [" + ", ".join(els) + "];\n"
s += "sys::exit(sys::time::after_idle(duration:100.ms, 0));\n"
s += "r\n"
open(sys.argv[1], "w").write(s)
EOF

run() {
  local cache="$dir/cache-$1"
  env XDG_CACHE_HOME="$cache" "${@:2}" timeout -s KILL 120 "$GRAPHIX" --no-netidx \
    "$dir/gh.gx" > "$dir/out-$1" 2> "$dir/err-$1"
  echo "== $1: exit $?, value $(tail -1 "$dir/out-$1" | cut -c1-60)..."
  for img in $(find "$cache" -name '*.img'); do
    if grep -a -q gqgqgq "$img"; then
      echo "   program image $(stat -c %s "$img") bytes:" \
        "gqgqgq=$(grep -a -o gqgqgq "$img" | wc -l)" \
        "hqhqhq=$(grep -a -o hqhqhq "$img" | wc -l)" \
        "fqfqfq=$(grep -a -o fqfqfq "$img" | wc -l)"
    fi
  done
}

run default
run serial GRAPHIX_FUSE_SERIAL=1
