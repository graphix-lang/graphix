#!/bin/sh
# Title: kernel_ssa_growth's bounds hold when nothing fuses
# Command: sh design/review-2026-11-16/repro/tests-shell-compiler-b-ssa-growth-vacuous.sh
# The program is scalar_select_chain_has_no_alias_per_join's (N = 100). The
# test asserts only `aliases <= 2 * N`; it never asserts a kernel was built.
# Expected: a run whose `f` builds no kernel is a failure of the test.
# Observed: fused, 99 aliases (bound 200); not fused (here forced with
# --no-fusion, standing in for a de-fuse regression), 0 aliases, and the
# same assertion passes.
GX=${GX:-/home/eric/tmp/review-2026-11-16/bin/graphix}
D=$(mktemp -d)
python3 -I - "$D/p.gx" <<'EOF'
import sys
n = 100
s = "let f = |x: i64| {\n  let a0 = x;\n"
for i in range(1, n):
    s += f"  let a{i} = select a{i-1} {{ v if v > {i} => v - 1, v => v + 2 }};\n"
s += f"  a{n-1}\n}};\nlet r = f(1);\nsys::exit(0)\n"
open(sys.argv[1], "w").write(s)
EOF
for args in "" "--no-fusion"; do
  n=$(GRAPHIX_DUMP_CLIF=1 timeout -s KILL 120 "$GX" --no-cache --no-netidx $args "$D/p.gx" 2>&1 >/dev/null | grep -c ' -> v')
  echo "args [$args]: $n aliases (the test's bound is 200)"
done
rm -rf "$D"
