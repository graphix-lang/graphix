#!/bin/bash
# The build of a chain of generic pair-constructor calls is quadratic in the
# chain while its check is linear. deep_nesting.rs's
# generic_pair_call_chains_check_in_linear_time holds that build "where it
# stands" and names call-dag-arg-walk-01 for it, a CR no longer in the tree,
# so the limitation is tracked nowhere.
#
# Run:  bash design/review-2026-11-16/repro/tests-shell-compiler-a-pair-chain-elab-quadratic.sh
# Observed (debug binary at e1be2fbf2; an empty program costs 0.12 s):
#   links  --check   --expand --no-fusion
#     640   0.14 s    0.30 s
#    1280   0.16 s    0.89 s
#    2560   0.21 s    3.33 s       (x4.2 extra per doubling: quadratic)
GX=${GX:-/home/eric/tmp/review-2026-11-16/bin/graphix}
T=$(mktemp -d)
export XDG_CACHE_HOME=$T/cache
TIMEFORMAT=%U
for n in 640 1280 2560; do
  python3 -c "
n=$n
s='let mk = |f| (f, f);\nlet x0 = 1;\n'
for i in range(1, n + 1): s += f'let x{i} = mk(x{i-1});\n'
print(s + '0')" > $T/p.gx
  c=$( { time timeout -s KILL 170 "$GX" --check --no-cache $T/p.gx >/dev/null 2>&1; } 2>&1 )
  e=$( { time timeout -s KILL 170 "$GX" --expand --no-fusion --no-cache $T/p.gx >/dev/null 2>&1; } 2>&1 )
  echo "links=$n check ${c}s  expand --no-fusion ${e}s"
done
rm -rf "$T"
