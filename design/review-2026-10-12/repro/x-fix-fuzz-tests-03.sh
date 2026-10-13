#!/bin/bash
# x-fix-fuzz-tests-03: a check-one death that the plain run-one re-runs do
# not reproduce reads as AGREE (died() in graphix-fuzz/src/lib.rs), with the
# image-session and forked comparisons never made.
#
# The program allocates one 576 MB array. Under a 2 GB address-space cap each
# plain run (run-one interp, run-one jit) traces, but check-one, which runs
# its runs concurrently in one process under one cap, aborts with
# "memory allocation of 576000024 bytes failed". died() sees an address-space
# abort below the cap, re-runs interp and jit alone, both trace, and returns
# PoolResult::Agree. The same path hides a runaway confined to a warm/cold
# image session or a forked Par/JitPar run.
#
# Usage: x-fix-fuzz-tests-03.sh <graphix-fuzz binary>
# Expected: a CRASH (or the owed comparisons made); observed:
#   run-one interp: trace / run-one jit: trace
#   check-one: exit 134, "memory allocation of 576000024 bytes failed", no verdict
#   check-isolated: AGREE
set -u
F=${1:-graphix-fuzz}
D=$(mktemp -d)
printf 'let a = array::init(6000, |i: i64| i);\nlet b = array::flatten(array::map(a, |x: i64| a));\narray::len(b)\n' > "$D/big.gx"
CAP=2147483648
for m in interp jit; do
  GRAPHIX_FUZZ_SANDBOXED=1 GRAPHIX_FUZZ_MEM_LIMIT=$CAP timeout -s KILL 170 "$F" run-one "$D/v.$m" $m < "$D/big.gx" >/dev/null 2>&1
  echo "run-one $m: $(cat "$D/v.$m" 2>/dev/null)"
done
GRAPHIX_FUZZ_SANDBOXED=1 GRAPHIX_FUZZ_MEM_LIMIT=$CAP timeout -s KILL 170 "$F" check-one "$D/v.check" < "$D/big.gx" >/dev/null 2>"$D/err"
echo "check-one: exit $? verdict='$(cat "$D/v.check" 2>/dev/null)' $(grep -m1 'memory allocation' "$D/err")"
echo "check-isolated: $(GRAPHIX_FUZZ_MEM_LIMIT=$CAP timeout -s KILL 175 "$F" check-isolated "$D/big.gx" 2>/dev/null | tail -1)"
rm -rf "$D"
