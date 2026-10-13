#!/bin/sh
# x-fix-fusion-02: fused kernel frames grow with region size and have no stack
# probes: a 40k-op pure body overflows the worker stack inside JIT code.
#
# HelperRefs::call makes a fresh 16-byte ExplicitSlot for every pair argument
# and pair return, so a decimal `+` costs ~64 bytes of frame (n=2000: 8017
# explicit_slot, 128272 bytes under GRAPHIX_DUMP_CLIF=1). At n=40000 the one
# fused region's frame (~2.5 MB) exceeds the tokio worker's 2 MB stack, and
# enable_probestack is off, so the prologue moves rsp past the guard page.
#
# command: sh x-fix-fusion-02.sh <graphix binary> [n=40000]
# expected: both runs print 80001.0 (the node-walk does; it is killed by the
#           timeout afterwards, rc=137)
# observed (fused): "thread 'tokio-rt-worker' has overflowed its stack /
#           fatal runtime error: stack overflow, aborting", rc=134; the core's
#           faulting frames are JIT code called from
#           fusion::par_loop::with_par_loan <- FusedKernel::update.
#           n=8000 and n=2000 fused print the right value.
G=${1:-graphix}; N=${2:-40000}; F=$(mktemp --suffix=.gx)
python3 - "$N" "$F" <<'PY'
import sys
n, out = int(sys.argv[1]), sys.argv[2]
lines = ["let f = |d: decimal, k: decimal| {", "  let v0 = d;"]
lines += [f"  let v{i} = v{i-1} + k;" for i in range(1, n + 1)]
lines += [f"  v{n}", "};", "f(decimal:1.0, decimal:2.0)"]
open(out, "w").write("\n".join(lines) + "\n")
PY
echo "--- node-walk"; timeout -s KILL 90 "$G" --no-cache --no-fusion "$F"; echo "rc=$?"
echo "--- fused";     timeout -s KILL 170 "$G" --no-cache "$F"; echo "rc=$?"
rm -f "$F"
