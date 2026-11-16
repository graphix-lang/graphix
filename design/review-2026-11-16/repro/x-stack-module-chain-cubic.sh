#!/bin/bash
# --check of a chain of nested module files is cubic in its length
# run: bash x-stack-module-chain-cubic.sh 1000
# each file m.gx says `mod m; let k = m::k`, its module in m/m.gx, and so on
# expected: time linear in the number of files
# observed (debug build): 250 files: 0.14 s, 500: 0.84 s, 1000: 6.0 s.
#   gdb samples sit in resolver::LoadChain::push: chain_source canonicalizes
#   the new file (a realpath over a path as long as the chain) and the cycle
#   check compares it with every source on the chain by Source::eq, a path
#   compare as long as the chain.
n=${1:-1000}
d=$(mktemp -d)
python3 - "$n" "$d" <<'PY'
import os, sys
n=int(sys.argv[1]); out=sys.argv[2]
open(os.path.join(out,'main.gx'),'w').write("mod m;\nlet r = m::k;\nr\n")
at=out
for i in range(n):
    last = i + 1 == n
    open(os.path.join(at,'m.gx'),'w').write("let k = 1" if last else "mod m;\nlet k = m::k")
    at=os.path.join(at,'m'); os.makedirs(at, exist_ok=True)
PY
cd "$d" && time timeout -s KILL 170 /home/eric/tmp/review-2026-11-16/bin/graphix --check main.gx
echo "exit $?"
rm -rf "$d"
