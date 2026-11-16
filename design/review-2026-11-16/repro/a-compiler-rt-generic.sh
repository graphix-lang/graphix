#!/bin/bash
# Title: the `R: Rt` / `X: GXExt` parameters have one production instantiation,
#        yet the whole node graph is generic over them and is compiled downstream.
# Command: bash design/review-2026-11-16/repro/a-compiler-rt-generic.sh
# Expected: an extension point with one implementation costs nothing.
# Observed (HEAD e1be2fbf2, debug `graphix`, 2026-11-16):
#   impls of Rt: 1 (graphix-rt/src/rt.rs: impl<X: GXExt> Rt for GXRt<X>)
#   impls of GXExt: NoExt (graphix-rt/src/lib.rs) and a test Probe
#   `R: Rt` lines: 1113 (207 in stdlib/)
#   text bytes 49941334 syms 190315 mentioning GXRt/NoExt 7338181 23572 14.7%
cd "$(git rev-parse --show-toplevel)" || exit 1
echo "impls of Rt:"; git grep -nE "Rt for " -- '*.rs'
echo "impls of GXExt:"; git grep -nE "GXExt for " -- '*.rs'
echo "R: Rt lines: $(git grep -n 'R: Rt' -- '*.rs' | wc -l) ($(git grep -n 'R: Rt' -- stdlib | wc -l) in stdlib/)"
BIN=${1:-/home/eric/tmp/review-2026-11-16/bin/real/graphix}
timeout -s KILL 120 nm -C --size-sort -S "$BIN" 2>/dev/null | timeout -s KILL 60 python3 -c '
import sys
tot=gx=n=ngx=0
for l in sys.stdin:
    p=l.split(" ",3)
    if len(p)<4 or p[2] not in "tTwW": continue
    sz=int(p[1],16); tot+=sz; n+=1
    if "GXRt" in p[3] or "NoExt" in p[3]: gx+=sz; ngx+=1
print("text bytes",tot,"syms",n,"mentioning GXRt/NoExt",gx,ngx,f"{100*gx/tot:.1f}%")'
