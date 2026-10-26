#!/bin/bash
# Title: most main-scope imports the module generator emits are never read
# Shape: generate/modules.rs gen_module, the `use m<i>::k<i>` and `use m<i>::f<j> as v<N>` arms:
#   the imported name is pushed into the vocabulary but nothing makes a later slot reference it.
# Command: bash design/review-2026-10-26/repro/fuzz-gen-b-unused-imports.sh
# Expected: an emitted import is read at least most of the time, so the lookup through it is tested.
# Observed (seed 1234, the first 100 module-bearing programs of `gen 2000`):
#   main-scope item imports: 63 unused: 50   (each unused one draws an "unused import `vN`" warning)
set -u
BIN=/home/eric/tmp/review-2026-10-26/bin/graphix-fuzz
W=$(mktemp -d)
timeout -s KILL 60 $BIN gen 2000 1234 > $W/g.txt 2>/dev/null
python3 -I - "$W" <<'PY'
import sys, os
d = sys.argv[1]
ps = [p for p in open(d + "/g.txt").read().split("\n\n") if p.strip() and "file-v1: m" in p]
for i, p in enumerate(ps[:100]):
    open(f"{d}/c{i}.gx", "w").write(p.strip() + "\n")
PY
tot=0; unused=0
for f in $W/c*.gx; do
  a=$(grep -oE "use m[0-9]+::(f[0-9]+|k[0-9]+) as v[0-9]+|use m[0-9]+::k[0-9]+" $f | wc -l); tot=$((tot+a))
  u=$(timeout -s KILL 60 $BIN run $f 2>&1 | grep -E "unused import \`(v|k)[0-9]+\`" | sort -u | wc -l); unused=$((unused+u))
done
echo "main-scope item imports: $tot unused: $unused"
rm -rf $W
