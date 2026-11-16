#!/bin/bash
# Title: the generator's whole-composite consumers (struct functional update, array slice, list tail ladder)
#        need a visible binding of exactly the requested composite type, which independent random_type
#        draws almost never produce.
# Command: GRAPHIX_FUZZ=<graphix-fuzz binary> bash design/review-2026-11-16/repro/fuzz-gen-a-composite-consumers-unreached.sh
# Expected: each form appears in some generated programs (exprs.rs: gen_typed's Struct arm, try_accessor's
#           Array slice and List tail arms).
# Observed (gen 6000 1, 6000 programs): struct functional update `{ v with f: .. }` 0,
#           array slice `v[..1]$` etc. 9, list tail ladder `[<h1, tl0..>]` 5.
F=${GRAPHIX_FUZZ:-graphix-fuzz}
out=$(mktemp)
timeout -s KILL 120 "$F" gen 6000 1 > "$out"
python3 -I - "$out" <<'PY'
import re, sys
progs = [p for p in open(sys.argv[1]).read().split("\n\n") if p.strip()]
def count(rx): return sum(1 for p in progs if re.search(rx, p))
print(len(progs), "programs")
print("struct functional update", count(r'\{ v\d+ with [a-z]+: '))
print("array slice", count(r'v\d+\[(\.\.1|1\.\.|-1\.\.|\.\.-1|-3\.\.-1|u8:1\.\.|i16:1\.\.|u32:1\.\.)\]\$'))
print("list tail ladder", count(r'\[<h1, tl0\.\.>\]'))
PY
rm -f "$out"
