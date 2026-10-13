#!/bin/sh
# fuzz-gen-b-01: generated programs that the oracle demotes to Excluded only
# because exprs.rs::try_format_round_trip emitted an async reader
# (json/toml/pack::read) into a program that also holds a `<-`.
# Usage: fuzz-gen-b-01.sh <graphix-fuzz binary>
# Expect: both repro programs print "EXCLUDED", and the census shows ~12% of
# plain and ~7% of reactive programs excluded, all through the round trip.
GF=${1:-graphix-fuzz}
D=$(dirname "$0")
timeout -s KILL 170 "$GF" check "$D/fuzz-gen-b-01-plain.gx" 2>/dev/null | tail -1
timeout -s KILL 170 "$GF" check "$D/fuzz-gen-b-01-reactive.gx" 2>/dev/null | tail -1
T=$(mktemp -d)
timeout -s KILL 60 "$GF" gen 20000 7 > "$T/plain.txt" 2>/dev/null
timeout -s KILL 60 "$GF" gen 3000 77 --reactive > "$T/reactive.txt" 2>/dev/null
python3 -I - "$T/plain.txt" "$T/reactive.txt" <<'PY'
import re, sys
for fn in sys.argv[1:]:
    ps = [p for p in open(fn).read().strip().split('\n\n') if p.strip()]
    rt = [p for p in ps if re.search(r'(json|toml|pack)::read', p)]
    other = [p for p in ps if re.search(r'\b(sys|http|xls|db|sqlite)::', p)]
    fcs = ["count(", "sum(", "product(", "mean(", "min(", "max(", "all(", "and(", "or(",
           "any(", "queue(", "take(", "skip(", "window(", "iterq", "iter(", "group(", "hold("]
    exc = [p for p in rt if '<-' in p or any(m in p for m in fcs)]
    print(f"{fn}: {len(ps)} programs, round-trip {len(rt)}, other async {len(other)}, excluded {len(exc)} ({100*len(exc)/len(ps):.1f}%)")
PY
rm -rf "$T"
