#!/bin/sh
# Oracle tier of generated programs by size, using oracle_tier's markers
# (graphix-fuzz/src/lib.rs). Usage: fuzz-gen-a-03-tier-by-size.sh <graphix-fuzz binary>
# At HEAD 7324207f, seed 99, 3000 programs: >=8k chars -> 5% exact, 3% final,
# 92% excluded; 3-8k -> 27/19/54.
set -e
F=${1:-graphix-fuzz}
T=$(mktemp)
timeout -s KILL 100 "$F" gen 3000 99 > "$T" 2>/dev/null
timeout 60 python3 -I - "$T" <<'PY'
import sys, collections
t = open(sys.argv[1]).read()
ch = [c for c in t.split('\n\n') if c.strip()]
asyncm = ['sys::','http::','json::read','toml::read','pack::read','xls::','db::','sqlite::']
fc = ['count(','sum(','product(','mean(','min(','max(','all(','and(','or(','any(',
      'queue(','take(','skip(','window(','iterq','iter(','group(','hold(']
def tier(c):
    if any(m in c for m in asyncm):
        return 'X' if ('<-' in c or any(m in c for m in fc)) else 'F'
    return 'E'
bk = collections.defaultdict(collections.Counter)
for c in ch:
    L = len(c)
    b = '<1k' if L < 1000 else '1-3k' if L < 3000 else '3-8k' if L < 8000 else '>=8k'
    bk[b][tier(c)] += 1
for b in ['<1k','1-3k','3-8k','>=8k']:
    c = bk[b]; n = sum(c.values())
    print(b, n, 'exact %.0f%% final %.0f%% excluded %.0f%%' %
          (100*c['E']/n, 100*c['F']/n, 100*c['X']/n))
PY
rm -f "$T"
