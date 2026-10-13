#!/bin/sh
# fuzz-gen-b-03: reactive programs bind values the oracle never sees. The
# reactive lane has no observe() (the plain lane prints every binding), and
# the tail reads one fires_per_injection cell plus 0-2 random i64s, so most
# slept-arm selects, cross-cycle builtins, dynamic-reload results and sync
# lets are read by nothing.
# Usage: fuzz-gen-b-03.sh <graphix-fuzz binary>
# Expect: a large "never read again" share in every row.
GF=${1:-graphix-fuzz}
T=$(mktemp -d)
timeout -s KILL 60 "$GF" gen 3000 77 --reactive > "$T/r.txt" 2>/dev/null
python3 -I - "$T/r.txt" <<'PY'
import re, sys
ps = [p for p in open(sys.argv[1]).read().strip().split('\n\n')
      if p.strip() and 'TwinDiverged' not in p]
kinds = {
    'slept-arm select': r'let (v\d+) = select \(in\d % i64:2\)',
    'cross-cycle builtin': r'let (v\d+) = (?:count|uniq|once|take|skip|filter|cast<i64>\(sum)\(',
    'dynamic reload': r'let (v\d+) = select v\d+ \{ error as _ => i64:-1',
    'annotated sync let': r'let (v\d+): [^=]+ = ',
}
for k, rx in kinds.items():
    tot = unread = 0
    for p in ps:
        for m in re.finditer(rx, p):
            tot += 1
            if not re.search(r'\b' + m.group(1) + r'\b', p[m.end():]):
                unread += 1
    print(f"{k}: {tot} bound, never read again: {unread}")
PY
rm -rf "$T"
