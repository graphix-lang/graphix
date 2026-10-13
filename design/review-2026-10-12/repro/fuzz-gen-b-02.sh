#!/bin/sh
# fuzz-gen-b-02: the only or-pattern with binds the generator emits
# (patterns.rs::general_select's `(l0, x) | (x, l1)`) never appears, and if it
# did, a following `(l0, _)` arm would be refused (fuzz-gen-b-02.gx).
# Usage: fuzz-gen-b-02.sh <graphix-fuzz binary> <graphix binary>
# Expect: "bound or-arms: 0" and an "unreachable arm" refusal.
GF=${1:-graphix-fuzz}; GX=${2:-graphix}
T=$(mktemp -d)
timeout -s KILL 60 "$GF" gen 20000 7 > "$T/p.txt" 2>/dev/null
python3 -I - "$T/p.txt" <<'PY'
import re, sys
t = open(sys.argv[1]).read()
print("bound or-arms:", len(re.findall(r'\(\w+:-?\d+, (\w+)\) \| \(\1, ', t)))
PY
rm -rf "$T"
timeout -s KILL 30 "$GX" --check "$(dirname "$0")/fuzz-gen-b-02.gx" 2>&1 | tail -1
