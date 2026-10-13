#!/bin/sh
# CLIF size of one kernel holding a chain of n array::map lets, per n.
# Each loop's abort path drops every owned local bound so far
# (call.rs emit_pending_cleanup), and each outlined loop stores every local
# into a frame record of its own (outline.rs stack_record): lines, helper
# calls and explicit stack-slot bytes grow quadratically.
# HEAD: n=10: ~1.5k lines, 1840 slot bytes; n=40: ~10k lines, 16960 bytes;
# n=400: ~605k lines, 1.3 MB of stack slots.
G=${GRAPHIX:-graphix}
T=$(mktemp -d)
for n in 10 20 40; do
  python3 - "$n" > "$T/p.gx" <<'PY'
import sys
n = int(sys.argv[1])
print("let f = |a: Array<i64>| {\n  let b0 = array::map(a, |x| x + 1);")
for i in range(1, n):
    print(f"  let b{i} = array::map(b{i-1}, |x| x + {i});")
print(f"  b{n-1}\n}};")
print("sys::exit(sys::time::after_idle(duration:1.ms, 0));")
print("f([1, 2, 3])")
PY
  GRAPHIX_DUMP_CLIF=1 timeout -s KILL 60 "$G" --no-cache "$T/p.gx" > "$T/clif.txt" 2>&1
  python3 - "$n" "$T/clif.txt" <<'PY'
import re, sys
txt = open(sys.argv[2]).read()
fs = [f for f in re.split(r"\n(?=function )", txt) if f.startswith("function")]
f = max(fs, key=len)
slots = sum(int(x) for x in re.findall(r"= explicit_slot (\d+)", f))
print(f"n={sys.argv[1]} lines={len(f.splitlines())} calls={len(re.findall(r'call ', f))} slot_bytes={slots}")
PY
done
rm -rf "$T"
