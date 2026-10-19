#!/bin/sh
# x-rerun-6 (residue of x-perf-compile-01/06): every outlined loop still stores
# every local in scope into the shared frame record (outline.rs emit_outlined,
# the `for (i, vv) in locals` loop), and its chunk loads every one back
# (emit_chunk), whether the loop's body reads it or not. A chain of n
# array::map lets in one function therefore emits n^2 / 2 store pairs.
# command: GRAPHIX=/path/to/graphix sh x-rerun-6-outline-stores-quadratic.sh
# expected: store count linear in n, like calls and stack-slot bytes
# observed (HEAD 9445a1d21, debug build): n=80 6478 stores, n=160 25758
#   (x4 per doubling); calls 403 -> 803, slot bytes 1376 -> 2656 (linear);
#   wall time with fusion 0.31 s (n=200), 0.73 s (400), 2.1 s (800), vs
#   0.16 / 0.28 / 0.36 s with --no-fusion.
G=${GRAPHIX:-graphix}
T=$(mktemp -d)
for n in 40 80 160; do
  python3 -I - "$n" > "$T/p.gx" <<'PY'
import sys
n = int(sys.argv[1])
print("let f = |a: Array<i64>| {\n  let b0 = array::map(a, |x| x + 1);")
for i in range(1, n):
    print(f"  let b{i} = array::map(b{i-1}, |x| x + {i});")
print(f"  b{n-1}\n}};")
print("sys::exit(sys::time::after_idle(duration:1.ms, 0));")
print("f([1, 2, 3])")
PY
  GRAPHIX_DUMP_CLIF=1 timeout -s KILL 120 "$G" --no-cache "$T/p.gx" > "$T/clif.txt" 2>&1
  python3 -I - "$n" "$T/clif.txt" <<'PY'
import re, sys
txt = open(sys.argv[2]).read()
fs = [f for f in re.split(r"\n(?=function )", txt) if f.startswith("function")]
f = max(fs, key=len)
print(f"n={sys.argv[1]} stores={len(re.findall(r'store.i64', f))} calls={len(re.findall(r'call ', f))} slot_bytes={sum(int(x) for x in re.findall(r'= explicit_slot (\d+)', f))}")
PY
done
rm -rf "$T"
