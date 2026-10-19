#!/bin/sh
# Title: every outlined loop stores every local in scope into its frame record,
# and every chunk loads them all back: kernel code is quadratic in (loops x locals).
# Command: GRAPHIX=/home/eric/tmp/review-2026-10-19/bin/graphix sh x-perf-compile-outline-frame-stores.sh
# Expected: stores and chunk loads grow linearly with n (a chunk reads only its
#   callback's free locals: here one array).
# Observed (debug build, HEAD 9445a1d21):
#   n=100 body_stores=10900 chunk_loads=208 (per chunk, 100 chunks)
#   n=200 body_stores=41800 chunk_loads=408 (per chunk, 200 chunks)
#   n=400 body_stores=163600 chunk_loads=808; run to first value 0.64 s (n=400) / 2.09 s (n=800) /
#   8.09 s (n=1600), most of it phase=Link (cranelift) under GRAPHIX_PROFILE=1.
G=${GRAPHIX:-graphix}
T=$(mktemp -d)
for n in 100 200 400; do
  python3 - "$n" > "$T/p.gx" <<'PY'
import sys
n = int(sys.argv[1])
print("sys::exit(sys::time::after_idle(duration:1.ms, 0));")
print("let f = |a: Array<i64>| {\n  let b0 = array::map(a, |x| x + 1);")
for i in range(1, n):
    print(f"  let b{i} = array::map(b{i-1}, |x| x + {i});")
print(f"  b{n-1}\n}};")
print("f([1, 2, 3])")
PY
  GRAPHIX_DUMP_CLIF=1 timeout -s KILL 120 "$G" --no-cache "$T/p.gx" > "$T/clif.txt" 2>&1
  python3 - "$n" "$T/clif.txt" <<'PY'
import re, sys
txt = open(sys.argv[2]).read()
fs = sorted([f for f in re.split(r"\n(?=function )", txt) if f.startswith("function")], key=len)
body, chunk = fs[-1], fs[-2]
print(f"n={sys.argv[1]} body_stores={len(re.findall(r'store', body))} chunk_loads={len(re.findall(r'= load', chunk))}")
PY
done
rm -rf "$T"
