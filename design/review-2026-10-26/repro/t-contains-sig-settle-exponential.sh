#!/bin/sh
# Title: SigPairs::settle backtracks over every permutation of every
#        deferred union, so a signature with a few open unions that cannot
#        pair takes (n!)^k attempts; four 6-member unions do not finish.
# Command: sh design/review-2026-10-26/repro/t-contains-sig-settle-exponential.sh
# Expected: a prompt refusal ("signature has string, implementation has f64").
# Observed: with K=3 the refusal takes ~7 s (debug build); with K=4 (default)
#   the check is still running when the 150 s timeout kills it (exit 137).
G=${G:-/home/eric/tmp/review-2026-10-26/bin/graphix}
K=${K:-4}
d=$(mktemp -d)
printf 'mod inner;\n0\n' > "$d/main.gx"
python3 - "$K" "$d" <<'PY'
import sys
k, d = int(sys.argv[1]), sys.argv[2]
def u(p, last): return "[" + ", ".join(f"'{p}{i}" for i in range(5)) + f", {last}]"
sig = [f"x{j}: {u(chr(97+j), 'i64' if j else 'string')}" for j in range(k)]
imp = [f"x{j}: {u(chr(97+j), 'i64' if j else 'f64')}" for j in range(k)]
open(f"{d}/inner.gxi", "w").write(f"val f: fn({', '.join(sig)}) -> i64\n")
open(f"{d}/inner.gx", "w").write(f"let f = |{', '.join(imp)}| -> i64 0\n")
PY
timeout -s KILL 150 "$G" --check "$d/main.gx" >/dev/null 2>&1; echo "exit $?"
rm -r "$d"
