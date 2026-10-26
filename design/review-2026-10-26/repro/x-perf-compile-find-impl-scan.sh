#!/bin/sh
# find_impl's abstract fast path never fires: an impl's target is stored as the
# typedef reference (`impl Show for C0` -> Type::Ref), so every lookup contains-
# checks the query against every impl of the trait, expanding the head's ref
# both ways: O(calls x impls) ref expansions.
# Command: sh x-perf-compile-find-impl-scan.sh
# Expected: --check of N calls to N impls costs about N calls to one impl plus
#   N impls registered (0.24 s + 0.92 s at N=3200).
# Observed (debug, HEAD e6ffc6eed, --no-cache --check):
#   N=800 0.36 s, N=1600 1.62 s, N=3200 5.84 s; gdb samples sit in
#   find_impl_with -> contains_int -> expand_ref -> lookup_ref_by, reached from
#   trait_contains (the call's bound check).
GX=${GX:-/home/eric/tmp/review-2026-10-26/bin/graphix}
D=$(mktemp -d)
for n in 800 1600 3200; do
  python3 - "$n" > "$D/t.gx" <<'PY'
import sys
n = int(sys.argv[1])
s = ["trait Show { val show: fn(self) -> i64 }"]
for i in range(n):
    s.append(f"type C{i} = Abstract<i64>")
    s.append(f"impl Show for C{i} {{ let show = |c| c.0 + {i} }}")
s.append("let r = [" + ", ".join(f"Show::show(C{i}({i}))" for i in range(n)) + "]")
s.append("array::len(r)")
print(";\n".join(s))
PY
  t0=$(date +%s%N); timeout -s KILL 170 "$GX" --no-cache --check "$D/t.gx"; rc=$?; t1=$(date +%s%N)
  echo "n=$n rc=$rc $(( (t1 - t0) / 1000000 )) ms"
done
rm -rf "$D"
