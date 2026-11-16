#!/bin/sh
# Every call walks each argument's type through its typedefs, even where the
# callee's signature holds no type variable, and the walk is quadratic in the
# typedef's size.
#
# run: sh c-callsite-unread-walk-per-call.sh /path/to/graphix
#
# The program declares a recursive union W of N struct variants and calls
# `h = |w: Any, x: i64| x` 500 times with a value of W; the baseline calls
# `g = |x: i64| x` 500 times beside the same typedef.
#
# expected: the two take about the same time (the signature of `h` has no
#           variable, so no cell can be unread), and doubling N at most
#           doubles the check
# observed (debug build, --check, wall clock):
#   N=150  baseline 0.10s   calls with a W argument 0.80s
#   N=300  baseline 0.10s   calls with a W argument 2.55s
#   N=600  baseline 0.13s   calls with a W argument 9.13s
# gdb samples of the N=600 run are all in CallSite::typecheck0 ->
# for_each_unread -> Carriers::carries -> Type::holds -> Verdicts::search_once.
G=${1:-graphix}
D=$(mktemp -d)
gen() { # N kind
python3 - "$1" "$2" <<'PY'
import sys
n, kind = int(sys.argv[1]), sys.argv[2]
out = []
for i in range(n):
    fields = ", ".join(f"f{j}: [i64, string, null]" for j in range(20))
    out.append(f"type S{i} = {{{fields}, kids: Array<W>}};")
out.append("type W = [" + ", ".join(f"`V{i}(S{i})" for i in range(n)) + ", `Leaf];")
out.append("let w: W = `Leaf;")
out.append("let h = |w: Any, x: i64| x;")
out.append("let g = |x: i64| x;")
for i in range(500):
    out.append(f"h(w, {i});" if kind == "w" else f"g({i});")
out.append("0")
print("\n".join(out))
PY
}
for n in 150 300 600; do
  for kind in g w; do
    gen $n $kind > $D/p.gx
    s=$(date +%s%N)
    timeout -s KILL 170 "$G" --no-cache --check $D/p.gx >/dev/null 2>&1
    rc=$?
    e=$(date +%s%N)
    echo "N=$n kind=$kind rc=$rc $(( (e - s) / 1000000 ))ms"
  done
done
rm -rf $D
