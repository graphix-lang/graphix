#!/bin/sh
# Title: the check of `?` (and of `$` over a union holding null and an error)
# costs time quadratic in the operand union's member count: Strip::taken is
# `typ.diff(typ - error)`, a difference of an n-member union by an
# (n-1)-member one, and `?` computes it at every site.
# Command: sh c-error-op-strip-taken-quadratic.sh
# Expected: `?`, `$` and a plain read of a 400- or 800-member union cost about
#   alike per site, growing linearly with the member count.
# Observed (debug binary, 100 sites, ms; noisy shared machine):
#   plain-400 48   $-400 160   $-400-with-null 1019   ?-400 1204   ?-800 4678
G=${G:-/home/eric/tmp/review-2026-11-16/bin/graphix}
D=$(mktemp -d)
gen() { # members, op, extra-member, file
  python3 - "$1" "$2" "$3" "$4" <<'PY'
import sys
nv, op, extra, path = int(sys.argv[1]), sys.argv[2], sys.argv[3], sys.argv[4]
members = ", ".join(f"`A{i}" for i in range(nv))
lines = [f"type T = [{members}{extra}, Error<`E>];", "let x: T = `A0;", '{ catch(e) println("[e]");']
lines += [f"let y{i} = x{op};" for i in range(100)]
lines.append("y0 }")
open(path, "w").write("\n".join(lines) + "\n")
PY
}
run() {
  s=$(date +%s%N); timeout -s KILL 170 "$G" --check "$2" >/dev/null 2>&1
  echo "$1 $(( ($(date +%s%N) - s) / 1000000 ))ms"
}
gen 400 "" "" "$D/plain.gx"; run plain-400 "$D/plain.gx"
gen 400 '$' "" "$D/d.gx"; run '$-400' "$D/d.gx"
gen 400 '$' ", null" "$D/dn.gx"; run '$-400-with-null' "$D/dn.gx"
gen 400 '?' "" "$D/q.gx"; run '?-400' "$D/q.gx"
gen 800 '?' "" "$D/q8.gx"; run '?-800' "$D/q8.gx"
rm -rf "$D"
